// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.Compiler.SimpleSourceCodeServices

    open System
    open System.IO
    open System.Text
    open System.Reflection.Emit
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open Microsoft.FSharp.Compiler.Driver
    open Microsoft.FSharp.Compiler
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.CompileOps
    open Microsoft.FSharp.Compiler.ErrorLogger
    open Microsoft.FSharp.Compiler.AbstractIL
    open Microsoft.FSharp.Compiler.AbstractIL.IL

    [<AutoOpen>]
    module private Utils =

        let buildFormatComment (xmlCommentRetriever: string * string -> string) cmt (sb: StringBuilder) =
            match cmt with
            | FSharpXmlDoc.Text(s) -> sb.AppendLine(s) |> ignore
            | FSharpXmlDoc.XmlDocFileSignature(file, signature) ->
                let comment = xmlCommentRetriever (file, signature)
                if (not (comment.Equals(null))) && comment.Length > 0 then sb.AppendLine(comment) |> ignore
            | FSharpXmlDoc.None -> ()

        let buildFormatElement isSingle el (sb: StringBuilder) xmlCommentRetriever =
            match el with
            | FSharpToolTipElement.None -> ()
            | FSharpToolTipElement.Single(it, comment) ->
                sb.AppendLine(it) |> buildFormatComment xmlCommentRetriever comment
            | FSharpToolTipElement.SingleParameter(it, comment, _) ->
                sb.AppendLine(it) |> buildFormatComment xmlCommentRetriever comment
            | FSharpToolTipElement.Group(items) ->
                let items, msg =
                  if items.Length > 10 then
                    (items |> Seq.take 10 |> List.ofSeq),
                      sprintf "   (+%d other overloads)" (items.Length - 10)
                  else items, null
                if isSingle && items.Length > 1 then
                  sb.AppendLine("Multiple overloads") |> ignore
                for (it, comment) in items do
                  sb.AppendLine(it) |> buildFormatComment xmlCommentRetriever comment
                if msg <> null then sb.AppendFormat(msg) |> ignore
            | FSharpToolTipElement.CompositionError(err) ->
                sb.Append("Composition error: " + err) |> ignore

        // Convert ToolTipText to string
        let formatTip tip xmlCommentRetriever =
            let commentRetriever = defaultArg xmlCommentRetriever (fun _ -> "")
            let sb = new StringBuilder()
            match tip with
            | FSharpToolTipText([single]) -> buildFormatElement true single sb commentRetriever
            | FSharpToolTipText(its) -> for item in its do buildFormatElement false item sb commentRetriever
            sb.ToString().Trim('\n', '\r')

    /// Represents a declaration returned by GetDeclarations
    type SimpleDeclaration internal (name: string, description: unit -> string) = 
        /// Get the name of a declaration
        member x.Name = name
        /// Compute the description for a declaration
        member x.GetDescription() = description()

    /// Represents the results of type checking
    type SimpleCheckFileResults(info: Microsoft.FSharp.Compiler.SourceCodeServices.ParseFileResults,
                                results:Microsoft.FSharp.Compiler.SourceCodeServices.CheckFileResults,
                                source: string[]) = 

        let identToken = FSharpTokenTag.Identifier
        let hasChangedSinceLastTypeCheck _ = false

        /// Return the errors resulting from the type-checking
        member x.Errors = results.Errors

        /// Get the declarations at the given code location.
        member x.GetDeclarationListInfo(line, col, qualifyingNames, partialName, ?xmlCommentRetriever) =
            async { let! items = results.GetDeclarationListInfo(Some info, line, col, source.[int line], qualifyingNames, partialName, hasChangedSinceLastTypeCheck)
                    return [| for i in items.Items -> SimpleDeclaration(i.Name, (fun () -> formatTip i.DescriptionText xmlCommentRetriever)) |] }

        /// Get the Visual Studio F1-help keyword for the item at the given position
        member x.GetF1KeywordAlternate(line, col, names) =
            results.GetF1KeywordAlternate(line, col, source.[int line], names)

        /// Get the data tip text at the given position
        member x.GetToolTipTextAlternate(line, col, names, ?xmlCommentRetriever) =
            async { 
                let! tip = results.GetToolTipTextAlternate(line, col, source.[int line], names, identToken)
                return formatTip tip xmlCommentRetriever
            }

        /// Get the location of the declaration at the given position
        member x.GetDeclarationLocationAlternate(line, col, names, preferSig) =
            results.GetDeclarationLocationAlternate(line, col, source.[int line], names, preferSig)

        /// Get the full type checking results 
        member x.FullResults = results


        // Obsolete
        
        member x.GetF1Keyword(line, col, names) = x.GetF1KeywordAlternate(Line.fromZ line, col, names) |> Async.RunSynchronously
        member x.GetToolTipText(line, col, names, ?xmlCommentRetriever) = x.GetToolTipTextAlternate(Line.fromZ line, col, names, ?xmlCommentRetriever=xmlCommentRetriever) |> Async.RunSynchronously
        member x.GetDeclarationLocation(line, col, names, preferSig) = x.GetDeclarationLocationAlternate(Line.fromZ line, col, names, preferSig) |> Async.RunSynchronously
        member x.GetDataTipText(line, col, names, ?xmlCommentRetriever) = x.GetToolTipText(line, col, names, ?xmlCommentRetriever=xmlCommentRetriever) 
        member x.GetDeclarations(line, col, qualifyingNames, partialName, ?xmlCommentRetriever) = x.GetDeclarationListInfo(Line.fromZ line, col, qualifyingNames, partialName, ?xmlCommentRetriever=xmlCommentRetriever)

    /// Provides simple services for checking and compiling F# scripts
    type public SimpleSourceCodeServices(?msbuildEnabled, ?msbuildVersion) =

        let checker = InteractiveChecker.Create(?msbuildEnabled=msbuildEnabled, ?msbuildVersion=msbuildVersion)
        let fileversion = 0
        let loadTime = DateTime.Now
        let referenceResolver = checker.ReferenceResolver

        /// Tokenize a single line, returning token information and a tokenization state represented by an integer
        member x.TokenizeLine (line: string, state: int64) : FSharpTokenInfo[] * int64 = 
            let tokenizer = FSharpSourceTokenizer([], None)
            let lineTokenizer = tokenizer.CreateLineTokenizer line
            let state = ref (None, state)
            let tokens = 
                [| while (state := lineTokenizer.ScanToken (snd !state); (fst !state).IsSome) do
                       yield (fst !state).Value |]
            tokens, snd !state 

        /// Tokenize an entire file, line by line
        member x.TokenizeFile (source: string) : FSharpTokenInfo[][] = 
            let lines = source.Split('\n')
            let tokens = 
                [| let state = ref 0L
                   for line in lines do 
                         let tokens, n = x.TokenizeLine(line, !state) 
                         state := n 
                         yield tokens |]
            tokens

        /// Return information about matching braces in a single file.
        member x.MatchBracesAlternate (filename, source: string, ?otherFlags) = 
            async { 
                let! options = checker.GetProjectOptionsFromScript(filename, source, loadTime, ?otherFlags=otherFlags)
                return! checker.MatchBracesAlternate(filename, source,  options)
            }

        member x.MatchBraces (filename, source, ?otherFlags) = 
            async { 
                let! options = checker.GetProjectOptionsFromScript(filename, source, loadTime, ?otherFlags=otherFlags)
                return checker.MatchBraces(filename, source,  options)
            } |> Async.RunSynchronously

        [<System.Obsolete("This method has been renamed to ParseAndCheckScript")>]
        member x.TypeCheckScript (filename, source, otherFlags) = 
            x.ParseAndCheckScript (filename, source, otherFlags) 

        /// For errors, quick info, goto-definition, declaration list intellisense, method overload intellisense
        member x.ParseAndCheckScript (filename, source, ?otherFlags) = 
          async { 
            let! options = checker.GetProjectOptionsFromScript(filename, source, loadTime, ?otherFlags=otherFlags)
            // do an typecheck
            let textSnapshotInfo = "" // TODO
            let! parseResults, checkResults = checker.ParseAndCheckFileInProject(filename, fileversion, source, options, IsResultObsolete (fun _ -> false), textSnapshotInfo) 
            // return the info
            match checkResults with 
            | CheckFileAnswer.Aborted -> return! invalidOp "aborted"
            | CheckFileAnswer.Succeeded res -> return SimpleCheckFileResults(parseResults, res, source.Split('\n'))
          }

        member x.ParseAndCheckProject (projectFileName, argv:string[]) = 
            let options = checker.GetProjectOptionsFromCommandLineArgs(projectFileName, argv)
            checker.ParseAndCheckProject(options)

        member x.Compile (argv: string[])  = 
            CompileHelpers.compileFromArgs (argv, referenceResolver,None, None)

        member x.Compile (ast:ParsedInput list, assemblyName:string, outFile:string, dependencies:string list, ?pdbFile:string, ?executable:bool, ?noframework:bool) =
            let noframework = defaultArg noframework false
            CompileHelpers.compileFromAsts (referenceResolver, ast, assemblyName, outFile, dependencies, noframework, pdbFile, executable, None, None)

        member x.CompileToDynamicAssembly (otherFlags: string[], execute: (TextWriter * TextWriter) option)  = 
            CompileHelpers.setOutputStreams execute
            
            // References used to capture the results of compilation
            let tcImportsRef = ref (None: TcImports option)
            let assemblyBuilderRef = ref None
            let tcImportsCapture = Some (fun tcImports -> tcImportsRef := Some tcImports)

            // Function to generate and store the results of compilation 
            let debugInfo =  otherFlags |> Array.exists (fun arg -> arg = "-g" || arg = "--debug:+" || arg = "/debug:+")
            let dynamicAssemblyCreator = Some (CompileHelpers.createDynamicAssembly (debugInfo, tcImportsRef, execute.IsSome, assemblyBuilderRef))

            // Perform the compilation, given the above capturing function.
            let errorsAndWarnings, result = CompileHelpers.compileFromArgs (otherFlags, referenceResolver, tcImportsCapture, dynamicAssemblyCreator)

            // Retrieve and return the results
            let assemblyOpt = 
                match assemblyBuilderRef.Value with 
                | None -> None
                | Some a ->  Some (a :> System.Reflection.Assembly)

            errorsAndWarnings, result, assemblyOpt

        member x.CompileToDynamicAssembly (asts:ParsedInput list, assemblyName:string, dependencies:string list, execute: (TextWriter * TextWriter) option, ?debug:bool, ?noframework:bool) =
            CompileHelpers.setOutputStreams execute

            // References used to capture the results of compilation
            let tcImportsRef = ref (None: TcImports option)
            let assemblyBuilderRef = ref None
            let tcImportsCapture = Some (fun tcImports -> tcImportsRef := Some tcImports)

            let debugInfo = defaultArg debug false
            let noframework = defaultArg noframework false
            let location = Path.Combine(Path.GetTempPath(),"test"+string(hash assemblyName))
            try Directory.CreateDirectory(location) |> ignore with _ -> ()

            let outFile = Path.Combine(location, assemblyName + ".dll")

            // Function to generate and store the results of compilation 
            let dynamicAssemblyCreator = Some (CompileHelpers.createDynamicAssembly (debugInfo, tcImportsRef, execute.IsSome, assemblyBuilderRef))

            // Perform the compilation, given the above capturing function.
            let errorsAndWarnings, result = 
                CompileHelpers.compileFromAsts (referenceResolver, asts, assemblyName, outFile, dependencies, noframework, None, Some execute.IsSome, tcImportsCapture, dynamicAssemblyCreator)

            // Retrieve and return the results
            let assemblyOpt = 
                match assemblyBuilderRef.Value with 
                | None -> None
                | Some a ->  Some (a :> System.Reflection.Assembly)

            errorsAndWarnings, result, assemblyOpt
