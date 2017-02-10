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

    /// Provides simple services for checking and compiling F# scripts
    type public SimpleSourceCodeServices(?msbuildEnabled) =

        let checker = FSharpChecker.Create(?msbuildEnabled=msbuildEnabled)
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
