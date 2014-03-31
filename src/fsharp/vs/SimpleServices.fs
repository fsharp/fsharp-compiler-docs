
namespace Microsoft.FSharp.Compiler.SimpleSourceCodeServices

    open System
    open System.IO
    open System.Text
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open Microsoft.FSharp.Compiler.Driver
    open Microsoft.FSharp.Compiler
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.ErrorLogger
    open Microsoft.FSharp.Compiler.AbstractIL
    open Microsoft.FSharp.Compiler.AbstractIL.IL

    [<AutoOpen>]
    module private Utils =

        let buildFormatComment (xmlCommentRetriever: string * string -> string) cmt (sb: StringBuilder) =
            match cmt with
            | XmlCommentText(s) -> sb.AppendLine(s) |> ignore
            | XmlCommentSignature(file, signature) ->
                let comment = xmlCommentRetriever (file, signature)
                if (not (comment.Equals(null))) && comment.Length > 0 then sb.AppendLine(comment) |> ignore
            | XmlCommentNone -> ()

        let buildFormatElement isSingle el (sb: StringBuilder) xmlCommentRetriever =
            match el with
            | ToolTipElementNone -> ()
            | ToolTipElement(it, comment) ->
                sb.AppendLine(it) |> buildFormatComment xmlCommentRetriever comment
            | ToolTipElementGroup(items) ->
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
            | ToolTipElementCompositionError(err) ->
                sb.Append("Composition error: " + err) |> ignore

        // Convert ToolTipText to string
        let formatTip tip xmlCommentRetriever =
            let commentRetriever = defaultArg xmlCommentRetriever (fun _ -> "")
            let sb = new StringBuilder()
            match tip with
            | ToolTipText([single]) -> buildFormatElement true single sb commentRetriever
            | ToolTipText(its) -> for item in its do buildFormatElement false item sb commentRetriever
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

        let identToken = Microsoft.FSharp.Compiler.Parser.tagOfToken (Microsoft.FSharp.Compiler.Parser.IDENT "")
        let hasChangedSinceLastTypeCheck _ = false

        /// Return the errors resulting from the type-checking
        member x.Errors = results.Errors

        /// Get the declarations at the given code location.
        member x.GetDeclarationsAlternate(line, col, qualifyingNames, partialName, ?xmlCommentRetriever) =
            async { let! items = results.GetDeclarationsAlternate(Some info, line, col, source.[int line], qualifyingNames, partialName, hasChangedSinceLastTypeCheck)
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
        member x.GetDeclarationLocationAlternate(line, col, names, isDecl) =
            results.GetDeclarationLocationAlternate(line, col, source.[int line], names, isDecl)

        /// Get the full type checking results 
        member x.FullResults = results


        // Obsolete
        
        member x.GetF1Keyword(line, col, names) = x.GetF1KeywordAlternate(Line.fromZ line, col, names) |> Async.RunSynchronously
        member x.GetToolTipText(line, col, names, ?xmlCommentRetriever) = x.GetToolTipTextAlternate(Line.fromZ line, col, names, ?xmlCommentRetriever=xmlCommentRetriever) |> Async.RunSynchronously
        member x.GetDeclarationLocation(line, col, names, isDecl) = x.GetDeclarationLocationAlternate(Line.fromZ line, col, names, isDecl) |> Async.RunSynchronously
        member x.GetDataTipText(line, col, names, ?xmlCommentRetriever) = x.GetToolTipText(line, col, names, ?xmlCommentRetriever=xmlCommentRetriever) 
        member x.GetDeclarations(line, col, qualifyingNames, partialName, ?xmlCommentRetriever) = x.GetDeclarationsAlternate(Line.fromZ line, col, qualifyingNames, partialName, ?xmlCommentRetriever=xmlCommentRetriever)

    /// Provides simple services for checking and compiling F# scripts
    type public SimpleSourceCodeServices() =

        let checker = InteractiveChecker.Create()
        let fileversion = 0
        let loadTime = DateTime.Now
 
        /// Tokenize a single line, returning token information and a tokenization state represented by an integer
        member x.TokenizeLine (line: string, state: int64) : TokenInformation[] * int64 = 
            let tokenizer = SourceTokenizer([], "example.fsx")
            let lineTokenizer = tokenizer.CreateLineTokenizer line
            let state = ref (None, state)
            let tokens = 
                [| while (state := lineTokenizer.ScanToken (snd !state); (fst !state).IsSome) do
                       yield (fst !state).Value |]
            tokens, snd !state 

        /// Tokenize an entire file, line by line
        member x.TokenizeFile (source: string) : TokenInformation[][] = 
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
            checker.StartBackgroundCompile options
            // wait for the antecedent to appear
            checker.WaitForBackgroundCompile()
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

        /// Compile using the given flags.  Source files names are resolved via the FileSystem API. The output file must be given by a -o flag. 
        member x.Compile (argv: string[], tcImportsCapture, dynamicAssemblyCreator)  = 
            let errors = ResizeArray<_>()

            let errorSink warn exn = 
                let mainError,relatedErrors = Build.SplitRelatedErrors exn 
                let oneError trim e = errors.Add(ErrorInfo.CreateFromException (e, warn, trim, Range.range0))
                oneError false mainError
                List.iter (oneError true) relatedErrors

            let errorLogger = 
                { new ErrorLogger("CompileAPI") with 
                    member x.WarnSinkImpl(exn) = errorSink true exn
                    member x.ErrorSinkImpl(exn) = errorSink false exn
                    member x.ErrorCount = errors |> Seq.filter (fun e -> e.Severity = Severity.Error) |> Seq.length }

            let loggerProvider (_tcConfigBuilder, _exiter) = errorLogger 
     
            let result = 
                use unwindParsePhase = PushThreadBuildPhaseUntilUnwind (BuildPhase.Parse)            
                use unwindEL_2 = PushErrorLoggerPhaseUntilUnwind (fun _ -> errorLogger)
                let exiter = { new Exiter with member x.Exit n = raise StopProcessing }
                try 
                    mainCompile (argv, true, exiter, Some loggerProvider, tcImportsCapture, dynamicAssemblyCreator) 
                    0
                with e -> 
                    stopProcessingRecovery e Range.range0
                    1
        
            errors.ToArray(), result

        member x.Compile (asts, assemblyName, outFile, dependencies, pdbFile, executable, tcImportsCapture, dynamicAssemblyCreator) =
            let errors = ResizeArray<_>()

            let errorSink warn exn = 
                let mainError,relatedErrors = Build.SplitRelatedErrors exn 
                let oneError trim e = errors.Add(ErrorInfo.CreateFromException (e, warn, trim, Range.range0))
                oneError false mainError
                List.iter (oneError true) relatedErrors

            let errorLogger = 
                { new ErrorLogger("CompileAPI") with 
                    member x.WarnSinkImpl(exn) = errorSink true exn
                    member x.ErrorSinkImpl(exn) = errorSink false exn
                    member x.ErrorCount = errors |> Seq.filter (fun e -> e.Severity = Severity.Error) |> Seq.length }

            let executable = defaultArg executable true
            let target = if executable then Build.CompilerTarget.ConsoleExe else Build.CompilerTarget.Dll
     
            let result = 
                use unwindParsePhase = PushThreadBuildPhaseUntilUnwind (BuildPhase.Parse)            
                use unwindEL_2 = PushErrorLoggerPhaseUntilUnwind (fun _ -> errorLogger)
                let exiter = { new Exiter with member x.Exit n = raise StopProcessing }
                try 
                    compileOfAst (assemblyName, target, outFile, pdbFile, dependencies, exiter, asts, tcImportsCapture, dynamicAssemblyCreator)
                    0
                with e -> 
                    stopProcessingRecovery e Range.range0
                    1

            errors.ToArray(), result

        member x.Compile (argv: string[])  = x.Compile(argv, None, None)

        member x.Compile (ast:ParsedInput list, assemblyName:string, outFile:string, dependencies:string list, ?pdbFile:string, ?executable:bool) =
            x.Compile(ast, assemblyName, outFile, dependencies, pdbFile, executable, None, None)

        /// Compiles to a dynamic assembly usinng the given flags.  Any source files names 
        /// are resolved via the FileSystem API. An output file name must be given by a -o flag, but this will not
        /// be written - instead a dynamic assembly will be created and loaded.
        ///
        /// If the 'execute' parameter is given the entry points for the code are executed and 
        /// the given TextWriters are used for the stdout and stderr streams respectively. In this 
        /// case, a global setting is modified during the execution.
        member x.CompileToDynamicAssembly (otherFlags: string[], execute: (TextWriter * TextWriter) option)  = 
            // Set the output streams, if requested
            match execute with
            | Some (writer,error) -> 
#if SILVERLIGHT
                Microsoft.FSharp.Core.Printf.setWriter writer
                Microsoft.FSharp.Core.Printf.setError error
#else
                System.Console.SetOut writer
                System.Console.SetError error
#endif
            | None -> ()
            
            // References used to capture the results of compilation
            let tcImportsRef = ref (None: Build.TcImports option)
            let res = ref None
            let tcImportsCapture = Some (fun tcImports -> tcImportsRef := Some tcImports)

            // Function to generate and store the results of compilation 
            let dynamicAssemblyCreator = 
                Some (fun (_tcConfig,ilGlobals,_errorLogger,outfile,_pdbfile,ilxMainModule,_signingInfo) ->

                    // Create an assembly builder
                    let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(System.Reflection.AssemblyName(System.IO.Path.GetFileNameWithoutExtension outfile),System.Reflection.Emit.AssemblyBuilderAccess.Run)
                    let debugInfo =  otherFlags |> Array.exists (fun arg -> arg = "-g" || arg = "--debug:+" || arg = "/debug:+")
                    let moduleBuilder = assemblyBuilder.DefineDynamicModule("IncrementalModule", debugInfo)     

                    // Omit resources in dynamic assemblies, because the module builder is constructed without a filename the module 
                    // is tagged as transient and as such DefineManifestResource will throw an invalid operation if resources are present.
                    // 
                    // Also, the dynamic assembly creator can't currently handle types called "<Module>" from statically linked assemblies.
                    let ilxMainModule = 
                       { ilxMainModule with 
                            TypeDefs = ilxMainModule.TypeDefs.AsList |> List.filter (fun td -> not (isTypeNameForGlobalFunctions td.Name)) |> mkILTypeDefs
                            Resources=mkILResources [] }

                    // The function used to resolve typees while emitting the code
                    let assemblyResolver s = 
                        match tcImportsRef.Value.Value.TryFindExistingFullyQualifiedPathFromAssemblyRef s with 
                        | Some res -> Some (Choice1Of2 res)
                        | None -> None

                    // Emit the code
                    let _emEnv,execs = ILRuntimeWriter.emitModuleFragment(ilGlobals, ILRuntimeWriter.emEnv0, assemblyBuilder, moduleBuilder, ilxMainModule, debugInfo, assemblyResolver)

                    // Execute the top-level initialization, if requested
                    if execute.IsSome then 
                        for exec in execs do 
                            match exec() with 
                            | None -> ()
                            | Some exn -> raise exn

                    // Register the reflected definitions for the dynamically generated assembly
                    for resource in ilxMainModule.Resources.AsList do 
                        if Build.IsReflectedDefinitionsResource resource then 
                            Quotations.Expr.RegisterReflectedDefinitions(assemblyBuilder, moduleBuilder.Name, resource.Bytes)

                    // Save the result
                    res := Some assemblyBuilder)
            

            // Perform the compilation, given the above capturing function.
            let errorsAndWarnings, result = x.Compile (otherFlags, tcImportsCapture, dynamicAssemblyCreator)

            // Retrieve and return the results
            let assemblyOpt = 
                match res.Value with 
                | None -> None
                | Some a ->  Some (a :> System.Reflection.Assembly)

            errorsAndWarnings, result, assemblyOpt

        member x.CompileToDynamicAssembly (asts:ParsedInput list, assemblyName:string, dependencies:string list, execute: (TextWriter * TextWriter) option, ?debug) =
            // Set the output streams, if requested
            match execute with
            | Some (writer,error) -> 
#if SILVERLIGHT
                Microsoft.FSharp.Core.Printf.setWriter writer
                Microsoft.FSharp.Core.Printf.setError error
#else
                System.Console.SetOut writer
                System.Console.SetError error
#endif
            | None -> ()

            // References used to capture the results of compilation
            let tcImportsRef = ref (None: Build.TcImports option)
            let res = ref None
            let tcImportsCapture = Some (fun tcImports -> tcImportsRef := Some tcImports)

            let debugInfo = defaultArg debug false
            let outFile = Path.Combine(Path.GetTempPath(), assemblyName + ".dll")

            // Function to generate and store the results of compilation 
            let dynamicAssemblyCreator = 
                Some (fun (_tcConfig,ilGlobals,_errorLogger,outfile,_pdbfile,ilxMainModule,_signingInfo) ->

                    // Create an assembly builder
                    let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(System.Reflection.AssemblyName(System.IO.Path.GetFileNameWithoutExtension outfile),System.Reflection.Emit.AssemblyBuilderAccess.Run)
                    let moduleBuilder = assemblyBuilder.DefineDynamicModule("IncrementalModule", debugInfo)     

                    // Omit resources in dynamic assemblies, because the module builder is constructed without a filename the module 
                    // is tagged as transient and as such DefineManifestResource will throw an invalid operation if resources are present.
                    // 
                    // Also, the dynamic assembly creator can't currently handle types called "<Module>" from statically linked assemblies.
                    let ilxMainModule = 
                       { ilxMainModule with 
                            TypeDefs = ilxMainModule.TypeDefs.AsList |> List.filter (fun td -> not (isTypeNameForGlobalFunctions td.Name)) |> mkILTypeDefs
                            Resources=mkILResources [] }

                    // The function used to resolve typees while emitting the code
                    let assemblyResolver s = 
                        match tcImportsRef.Value.Value.TryFindExistingFullyQualifiedPathFromAssemblyRef s with 
                        | Some res -> Some (Choice1Of2 res)
                        | None -> None

                    // Emit the code
                    let _emEnv,execs = ILRuntimeWriter.emitModuleFragment(ilGlobals, ILRuntimeWriter.emEnv0, assemblyBuilder, moduleBuilder, ilxMainModule, debugInfo, assemblyResolver)

                    // Execute the top-level initialization, if requested
                    if execute.IsSome then 
                        for exec in execs do 
                            match exec() with 
                            | None -> ()
                            | Some exn -> raise exn

                    // Register the reflected definitions for the dynamically generated assembly
                    for resource in ilxMainModule.Resources.AsList do 
                        if Build.IsReflectedDefinitionsResource resource then 
                            Quotations.Expr.RegisterReflectedDefinitions(assemblyBuilder, moduleBuilder.Name, resource.Bytes)

                    // Save the result
                    res := Some assemblyBuilder)
            

            // Perform the compilation, given the above capturing function.
            let errorsAndWarnings, result = 
                x.Compile(asts, assemblyName, outFile, dependencies, None, Some execute.IsSome, tcImportsCapture, dynamicAssemblyCreator)

            // Retrieve and return the results
            let assemblyOpt = 
                match res.Value with 
                | None -> None
                | Some a ->  Some (a :> System.Reflection.Assembly)

            errorsAndWarnings, result, assemblyOpt