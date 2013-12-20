

#r @"../../lib/debug/FSharp.Compiler.Service.dll"
#nowarn "57"

module SimpleTokenizeExample = 
    open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
    let service = SimpleSourceCodeServices()

    service.TokenizeFile "let x = 1"
    service.TokenizeFile """
let x = 1"
let y = x + 1
"""


module SimpleMatchBraceExample = 
    open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
    let service = SimpleSourceCodeServices()
    /// TODO: you must currently give a fully qualified path and the file must exist. That should not be needed to macth braces.
    service.MatchBraces(@"c:\misc\a.fsx", """
[ (1 + 1) ] = [ (2 + 0) ]
"""
    )

module SimpleCompileExample = 
    open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
    let service = SimpleSourceCodeServices()

    (try System.IO.File.Delete(@"c:\misc\a.dll") with _ -> ())
    System.IO.File.WriteAllText(@"c:\misc\a.fs", """module M 
let x = 1""")
    service.Compile [| "fsc.exe"; @"c:\misc\a.fs"; "-a"; @"-o:c:\misc\a.dll"  |]
    System.IO.File.Exists(@"c:\misc\a.dll")

module SimpleCompileToDynamicAssemblyExample = 
    open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
    let service = SimpleSourceCodeServices()

    let errors, exitCode, assemOpt = service.CompileToDynamicAssembly([| "fsc.exe"; @"c:\misc\a.fs"; "-a"; @"-o:c:\misc\a.dll"  |], None)
    let assem = assemOpt.Value
    assem.GetName().Name = "a"
    let modM = assem.GetType("M")
    let xM = modM.GetProperty("x")

module InteractiveShellExample = 
    open Microsoft.FSharp.Compiler.Interactive.Shell
    open Microsoft.FSharp.Compiler.Interactive
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler

    open System
    open System.IO
    open System.Reflection
    open System.Collections.Generic

    let stdinStream = new CompilerInputStream()
    let stdin = new System.IO.StreamReader(stdinStream)


    let stdoutStream = new CompilerOutputStream()
    let stdout = StreamWriter.Synchronized(new System.IO.StreamWriter(stdoutStream, AutoFlush=true))

    let stderrStream = new CompilerOutputStream()
    let stderr = StreamWriter.Synchronized(new System.IO.StreamWriter(stderrStream, AutoFlush=true))

    System.Console.SetOut stdout
    System.Console.SetError stderr

    stdinStream.Add("eprintfn \"writing to error\";;\n")
    stdinStream.Add("printfn \"hello world to out\";;\n")
    stdinStream.Add("let x = 1;;\n")
    stdinStream.Add("eprintfn \"this is me on error, x = %d\" x;;\n\n")
    stdinStream.Add("printfn \"this is me on out, x = %d\" x;;\n\n")
    stdinStream.Add("let x = 1;;\n")

    let fsiConfig = 
        { // Connect the configuration through to the 'fsi' object
            new FsiEvaluationSessionHostConfig with 
            member __.FormatProvider = fsi.FormatProvider
            member __.FloatingPointFormat = fsi.FloatingPointFormat
            member __.AddedPrinters = typeof<InteractiveSession>.InvokeMember("AddedPrinters",(BindingFlags.GetProperty ||| BindingFlags.NonPublic ||| BindingFlags.Instance),null,box fsi, [| |]) |> unbox
            member __.ShowDeclarationValues = fsi.ShowDeclarationValues
            member __.ShowIEnumerable = fsi.ShowIEnumerable
            member __.ShowProperties = fsi.ShowProperties
            member __.PrintSize = fsi.PrintSize  
            member __.PrintDepth = fsi.PrintDepth
            member __.PrintWidth = fsi.PrintWidth
            member __.PrintLength = fsi.PrintLength
            member __.ReportUserCommandLineArgs with set args = fsi.CommandLineArgs <- args
            member __.StartServer(fsiServerName) =  failwith "--fsi-server not implemented in this version of fsi.exe"
            member __.EventLoopRun() = fsi.EventLoop.Run()
            member __.EventLoopInvoke(f) = fsi.EventLoop.Invoke(f)
            member __.EventLoopScheduleRestart() = fsi.EventLoop.ScheduleRestart()
            member __.ConsoleReadLine = None }


    let session = FsiEvaluationSession(fsiConfig, [| "fsiAnyCpu.exe" |], stdin, stdout, stderr)

    // Start the session in the background
    async { printfn "entering 'MAIN' thread"
            do session.Run() 
            printfn "exited 'MAIN' thread" } |> Async.Start

    //session.Interrupt()

    //-----------------------------------------------------------------------------


    // Wait a bit before executing these lines
    let text1 = stdoutStream.Read()
    let text2 = stderrStream.Read()

    (*
    stdinStream.Add("let x = 1;;\n")

    let text3 = stdoutStream.Read()
    *)

    session.ExecuteInteraction("let ;;")

    // This one can be run synchronously because there is an error in parsing
    let p1 = (session.ExecuteInteractionAsync("let ;;") |> Async.RunSynchronously = "error")

    // TODO: This one can't yet be run synchronously because of some UI thread lock problem
    let p2 = session.ExecuteInteractionAsync("let x = 1;;")
    p2 |> Async.RunSynchronously  // "ok"

    let p3 = session.ExecuteInteractionAsync("""printfn "hello";;""")
    p3 |> Async.RunSynchronously 
    stdoutStream.Read()   // "hello\nval it : unit = ()"   // TODO: give more control over type etc. output

    let p4 = session.ExecuteInteractionAsync("""printfn "hello""")
    p4 |> Async.RunSynchronously 
    stdoutStream.Read()
    stderrStream.Read() // "End of file in string begun at or before here"

    let p5 = session.ExecuteInteractionAsync("""printfn (* hello""")
    p5 |> Async.RunSynchronously 
    stdoutStream.Read()
    stderrStream.Read() // "End of file in comment begun at or before here"

    let p6 = session.ExecuteInteractionAsync("""let TEST6 = 1""")
    p6 |> Async.RunSynchronously 
    stdoutStream.Read() // "val TEST6 : int = 1"
    let p6b = session.ExecuteInteractionAsync("""TEST6""")
    p6b |> Async.RunSynchronously 
    stdoutStream.Read() // "val it : int = 1"

    let lineText = "let TEST7 = 1 in TEST7"
    let p7u,p7t = session.TypeCheckScriptFragment(lineText)


    p7t.Errors.Length = 0

    let Z x = Line.assertZeroBasedLine x

    // lineText is used to resolve various corner case conditions like ".."

    let identTag = Parser.tagOfToken (Parser.IDENT "a")

    [ for i in 1 .. lineText.Length -> p7t.GetDataTipText(Z 0, i, lineText, [ "TEST7" ], identTag) ]
    [ for i in 1 .. lineText.Length-1 -> p7t.GetDeclarationLocation(Z 0, i, lineText, [ "TEST7" ], identTag, preferSignature=false) ]

    let crack0 (s:string) = 
        let els = s.Split('.') 
        Array.toList els

    let crack (s:string) = 
        let els = s.Split('.') 
        Array.toList els.[0..els.Length-2], els.[els.Length-1]

    let complete longIdent = 
        let qualifyingNames, partialName = crack longIdent
        let p7u, p7t = session.TypeCheckScriptFragment(longIdent)

        let decls = p7t.GetDeclarations(Some p7u, Z 0,longIdent.Length, longIdent, qualifyingNames, partialName, (fun _ -> false)) |> Async.RunSynchronously

        let names0 = crack0 longIdent
        let tips = p7t.GetDataTipText(Z 0,longIdent.Length, longIdent, names0, identTag) 
        [ for d in decls.Items -> d.Name (* , d.DescriptionText *) ], tips

    complete "S"
    complete "System"
    complete "System.Conso"
    complete "System.Console"
    complete "System.Console."
    complete "System.Console.W"
    complete "System.Console.WriteLine"

// GetDeclarations 
//    If UntypedParseInfoImpl.TryGetCompletionContext Invalid then None


