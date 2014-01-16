module FSharp.Compiler.Service.Tests.FscTests

    open System
    open System.Diagnostics
    open System.IO

    open Microsoft.Build.Utilities
    open Microsoft.FSharp.Compiler

    open NUnit.Framework

    exception VerificationException of (*assembly:*)string * (*errorCode:*)int * (*output:*)string
    with
        override e.Message = sprintf "Verification of '%s' failed with code %d." e.Data0 e.Data1

    exception CompilationError of (*assembly:*)string * (*errorCode:*)int * (*info:*)ErrorInfo []
    with
        override e.Message = sprintf "Compilation of '%s' failed with code %d (%A)" e.Data0 e.Data1 e.Data2

    type PEVerifier () =

        static let expectedExitCode = 0

        let runsOnMono = try System.Type.GetType("Mono.Runtime") <> null with _ -> false
        let verifierPath, switches =
            if runsOnMono then
                "pedump", "--verify all"
            else
                let path = ToolLocationHelper.GetPathToDotNetFrameworkSdkFile("peverify.exe", TargetDotNetFrameworkVersion.VersionLatest)
                path, "/UNIQUE /IL /NOLOGO"

        static let execute (fileName : string, arguments : string) =
            let psi = new ProcessStartInfo(fileName, arguments)
            psi.UseShellExecute <- false
            psi.ErrorDialog <- false
            psi.CreateNoWindow <- true
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true

            use proc = Process.Start(psi)
            let stdOut = proc.StandardOutput.ReadToEnd()
            let stdErr = proc.StandardError.ReadToEnd()
            proc.ExitCode, stdOut, stdErr

        member __.Verify(assemblyPath : string) =
            let id,stdOut,stdErr = execute(verifierPath, sprintf "%s \"%s\"" switches assemblyPath)
            if id = expectedExitCode && String.IsNullOrWhiteSpace stdErr then ()
            else
                raise <| VerificationException(assemblyPath, id, stdOut + "\n" + stdErr)


    type DebugMode =
        | Off
        | PdbOnly
        | Full

    let compileAndVerify isDll debugMode (assemblyName : string) (code : string) (dependencies : string list) =
        let scs = new Microsoft.FSharp.Compiler.SimpleSourceCodeServices.SimpleSourceCodeServices()
        let verifier = new PEVerifier ()
        let tmp = Path.GetTempPath()
        let sourceFile = Path.Combine(tmp, assemblyName + ".fs")
        let outFile = Path.Combine(tmp, assemblyName + if isDll then ".dll" else ".exe")
        let pdbFile = Path.Combine(tmp, assemblyName + ".pdb")
        do File.WriteAllText(sourceFile, code)
        let args =
            [|
                // fsc parser skips the first argument by default;
                // perhaps this shouldn't happen in library code.
                yield "fsc.exe"

                if isDll then yield "--target:library"

                match debugMode with
                | Off -> () // might need to include some switches here
                | PdbOnly ->
                    yield "--debug:pdbonly"
                    yield sprintf "--pdb:%s" pdbFile
                | Full ->
                    yield "--debug:full"
                    yield sprintf "--pdb:%s" pdbFile

                for d in dependencies do
                    yield sprintf "-r:%s" d

                yield sprintf "--out:%s" outFile

                yield sourceFile
            |]
        
        let errorInfo, id = scs.Compile args
        if id <> 0 then raise <| CompilationError(assemblyName, id, errorInfo)
        verifier.Verify outFile
        outFile


    [<Test>]
    let ``1. PEVerifier sanity check`` () =
        let verifier = new PEVerifier()

        let fscorlib = typeof<int option>.Assembly
        verifier.Verify fscorlib.Location

        let nonAssembly = Path.Combine(Directory.GetCurrentDirectory(), typeof<PEVerifier>.Assembly.GetName().Name + ".pdb")
        Assert.Throws<VerificationException>(fun () -> verifier.Verify nonAssembly |> ignore) |> ignore


    [<Test>]
    let ``2. Simple FSC library test`` () =
        let code = """
module Foo

    let f x = (x,x)

    type Foo = class end

    exception E of int * string
"""

        compileAndVerify true PdbOnly "Foo" code [] |> ignore

    [<Test>]
    let ``3. Simple FSC executable test`` () =
        let code = """
module Bar

    [<EntryPoint>]
    let main _ = printfn "Hello, World!" ; 42

"""
        let outFile = compileAndVerify false PdbOnly "Bar" code []

        use proc = Process.Start(outFile, "")
        while not proc.HasExited do ()
        Assert.AreEqual(proc.ExitCode, 42)