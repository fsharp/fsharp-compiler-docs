
#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "FsUnit.fs"
#load "Common.fs"
#else
module FSharp.Compiler.Service.Tests.FscTests
#endif


open System
open System.Diagnostics
open System.IO

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices

open NUnit.Framework

exception 
   VerificationException of (*assembly:*)string * (*errorCode:*)int * (*output:*)string
   with override e.Message = sprintf "Verification of '%s' failed with code %d." e.Data0 e.Data1

exception 
   CompilationError of (*assembly:*)string * (*errorCode:*)int * (*info:*)ErrorInfo []
   with override e.Message = sprintf "Compilation of '%s' failed with code %d (%A)" e.Data0 e.Data1 e.Data2

let runningOnMono = try System.Type.GetType("Mono.Runtime") <> null with e->  false        
let pdbExtension isDll = (if runningOnMono then (if isDll then ".dll.mdb" else ".exe.mdb") else ".pdb")

type PEVerifier () =

    static let expectedExitCode = 0
    static let runsOnMono = try System.Type.GetType("Mono.Runtime") <> null with _ -> false

    let verifierPath, switches =
        if runsOnMono then
            "pedump", "--verify all"
        else
            let rec tryFindFile (fileName : string) (dir : DirectoryInfo) =
                let file = Path.Combine(dir.FullName, fileName)
                if File.Exists file then Some file
                else
                    dir.GetDirectories() 
                    |> Array.sortBy(fun d -> d.Name)
                    |> Array.rev // order by descending -- get latest version
                    |> Array.tryPick (tryFindFile fileName)

            let tryGetSdkDir (progFiles : Environment.SpecialFolder) =
                let progFilesFolder = Environment.GetFolderPath(progFiles)
                let dI = DirectoryInfo(Path.Combine(progFilesFolder, "Microsoft SDKs", "Windows"))
                if dI.Exists then Some dI
                else None

            match Array.tryPick tryGetSdkDir [| Environment.SpecialFolder.ProgramFiles ; Environment.SpecialFolder.ProgramFilesX86 |] with
            | None -> failwith "Could not resolve .NET SDK folder."
            | Some sdkDir ->
                match tryFindFile "peverify.exe" sdkDir with
                | None -> failwith "Could not locate 'peverify.exe'."
                | Some pe -> pe, "/UNIQUE /IL /NOLOGO"

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
        while not proc.HasExited do ()
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

let checker = InteractiveChecker.Create()
let compiler = new SimpleSourceCodeServices()

let compileAndVerify isDll debugMode (assemblyName : string) (code : string) (dependencies : string list) =
    let verifier = new PEVerifier ()
    let tmp = Path.GetTempPath()
    let sourceFile = Path.Combine(tmp, assemblyName + ".fs")
    let outFile = Path.Combine(tmp, assemblyName + if isDll then ".dll" else ".exe")
    let pdbFile = Path.Combine(tmp, assemblyName + pdbExtension isDll)
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
                if not runningOnMono then  // on Mono, the debug file name is not configurable
                    yield sprintf "--pdb:%s" pdbFile
            | Full ->
                yield "--debug:full"
                if not runningOnMono then // on Mono, the debug file name is not configurable
                    yield sprintf "--pdb:%s" pdbFile

            for d in dependencies do
                yield sprintf "-r:%s" d

            yield sprintf "--out:%s" outFile

            yield sourceFile
        |]
        
    printfn "args: %A" args
    let errorInfo, id = compiler.Compile args
    for err in errorInfo do 
       printfn "error: %A" err
    Assert.AreEqual (errorInfo.Length, 0)
    if id <> 0 then raise <| CompilationError(assemblyName, id, errorInfo)
    verifier.Verify outFile
    outFile

let parseSourceCode (name : string, code : string) =
    let location = Path.GetTempPath()
    let projPath = Path.Combine(location, name + ".fsproj")
    let filePath = Path.Combine(location, name + ".fs")
    let dllPath = Path.Combine(location, name + ".dll")
    let args = Common.mkProjectCommandLineArgs(dllPath, [filePath])
    let options = checker.GetProjectOptionsFromCommandLineArgs(projPath, args)
    let parseResults = checker.ParseFileInProject(filePath, code, options) |> Async.RunSynchronously
    parseResults.ParseTree |> Option.toList


let compileAndVerifyAst (name : string, ast : Ast.ParsedInput list, references : string list) =
    let outDir = Path.GetTempPath()
    
    let outFile = Path.Combine(outDir, name + ".dll")

    let errors, id = compiler.Compile(ast, name, outFile, references, executable = false)
    for err in errors do printfn "error: %A" err
    Assert.AreEqual (errors.Length, 0)
    if id <> 0 then raise <| CompilationError(name, id, errors)

    // copy local explicit references for verification
    for ref in references do 
        let name = Path.GetFileName ref
        File.Copy(ref, Path.Combine(outDir, name), overwrite = true)

    let verifier = new PEVerifier()

    verifier.Verify outFile

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

    printfn "done!" // make the code have some initialization effect
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
    proc.WaitForExit()
    Assert.AreEqual(proc.ExitCode, 42)



[<Test>]
let ``4. Compile from simple AST`` () =
    let code = """
module Foo

    let f x = (x,x)

    type Foo = class end

    exception E of int * string

    printfn "done!" // make the code have some initialization effect
"""
    let ast = parseSourceCode("foo", code)
    compileAndVerifyAst("foo", ast, [])

[<Test>]
let ``5. Compile from AST with explicit assembly reference`` () =
    let code = """
module Bar

    open Microsoft.FSharp.Compiler.SourceCodeServices

    let f x = (x,x)

    type Bar = class end

    exception E of int * string

    // depends on FSharp.Compiler.Service
    // note : mono's pedump fails if this is a value; will not verify type initializer for module
    let checker () = InteractiveChecker.Create()

    printfn "done!" // make the code have some initialization effect
"""
    let serviceAssembly = typeof<InteractiveChecker>.Assembly.Location
    let ast = parseSourceCode("bar", code)
    compileAndVerifyAst("bar", ast, [serviceAssembly])