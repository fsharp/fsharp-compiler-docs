module FSharp.Compiler.Service.Tests.FscTests

    open System
    open System.Diagnostics
    open System.IO

    open Microsoft.FSharp.Compiler

    open Mono.Cecil

    open NUnit.Framework

    exception VerificationException of (*assembly:*)string * (*errorCode:*)int * (*output:*)string
    with
        override e.Message = sprintf "Verification of '%s' failed with code %d." e.Data0 e.Data1

    exception CompilationError of (*assembly:*)string * (*errorCode:*)int * (*info:*)ErrorInfo []
    with
        override e.Message = sprintf "Compilation of '%s' failed with code %d (%A)" e.Data0 e.Data1 e.Data2

    // cecil data dump with structural equality
    // not exhaustive traversal, but at least ensures that closure types do not differ
    type CecilAssemblyInfo =
        {
            FullName : string
            EntryPoint : string option
            MetaDataToken : string
            Methods : Set<string>
        }
    with
        static member OfAssembly(path : string) =
            let rec walkAssembly (a : AssemblyDefinition) =
                a.Modules 
                |> Seq.collect (fun m -> walkTypes m.Types)
                |> set

            and walkTypes (ts : seq<TypeDefinition>) =
                seq {
                    for t in ts do
                        yield! walkMethods t
                        yield! walkTypes t.NestedTypes
                }

            and walkMethods (t : TypeDefinition) =
                t.Methods |> Seq.map (fun m -> m.ToString())

            let a = AssemblyDefinition.ReadAssembly(path)

            {
                FullName = a.FullName
                EntryPoint = match a.EntryPoint with null -> None | m -> Some <| m.ToString()
                MetaDataToken = a.MetadataToken.ToString()
                Methods = walkAssembly a
            }

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

    let compileAndVerify isDll debugMode (assemblyName : string) (code : string) (dependencies : string list) =
        let scs = new Microsoft.FSharp.Compiler.SimpleSourceCodeServices.SimpleSourceCodeServices()
        let verifier = new PEVerifier ()
        let tmp = Path.GetTempPath()
        let sourceFile = Path.Combine(tmp, assemblyName + ".fs")
        let outFile = Path.Combine(tmp, assemblyName + if isDll then ".dll" else ".exe")
        let pdbFile = Path.Combine(tmp, assemblyName + ".pdb")
        do
            let deleteIfExists file = if File.Exists file then File.Delete file
            deleteIfExists sourceFile
            deleteIfExists outFile
            deleteIfExists pdbFile
            File.WriteAllText(sourceFile, code)

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


    let libCode = """
module Foo

    let f x y = x + y

    let g = f 42

    type IFoo = abstract Value : int

    let instance = { new IFoo with member __.Value = 42 }

    type DU =
        | A
        | B
        | C
        | D of int

    exception E of int * string
"""

    [<Test>]
    let ``2. Simple FSC library test`` () =

        try compileAndVerify true PdbOnly "Foo" libCode [] |> ignore
        with VerificationException(_,_,out) -> printfn "%A" out ; reraise ()

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


    [<Test>]
    let ``4. Parallel FSC compilation`` () =

        let compileMany (concurrentCompiles : int) =
            async {
                let compileOnce i = async { return compileAndVerify true PdbOnly (sprintf "Foo-%d" i) libCode [] }
                return! Async.Parallel <| Array.init concurrentCompiles compileOnce
            }

        let files = Async.RunSynchronously <| compileMany 4

        ()


    [<Test>]
    let ``5. Referential Transparency in FSC compiles`` () =

        let compile () = compileAndVerify true PdbOnly "Foo" libCode []

        let lib1 = compile ()
        let il1 = File.ReadAllBytes(lib1)
        let data1 = CecilAssemblyInfo.OfAssembly lib1
        let lib2 = compile ()
        let lib3 = compile ()
        let il3 = File.ReadAllBytes(lib3)
        let data3 = CecilAssemblyInfo.OfAssembly lib3

        Assert.That(il1.Length = il3.Length)
        Assert.AreEqual(data1, data3)

        let gatherDiffs (bs : byte []) (bs' : byte []) =
            (bs, bs')
            ||> Array.zip
            |> Array.mapi (fun i bs -> i,bs)
            |> Array.filter (fun (_,(b,b')) -> b <> b')

        let diffs = gatherDiffs il1 il3

        printfn "%A" diffs
