#if INTERACTIVE
#r "../../bin/v4.5/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#r "../../packages/Foq.1.6/Lib/net45/Foq.dll"
#load "FsUnit.fs"
#load "Common.fs"
#else
module FSharp.Compiler.Service.Tests.FileSystemTests
#endif


open NUnit.Framework
open FsUnit
open System
open System.IO
open System.Collections.Generic
open System.Text
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Interactive.Shell
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open FSharp.Compiler.Service.Tests.Common

let originalFileSystem = Shim.FileSystem
[<TearDown>]
let teardown () =
    Shim.FileSystem <- originalFileSystem

let references = 
    @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll" ::
    (["mscorlib"; "System"; "System.Core"]
    |> List.map FrameworkReferenceResolver.find)

module Fake =
    type FileSystemHandler = {
        ReadFile: string -> byte array option;
        WriteFile: string -> Stream option}

        with
            static member Empty: FileSystemHandler =
                { ReadFile = fun name -> raise (new System.UnauthorizedAccessException(sprintf "Unconfigured read '%s'" name));
                  WriteFile = fun name -> raise (new System.UnauthorizedAccessException(sprintf "Unconfigured write '%s'" name)) }

    let create (init: FileSystemHandler -> FileSystemHandler) (defaultFileSystem: IFileSystem) =
        let mock =
            FileSystemHandler.Empty |> init
        
        { new IFileSystem with
            member fs.FileStreamReadShim fileName =
                match mock.ReadFile fileName with
                | Some content -> new MemoryStream (content) :> Stream
                | None -> defaultFileSystem.FileStreamReadShim fileName
            
            member fs.FileStreamCreateShim fileName = 
                match mock.WriteFile fileName with
                | Some stream -> stream
                | None -> defaultFileSystem.FileStreamCreateShim fileName

            member fs.FileStreamWriteExistingShim fileName = 
                match mock.WriteFile fileName with
                | Some stream -> stream
                | None -> defaultFileSystem.FileStreamWriteExistingShim fileName

            member fs.ReadAllBytesShim fileName = 
                match mock.ReadFile fileName with
                | Some content -> content
                | None -> defaultFileSystem.ReadAllBytesShim fileName

            // Implement the service related to temporary paths and file time stamps
            member __.GetTempPathShim() = defaultFileSystem.GetTempPathShim()
            member __.GetLastWriteTimeShim(fileName) = defaultFileSystem.GetLastWriteTimeShim(fileName)
            member __.GetFullPathShim(fileName) = defaultFileSystem.GetFullPathShim(fileName)
            member __.IsInvalidPathShim(fileName) = defaultFileSystem.IsInvalidPathShim(fileName)
            member __.IsPathRootedShim(fileName) = defaultFileSystem.IsPathRootedShim(fileName)

            // Implement the service related to file existence and deletion
            member __.SafeExists(fileName) = 
                match mock.ReadFile fileName with
                | Some _ -> true
                | None -> defaultFileSystem.SafeExists fileName

            member __.FileDelete(fileName) = defaultFileSystem.FileDelete(fileName)

            // Implement the service related to assembly loading, used to load type providers
            // and for F# interactive.
            member __.AssemblyLoadFrom(fileName) = defaultFileSystem.AssemblyLoadFrom fileName
            member __.AssemblyLoad(assemblyName) = defaultFileSystem.AssemblyLoad assemblyName}

    let readFiles (files: Map<string, string>) (handler: FileSystemHandler) =
        { handler with 
            ReadFile = 
                fun name ->
                    match Map.tryFind name files with
                    | Some content -> Some (Encoding.Default.GetBytes content)
                    | None -> handler.ReadFile name }

    let readReferences (references: string list) (handler: FileSystemHandler) =
        let fn n =
            Path.Combine (n |> Path.GetDirectoryName, n |> Path.GetFileNameWithoutExtension)
        { handler with
            ReadFile =
                fun name ->
                    match List.tryFind (fun r -> String.Equals (r |> fn, name |> fn, StringComparison.OrdinalIgnoreCase)) references with
                    | Some r -> Some (File.ReadAllBytes name)
                    | None -> handler.ReadFile name}

    let writeFiles (writer: string -> Stream option) (handler: FileSystemHandler) =
        { handler with
            WriteFile =
                fun name ->
                    match writer name with
                    | Some stream -> Some stream
                    | None -> handler.WriteFile name }

    let set (ctor: IFileSystem -> IFileSystem) =
        let original = Shim.FileSystem
        let fs = ctor original
        Shim.FileSystem <- fs
        { new IDisposable with 
            member x.Dispose() = 
                printfn "Resetting file system"
                Shim.FileSystem <- original }

[<Test>]
let ``Reads sources from shimmed filesystem`` () =
    let files =
        Map.empty
        |> Map.add @"c:\mycode\test1.fs" """
module File1
let A = 1"""
        |> Map.add @"c:\mycode\test2.fs" """
module File2
let B = File1.A + File1.A"""

    let projectOptions = 
        let allFlags = 
            [| yield "--simpleresolution"; 
               yield "--noframework"; 
               yield "--debug:full"; 
               yield "--define:DEBUG"; 
               yield "--optimize-"; 
               yield "--doc:test.xml"; 
               yield "--warn:3"; 
               yield "--fullpaths"; 
               yield "--flaterrors"; 
               yield "--target:library"; 
               for r in references do 
                     yield "-r:" + r |]

        let fileNames, _ = Map.toList files |> List.unzip
 
        { ProjectFileName = @"c:\mycode\compilation.fsproj" // Make a name that is unique in this directory.
          ProjectFileNames = fileNames |> Array.ofList
          OtherOptions = allFlags 
          ReferencedProjects = Array.empty
          IsIncompleteTypeCheckEnvironment = false
          UseScriptResolutionRules = true 
          LoadTime = System.DateTime.Now // Not 'now', we don't want to force reloading
          UnresolvedReferences = None }

    use mock =
        Fake.readReferences references
        >> Fake.readFiles files
        |> Fake.create
        |> Fake.set

    let results = checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously

    results.Errors.Length |> shouldEqual 0
    results.AssemblySignature.Entities.Count |> shouldEqual 2
    results.AssemblySignature.Entities.[0].MembersFunctionsAndValues.Count |> shouldEqual 1
    results.AssemblySignature.Entities.[0].MembersFunctionsAndValues.[0].DisplayName |> shouldEqual "B"

[<Test>]
let ``Writes artifacts to shimmed filesystem`` () =
    let files =
        Map.empty
        |> Map.add @"c:\mycode\test1.fs" """
module File1
let A = 1"""
        |> Map.add @"c:\mycode\test2.fs" """
module File2
let B = File1.A + File1.A"""

    let compilerFlags = 
        let fileNames, _ = Map.toList files |> List.unzip

        [|  yield "--simpleresolution"; 
            yield "--noframework"; 
            yield "--debug:full"; 
            yield "--define:DEBUG"; 
            yield "--optimize-"; 
            yield "--doc:test.xml"; 
            yield "--out:test.dll";
            yield "--pdb:test.pdb";
            yield "--warn:3"; 
            yield "--fullpaths"; 
            yield "--flaterrors"; 
            yield "--target:library";
            for r in references do 
                  yield "-r:" + r;
            for f in fileNames do
                  yield f; |]

    let written = ref Set.empty
    use mock =
        Fake.readReferences references
        >> Fake.readFiles files
        >> Fake.writeFiles (fun name ->
            written := Set.add name !written
            Some (new MemoryStream () :> Stream))
        |> Fake.create
        |> Fake.set

    let sscs = new SimpleSourceCodeServices ()
    let errors, result = sscs.Compile compilerFlags
    
    let cdirPath n = Path.Combine (Directory.GetCurrentDirectory (), n)

    result |> shouldEqual 0
    errors.Length |> shouldEqual 0
    let written = !written
    Seq.length written |> shouldEqual 3
    written |> should contain (cdirPath "test.dll")
    written |> should contain (cdirPath "test.pdb")
    written |> should contain (cdirPath "test.xml")

    //TODO: capture and test file content?
