
#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "FsUnit.fs"
#load "Common.fs"
#else
module FSharp.Compiler.Service.Tests.MultiProjectAnalysisTests
#endif

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

open NUnit.Framework
open FsUnit
open System
open System.IO

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Service.Tests.Common

let checker = InteractiveChecker.Create()

/// Extract range info 
let tups (m:Range.range) = (m.StartLine, m.StartColumn), (m.EndLine, m.EndColumn)


module Project1A = 
    open System.IO

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1 = """
module Project1A

type C() = 
    static member M(arg1: int, arg2: int, ?arg3 : int) = arg1 + arg2 + defaultArg arg3 4

let x1 = C.M(arg1 = 3, arg2 = 4, arg3 = 5)

let x2 = C.M(arg1 = 3, arg2 = 4, ?arg3 = Some 5)

    """
    File.WriteAllText(fileName1, fileSource1)

    let cleanFileName a = if a = fileName1 then "file1" else "??"

    let fileNames = [fileName1]
    let args = mkProjectCommandLineArgs (dllName, fileNames)
    let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)



//-----------------------------------------------------------------------------------------
module Project1B = 
    open System.IO

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1 = """
module Project1B

type A = B of xxx: int * yyy : int
let b = B(xxx=1, yyy=2)

let x = 
    match b with
    // does not find usage here
    | B (xxx = a; yyy = b) -> ()
    """
    File.WriteAllText(fileName1, fileSource1)

    let cleanFileName a = if a = fileName1 then "file1" else "??"

    let fileNames = [fileName1]
    let args = mkProjectCommandLineArgs (dllName, fileNames)
    let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)


// A project referencing two sub-projects
module MultiProject1 = 
    open System.IO

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base1 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base1, ".dll")
    let projFileName = Path.ChangeExtension(base1, ".fsproj")
    let fileSource1 = """

module MoultiProject1

open Project1A
open Project1B

let p = (Project1A.x1, Project1B.b)

    """
    File.WriteAllText(fileName1, fileSource1)

    let fileNames = [fileName1]
    let args = mkProjectCommandLineArgs (dllName, fileNames)
    let options = 
        let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)
        { options with 
            ProjectOptions = Array.append options.ProjectOptions [| ("-r:" + Project1A.dllName); ("-r:" + Project1B.dllName) |]
            ReferencedProjects = [| (Project1A.dllName, Project1A.options);
                                    (Project1B.dllName, Project1B.options); |] }
    let cleanFileName a = if a = fileName1 then "file1" else "??"



[<Test>]
let ``Test multi project 1 whole project errors`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(MultiProject1.options) |> Async.RunSynchronously

    wholeProjectResults .Errors.Length |> shouldEqual 0

[<Test>]
let ``Test multi project 1 basic`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(MultiProject1.options) |> Async.RunSynchronously

    [ for x in wholeProjectResults.AssemblySignature.Entities -> x.DisplayName ] |> shouldEqual ["MoultiProject1"]

    [ for x in wholeProjectResults.AssemblySignature.Entities.[0].NestedEntities -> x.DisplayName ] |> shouldEqual []


    [ for x in wholeProjectResults.AssemblySignature.Entities.[0].MembersFunctionsAndValues -> x.DisplayName ] 
        |> shouldEqual ["p"]

[<Test>]
let ``Test multi project 1 all symbols`` () = 

    let p1A = checker.ParseAndCheckProject(Project1A.options) |> Async.RunSynchronously
    let p1B = checker.ParseAndCheckProject(Project1B.options) |> Async.RunSynchronously
    let mp = checker.ParseAndCheckProject(MultiProject1.options) |> Async.RunSynchronously

    let x1FromProject1A = 
        [ for s in p1A.GetAllUsesOfAllSymbols() do
             if  s.Symbol.DisplayName = "x1" then 
                 yield s.Symbol ]   |> List.head

    let x1FromProjectMultiProject = 
        [ for s in mp.GetAllUsesOfAllSymbols() do
             if  s.Symbol.DisplayName = "x1" then 
                 yield s.Symbol ]   |> List.head

    let usesOfx1FromProject1AInMultiProject1 = 
       mp.GetUsesOfSymbol(x1FromProject1A) 
            |> Array.map (fun s -> s.Symbol.DisplayName, MultiProject1.cleanFileName  s.FileName, tups s.Symbol.DeclarationLocation.Value) 

    let usesOfx1FromMultiProject1InMultiProject1 = 
       mp.GetUsesOfSymbol(x1FromProjectMultiProject) 
            |> Array.map (fun s -> s.Symbol.DisplayName, MultiProject1.cleanFileName  s.FileName, tups s.Symbol.DeclarationLocation.Value) 

    usesOfx1FromProject1AInMultiProject1 |> shouldEqual usesOfx1FromMultiProject1InMultiProject1

