
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

let numProjectsForStressTest = 100
let checker = InteractiveChecker.Create(numProjectsForStressTest + 10)

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

module MultiProject1

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
    wholeProjectResults.ProjectContext.GetReferencedAssemblies().Length |> shouldEqual 6

[<Test>]
let ``Test multi project 1 basic`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(MultiProject1.options) |> Async.RunSynchronously

    [ for x in wholeProjectResults.AssemblySignature.Entities -> x.DisplayName ] |> shouldEqual ["MultiProject1"]

    [ for x in wholeProjectResults.AssemblySignature.Entities.[0].NestedEntities -> x.DisplayName ] |> shouldEqual []


    [ for x in wholeProjectResults.AssemblySignature.Entities.[0].MembersFunctionsAndValues -> x.DisplayName ] 
        |> shouldEqual ["p"]

[<Test>]
let ``Test multi project 1 all symbols`` () = 

    let p1A = checker.ParseAndCheckProject(Project1A.options) |> Async.RunSynchronously
    let p1B = checker.ParseAndCheckProject(Project1B.options) |> Async.RunSynchronously
    let mp = checker.ParseAndCheckProject(MultiProject1.options) |> Async.RunSynchronously

    let x1FromProject1A = 
        [ for s in p1A.GetAllUsesOfAllSymbols() |> Async.RunSynchronously do
             if  s.Symbol.DisplayName = "x1" then 
                 yield s.Symbol ]   |> List.head

    let x1FromProjectMultiProject = 
        [ for s in mp.GetAllUsesOfAllSymbols() |> Async.RunSynchronously do
             if  s.Symbol.DisplayName = "x1" then 
                 yield s.Symbol ]   |> List.head

    let bFromProjectMultiProject = 
        [ for s in mp.GetAllUsesOfAllSymbols() |> Async.RunSynchronously do
             if  s.Symbol.DisplayName = "b" then 
                 yield s.Symbol ]   |> List.head

    x1FromProject1A.Assembly.FileName.IsNone |> shouldEqual true // For now, the assembly being analyzed doesn't return a filename
    x1FromProject1A.Assembly.QualifiedName |> shouldEqual "" // For now, the assembly being analyzed doesn't return a qualified name
    x1FromProject1A.Assembly.SimpleName |> shouldEqual (Path.GetFileNameWithoutExtension Project1A.dllName) 
    x1FromProjectMultiProject.Assembly.FileName |> shouldEqual (Some Project1A.dllName)
    bFromProjectMultiProject.Assembly.FileName |> shouldEqual  (Some Project1B.dllName)

    let usesOfx1FromProject1AInMultiProject1 = 
       mp.GetUsesOfSymbol(x1FromProject1A) 
            |> Async.RunSynchronously
            |> Array.map (fun s -> s.Symbol.DisplayName, MultiProject1.cleanFileName  s.FileName, tups s.Symbol.DeclarationLocation.Value) 

    let usesOfx1FromMultiProject1InMultiProject1 = 
       mp.GetUsesOfSymbol(x1FromProjectMultiProject) 
            |> Async.RunSynchronously
            |> Array.map (fun s -> s.Symbol.DisplayName, MultiProject1.cleanFileName  s.FileName, tups s.Symbol.DeclarationLocation.Value) 

    usesOfx1FromProject1AInMultiProject1 |> shouldEqual usesOfx1FromMultiProject1InMultiProject1

//------------------------------------------------------------------------------------



// A project referencing many sub-projects
module ManyProjectsStressTest = 
    open System.IO

    type Project = { ModuleName: string; FileName: string; Options: ProjectOptions; DllName: string } 
    let projects = 
        [ for i in 1 .. numProjectsForStressTest do 
                let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
                let moduleName = "Project" + string i
                let fileSource1 = "module " + moduleName + """

// Some random code
open System

type C() = 
    static member Print() = System.Console.WriteLine("Hello World")
    
let v = C()

let p = C.Print()

    """
                File.WriteAllText(fileName1, fileSource1)
                let base2 = Path.GetTempFileName()
                let dllName = Path.ChangeExtension(base2, ".dll")
                let projFileName = Path.ChangeExtension(base2, ".fsproj")
                let fileNames = [fileName1 ]
                let args = mkProjectCommandLineArgs (dllName, fileNames)
                let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)
                yield { ModuleName = moduleName; FileName=fileName1; Options = options; DllName=dllName } ]

    let jointProject = 
        let fileName = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
        let dllBase = Path.GetTempFileName()
        let dllName = Path.ChangeExtension(dllBase, ".dll")
        let projFileName = Path.ChangeExtension(dllBase, ".fsproj")
        let fileSource = 
            """

module JointProject

"""          + String.concat "\r\n" [ for p in projects -> "open " + p.ModuleName ] +  """

let p = (""" 
             + String.concat ",\r\n         " [ for p in projects -> p.ModuleName  + ".v" ] +  ")"
        File.WriteAllText(fileName, fileSource)

        let fileNames = [fileName]
        let args = mkProjectCommandLineArgs (dllName, fileNames)
        let options = 
            let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)
            { options with 
                ProjectOptions = Array.append options.ProjectOptions [| for p in  projects -> ("-r:" + p.DllName) |]
                ReferencedProjects = [| for p in projects -> (p.DllName, p.Options); |] }
        { ModuleName = "JointProject"; FileName=fileName; Options = options; DllName=dllName } 

    let cleanFileName a = 
        projects |> List.tryPick (fun m -> if a = m.FileName then Some m.ModuleName else None)
        |> function Some x -> x | None -> if a = jointProject.FileName then "fileN" else "??"



[<Test>]
let ``Test ManyProjectsStressTest whole project errors`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(ManyProjectsStressTest.jointProject.Options) |> Async.RunSynchronously

    wholeProjectResults .Errors.Length |> shouldEqual 0
    wholeProjectResults.ProjectContext.GetReferencedAssemblies().Length |> shouldEqual (numProjectsForStressTest + 4)

[<Test>]
let ``Test ManyProjectsStressTest basic`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(ManyProjectsStressTest.jointProject.Options) |> Async.RunSynchronously

    [ for x in wholeProjectResults.AssemblySignature.Entities -> x.DisplayName ] |> shouldEqual ["JointProject"]

    [ for x in wholeProjectResults.AssemblySignature.Entities.[0].NestedEntities -> x.DisplayName ] |> shouldEqual []


    [ for x in wholeProjectResults.AssemblySignature.Entities.[0].MembersFunctionsAndValues -> x.DisplayName ] 
        |> shouldEqual ["p"]

[<Test>]
let ``Test ManyProjectsStressTest all symbols`` () = 

  for i in 1 .. 100 do 
    printfn "stress test iteration %d (first may be slow, rest fast)" i
    let projectsResults = [ for p in ManyProjectsStressTest.projects -> p, checker.ParseAndCheckProject(p.Options) |> Async.RunSynchronously ]
    let jointProjectResults = checker.ParseAndCheckProject(ManyProjectsStressTest.jointProject.Options) |> Async.RunSynchronously

    let vsFromJointProject = 
        [ for s in jointProjectResults.GetAllUsesOfAllSymbols() |> Async.RunSynchronously do
             if  s.Symbol.DisplayName = "v" then 
                 yield s.Symbol ]   

    for (p,pResults) in projectsResults do 
        let vFromProject = 
            [ for s in pResults.GetAllUsesOfAllSymbols() |> Async.RunSynchronously do
                if  s.Symbol.DisplayName = "v" then 
                   yield s.Symbol ]   |> List.head 
        vFromProject.Assembly.FileName.IsNone |> shouldEqual true // For now, the assembly being analyzed doesn't return a filename
        vFromProject.Assembly.QualifiedName |> shouldEqual "" // For now, the assembly being analyzed doesn't return a qualified name
        vFromProject.Assembly.SimpleName |> shouldEqual (Path.GetFileNameWithoutExtension p.DllName) 

        let usesFromJointProject = 
            jointProjectResults.GetUsesOfSymbol(vFromProject) 
                |> Async.RunSynchronously
                |> Array.map (fun s -> s.Symbol.DisplayName, ManyProjectsStressTest.cleanFileName  s.FileName, tups s.Symbol.DeclarationLocation.Value) 

        usesFromJointProject.Length |> shouldEqual 1

//-----------------------------------------------------------------------------------------

module MultiProjectDirty1 = 
    open System.IO

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base1 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base1, ".dll")
    let projFileName = Path.ChangeExtension(base1, ".fsproj")
    let content = """module Project1

let x = "F#"
"""                   
    
    File.WriteAllText(fileName1, content)

    let cleanFileName a = if a = fileName1 then "Project1" else "??"

    let fileNames = [fileName1]
    
    let getOptions() = 
        let args = mkProjectCommandLineArgs (dllName, fileNames)
        checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)

module MultiProjectDirty2 = 
    open System.IO


    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base1 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base1, ".dll")
    let projFileName = Path.ChangeExtension(base1, ".fsproj")
    
    let content = """module Project2

open Project1

let y = x
let z = Project1.x
"""
    File.WriteAllText(fileName1, content)

    let cleanFileName a = if a = fileName1 then "Project2" else "??"

    let fileNames = [fileName1]    
   
    let getOptions() = 
        let args = mkProjectCommandLineArgs (dllName, fileNames)
        let options = checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)
        { options with 
            ProjectOptions = Array.append options.ProjectOptions [| ("-r:" + MultiProjectDirty1.dllName) |]
            ReferencedProjects = [| (MultiProjectDirty1.dllName, MultiProjectDirty1.getOptions()) |] }

[<Test>]
let ``Test multi project symbols should pick up changes in dependent projects`` () = 

    //  register to count the file checks
    let count = ref 0
    checker.FileChecked.Add (fun _ -> incr count)

    //---------------- Write the first version of the file in project 1 and check the project --------------------

    let proj1options = MultiProjectDirty1.getOptions()

    let wholeProjectResults1 = checker.ParseAndCheckProject(proj1options) |> Async.RunSynchronously

    count.Value |> shouldEqual 1

    let backgroundParseResults1, backgroundTypedParse1 = 
        checker.GetBackgroundCheckResultsForFileInProject(MultiProjectDirty1.fileName1, proj1options) 
        |> Async.RunSynchronously    

    count.Value |> shouldEqual 1

    //---------------- Get a symbol from project 1 and look up its uses in both projects --------------------

    let xSymbolUse = backgroundTypedParse1.GetSymbolUseAtLocation(3, 4, "", ["x"]) |> Async.RunSynchronously
    xSymbolUse.IsSome |> shouldEqual true  
    let xSymbol = xSymbolUse.Value.Symbol

    printfn "Symbol found. Checking symbol uses in another project..."

    let wholeProjectResults2 = checker.ParseAndCheckProject(MultiProjectDirty2.getOptions()) |> Async.RunSynchronously

    count.Value |> shouldEqual 2
    
    let _ = checker.ParseAndCheckProject(MultiProjectDirty2.getOptions()) |> Async.RunSynchronously

    count.Value |> shouldEqual 2 // cached

    let usesOfXSymbolInProject1 = 
        wholeProjectResults1.GetUsesOfSymbol(xSymbol) 
        |> Async.RunSynchronously
        |> Array.map (fun su -> su.Symbol.ToString(), MultiProjectDirty1.cleanFileName su.FileName, tups su.RangeAlternate)

    usesOfXSymbolInProject1
    |> shouldEqual 
        [|("val x", "Project1", ((3, 4), (3, 5))) |]

    let usesOfXSymbolInProject2 = 
        wholeProjectResults2.GetUsesOfSymbol(xSymbol) 
        |> Async.RunSynchronously
        |> Array.map (fun su -> su.Symbol.ToString(), MultiProjectDirty2.cleanFileName su.FileName, tups su.RangeAlternate)

    usesOfXSymbolInProject2 
    |> shouldEqual 
        [|("val x", "Project2", ((5, 8), (5, 9)));
          ("val x", "Project2", ((6, 8), (6, 18)))|]

    //---------------- Change the file by adding a line, then re-check everything --------------------
    
    let wt0 = System.DateTime.Now
    let wt1 = File.GetLastWriteTime MultiProjectDirty1.fileName1
    printfn "Writing new content to file '%s'" MultiProjectDirty1.fileName1
    File.WriteAllText(MultiProjectDirty1.fileName1, System.Environment.NewLine + MultiProjectDirty1.content)
    printfn "Wrote new content to file '%s'"  MultiProjectDirty1.fileName1
    let wt2 = File.GetLastWriteTime MultiProjectDirty1.fileName1
    printfn "Current time: '%A', ticks = %d"  wt0 wt0.Ticks
    printfn "Old write time: '%A', ticks = %d"  wt1 wt1.Ticks
    printfn "New write time: '%A', ticks = %d"  wt2 wt2.Ticks

    let wholeProjectResults1AfterChange1 = checker.ParseAndCheckProject(proj1options) |> Async.RunSynchronously
    System.Threading.Thread.Sleep(1000)
    count.Value |> shouldEqual 3

    let backgroundParseResults1AfterChange1, backgroundTypedParse1AfterChange1 = 
        checker.GetBackgroundCheckResultsForFileInProject(MultiProjectDirty1.fileName1, proj1options) 
        |> Async.RunSynchronously    

    let xSymbolUseAfterChange1 = backgroundTypedParse1AfterChange1.GetSymbolUseAtLocation(4, 4, "", ["x"]) |> Async.RunSynchronously
    xSymbolUseAfterChange1.IsSome |> shouldEqual true  
    let xSymbolAfterChange1 = xSymbolUseAfterChange1.Value.Symbol

    let wholeProjectResults2AfterChange1 = checker.ParseAndCheckProject(MultiProjectDirty2.getOptions()) |> Async.RunSynchronously

    System.Threading.Thread.Sleep(1000)
    count.Value |> shouldEqual 4

    let usesOfXSymbolInProject1AfterChange1 = 
        wholeProjectResults1AfterChange1.GetUsesOfSymbol(xSymbolAfterChange1) 
        |> Async.RunSynchronously
        |> Array.map (fun su -> su.Symbol.ToString(), MultiProjectDirty1.cleanFileName su.FileName, tups su.RangeAlternate)
    
    usesOfXSymbolInProject1AfterChange1
    |> shouldEqual 
        [|("val x", "Project1", ((4, 4), (4, 5))) |]

    let usesOfXSymbolInProject2AfterChange1 = 
        wholeProjectResults2AfterChange1.GetUsesOfSymbol(xSymbolAfterChange1) 
        |> Async.RunSynchronously
        |> Array.map (fun su -> su.Symbol.ToString(), MultiProjectDirty2.cleanFileName su.FileName, tups su.RangeAlternate)

    usesOfXSymbolInProject2AfterChange1 
    |> shouldEqual 
        [|("val x", "Project2", ((5, 8), (5, 9)));
          ("val x", "Project2", ((6, 8), (6, 18)))|]

    //---------------- Revert the change to the file --------------------

    printfn "Writing old content to file '%s'" MultiProjectDirty1.fileName1
    File.WriteAllText(MultiProjectDirty1.fileName1, MultiProjectDirty1.content)
    printfn "Wrote new content to file '%s'" MultiProjectDirty1.fileName1

    count.Value |> shouldEqual 4

    let wholeProjectResults2AfterChange2 = checker.ParseAndCheckProject(MultiProjectDirty2.getOptions()) |> Async.RunSynchronously

    System.Threading.Thread.Sleep(1000)
    count.Value |> shouldEqual 6 // note, causes two files to be type checked, one from each project


    let wholeProjectResults1AfterChange2 = checker.ParseAndCheckProject(proj1options) |> Async.RunSynchronously

    count.Value |> shouldEqual 6 // the project is already checked

    let backgroundParseResults1AfterChange2, backgroundTypedParse1AfterChange2 = 
        checker.GetBackgroundCheckResultsForFileInProject(MultiProjectDirty1.fileName1, proj1options) 
        |> Async.RunSynchronously    

    let xSymbolUseAfterChange2 = backgroundTypedParse1AfterChange2.GetSymbolUseAtLocation(4, 4, "", ["x"]) |> Async.RunSynchronously
    xSymbolUseAfterChange2.IsSome |> shouldEqual true  
    let xSymbolAfterChange2 = xSymbolUseAfterChange2.Value.Symbol


    let usesOfXSymbolInProject1AfterChange2 = 
        wholeProjectResults1AfterChange2.GetUsesOfSymbol(xSymbolAfterChange2) 
        |> Async.RunSynchronously
        |> Array.map (fun su -> su.Symbol.ToString(), MultiProjectDirty1.cleanFileName su.FileName, tups su.RangeAlternate)

    usesOfXSymbolInProject1AfterChange2
    |> shouldEqual 
        [|("val x", "Project1", ((3, 4), (3, 5))) |]


    let usesOfXSymbolInProject2AfterChange2 = 
        wholeProjectResults2AfterChange2.GetUsesOfSymbol(xSymbolAfterChange2) 
        |> Async.RunSynchronously
        |> Array.map (fun su -> su.Symbol.ToString(), MultiProjectDirty2.cleanFileName su.FileName, tups su.RangeAlternate)

    usesOfXSymbolInProject2AfterChange2
    |> shouldEqual 
        [|("val x", "Project2", ((5, 8), (5, 9)));
          ("val x", "Project2", ((6, 8), (6, 18)))|]

