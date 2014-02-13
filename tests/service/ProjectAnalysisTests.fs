#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "FsUnit.fs"
#load "Common.fs"
#else
module FSharp.Compiler.Service.Tests.ProjectAnalysisTests
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

// Create an interactive checker instance 
let checker = InteractiveChecker.Create()

module Inputs = 
    open System.IO

    let base1 = Path.GetTempFileName()
    let fileName1 = Path.ChangeExtension(base1, ".fs")
    let base2 = Path.GetTempFileName()
    let fileName2 = Path.ChangeExtension(base2, ".fs")
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1 = """
module M

type C() = 
    member x.P = 1

let xxx = 3 + 4
let fff () = xxx + xxx

type CAbbrev = C
    """
    File.WriteAllText(fileName1, fileSource1)

    let fileSource2 = """
module N

open M

type D1() = 
    member x.SomeProperty = M.xxx

type D2() = 
    member x.SomeProperty = M.fff() + D1().P

// Generate a warning
let y2 = match 1 with 1 -> M.xxx

// A class with some 'let' bindings
type D3(a:int) = 
    let b = a + 4

    [<DefaultValue(false)>]
    val mutable x : int

    member x.SomeProperty = a + b

let pair1,pair2 = (3 + 4 + int32 System.DateTime.Now.Ticks, 5 + 6)

// Check enum values
type SaveOptions = 
  | None = 0
  | DisableFormatting = 1

let enumValue = SaveOptions.DisableFormatting

let (++) x y = x + y
    
let c1 = 1 ++ 2

let c2 = 1 ++ 2

let mmmm1 : M.C = new M.C()             // note, these don't count as uses of CAbbrev
let mmmm2 : M.CAbbrev = new M.CAbbrev() // note, these don't count as uses of C

    """
    File.WriteAllText(fileName2, fileSource2)


let projectOptions = 
    checker.GetProjectOptionsFromCommandLineArgs
       (Inputs.projFileName,
        [| yield "--simpleresolution" 
           yield "--noframework" 
           yield "--debug:full" 
           yield "--define:DEBUG" 
           yield "--optimize-" 
           yield "--out:" + Inputs.dllName
           yield "--doc:test.xml" 
           yield "--warn:3" 
           yield "--fullpaths" 
           yield "--flaterrors" 
           yield "--target:library" 
           yield Inputs.fileName1
           yield Inputs.fileName2
           let references = 
             [ @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\mscorlib.dll" 
               @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\System.dll" 
               @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\System.Core.dll" 
               @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll"]  
           for r in references do
                 yield "-r:" + r |])


let rec allSymbolsInEntities compGen (entities: IList<FSharpEntity>) = 
    [ for e in entities do 
          yield (e :> FSharpSymbol) 
          for x in e.MembersFunctionsAndValues do
             if compGen || not x.IsCompilerGenerated then 
               yield (x :> FSharpSymbol)
          for x in e.UnionCases do
             yield (x :> FSharpSymbol)
          for x in e.FSharpFields do
             if compGen || not x.IsCompilerGenerated then 
                 yield (x :> FSharpSymbol)
          yield! allSymbolsInEntities compGen e.NestedEntities ]



[<Test>]
let ``Test project whole project errors`` () = 
  if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then // file references only valid on Windows 
    let wholeProjectResults = checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously
    wholeProjectResults .Errors.Length |> shouldEqual 2
    wholeProjectResults.Errors.[1].Message.Contains("Incomplete pattern matches on this expression") |> shouldEqual true // yes it does

    wholeProjectResults.Errors.[0].StartLine |> shouldEqual 9
    wholeProjectResults.Errors.[0].EndLine |> shouldEqual 9
    wholeProjectResults.Errors.[0].StartColumn |> shouldEqual 43
    wholeProjectResults.Errors.[0].EndColumn |> shouldEqual 44

[<Test>]
let ``Test project basic`` () = 
  if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then // file references only valid on Windows 

    let wholeProjectResults = checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously

    set [ for x in wholeProjectResults.AssemblySignature.Entities -> x.DisplayName ] |> shouldEqual (set ["N"; "M"])

    [ for x in wholeProjectResults.AssemblySignature.Entities.[0].NestedEntities -> x.DisplayName ] |> shouldEqual ["D1"; "D2"; "D3"; "SaveOptions" ]

    [ for x in wholeProjectResults.AssemblySignature.Entities.[1].NestedEntities -> x.DisplayName ] |> shouldEqual ["C"; "CAbbrev"]

    set [ for x in wholeProjectResults.AssemblySignature.Entities.[0].MembersFunctionsAndValues -> x.DisplayName ] 
        |> shouldEqual (set ["y2"; "pair2"; "pair1"; "( ++ )"; "c1"; "c2"; "mmmm1"; "mmmm2"; "enumValue" ])

[<Test>]
let ``Test project all symbols`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously
    let allSymbols = allSymbolsInEntities true wholeProjectResults.AssemblySignature.Entities
    set [ for x in allSymbols -> x.ToString() ] 
      |> 
       shouldEqual 
         (set 
           ["N"; "val y2"; "val pair2"; "val pair1"; "val enumValue"; "val ( ++ )";
            "val c1"; "val c2"; "val mmmm1"; "val mmmm2"; "D1"; "member ( .ctor )";
            "member SomeProperty"; "D2"; "member ( .ctor )"; "member SomeProperty";
            "D3"; "member ( .ctor )"; "member SomeProperty"; "field a"; "field b";
            "field x"; "SaveOptions"; "field value__"; "field None";
            "field DisableFormatting"; "M"; "val xxx"; "val fff"; "C";
            "member ( .ctor )"; "member P"; "CAbbrev"])


[<Test>]
let ``Test project all symbols excluding compiler generated`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously
    let allSymbolsNoCompGen = allSymbolsInEntities false wholeProjectResults.AssemblySignature.Entities
    [ for x in allSymbolsNoCompGen -> x.ToString() ] 
      |> shouldEqual 
              ["N"; "val y2"; "val pair2"; "val pair1"; "val enumValue"; "val ( ++ )";
               "val c1"; "val c2"; "val mmmm1"; "val mmmm2"; "D1"; "member ( .ctor )";
               "member SomeProperty"; "D2"; "member ( .ctor )"; "member SomeProperty";
               "D3"; "member ( .ctor )"; "member SomeProperty"; "field x"; "SaveOptions";
               "field None"; "field DisableFormatting"; "M"; "val xxx"; "val fff"; "C";
               "member ( .ctor )"; "member P"; "CAbbrev"]

[<Test>]
let ``Test project xxx symbols`` () = 

  if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then // file references only valid on Windows 
    let wholeProjectResults = checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously
    let backgroundParseResults1, backgroundTypedParse1 = 
        checker.GetBackgroundCheckResultsForFileInProject(Inputs.fileName1, projectOptions) 
        |> Async.RunSynchronously

    let xSymbol = backgroundTypedParse1.GetSymbolAtLocation(8,9,"",["xxx"]).Value
    xSymbol.ToString() |> shouldEqual "val xxx"

    let usesOfXSymbol = wholeProjectResults.GetUsesOfSymbol(xSymbol)
    usesOfXSymbol |> shouldEqual [|(Inputs.fileName1, ((6, 4), (6, 7)));
                                   (Inputs.fileName1, ((7, 13), (7, 16)));
                                   (Inputs.fileName1, ((7, 19), (7, 22)));
                                   (Inputs.fileName2, ((6, 28), (6, 33)));
                                   (Inputs.fileName2, ((12, 27), (12, 32)))|]

[<Test>]
let ``Test project all uses of all signature symbols`` () = 
  if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then // file references only valid on Windows 
    let wholeProjectResults = checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously
    let allSymbols = allSymbolsInEntities true wholeProjectResults.AssemblySignature.Entities
    let allUsesOfAllSymbols = 
        [ for s in allSymbols do 
             yield s.ToString(), wholeProjectResults.GetUsesOfSymbol(s) ]
    let expected =      
              [("N",
                [|(Inputs.fileName2, ((1, 7), (1, 8)))|]);
               ("field a", [| |]);
               ("field b", [| |]);
               ("field x",
                [|(Inputs.fileName2, ((19, 16), (19, 17)))|]);
               ("val pair1",
                [|(Inputs.fileName2, ((23, 4), (23, 9)))|]);
               ("val pair2",
                [|(Inputs.fileName2, ((23, 10), (23, 15)))|]);
               ("val y2",
                [|(Inputs.fileName2, ((12, 4), (12, 6)))|]);
               ("D1",
                [|(Inputs.fileName2, ((5, 5), (5, 7)));
                  (Inputs.fileName2, ((9, 38), (9, 40)))|]);
               ("member ( .ctor )",
                [|(Inputs.fileName2, ((5, 5), (5, 7)))|]);
               ("member SomeProperty",
                [|(Inputs.fileName2, ((6, 13), (6, 25)))|]);
               ("D2",
                [|(Inputs.fileName2, ((8, 5), (8, 7)))|]);
               ("member ( .ctor )",
                [|(Inputs.fileName2, ((8, 5), (8, 7)))|]);
               ("member SomeProperty",
                [|(Inputs.fileName2, ((9, 13), (9, 25)))|]);
               ("D3",
                [|(Inputs.fileName2, ((15, 5), (15, 7)))|]);
               ("member ( .ctor )",
                [|(Inputs.fileName2, ((15, 5), (15, 7)))|]);
               ("member SomeProperty",
                [|(Inputs.fileName2, ((21, 13), (21, 25)))|]);
               ("C",
                  [|(Inputs.fileName1, ((3, 5), (3, 6)));
                    (Inputs.fileName1, ((9, 15), (9, 16)));
                    (Inputs.fileName2, ((38, 12), (38, 15)));
                    (Inputs.fileName2, ((38, 22), (38, 25)))|]);
               ("M",
                  [|(Inputs.fileName1, ((1, 7), (1, 8)));
                    (Inputs.fileName2, ((6, 28), (6, 29)));
                    (Inputs.fileName2, ((9, 28), (9, 29)));
                    (Inputs.fileName2, ((12, 27), (12, 28)));
                    (Inputs.fileName2, ((38, 12), (38, 13)));
                    (Inputs.fileName2, ((38, 22), (38, 23)));
                    (Inputs.fileName2, ((39, 12), (39, 13)));
                    (Inputs.fileName2, ((39, 28), (39, 29)))|])
               ("SaveOptions",
                [|(Inputs.fileName2, ((26, 5), (26, 16)));
                  (Inputs.fileName2, ((30, 16), (30, 27)))|]);
               ("field value__", [||]);
               ("field None",
                [|(Inputs.fileName2, ((27, 4), (27, 8)))|]);
               ("field DisableFormatting",
                [|(Inputs.fileName2, ((28, 4), (28, 21)));
                  (Inputs.fileName2, ((30, 16), (30, 45)))|]);
               ("val enumValue",
                [|(Inputs.fileName2, ((30, 4), (30, 13)))|]);
               ("val ( ++ )",
                [|(Inputs.fileName2, ((32, 5), (32, 7)));
                  (Inputs.fileName2, ((34, 11), (34, 13)));
                  (Inputs.fileName2, ((36, 11), (36, 13)))|]);
               ("val c1",
                [|(Inputs.fileName2, ((34, 4), (34, 6)))|]);
               ("val c2",
                [|(Inputs.fileName2, ((36, 4), (36, 6)))|]);
               ("val mmmm1",
                [|(Inputs.fileName2, ((38, 4), (38, 9)))|]);
               ("val mmmm2",
                [|(Inputs.fileName2, ((39, 4), (39, 9)))|]);
               ("val xxx",
                [|(Inputs.fileName1, ((6, 4), (6, 7)));
                  (Inputs.fileName1, ((7, 13), (7, 16)));
                  (Inputs.fileName1, ((7, 19), (7, 22)));
                  (Inputs.fileName2, ((6, 28), (6, 33)));
                  (Inputs.fileName2, ((12, 27), (12, 32)))|]);
               ("val fff",
                [|(Inputs.fileName1, ((7, 4), (7, 7)));
                  (Inputs.fileName2, ((9, 28), (9, 33)))|]);
               ("member ( .ctor )",
                [|(Inputs.fileName1, ((3, 5), (3, 6)))|]);
               ("member P",
                [|(Inputs.fileName1, ((4, 13), (4, 14)))|])
               ("CAbbrev",
                [|(Inputs.fileName1, ((9, 5), (9, 12)));
                  (Inputs.fileName2, ((39, 12), (39, 21)));
                  (Inputs.fileName2, ((39, 28), (39, 37)))|])
              ]
    set allUsesOfAllSymbols - set expected |> shouldEqual Set.empty
    set expected - set allUsesOfAllSymbols |> shouldEqual Set.empty
    (set expected = set allUsesOfAllSymbols) |> shouldEqual true

[<Test>]
let ``Test project all uses of all symbols`` () = 
  if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then // file references only valid on Windows 
    let wholeProjectResults = checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously
    let allUsesOfAllSymbols = [ for (s,f,m) in wholeProjectResults.GetAllUsesOfAllSymbols() -> s.DisplayName, (if f = Inputs.fileName1 then "file1" else if f = Inputs.fileName2 then "file2" else "???"), m ]
    let expected =      
          [("C", "file1", ((3, 5), (3, 6))); ("( .ctor )", "file1", ((3, 5), (3, 6)));
           ("P", "file1", ((4, 13), (4, 14))); ("x", "file1", ((4, 11), (4, 12)));
           ("( + )", "file1", ((6, 12), (6, 13))); ("xxx", "file1", ((6, 4), (6, 7)));
           ("( + )", "file1", ((7, 17), (7, 18)));
           ("xxx", "file1", ((7, 13), (7, 16))); ("xxx", "file1", ((7, 19), (7, 22)));
           ("fff", "file1", ((7, 4), (7, 7))); ("C", "file1", ((9, 15), (9, 16)));
           ("C", "file1", ((9, 15), (9, 16))); ("C", "file1", ((9, 15), (9, 16)));
           ("C", "file1", ((9, 15), (9, 16))); ("CAbbrev", "file1", ((9, 5), (9, 12)));
           ("M", "file1", ((1, 7), (1, 8))); ("D1", "file2", ((5, 5), (5, 7)));
           ("( .ctor )", "file2", ((5, 5), (5, 7)));
           ("SomeProperty", "file2", ((6, 13), (6, 25)));
           ("x", "file2", ((6, 11), (6, 12))); ("M", "file2", ((6, 28), (6, 29)));
           ("xxx", "file2", ((6, 28), (6, 33))); ("D2", "file2", ((8, 5), (8, 7)));
           ("( .ctor )", "file2", ((8, 5), (8, 7)));
           ("SomeProperty", "file2", ((9, 13), (9, 25)));
           ("x", "file2", ((9, 11), (9, 12))); ("( + )", "file2", ((9, 36), (9, 37)));
           ("M", "file2", ((9, 28), (9, 29))); ("fff", "file2", ((9, 28), (9, 33)));
           ("D1", "file2", ((9, 38), (9, 40))); ("M", "file2", ((12, 27), (12, 28)));
           ("xxx", "file2", ((12, 27), (12, 32))); ("y2", "file2", ((12, 4), (12, 6)));
           ("DefaultValueAttribute", "file2", ((18, 6), (18, 18)));
           ("DefaultValueAttribute", "file2", ((18, 6), (18, 18)));
           ("DefaultValueAttribute", "file2", ((18, 6), (18, 18)));
           ("int", "file2", ((19, 20), (19, 23)));
           ("DefaultValueAttribute", "file2", ((18, 6), (18, 18)));
           ("DefaultValueAttribute", "file2", ((18, 6), (18, 18)));
           ("DefaultValueAttribute", "file2", ((18, 6), (18, 18)));
           ("x", "file2", ((19, 16), (19, 17))); ("D3", "file2", ((15, 5), (15, 7)));
           ("int", "file2", ((15, 10), (15, 13))); ("a", "file2", ((15, 8), (15, 9)));
           ("( .ctor )", "file2", ((15, 5), (15, 7)));
           ("SomeProperty", "file2", ((21, 13), (21, 25)));
           ("( + )", "file2", ((16, 14), (16, 15)));
           ("a", "file2", ((16, 12), (16, 13))); ("b", "file2", ((16, 8), (16, 9)));
           ("x", "file2", ((21, 11), (21, 12)));
           ("( + )", "file2", ((21, 30), (21, 31)));
           ("a", "file2", ((21, 28), (21, 29))); ("b", "file2", ((21, 32), (21, 33)));
           ("( + )", "file2", ((23, 25), (23, 26)));
           ("( + )", "file2", ((23, 21), (23, 22)));
           ("int32", "file2", ((23, 27), (23, 32)));
           ("DateTime", "file2", ((23, 40), (23, 48)));
           ("System", "file2", ((23, 33), (23, 39)));
           ("Now", "file2", ((23, 33), (23, 52)));
           ("Ticks", "file2", ((23, 33), (23, 58)));
           ("( + )", "file2", ((23, 62), (23, 63)));
           ("pair2", "file2", ((23, 10), (23, 15)));
           ("pair1", "file2", ((23, 4), (23, 9)));
           ("None", "file2", ((27, 4), (27, 8)));
           ("DisableFormatting", "file2", ((28, 4), (28, 21)));
           ("SaveOptions", "file2", ((26, 5), (26, 16)));
           ("SaveOptions", "file2", ((30, 16), (30, 27)));
           ("DisableFormatting", "file2", ((30, 16), (30, 45)));
           ("enumValue", "file2", ((30, 4), (30, 13)));
           ("x", "file2", ((32, 9), (32, 10))); ("y", "file2", ((32, 11), (32, 12)));
           ("( + )", "file2", ((32, 17), (32, 18)));
           ("x", "file2", ((32, 15), (32, 16))); ("y", "file2", ((32, 19), (32, 20)));
           ("( ++ )", "file2", ((32, 5), (32, 7)));
           ("( ++ )", "file2", ((34, 11), (34, 13)));
           ("c1", "file2", ((34, 4), (34, 6)));
           ("( ++ )", "file2", ((36, 11), (36, 13)));
           ("c2", "file2", ((36, 4), (36, 6))); ("M", "file2", ((38, 12), (38, 13)));
           ("C", "file2", ((38, 12), (38, 15))); ("M", "file2", ((38, 22), (38, 23)));
           ("C", "file2", ((38, 22), (38, 25))); ("C", "file2", ((38, 22), (38, 25)));
           ("mmmm1", "file2", ((38, 4), (38, 9)));
           ("M", "file2", ((39, 12), (39, 13)));
           ("CAbbrev", "file2", ((39, 12), (39, 21)));
           ("M", "file2", ((39, 28), (39, 29)));
           ("CAbbrev", "file2", ((39, 28), (39, 37)));
           ("C", "file2", ((39, 28), (39, 37)));
           ("mmmm2", "file2", ((39, 4), (39, 9))); ("N", "file2", ((1, 7), (1, 8)))]
    set allUsesOfAllSymbols - set expected |> shouldEqual Set.empty
    set expected - set allUsesOfAllSymbols |> shouldEqual Set.empty
    (set expected = set allUsesOfAllSymbols) |> shouldEqual true

[<Test>]
let ``Test file explicit parse symbols`` () = 
  if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then // file references only valid on Windows 

    let wholeProjectResults = checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously
    let parseResults1 = checker.ParseFileInProject(Inputs.fileName1, Inputs.fileSource1, projectOptions) 
    let parseResults2 = checker.ParseFileInProject(Inputs.fileName2, Inputs.fileSource2, projectOptions) 

    let checkResults1 = 
        checker.CheckFileInProject(parseResults1, Inputs.fileName1, 0, Inputs.fileSource1, projectOptions) 
        |> Async.RunSynchronously
        |> function CheckFileAnswer.Succeeded x ->  x | _ -> failwith "unexpected aborted"

    let checkResults2 = 
        checker.CheckFileInProject(parseResults2, Inputs.fileName2, 0, Inputs.fileSource2, projectOptions)
        |> Async.RunSynchronously
        |> function CheckFileAnswer.Succeeded x ->  x | _ -> failwith "unexpected aborted"

    let xSymbol2 = checkResults1.GetSymbolAtLocation(8,9,"",["xxx"]).Value
    let usesOfXSymbol2 = wholeProjectResults.GetUsesOfSymbol(xSymbol2)

    usesOfXSymbol2
         |> shouldEqual [|(Inputs.fileName1, ((6, 4), (6, 7)));
                          (Inputs.fileName1, ((7, 13), (7, 16)));
                          (Inputs.fileName1, ((7, 19), (7, 22)));
                          (Inputs.fileName2, ((6, 28), (6, 33)));
                          (Inputs.fileName2, ((12, 27), (12, 32)))|]

[<Test>]
let ``Test file explicit parse all symbols`` () = 
  if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then // file references only valid on Windows 

    let wholeProjectResults = checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously
    let parseResults1 = checker.ParseFileInProject(Inputs.fileName1, Inputs.fileSource1, projectOptions) 
    let parseResults2 = checker.ParseFileInProject(Inputs.fileName2, Inputs.fileSource2, projectOptions) 

    let checkResults1 = 
        checker.CheckFileInProject(parseResults1, Inputs.fileName1, 0, Inputs.fileSource1, projectOptions) 
        |> Async.RunSynchronously
        |> function CheckFileAnswer.Succeeded x ->  x | _ -> failwith "unexpected aborted"

    let checkResults2 = 
        checker.CheckFileInProject(parseResults2, Inputs.fileName2, 0, Inputs.fileSource2, projectOptions)
        |> Async.RunSynchronously
        |> function CheckFileAnswer.Succeeded x ->  x | _ -> failwith "unexpected aborted"

    let usesOfSymbols = checkResults1.GetAllUsesOfAllSymbolsInFile()
    let cleanedUsesOfSymbols = 
         [ for (s,f,m) in usesOfSymbols -> s.DisplayName, (if f = Inputs.fileName1 then "file1" else if f = Inputs.fileName2 then "file2" else "???"), m ]

    cleanedUsesOfSymbols 
       |> shouldEqual 
              [("C", "file1", ((3, 5), (3, 6))); ("( .ctor )", "file1", ((3, 5), (3, 6)));
               ("P", "file1", ((4, 13), (4, 14))); ("x", "file1", ((4, 11), (4, 12)));
               ("( + )", "file1", ((6, 12), (6, 13))); ("xxx", "file1", ((6, 4), (6, 7)));
               ("( + )", "file1", ((7, 17), (7, 18)));
               ("xxx", "file1", ((7, 13), (7, 16))); ("xxx", "file1", ((7, 19), (7, 22)));
               ("fff", "file1", ((7, 4), (7, 7))); ("C", "file1", ((9, 15), (9, 16)));
               ("C", "file1", ((9, 15), (9, 16))); ("C", "file1", ((9, 15), (9, 16)));
               ("C", "file1", ((9, 15), (9, 16))); ("CAbbrev", "file1", ((9, 5), (9, 12)));
               ("M", "file1", ((1, 7), (1, 8)))]


