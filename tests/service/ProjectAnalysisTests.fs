#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "FsUnit.fs"
#load "Common.fs"
#else
module FSharp.Compiler.Service.Tests.ProjectAnalysisTests
#endif


open NUnit.Framework
open FsUnit
open System
open System.IO
open System.Collections.Generic

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

open FSharp.Compiler.Service.Tests.Common

// Create an interactive checker instance 
let checker = InteractiveChecker.Create()

/// Extract range info 
let tups (m:Range.range) = (m.StartLine, m.StartColumn), (m.EndLine, m.EndColumn)

/// Extract range info  and convert to zero-based line  - please don't use this one any more
let tupsZ (m:Range.range) = (m.StartLine-1, m.StartColumn), (m.EndLine-1, m.EndColumn)

let attribsOfSymbolUse (s:FSharpSymbolUse) = 
    [ if s.IsFromDefinition then yield "defn" 
      if s.IsFromType then yield "type"
      if s.IsFromAttribute then yield "attribute"
      if s.IsFromDispatchSlotImplementation then yield "override"
      if s.IsFromPattern then yield "pattern" 
      if s.IsFromComputationExpression then yield "compexpr" ] 

module Project1 = 
    open System.IO

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
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

    let fileNames = [fileName1; fileName2]
    let args = mkProjectCommandLineArgs (dllName, fileNames)
    let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)
    let cleanFileName a = if a = fileName1 then "file1" else if a = fileName2 then "file2" else "??"

let rec allSymbolsInEntities compGen (entities: IList<FSharpEntity>) = 
    [ for e in entities do 
          yield (e :> FSharpSymbol) 
          for gp in e.GenericParameters do 
            if compGen || not gp.IsCompilerGenerated then 
             yield (gp :> FSharpSymbol)
          for x in e.MembersFunctionsAndValues do
             if compGen || not x.IsCompilerGenerated then 
               yield (x :> FSharpSymbol)
             for gp in x.GenericParameters do 
              if compGen || not gp.IsCompilerGenerated then 
               yield (gp :> FSharpSymbol)
          for x in e.UnionCases do
             yield (x :> FSharpSymbol)
             for f in x.UnionCaseFields do
                 if compGen || not f.IsCompilerGenerated then 
                     yield (f :> FSharpSymbol)
          for x in e.FSharpFields do
             if compGen || not x.IsCompilerGenerated then 
                 yield (x :> FSharpSymbol)
          yield! allSymbolsInEntities compGen e.NestedEntities ]



[<Test>]
let ``Test project1 whole project errors`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project1.options) |> Async.RunSynchronously
    wholeProjectResults .Errors.Length |> shouldEqual 2
    wholeProjectResults.Errors.[1].Message.Contains("Incomplete pattern matches on this expression") |> shouldEqual true // yes it does

    wholeProjectResults.Errors.[0].StartLineAlternate |> shouldEqual 10
    wholeProjectResults.Errors.[0].EndLineAlternate |> shouldEqual 10
    wholeProjectResults.Errors.[0].StartColumn |> shouldEqual 43
    wholeProjectResults.Errors.[0].EndColumn |> shouldEqual 44

[<Test>]
let ``Test project1 basic`` () = 


    let wholeProjectResults = checker.ParseAndCheckProject(Project1.options) |> Async.RunSynchronously

    set [ for x in wholeProjectResults.AssemblySignature.Entities -> x.DisplayName ] |> shouldEqual (set ["N"; "M"])

    [ for x in wholeProjectResults.AssemblySignature.Entities.[0].NestedEntities -> x.DisplayName ] |> shouldEqual ["D1"; "D2"; "D3"; "SaveOptions" ]

    [ for x in wholeProjectResults.AssemblySignature.Entities.[1].NestedEntities -> x.DisplayName ] |> shouldEqual ["C"; "CAbbrev"]

    set [ for x in wholeProjectResults.AssemblySignature.Entities.[0].MembersFunctionsAndValues -> x.DisplayName ] 
        |> shouldEqual (set ["y2"; "pair2"; "pair1"; "( ++ )"; "c1"; "c2"; "mmmm1"; "mmmm2"; "enumValue" ])

[<Test>]
let ``Test project1 all symbols`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project1.options) |> Async.RunSynchronously
    let allSymbols = allSymbolsInEntities true wholeProjectResults.AssemblySignature.Entities
    for s in allSymbols do 
        s.DeclarationLocation.IsSome |> shouldEqual true

    let allDeclarationLocations = 
        [ for s in allSymbols -> s.ToString(), s.DeclarationLocation.Value ]
            |> List.map (fun (s,m) -> s, Project1.cleanFileName  m.FileName, (m.StartLine, m.StartColumn), (m.EndLine, m.EndColumn )) //(a,b) -> (Project1.cleanFileName a, b))

    allDeclarationLocations |> shouldEqual
          [("N", "file2", (2, 7), (2, 8)); ("val y2", "file2", (13, 4), (13, 6));
           ("val pair2", "file2", (24, 10), (24, 15));
           ("val pair1", "file2", (24, 4), (24, 9));
           ("val enumValue", "file2", (31, 4), (31, 13));
           ("val ( ++ )", "file2", (33, 5), (33, 7));
           ("val c1", "file2", (35, 4), (35, 6));
           ("val c2", "file2", (37, 4), (37, 6));
           ("val mmmm1", "file2", (39, 4), (39, 9));
           ("val mmmm2", "file2", (40, 4), (40, 9)); 
           ("D1", "file2", (6, 5), (6, 7));
           ("member ( .ctor )", "file2", (6, 5), (6, 7));
           ("member SomeProperty", "file2", (7, 13), (7, 25));
           ("D2", "file2", (9, 5), (9, 7));
           ("member ( .ctor )", "file2", (9, 5), (9, 7));
           ("member SomeProperty", "file2", (10, 13), (10, 25));
           ("D3", "file2", (16, 5), (16, 7));
           ("member ( .ctor )", "file2", (16, 5), (16, 7));
           ("member SomeProperty", "file2", (22, 13), (22, 25));
           ("field a", "file2", (16, 8), (16, 9));
           ("field b", "file2", (17, 8), (17, 9));
           ("field x", "file2", (20, 16), (20, 17));
           ("SaveOptions", "file2", (27, 5), (27, 16));
           ("field value__", "file2", (28, 2), (29, 25));
           ("field None", "file2", (28, 4), (28, 8));
           ("field DisableFormatting", "file2", (29, 4), (29, 21));
           ("M", "file1", (2, 7), (2, 8)); ("val xxx", "file1", (7, 4), (7, 7));
           ("val fff", "file1", (8, 4), (8, 7)); ("C", "file1", (4, 5), (4, 6));
           ("member ( .ctor )", "file1", (4, 5), (4, 6));
           ("member P", "file1", (5, 13), (5, 14));
           ("CAbbrev", "file1", (10, 5), (10, 12))]

    for s in allSymbols do 
        s.ImplementationLocation.IsSome |> shouldEqual true

    let allImplementationLocations = 
        [ for s in allSymbols -> s.ToString(), s.ImplementationLocation.Value ]
            |> List.map (fun (s,m) -> s, Project1.cleanFileName  m.FileName, (m.StartLine, m.StartColumn), (m.EndLine, m.EndColumn )) //(a,b) -> (Project1.cleanFileName a, b))

    allImplementationLocations |> shouldEqual
          [("N", "file2", (2, 7), (2, 8)); ("val y2", "file2", (13, 4), (13, 6));
           ("val pair2", "file2", (24, 10), (24, 15));
           ("val pair1", "file2", (24, 4), (24, 9));
           ("val enumValue", "file2", (31, 4), (31, 13));
           ("val ( ++ )", "file2", (33, 5), (33, 7));
           ("val c1", "file2", (35, 4), (35, 6));
           ("val c2", "file2", (37, 4), (37, 6));
           ("val mmmm1", "file2", (39, 4), (39, 9));
           ("val mmmm2", "file2", (40, 4), (40, 9)); 
           ("D1", "file2", (6, 5), (6, 7));
           ("member ( .ctor )", "file2", (6, 5), (6, 7));
           ("member SomeProperty", "file2", (7, 13), (7, 25));
           ("D2", "file2", (9, 5), (9, 7));
           ("member ( .ctor )", "file2", (9, 5), (9, 7));
           ("member SomeProperty", "file2", (10, 13), (10, 25));
           ("D3", "file2", (16, 5), (16, 7));
           ("member ( .ctor )", "file2", (16, 5), (16, 7));
           ("member SomeProperty", "file2", (22, 13), (22, 25));
           ("field a", "file2", (16, 8), (16, 9));
           ("field b", "file2", (17, 8), (17, 9));
           ("field x", "file2", (20, 16), (20, 17));
           ("SaveOptions", "file2", (27, 5), (27, 16));
           ("field value__", "file2", (28, 2), (29, 25));
           ("field None", "file2", (28, 4), (28, 8));
           ("field DisableFormatting", "file2", (29, 4), (29, 21));
           ("M", "file1", (2, 7), (2, 8)); ("val xxx", "file1", (7, 4), (7, 7));
           ("val fff", "file1", (8, 4), (8, 7)); ("C", "file1", (4, 5), (4, 6));
           ("member ( .ctor )", "file1", (4, 5), (4, 6));
           ("member P", "file1", (5, 13), (5, 14));
           ("CAbbrev", "file1", (10, 5), (10, 12))]

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
let ``Test project1 all symbols excluding compiler generated`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project1.options) |> Async.RunSynchronously
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
let ``Test project1 xxx symbols`` () = 


    let wholeProjectResults = checker.ParseAndCheckProject(Project1.options) |> Async.RunSynchronously
    let backgroundParseResults1, backgroundTypedParse1 = 
        checker.GetBackgroundCheckResultsForFileInProject(Project1.fileName1, Project1.options) 
        |> Async.RunSynchronously

    let xSymbol = backgroundTypedParse1.GetSymbolAtLocationAlternate(9,9,"",["xxx"]).Value
    xSymbol.ToString() |> shouldEqual "val xxx"

    let usesOfXSymbol = 
        wholeProjectResults.GetUsesOfSymbol(xSymbol) 
        |> Array.map (fun su -> su.FileName , tupsZ su.RangeAlternate)
        |> Array.map (fun (a,b) -> (Project1.cleanFileName a, b))

    usesOfXSymbol |> shouldEqual [|("file1", ((6, 4), (6, 7)));
                                   ("file1", ((7, 13), (7, 16)));
                                   ("file1", ((7, 19), (7, 22)));
                                   ("file2", ((6, 28), (6, 33)));
                                   ("file2", ((12, 27), (12, 32)))|]

[<Test>]
let ``Test project1 all uses of all signature symbols`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project1.options) |> Async.RunSynchronously
    let allSymbols = allSymbolsInEntities true wholeProjectResults.AssemblySignature.Entities
    let allUsesOfAllSymbols = 
        [ for s in allSymbols do 
             yield s.ToString(), [| for s in wholeProjectResults.GetUsesOfSymbol(s) -> (Project1.cleanFileName s.FileName, tupsZ s.RangeAlternate) |] ]
    let expected =      
              [("N",
                [|("file2", ((1, 7), (1, 8)))|]);
               ("field a", [| |]);
               ("field b", [| |]);
               ("field x",
                [|("file2", ((19, 16), (19, 17)))|]);
               ("val pair1",
                [|("file2", ((23, 4), (23, 9)))|]);
               ("val pair2",
                [|("file2", ((23, 10), (23, 15)))|]);
               ("val y2",
                [|("file2", ((12, 4), (12, 6)))|]);
               ("D1",
                [|("file2", ((5, 5), (5, 7)));
                  ("file2", ((9, 38), (9, 40)))|]);
               ("member ( .ctor )",
                [|("file2", ((5, 5), (5, 7)))|]);
               ("member SomeProperty",
                [|("file2", ((6, 13), (6, 25)))|]);
               ("D2",
                [|("file2", ((8, 5), (8, 7)))|]);
               ("member ( .ctor )",
                [|("file2", ((8, 5), (8, 7)))|]);
               ("member SomeProperty",
                [|("file2", ((9, 13), (9, 25)))|]);
               ("D3",
                [|("file2", ((15, 5), (15, 7)))|]);
               ("member ( .ctor )",
                [|("file2", ((15, 5), (15, 7)))|]);
               ("member SomeProperty",
                [|("file2", ((21, 13), (21, 25)))|]);
               ("C",
                  [|("file1", ((3, 5), (3, 6)));
                    ("file1", ((9, 15), (9, 16)));
                    ("file2", ((38, 12), (38, 15)));
                    ("file2", ((38, 22), (38, 25)))|]);
               ("M",
                  [|("file1", ((1, 7), (1, 8)));
                    ("file2", ((6, 28), (6, 29)));
                    ("file2", ((9, 28), (9, 29)));
                    ("file2", ((12, 27), (12, 28)));
                    ("file2", ((38, 12), (38, 13)));
                    ("file2", ((38, 22), (38, 23)));
                    ("file2", ((39, 12), (39, 13)));
                    ("file2", ((39, 28), (39, 29)))|])
               ("SaveOptions",
                [|("file2", ((26, 5), (26, 16)));
                  ("file2", ((30, 16), (30, 27)))|]);
               ("field value__", [||]);
               ("field None",
                [|("file2", ((27, 4), (27, 8)))|]);
               ("field DisableFormatting",
                [|("file2", ((28, 4), (28, 21)));
                  ("file2", ((30, 16), (30, 45)))|]);
               ("val enumValue",
                [|("file2", ((30, 4), (30, 13)))|]);
               ("val ( ++ )",
                [|("file2", ((32, 5), (32, 7)));
                  ("file2", ((34, 11), (34, 13)));
                  ("file2", ((36, 11), (36, 13)))|]);
               ("val c1",
                [|("file2", ((34, 4), (34, 6)))|]);
               ("val c2",
                [|("file2", ((36, 4), (36, 6)))|]);
               ("val mmmm1",
                [|("file2", ((38, 4), (38, 9)))|]);
               ("val mmmm2",
                [|("file2", ((39, 4), (39, 9)))|]);
               ("val xxx",
                [|("file1", ((6, 4), (6, 7)));
                  ("file1", ((7, 13), (7, 16)));
                  ("file1", ((7, 19), (7, 22)));
                  ("file2", ((6, 28), (6, 33)));
                  ("file2", ((12, 27), (12, 32)))|]);
               ("val fff",
                [|("file1", ((7, 4), (7, 7)));
                  ("file2", ((9, 28), (9, 33)))|]);
               ("member ( .ctor )",
                [|("file1", ((3, 5), (3, 6)))|]);
               ("member P",
                [|("file1", ((4, 13), (4, 14)))|])
               ("CAbbrev",
                [|("file1", ((9, 5), (9, 12)));
                  ("file2", ((39, 12), (39, 21)));
                  ("file2", ((39, 28), (39, 37)))|])
              ]
    set allUsesOfAllSymbols - set expected |> shouldEqual Set.empty
    set expected - set allUsesOfAllSymbols |> shouldEqual Set.empty
    (set expected = set allUsesOfAllSymbols) |> shouldEqual true

[<Test>]
let ``Test project1 all uses of all symbols`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project1.options) |> Async.RunSynchronously
    let allUsesOfAllSymbols = [ for s in wholeProjectResults.GetAllUsesOfAllSymbols() -> s.Symbol.DisplayName, s.Symbol.FullName, Project1.cleanFileName s.FileName, tupsZ s.RangeAlternate ]
    let expected =      
          [("C", "M.C", "file1", ((3, 5), (3, 6)));
           ("( .ctor )", "M.C.( .ctor )", "file1", ((3, 5), (3, 6)));
           ("P", "M.C.P", "file1", ((4, 13), (4, 14)));
           ("x", "x", "file1", ((4, 11), (4, 12)));
           ("( + )", "Microsoft.FSharp.Core.Operators.( + )", "file1",
            ((6, 12), (6, 13))); ("xxx", "M.xxx", "file1", ((6, 4), (6, 7)));
           ("( + )", "Microsoft.FSharp.Core.Operators.( + )", "file1",
            ((7, 17), (7, 18))); ("xxx", "M.xxx", "file1", ((7, 13), (7, 16)));
           ("xxx", "M.xxx", "file1", ((7, 19), (7, 22)));
           ("fff", "M.fff", "file1", ((7, 4), (7, 7)));
           ("C", "M.C", "file1", ((9, 15), (9, 16)));
           ("C", "M.C", "file1", ((9, 15), (9, 16)));
           ("C", "M.C", "file1", ((9, 15), (9, 16)));
           ("C", "M.C", "file1", ((9, 15), (9, 16)));
           ("CAbbrev", "M.CAbbrev", "file1", ((9, 5), (9, 12)));
           ("M", "M", "file1", ((1, 7), (1, 8)));
           ("D1", "N.D1", "file2", ((5, 5), (5, 7)));
           ("( .ctor )", "N.D1.( .ctor )", "file2", ((5, 5), (5, 7)));
           ("SomeProperty", "N.D1.SomeProperty", "file2", ((6, 13), (6, 25)));
           ("x", "x", "file2", ((6, 11), (6, 12)));
           ("M", "M", "file2", ((6, 28), (6, 29)));
           ("xxx", "M.xxx", "file2", ((6, 28), (6, 33)));
           ("D2", "N.D2", "file2", ((8, 5), (8, 7)));
           ("( .ctor )", "N.D2.( .ctor )", "file2", ((8, 5), (8, 7)));
           ("SomeProperty", "N.D2.SomeProperty", "file2", ((9, 13), (9, 25)));
           ("x", "x", "file2", ((9, 11), (9, 12)));
           ("( + )", "Microsoft.FSharp.Core.Operators.( + )", "file2",
            ((9, 36), (9, 37))); ("M", "M", "file2", ((9, 28), (9, 29)));
           ("fff", "M.fff", "file2", ((9, 28), (9, 33)));
           ("D1", "N.D1", "file2", ((9, 38), (9, 40)));
           ("M", "M", "file2", ((12, 27), (12, 28)));
           ("xxx", "M.xxx", "file2", ((12, 27), (12, 32)));
           ("y2", "N.y2", "file2", ((12, 4), (12, 6)));
           ("DefaultValueAttribute", "Microsoft.FSharp.Core.DefaultValueAttribute",
            "file2", ((18, 6), (18, 18)));
           ("DefaultValueAttribute", "Microsoft.FSharp.Core.DefaultValueAttribute",
            "file2", ((18, 6), (18, 18)));
           ("DefaultValueAttribute", "Microsoft.FSharp.Core.DefaultValueAttribute",
            "file2", ((18, 6), (18, 18)));
           ("int", "Microsoft.FSharp.Core.int", "file2", ((19, 20), (19, 23)));
           ("DefaultValueAttribute", "Microsoft.FSharp.Core.DefaultValueAttribute",
            "file2", ((18, 6), (18, 18)));
           ("DefaultValueAttribute", "Microsoft.FSharp.Core.DefaultValueAttribute",
            "file2", ((18, 6), (18, 18)));
           ("DefaultValueAttribute", "Microsoft.FSharp.Core.DefaultValueAttribute",
            "file2", ((18, 6), (18, 18)));
           ("x", "N.D3.x", "file2", ((19, 16), (19, 17)));
           ("D3", "N.D3", "file2", ((15, 5), (15, 7)));
           ("int", "Microsoft.FSharp.Core.int", "file2", ((15, 10), (15, 13)));
           ("a", "a", "file2", ((15, 8), (15, 9)));
           ("( .ctor )", "N.D3.( .ctor )", "file2", ((15, 5), (15, 7)));
           ("SomeProperty", "N.D3.SomeProperty", "file2", ((21, 13), (21, 25)));
           ("( + )", "Microsoft.FSharp.Core.Operators.( + )", "file2",
            ((16, 14), (16, 15))); ("a", "a", "file2", ((16, 12), (16, 13)));
           ("b", "b", "file2", ((16, 8), (16, 9)));
           ("x", "x", "file2", ((21, 11), (21, 12)));
           ("( + )", "Microsoft.FSharp.Core.Operators.( + )", "file2",
            ((21, 30), (21, 31))); ("a", "a", "file2", ((21, 28), (21, 29)));
           ("b", "b", "file2", ((21, 32), (21, 33)));
           ("( + )", "Microsoft.FSharp.Core.Operators.( + )", "file2",
            ((23, 25), (23, 26)));
           ("( + )", "Microsoft.FSharp.Core.Operators.( + )", "file2",
            ((23, 21), (23, 22)));
           ("int32", "Microsoft.FSharp.Core.Operators.int32", "file2",
            ((23, 27), (23, 32)));
           ("DateTime", "System.DateTime", "file2", ((23, 40), (23, 48)));
           ("System", "System", "file2", ((23, 33), (23, 39)));
           ("Now", "System.DateTime.Now", "file2", ((23, 33), (23, 52)));
           ("Ticks", "System.DateTime.Ticks", "file2", ((23, 33), (23, 58)));
           ("( + )", "Microsoft.FSharp.Core.Operators.( + )", "file2",
            ((23, 62), (23, 63))); ("pair2", "N.pair2", "file2", ((23, 10), (23, 15)));
           ("pair1", "N.pair1", "file2", ((23, 4), (23, 9)));
           ("None", "N.SaveOptions.None", "file2", ((27, 4), (27, 8)));
           ("DisableFormatting", "N.SaveOptions.DisableFormatting", "file2",
            ((28, 4), (28, 21)));
           ("SaveOptions", "N.SaveOptions", "file2", ((26, 5), (26, 16)));
           ("SaveOptions", "N.SaveOptions", "file2", ((30, 16), (30, 27)));
           ("DisableFormatting", "N.SaveOptions.DisableFormatting", "file2",
            ((30, 16), (30, 45)));
           ("enumValue", "N.enumValue", "file2", ((30, 4), (30, 13)));
           ("x", "x", "file2", ((32, 9), (32, 10)));
           ("y", "y", "file2", ((32, 11), (32, 12)));
           ("( + )", "Microsoft.FSharp.Core.Operators.( + )", "file2",
            ((32, 17), (32, 18))); ("x", "x", "file2", ((32, 15), (32, 16)));
           ("y", "y", "file2", ((32, 19), (32, 20)));
           ("( ++ )", "N.( ++ )", "file2", ((32, 5), (32, 7)));
           ("( ++ )", "N.( ++ )", "file2", ((34, 11), (34, 13)));
           ("c1", "N.c1", "file2", ((34, 4), (34, 6)));
           ("( ++ )", "N.( ++ )", "file2", ((36, 11), (36, 13)));
           ("c2", "N.c2", "file2", ((36, 4), (36, 6)));
           ("M", "M", "file2", ((38, 12), (38, 13)));
           ("C", "M.C", "file2", ((38, 12), (38, 15)));
           ("M", "M", "file2", ((38, 22), (38, 23)));
           ("C", "M.C", "file2", ((38, 22), (38, 25)));
           ("C", "M.C", "file2", ((38, 22), (38, 25)));
           ("mmmm1", "N.mmmm1", "file2", ((38, 4), (38, 9)));
           ("M", "M", "file2", ((39, 12), (39, 13)));
           ("CAbbrev", "M.CAbbrev", "file2", ((39, 12), (39, 21)));
           ("M", "M", "file2", ((39, 28), (39, 29)));
           ("CAbbrev", "M.CAbbrev", "file2", ((39, 28), (39, 37)));
           ("C", "M.C", "file2", ((39, 28), (39, 37)));
           ("mmmm2", "N.mmmm2", "file2", ((39, 4), (39, 9)));
           ("N", "N", "file2", ((1, 7), (1, 8)))]

    set allUsesOfAllSymbols - set expected |> shouldEqual Set.empty
    set expected - set allUsesOfAllSymbols |> shouldEqual Set.empty
    (set expected = set allUsesOfAllSymbols) |> shouldEqual true

[<Test>]
let ``Test file explicit parse symbols`` () = 


    let wholeProjectResults = checker.ParseAndCheckProject(Project1.options) |> Async.RunSynchronously
    let parseResults1 = checker.ParseFileInProject(Project1.fileName1, Project1.fileSource1, Project1.options)  |> Async.RunSynchronously
    let parseResults2 = checker.ParseFileInProject(Project1.fileName2, Project1.fileSource2, Project1.options)  |> Async.RunSynchronously

    let checkResults1 = 
        checker.CheckFileInProject(parseResults1, Project1.fileName1, 0, Project1.fileSource1, Project1.options) 
        |> Async.RunSynchronously
        |> function CheckFileAnswer.Succeeded x ->  x | _ -> failwith "unexpected aborted"

    let checkResults2 = 
        checker.CheckFileInProject(parseResults2, Project1.fileName2, 0, Project1.fileSource2, Project1.options)
        |> Async.RunSynchronously
        |> function CheckFileAnswer.Succeeded x ->  x | _ -> failwith "unexpected aborted"

    let xSymbol2 = checkResults1.GetSymbolAtLocationAlternate(9,9,"",["xxx"]).Value
    let usesOfXSymbol2 = 
        [| for s in wholeProjectResults.GetUsesOfSymbol(xSymbol2) -> (Project1.cleanFileName s.FileName, tupsZ s.RangeAlternate) |] 

    let usesOfXSymbol21 = 
        [| for s in checkResults1.GetUsesOfSymbolInFile(xSymbol2) -> (Project1.cleanFileName s.FileName, tupsZ s.RangeAlternate) |] 

    let usesOfXSymbol22 = 
        [| for s in checkResults2.GetUsesOfSymbolInFile(xSymbol2) -> (Project1.cleanFileName s.FileName, tupsZ s.RangeAlternate) |] 

    usesOfXSymbol2
         |> shouldEqual [|("file1", ((6, 4), (6, 7)));
                          ("file1", ((7, 13), (7, 16)));
                          ("file1", ((7, 19), (7, 22)));
                          ("file2", ((6, 28), (6, 33)));
                          ("file2", ((12, 27), (12, 32)))|]

    usesOfXSymbol21
         |> shouldEqual [|("file1", ((6, 4), (6, 7)));
                          ("file1", ((7, 13), (7, 16)));
                          ("file1", ((7, 19), (7, 22)))|]

    usesOfXSymbol22
         |> shouldEqual [|("file2", ((6, 28), (6, 33)));
                          ("file2", ((12, 27), (12, 32)))|]


[<Test>]
let ``Test file explicit parse all symbols`` () = 


    let wholeProjectResults = checker.ParseAndCheckProject(Project1.options) |> Async.RunSynchronously
    let parseResults1 = checker.ParseFileInProject(Project1.fileName1, Project1.fileSource1, Project1.options) |> Async.RunSynchronously
    let parseResults2 = checker.ParseFileInProject(Project1.fileName2, Project1.fileSource2, Project1.options) |> Async.RunSynchronously

    let checkResults1 = 
        checker.CheckFileInProject(parseResults1, Project1.fileName1, 0, Project1.fileSource1, Project1.options) 
        |> Async.RunSynchronously
        |> function CheckFileAnswer.Succeeded x ->  x | _ -> failwith "unexpected aborted"

    let checkResults2 = 
        checker.CheckFileInProject(parseResults2, Project1.fileName2, 0, Project1.fileSource2, Project1.options)
        |> Async.RunSynchronously
        |> function CheckFileAnswer.Succeeded x ->  x | _ -> failwith "unexpected aborted"

    let usesOfSymbols = checkResults1.GetAllUsesOfAllSymbolsInFile()
    let cleanedUsesOfSymbols = 
         [ for s in usesOfSymbols -> s.Symbol.DisplayName, Project1.cleanFileName s.FileName, tupsZ s.RangeAlternate ]

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


//-----------------------------------------------------------------------------------------

module Project2 = 
    open System.IO

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1 = """
module M

type DUWithNormalFields = 
    | DU1 of int * int
    | DU2 of int * int
    | D of int * int

let _ = DU1(1, 2)
let _ = DU2(1, 2)
let _ = D(1, 2)

type DUWithNamedFields = DU of x : int * y : int

let _ = DU(x=1, y=2)

type GenericClass<'T>() = 
    member x.GenericMethod<'U>(t: 'T, u: 'U) = 1

let c = GenericClass<int>()
let _ = c.GenericMethod<int>(3, 4)

let GenericFunction (x:'T, y: 'T) = (x,y) : ('T * 'T)

let _ = GenericFunction(3, 4)
    """
    File.WriteAllText(fileName1, fileSource1)

    let fileNames = [fileName1]
    let args = mkProjectCommandLineArgs (dllName, fileNames)
    let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)




[<Test>]
let ``Test project2 whole project errors`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project2.options) |> Async.RunSynchronously
    wholeProjectResults .Errors.Length |> shouldEqual 0


[<Test>]
let ``Test project2 basic`` () = 


    let wholeProjectResults = checker.ParseAndCheckProject(Project2.options) |> Async.RunSynchronously

    set [ for x in wholeProjectResults.AssemblySignature.Entities -> x.DisplayName ] |> shouldEqual (set ["M"])

    [ for x in wholeProjectResults.AssemblySignature.Entities.[0].NestedEntities -> x.DisplayName ] |> shouldEqual ["DUWithNormalFields"; "DUWithNamedFields"; "GenericClass" ]

    set [ for x in wholeProjectResults.AssemblySignature.Entities.[0].MembersFunctionsAndValues -> x.DisplayName ] 
        |> shouldEqual (set ["c"; "GenericFunction"])

[<Test>]
let ``Test project2 all symbols in signature`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project2.options) |> Async.RunSynchronously
    let allSymbols = allSymbolsInEntities true wholeProjectResults.AssemblySignature.Entities
    [ for x in allSymbols -> x.ToString() ] 
       |> shouldEqual 
              ["M"; "val c"; "val GenericFunction"; "generic parameter T";
               "DUWithNormalFields"; "DU1"; "field Item1"; "field Item2"; "DU2";
               "field Item1"; "field Item2"; "D"; "field Item1"; "field Item2";
               "DUWithNamedFields"; "DU"; "field x"; "field y"; "GenericClass`1";
               "generic parameter T"; "member ( .ctor )"; "generic parameter T";
               "member GenericMethod"; "generic parameter T"; "generic parameter U"]

[<Test>]
let ``Test project2 all uses of all signature symbols`` () = 
    let wholeProjectResults = checker.ParseAndCheckProject(Project2.options) |> Async.RunSynchronously
    let allSymbols = allSymbolsInEntities true wholeProjectResults.AssemblySignature.Entities
    let allUsesOfAllSymbols = 
        [ for s in allSymbols do 
             let uses = [ for s in wholeProjectResults.GetUsesOfSymbol(s) -> (if s.FileName = Project2.fileName1 then "file1" else "??"), tupsZ s.RangeAlternate ]
             yield s.ToString(), uses ]
    let expected =      
          [("M", [("file1", ((1, 7), (1, 8)))]);
           ("val c", [("file1", ((19, 4), (19, 5))); ("file1", ((20, 8), (20, 9)))]);
           ("val GenericFunction",
            [("file1", ((22, 4), (22, 19))); ("file1", ((24, 8), (24, 23)))]);
           ("generic parameter T",
            [("file1", ((22, 23), (22, 25))); ("file1", ((22, 30), (22, 32)));
             ("file1", ((22, 45), (22, 47))); ("file1", ((22, 50), (22, 52)))]);
           ("DUWithNormalFields", [("file1", ((3, 5), (3, 23)))]);
           ("DU1", [("file1", ((4, 6), (4, 9))); ("file1", ((8, 8), (8, 11)))]);
           ("field Item1", [("file1", ((4, 6), (4, 9))); ("file1", ((8, 8), (8, 11)))]);
           ("field Item2", [("file1", ((4, 6), (4, 9))); ("file1", ((8, 8), (8, 11)))]);
           ("DU2", [("file1", ((5, 6), (5, 9))); ("file1", ((9, 8), (9, 11)))]);
           ("field Item1", [("file1", ((5, 6), (5, 9))); ("file1", ((9, 8), (9, 11)))]);
           ("field Item2", [("file1", ((5, 6), (5, 9))); ("file1", ((9, 8), (9, 11)))]);
           ("D", [("file1", ((6, 6), (6, 7))); ("file1", ((10, 8), (10, 9)))]);
           ("field Item1",
            [("file1", ((6, 6), (6, 7))); ("file1", ((10, 8), (10, 9)))]);
           ("field Item2",
            [("file1", ((6, 6), (6, 7))); ("file1", ((10, 8), (10, 9)))]);
           ("DUWithNamedFields", [("file1", ((12, 5), (12, 22)))]);
           ("DU", [("file1", ((12, 25), (12, 27))); ("file1", ((14, 8), (14, 10)))]);
           ("field x",
            [("file1", ((12, 25), (12, 27))); ("file1", ((14, 8), (14, 10)))]);
           ("field y",
            [("file1", ((12, 25), (12, 27))); ("file1", ((14, 8), (14, 10)))]);
           ("GenericClass`1",
            [("file1", ((16, 5), (16, 17))); ("file1", ((19, 8), (19, 20)))]);
           ("generic parameter T",
            [("file1", ((16, 18), (16, 20))); ("file1", ((17, 34), (17, 36)))]);
           ("member ( .ctor )", [("file1", ((16, 5), (16, 17)))]);
           ("generic parameter T",
            [("file1", ((16, 18), (16, 20))); ("file1", ((17, 34), (17, 36)))]);
           ("member GenericMethod",
            [("file1", ((17, 13), (17, 26))); ("file1", ((20, 8), (20, 23)))]);
           ("generic parameter T",
            [("file1", ((16, 18), (16, 20))); ("file1", ((17, 34), (17, 36)))]);
           ("generic parameter U",
            [("file1", ((17, 27), (17, 29))); ("file1", ((17, 41), (17, 43)))])]
    set allUsesOfAllSymbols - set expected |> shouldEqual Set.empty
    set expected - set allUsesOfAllSymbols |> shouldEqual Set.empty
    (set expected = set allUsesOfAllSymbols) |> shouldEqual true

[<Test>]
let ``Test project2 all uses of all symbols`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project2.options) |> Async.RunSynchronously
    let allUsesOfAllSymbols = 
        [ for s in wholeProjectResults.GetAllUsesOfAllSymbols() -> 
            s.Symbol.DisplayName, (if s.FileName = Project2.fileName1 then "file1" else "???"), tupsZ s.RangeAlternate ]
    let expected =      
            [("int", "file1", ((4, 13), (4, 16))); ("int", "file1", ((4, 19), (4, 22)));
            ("int", "file1", ((5, 13), (5, 16))); ("int", "file1", ((5, 19), (5, 22)));
            ("int", "file1", ((6, 11), (6, 14))); ("int", "file1", ((6, 17), (6, 20)));
            ("int", "file1", ((4, 13), (4, 16))); ("int", "file1", ((4, 19), (4, 22)));
            ("int", "file1", ((5, 13), (5, 16))); ("int", "file1", ((5, 19), (5, 22)));
            ("int", "file1", ((6, 11), (6, 14))); ("int", "file1", ((6, 17), (6, 20)));
            ("DU1", "file1", ((4, 6), (4, 9))); ("DU2", "file1", ((5, 6), (5, 9)));
            ("D", "file1", ((6, 6), (6, 7)));
            ("DUWithNormalFields", "file1", ((3, 5), (3, 23)));
            ("DU1", "file1", ((8, 8), (8, 11))); ("DU2", "file1", ((9, 8), (9, 11)));
            ("D", "file1", ((10, 8), (10, 9))); ("int", "file1", ((12, 35), (12, 38)));
            ("int", "file1", ((12, 45), (12, 48)));
            ("int", "file1", ((12, 35), (12, 38)));
            ("x", "file1", ((12, 31), (12, 32)));
            ("int", "file1", ((12, 45), (12, 48)));
            ("y", "file1", ((12, 41), (12, 42))); ("DU", "file1", ((12, 25), (12, 27)));
            ("DUWithNamedFields", "file1", ((12, 5), (12, 22)));
            ("DU", "file1", ((14, 8), (14, 10))); ("x", "file1", ((14, 11), (14, 12)));
            ("y", "file1", ((14, 16), (14, 17))); ("T", "file1", ((16, 18), (16, 20)));
            ("GenericClass", "file1", ((16, 5), (16, 17)));
            ("( .ctor )", "file1", ((16, 5), (16, 17)));
            ("U", "file1", ((17, 27), (17, 29))); ("T", "file1", ((17, 34), (17, 36)));
            ("U", "file1", ((17, 41), (17, 43)));
            ("GenericMethod", "file1", ((17, 13), (17, 26)));
            ("x", "file1", ((17, 11), (17, 12))); ("T", "file1", ((17, 34), (17, 36)));
            ("U", "file1", ((17, 41), (17, 43))); ("u", "file1", ((17, 38), (17, 39)));
            ("t", "file1", ((17, 31), (17, 32)));
            ("GenericClass", "file1", ((19, 8), (19, 20)));
            ("int", "file1", ((19, 21), (19, 24))); ("c", "file1", ((19, 4), (19, 5)));
            ("c", "file1", ((20, 8), (20, 9)));
            ("GenericMethod", "file1", ((20, 8), (20, 23)));
            ("int", "file1", ((20, 24), (20, 27)));
            ("T", "file1", ((22, 23), (22, 25))); ("T", "file1", ((22, 30), (22, 32)));
            ("y", "file1", ((22, 27), (22, 28))); ("x", "file1", ((22, 21), (22, 22)));
            ("T", "file1", ((22, 45), (22, 47))); ("T", "file1", ((22, 50), (22, 52)));
            ("x", "file1", ((22, 37), (22, 38))); ("y", "file1", ((22, 39), (22, 40)));
            ("GenericFunction", "file1", ((22, 4), (22, 19)));
            ("GenericFunction", "file1", ((24, 8), (24, 23)));
            ("M", "file1", ((1, 7), (1, 8)))]
    set allUsesOfAllSymbols - set expected |> shouldEqual Set.empty
    set expected - set allUsesOfAllSymbols |> shouldEqual Set.empty
    (set expected = set allUsesOfAllSymbols) |> shouldEqual true

//-----------------------------------------------------------------------------------------

module Project3 = 
    open System.IO

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1 = """
module M

type IFoo =
    abstract InterfaceProperty: string
    abstract InterfaceMethod: methodArg:string -> string
    [<CLIEvent>]
    abstract InterfaceEvent: IEvent<int>

[<AbstractClass>]
type CFoo() =
    abstract AbstractClassProperty: string
    abstract AbstractClassMethod: methodArg:string -> string
    [<CLIEvent>]
    abstract AbstractClassEvent: IEvent<int>

type CBaseFoo() =
    let ev = Event<_>()
    abstract BaseClassProperty: string
    abstract BaseClassMethod: methodArg:string -> string
    [<CLIEvent>]
    abstract BaseClassEvent: IEvent<int>
    default __.BaseClassProperty = "dflt"
    default __.BaseClassMethod(m) = m
    [<CLIEvent>]
    default __.BaseClassEvent = ev.Publish

type IFooImpl() =
    let ev = Event<_>()
    interface IFoo with
        member this.InterfaceProperty = "v"
        member this.InterfaceMethod(x) = x
        [<CLIEvent>]
        member this.InterfaceEvent = ev.Publish

type CFooImpl() =
    inherit CFoo()
    let ev = Event<_>()
    override this.AbstractClassProperty = "v"
    override this.AbstractClassMethod(x) = x
    [<CLIEvent>]
    override this.AbstractClassEvent = ev.Publish

type CBaseFooImpl() =
    inherit CBaseFoo()
    let ev = Event<_>()
    override this.BaseClassProperty = "v"
    override this.BaseClassMethod(x) = x
    [<CLIEvent>]
    override this.BaseClassEvent = ev.Publish

let IFooImplObjectExpression() =
    let ev = Event<_>()
    { new IFoo with
        member this.InterfaceProperty = "v"
        member this.InterfaceMethod(x) = x
        [<CLIEvent>]
        member this.InterfaceEvent = ev.Publish }

let CFooImplObjectExpression() =
    let ev = Event<_>()
    { new CFoo() with
        override this.AbstractClassProperty = "v"
        override this.AbstractClassMethod(x) = x
        [<CLIEvent>]
        override this.AbstractClassEvent = ev.Publish }

let getP (foo: IFoo) = foo.InterfaceProperty
let getE (foo: IFoo) = foo.InterfaceEvent
let getM (foo: IFoo) = foo.InterfaceMethod("d")
    """
    File.WriteAllText(fileName1, fileSource1)

    let fileNames = [fileName1]
    let args = mkProjectCommandLineArgs (dllName, fileNames)
    let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)




[<Test>]
let ``Test project3 whole project errors`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project3.options) |> Async.RunSynchronously
    wholeProjectResults .Errors.Length |> shouldEqual 0


[<Test>]
let ``Test project3 basic`` () = 


    let wholeProjectResults = checker.ParseAndCheckProject(Project3.options) |> Async.RunSynchronously

    set [ for x in wholeProjectResults.AssemblySignature.Entities -> x.DisplayName ] |> shouldEqual (set ["M"])

    [ for x in wholeProjectResults.AssemblySignature.Entities.[0].NestedEntities -> x.DisplayName ] 
        |> shouldEqual ["IFoo"; "CFoo"; "CBaseFoo"; "IFooImpl"; "CFooImpl"; "CBaseFooImpl"]

    [ for x in wholeProjectResults.AssemblySignature.Entities.[0].MembersFunctionsAndValues -> x.DisplayName ] 
        |> shouldEqual ["IFooImplObjectExpression"; "CFooImplObjectExpression"; "getP"; "getE";"getM"]

[<Test>]
let ``Test project3 all symbols in signature`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project3.options) |> Async.RunSynchronously
    let allSymbols = allSymbolsInEntities false wholeProjectResults.AssemblySignature.Entities
    [ for x in allSymbols -> x.ToString() ] 
      |> shouldEqual 
              ["M"; "val IFooImplObjectExpression"; "val CFooImplObjectExpression";
               "val getP"; "val getE"; "val getM"; "IFoo"; "member InterfaceMethod";
               "member add_InterfaceEvent"; "member InterfaceEvent";
               "member InterfaceProperty"; "member remove_InterfaceEvent"; "CFoo";
               "member ( .ctor )"; "member AbstractClassMethod";
               "member add_AbstractClassEvent"; "member AbstractClassEvent";
               "member AbstractClassProperty"; "member remove_AbstractClassEvent";
               "CBaseFoo"; "member ( .ctor )"; "member BaseClassMethod";
               "member add_BaseClassEvent"; "member BaseClassEvent";
               "member BaseClassProperty"; "member remove_BaseClassEvent"; "IFooImpl";
               "member ( .ctor )"; "CFooImpl"; "member ( .ctor )"; "CBaseFooImpl";
               "member ( .ctor )"]


[<Test>]
let ``Test project3 all uses of all signature symbols`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project3.options) |> Async.RunSynchronously
    let allSymbols = allSymbolsInEntities false wholeProjectResults.AssemblySignature.Entities
    let allUsesOfAllSymbols = 
        [ for s in allSymbols do 
             let uses = [ for s in wholeProjectResults.GetUsesOfSymbol(s) -> 
                            ((if s.FileName = Project3.fileName1 then "file1" else "??"), 
                             tupsZ s.RangeAlternate, attribsOfSymbolUse s) ]
             yield s.ToString(), uses ]
    let expected =      
          [("M", [("file1", ((1, 7), (1, 8)), ["defn"])]);
           ("val IFooImplObjectExpression", [("file1", ((51, 4), (51, 28)), ["defn"])]);
           ("val CFooImplObjectExpression", [("file1", ((59, 4), (59, 28)), ["defn"])]);
           ("val getP", [("file1", ((67, 4), (67, 8)), ["defn"])]);
           ("val getE", [("file1", ((68, 4), (68, 8)), ["defn"])]);
           ("val getM", [("file1", ((69, 4), (69, 8)), ["defn"])]);
           ("IFoo",
            [("file1", ((3, 5), (3, 9)), ["defn"]);
             ("file1", ((29, 14), (29, 18)), ["type"]);
             ("file1", ((53, 10), (53, 14)), ["type"]);
             ("file1", ((67, 15), (67, 19)), ["type"]);
             ("file1", ((68, 15), (68, 19)), ["type"]);
             ("file1", ((69, 15), (69, 19)), ["type"])]);
           ("member InterfaceMethod",
            [("file1", ((5, 13), (5, 28)), ["defn"]);
             ("file1", ((55, 20), (55, 35)), ["override"]);
             ("file1", ((69, 23), (69, 42)), []);
             ("file1", ((31, 20), (31, 35)), ["override"])]);
           ("member add_InterfaceEvent",
            [("file1", ((7, 13), (7, 27)), ["defn"]);
             ("file1", ((57, 20), (57, 34)), ["override"]);
             ("file1", ((68, 23), (68, 41)), []);
             ("file1", ((33, 20), (33, 34)), ["override"])]);
           ("member InterfaceEvent",
            [("file1", ((7, 13), (7, 27)), ["defn"]);
             ("file1", ((57, 20), (57, 34)), ["override"]);
             ("file1", ((33, 20), (33, 34)), ["override"])]);
           ("member InterfaceProperty",
            [("file1", ((4, 13), (4, 30)), ["defn"]);
             ("file1", ((54, 20), (54, 37)), ["override"]);
             ("file1", ((67, 23), (67, 44)), []);
             ("file1", ((30, 20), (30, 37)), ["override"])]);
           ("member remove_InterfaceEvent",
            [("file1", ((7, 13), (7, 27)), ["defn"]);
             ("file1", ((57, 20), (57, 34)), ["override"]);
             ("file1", ((33, 20), (33, 34)), ["override"])]);
           ("CFoo",
            [("file1", ((10, 5), (10, 9)), ["defn"]);
             ("file1", ((36, 12), (36, 16)), ["type"]);
             ("file1", ((36, 12), (36, 16)), []);
             ("file1", ((61, 10), (61, 14)), ["type"]);
             ("file1", ((61, 10), (61, 14)), [])]);
           ("member ( .ctor )", [("file1", ((10, 5), (10, 9)), ["defn"])]);
           ("member AbstractClassMethod",
            [("file1", ((12, 13), (12, 32)), ["defn"]);
             ("file1", ((63, 22), (63, 41)), ["override"]);
             ("file1", ((39, 18), (39, 37)), ["override"])]);
           ("member add_AbstractClassEvent",
            [("file1", ((14, 13), (14, 31)), ["defn"]);
             ("file1", ((65, 22), (65, 40)), ["override"]);
             ("file1", ((41, 18), (41, 36)), ["override"])]);
           ("member AbstractClassEvent",
            [("file1", ((14, 13), (14, 31)), ["defn"]);
             ("file1", ((65, 22), (65, 40)), ["override"]);
             ("file1", ((41, 18), (41, 36)), ["override"])]);
           ("member AbstractClassProperty",
            [("file1", ((11, 13), (11, 34)), ["defn"]);
             ("file1", ((62, 22), (62, 43)), ["override"]);
             ("file1", ((38, 18), (38, 39)), ["override"])]);
           ("member remove_AbstractClassEvent",
            [("file1", ((14, 13), (14, 31)), ["defn"]);
             ("file1", ((65, 22), (65, 40)), ["override"]);
             ("file1", ((41, 18), (41, 36)), ["override"])]);
           ("CBaseFoo",
            [("file1", ((16, 5), (16, 13)), ["defn"]);
             ("file1", ((44, 12), (44, 20)), ["type"]);
             ("file1", ((44, 12), (44, 20)), [])]);
           ("member ( .ctor )", [("file1", ((16, 5), (16, 13)), ["defn"])]);
           ("member BaseClassMethod",
            [("file1", ((19, 13), (19, 28)), ["defn"]);
             ("file1", ((23, 15), (23, 30)), ["override"]);
             ("file1", ((47, 18), (47, 33)), ["override"])]);
           ("member add_BaseClassEvent",
            [("file1", ((21, 13), (21, 27)), ["defn"]);
             ("file1", ((25, 15), (25, 29)), ["override"]);
             ("file1", ((49, 18), (49, 32)), ["override"])]);
           ("member BaseClassEvent",
            [("file1", ((21, 13), (21, 27)), ["defn"]);
             ("file1", ((25, 15), (25, 29)), ["override"]);
             ("file1", ((49, 18), (49, 32)), ["override"])]);
           ("member BaseClassProperty",
            [("file1", ((18, 13), (18, 30)), ["defn"]);
             ("file1", ((22, 15), (22, 32)), ["override"]);
             ("file1", ((46, 18), (46, 35)), ["override"])]);
           ("member remove_BaseClassEvent",
            [("file1", ((21, 13), (21, 27)), ["defn"]);
             ("file1", ((25, 15), (25, 29)), ["override"]);
             ("file1", ((49, 18), (49, 32)), ["override"])]);
           ("IFooImpl", [("file1", ((27, 5), (27, 13)), ["defn"])]);
           ("member ( .ctor )", [("file1", ((27, 5), (27, 13)), ["defn"])]);
           ("CFooImpl", [("file1", ((35, 5), (35, 13)), ["defn"])]);
           ("member ( .ctor )", [("file1", ((35, 5), (35, 13)), ["defn"])]);
           ("CBaseFooImpl", [("file1", ((43, 5), (43, 17)), ["defn"])]);
           ("member ( .ctor )", [("file1", ((43, 5), (43, 17)), ["defn"])])]

    set allUsesOfAllSymbols - set expected |> shouldEqual Set.empty
    set expected - set allUsesOfAllSymbols |> shouldEqual Set.empty
    (set expected = set allUsesOfAllSymbols) |> shouldEqual true

//-----------------------------------------------------------------------------------------

module Project4 = 
    open System.IO

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1 = """
module M

type Foo<'T>(x : 'T, y : Foo<'T>) = class end

let inline twice(x : ^U, y : ^U) = x + y
    """
    File.WriteAllText(fileName1, fileSource1)

    let fileNames = [fileName1]
    let args = mkProjectCommandLineArgs (dllName, fileNames)
    let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)




[<Test>]
let ``Test project4 whole project errors`` () = 
    let wholeProjectResults = checker.ParseAndCheckProject(Project4.options) |> Async.RunSynchronously
    wholeProjectResults .Errors.Length |> shouldEqual 0


[<Test>]
let ``Test project4 basic`` () = 
    let wholeProjectResults = checker.ParseAndCheckProject(Project4.options) |> Async.RunSynchronously

    set [ for x in wholeProjectResults.AssemblySignature.Entities -> x.DisplayName ] |> shouldEqual (set ["M"])

    [ for x in wholeProjectResults.AssemblySignature.Entities.[0].NestedEntities -> x.DisplayName ] 
        |> shouldEqual ["Foo"]

    [ for x in wholeProjectResults.AssemblySignature.Entities.[0].MembersFunctionsAndValues -> x.DisplayName ] 
        |> shouldEqual ["twice"]

[<Test>]
let ``Test project4 all symbols in signature`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project4.options) |> Async.RunSynchronously
    let allSymbols = allSymbolsInEntities false wholeProjectResults.AssemblySignature.Entities
    [ for x in allSymbols -> x.ToString() ] 
      |> shouldEqual 
                ["M"; "val twice"; "generic parameter U"; "Foo`1"; "generic parameter T"; "member ( .ctor )"; "generic parameter T"]


[<Test>]
let ``Test project4 all uses of all signature symbols`` () = 
    let wholeProjectResults = checker.ParseAndCheckProject(Project4.options) |> Async.RunSynchronously
    let allSymbols = allSymbolsInEntities false wholeProjectResults.AssemblySignature.Entities
    let allUsesOfAllSymbols = 
        [ for s in allSymbols do 
             let uses = [ for s in wholeProjectResults.GetUsesOfSymbol(s) -> (if s.FileName = Project4.fileName1 then "file1" else "??"), tupsZ s.RangeAlternate ]
             yield s.ToString(), uses ]
    let expected =      
      [("M", [("file1", ((1, 7), (1, 8)))]);
       ("val twice", [("file1", ((5, 11), (5, 16)))]); ("generic parameter U",  [("file1", ((5, 21), (5, 23))); ("file1", ((5, 29), (5, 31)))]);
       ("Foo`1", [("file1", ((3, 5), (3, 8))); ("file1", ((3, 25), (3, 28)))]);
       ("generic parameter T",
        [("file1", ((3, 9), (3, 11))); ("file1", ((3, 17), (3, 19)));
         ("file1", ((3, 29), (3, 31)))]);
       ("member ( .ctor )", [("file1", ((3, 5), (3, 8)))]);
       ("generic parameter T",
        [("file1", ((3, 9), (3, 11))); ("file1", ((3, 17), (3, 19)));
         ("file1", ((3, 29), (3, 31)))])]
    
    set allUsesOfAllSymbols - set expected |> shouldEqual Set.empty
    set expected - set allUsesOfAllSymbols |> shouldEqual Set.empty
    (set expected = set allUsesOfAllSymbols) |> shouldEqual true

[<Test>]
let ``Test project4 T symbols`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project4.options) |> Async.RunSynchronously
    let backgroundParseResults1, backgroundTypedParse1 = 
        checker.GetBackgroundCheckResultsForFileInProject(Project4.fileName1, Project4.options) 
        |> Async.RunSynchronously

    let tSymbol2 = backgroundTypedParse1.GetSymbolAtLocationAlternate(4,19,"",["T"])
    tSymbol2.IsSome |> shouldEqual true
    tSymbol2.Value.ToString() |> shouldEqual "generic parameter T"

    tSymbol2.Value.ImplementationLocation.IsSome |> shouldEqual true

    let uses = backgroundTypedParse1.GetAllUsesOfAllSymbolsInFile()
    let allUsesOfAllSymbols = 
        [ for s in uses -> s.Symbol.ToString(), (if s.FileName = Project4.fileName1 then "file1" else "??"), tupsZ s.RangeAlternate ]
    allUsesOfAllSymbols |> shouldEqual
          [("generic parameter T", "file1", ((3, 9), (3, 11)));
           ("Foo`1", "file1", ((3, 5), (3, 8)));
           ("generic parameter T", "file1", ((3, 17), (3, 19)));
           ("Foo`1", "file1", ((3, 25), (3, 28)));
           ("generic parameter T", "file1", ((3, 29), (3, 31)));
           ("val y", "file1", ((3, 21), (3, 22)));
           ("val x", "file1", ((3, 13), (3, 14)));
           ("member ( .ctor )", "file1", ((3, 5), (3, 8)));
           ("generic parameter U", "file1", ((5, 21), (5, 23)));
           ("generic parameter U", "file1", ((5, 29), (5, 31)));
           ("val y", "file1", ((5, 25), (5, 26)));
           ("val x", "file1", ((5, 17), (5, 18)));
           ("val ( + )", "file1", ((5, 37), (5, 38)));
           ("val x", "file1", ((5, 35), (5, 36)));
           ("val y", "file1", ((5, 39), (5, 40)));
           ("val twice", "file1", ((5, 11), (5, 16)));
           ("M", "file1", ((1, 7), (1, 8)))]

    let tSymbol3 = backgroundTypedParse1.GetSymbolAtLocationAlternate(4,11,"",["T"])
    tSymbol3.IsSome |> shouldEqual true
    tSymbol3.Value.ToString() |> shouldEqual "generic parameter T"

    tSymbol3.Value.ImplementationLocation.IsSome |> shouldEqual true

    let usesOfTSymbol2 = 
        wholeProjectResults.GetUsesOfSymbol(tSymbol2.Value) 
        |> Array.map (fun su -> su.FileName , tupsZ su.RangeAlternate)
        |> Array.map (fun (a,b) -> (if a = Project4.fileName1 then "file1" else "??"), b)

    usesOfTSymbol2 |> shouldEqual 
          [|("file1", ((3, 9), (3, 11))); ("file1", ((3, 17), (3, 19)));
            ("file1", ((3, 29), (3, 31)))|]

    let usesOfTSymbol3 = 
        wholeProjectResults.GetUsesOfSymbol(tSymbol3.Value) 
        |> Array.map (fun su -> su.FileName , tupsZ su.RangeAlternate)
        |> Array.map (fun (a,b) -> (if a = Project4.fileName1 then "file1" else "??"), b)

    usesOfTSymbol3 |> shouldEqual usesOfTSymbol2

    let uSymbol2 = backgroundTypedParse1.GetSymbolAtLocationAlternate(6,23,"",["U"])
    uSymbol2.IsSome |> shouldEqual true
    uSymbol2.Value.ToString() |> shouldEqual "generic parameter U"

    uSymbol2.Value.ImplementationLocation.IsSome |> shouldEqual true

    let usesOfUSymbol2 = 
        wholeProjectResults.GetUsesOfSymbol(uSymbol2.Value) 
        |> Array.map (fun su -> su.FileName , tupsZ su.RangeAlternate)
        |> Array.map (fun (a,b) -> (if a = Project4.fileName1 then "file1" else "??"), b)

    usesOfUSymbol2 |> shouldEqual  [|("file1", ((5, 21), (5, 23))); ("file1", ((5, 29), (5, 31)))|]

//-----------------------------------------------------------------------------------------


module Project5 = 
    open System.IO


    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1 = """
module ActivePatterns 


let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd


let TestNumber input =
   match input with
   | Even -> printfn "%d is even" input
   | Odd -> printfn "%d is odd" input


let (|Float|_|) (str: string) =
   let mutable floatvalue = 0.0
   if System.Double.TryParse(str, &floatvalue) then Some(floatvalue)
   else None


let parseNumeric str =
   match str with
   | Float f -> printfn "%f : Floating point" f
   | _ -> printfn "%s : Not matched." str
    """
    File.WriteAllText(fileName1, fileSource1)

    let cleanFileName a = if a = fileName1 then "file1" else "??"

    let fileNames = [fileName1]
    let args = mkProjectCommandLineArgs (dllName, fileNames)
    let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)


[<Test>]
let ``Test project5 whole project errors`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project5.options) |> Async.RunSynchronously
    wholeProjectResults.Errors.Length |> shouldEqual 0


[<Test>]
let ``Test project 5 all symbols`` () =

    let wholeProjectResults = checker.ParseAndCheckProject(Project5.options) |> Async.RunSynchronously

    let allUsesOfAllSymbols = 
        wholeProjectResults.GetAllUsesOfAllSymbols()
        |> Array.map (fun su -> su.Symbol.ToString(), su.Symbol.FullName, Project5.cleanFileName su.FileName, tupsZ su.RangeAlternate, attribsOfSymbolUse su)

    allUsesOfAllSymbols |> shouldEqual
          [|("symbol ", "Even", "file1", ((4, 6), (4, 10)), ["defn"]);
            ("symbol ", "Odd", "file1", ((4, 11), (4, 14)), ["defn"]);
            ("val input", "input", "file1", ((4, 17), (4, 22)), ["defn"]);
            ("val ( = )", "Microsoft.FSharp.Core.Operators.( = )", "file1",
             ((4, 38), (4, 39)), []);
            ("val ( % )", "Microsoft.FSharp.Core.Operators.( % )", "file1",
             ((4, 34), (4, 35)), []);
            ("val input", "input", "file1", ((4, 28), (4, 33)), []);
            ("symbol ", "Even", "file1", ((4, 47), (4, 51)), []);
            ("symbol ", "Odd", "file1", ((4, 57), (4, 60)), []);
            ("val ( |Even|Odd| )", "ActivePatterns.( |Even|Odd| )", "file1",
             ((4, 5), (4, 15)), ["defn"]);
            ("val input", "input", "file1", ((7, 15), (7, 20)), ["defn"]);
            ("val input", "input", "file1", ((8, 9), (8, 14)), []);
            ("symbol Even", "ActivePatterns.( |Even|Odd| ).Even", "file1",
             ((9, 5), (9, 9)), ["pattern"]);
            ("val printfn", "Microsoft.FSharp.Core.ExtraTopLevelOperators.printfn",
             "file1", ((9, 13), (9, 20)), []);
            ("val input", "input", "file1", ((9, 34), (9, 39)), []);
            ("symbol Odd", "ActivePatterns.( |Even|Odd| ).Odd", "file1",
             ((10, 5), (10, 8)), ["pattern"]);
            ("val printfn", "Microsoft.FSharp.Core.ExtraTopLevelOperators.printfn",
             "file1", ((10, 12), (10, 19)), []);
            ("val input", "input", "file1", ((10, 32), (10, 37)), []);
            ("val TestNumber", "ActivePatterns.TestNumber", "file1", ((7, 4), (7, 14)),
             ["defn"]); ("symbol ", "Float", "file1", ((13, 6), (13, 11)), ["defn"]);
            ("string", "Microsoft.FSharp.Core.string", "file1", ((13, 22), (13, 28)),
             ["type"]); ("val str", "str", "file1", ((13, 17), (13, 20)), ["defn"]);
            ("val floatvalue", "floatvalue", "file1", ((14, 15), (14, 25)), ["defn"]);
            ("Double", "System.Double", "file1", ((15, 13), (15, 19)), []);
            ("System", "System", "file1", ((15, 6), (15, 12)), []);
            ("val str", "str", "file1", ((15, 29), (15, 32)), []);
            ("val ( ~& )",
             "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators.( ~& )",
             "file1", ((15, 34), (15, 35)), []);
            ("val floatvalue", "floatvalue", "file1", ((15, 35), (15, 45)), []);
            ("symbol TryParse", "System.Double.TryParse", "file1", ((15, 6), (15, 28)),
             []);
            ("Some", "Microsoft.FSharp.Core.Option<_>.Some", "file1",
             ((15, 52), (15, 56)), []);
            ("val floatvalue", "floatvalue", "file1", ((15, 57), (15, 67)), []);
            ("None", "Microsoft.FSharp.Core.Option<_>.None", "file1",
             ((16, 8), (16, 12)), []);
            ("val ( |Float|_| )", "ActivePatterns.( |Float|_| )", "file1",
             ((13, 5), (13, 14)), ["defn"]);
            ("val str", "str", "file1", ((19, 17), (19, 20)), ["defn"]);
            ("val str", "str", "file1", ((20, 9), (20, 12)), []);
            ("val f", "f", "file1", ((21, 11), (21, 12)), ["defn"]);
            ("symbol Float", "ActivePatterns.( |Float|_| ).Float", "file1",
             ((21, 5), (21, 10)), ["pattern"]);
            ("val printfn", "Microsoft.FSharp.Core.ExtraTopLevelOperators.printfn",
             "file1", ((21, 16), (21, 23)), []);
            ("val f", "f", "file1", ((21, 46), (21, 47)), []);
            ("val printfn", "Microsoft.FSharp.Core.ExtraTopLevelOperators.printfn",
             "file1", ((22, 10), (22, 17)), []);
            ("val str", "str", "file1", ((22, 38), (22, 41)), []);
            ("val parseNumeric", "ActivePatterns.parseNumeric", "file1",
             ((19, 4), (19, 16)), ["defn"]);
            ("ActivePatterns", "ActivePatterns", "file1", ((1, 7), (1, 21)), ["defn"])|]

[<Test>]
let ``Test complete active patterns's exact ranges from uses of symbols`` () =

    let wholeProjectResults = checker.ParseAndCheckProject(Project5.options) |> Async.RunSynchronously
    let backgroundParseResults1, backgroundTypedParse1 = 
        checker.GetBackgroundCheckResultsForFileInProject(Project5.fileName1, Project5.options) 
        |> Async.RunSynchronously


    let oddSymbol = backgroundTypedParse1.GetSymbolAtLocationAlternate(11,8,"",["Odd"])
    oddSymbol.IsSome |> shouldEqual true  
    oddSymbol.Value.ToString() |> shouldEqual "symbol Odd"

    let evenSymbol = backgroundTypedParse1.GetSymbolAtLocationAlternate(10,9,"",["Even"])
    evenSymbol.IsSome |> shouldEqual true  
    evenSymbol.Value.ToString() |> shouldEqual "symbol Even"

    let usesOfEvenSymbol = 
        wholeProjectResults.GetUsesOfSymbol(evenSymbol.Value) 
        |> Array.map (fun su -> su.Symbol.ToString(), Project5.cleanFileName su.FileName, tupsZ su.RangeAlternate)

    let usesOfOddSymbol = 
        wholeProjectResults.GetUsesOfSymbol(oddSymbol.Value) 
        |> Array.map (fun su -> su.Symbol.ToString(), Project5.cleanFileName su.FileName, tupsZ su.RangeAlternate)

    usesOfEvenSymbol |> shouldEqual 
          [|("symbol Even", "file1", ((4, 6), (4, 10)));
            ("symbol Even", "file1", ((4, 47), (4, 51)));
            ("symbol Even", "file1", ((9, 5), (9, 9)))|]

    usesOfOddSymbol |> shouldEqual 
          [|("symbol Odd", "file1", ((4, 11), (4, 14)));
            ("symbol Odd", "file1", ((4, 57), (4, 60)));
            ("symbol Odd", "file1", ((10, 5), (10, 8)))|]


[<Test>]
let ``Test partial active patterns's exact ranges from uses of symbols`` () =

    let wholeProjectResults = checker.ParseAndCheckProject(Project5.options) |> Async.RunSynchronously
    let backgroundParseResults1, backgroundTypedParse1 = 
        checker.GetBackgroundCheckResultsForFileInProject(Project5.fileName1, Project5.options) 
        |> Async.RunSynchronously    


    let floatSymbol = backgroundTypedParse1.GetSymbolAtLocationAlternate(22,10,"",["Float"])
    floatSymbol.IsSome |> shouldEqual true  
    floatSymbol.Value.ToString() |> shouldEqual "symbol Float"


    let usesOfFloatSymbol = 
        wholeProjectResults.GetUsesOfSymbol(floatSymbol.Value) 
        |> Array.map (fun su -> su.Symbol.ToString(), Project5.cleanFileName su.FileName, tups su.RangeAlternate)

    usesOfFloatSymbol |> shouldEqual 
          [|("symbol Float", "file1", ((14, 6), (14, 11)));
            ("symbol Float", "file1", ((22, 5), (22, 10)))|]

    // Should also return its definition
    backgroundTypedParse1.GetSymbolAtLocationAlternate(14,11,"",["Float"]).IsSome |> shouldEqual true


//-----------------------------------------------------------------------------------------

module Project6 = 
    open System.IO

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1 = """
module Exceptions

exception Fail of string

let f () =
   raise (Fail "unknown")
    """
    File.WriteAllText(fileName1, fileSource1)

    let cleanFileName a = if a = fileName1 then "file1" else "??"

    let fileNames = [fileName1]
    let args = mkProjectCommandLineArgs (dllName, fileNames)
    let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)


[<Test>]
let ``Test project6 whole project errors`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project6.options) |> Async.RunSynchronously
    wholeProjectResults.Errors.Length |> shouldEqual 0


[<Test>]
let ``Test project 6 all symbols`` () =

    let wholeProjectResults = checker.ParseAndCheckProject(Project6.options) |> Async.RunSynchronously

    let allUsesOfAllSymbols = 
        wholeProjectResults.GetAllUsesOfAllSymbols()
        |> Array.map (fun su -> su.Symbol.ToString(), Project6.cleanFileName su.FileName, tupsZ su.RangeAlternate)

    allUsesOfAllSymbols |> shouldEqual
          [|("string", "file1", ((3, 18), (3, 24)));
            ("Fail", "file1", ((3, 10), (3, 14)));
            ("val raise", "file1", ((6, 3), (6, 8)));
            ("Fail", "file1", ((6, 10), (6, 14)));
            ("val f", "file1", ((5, 4), (5, 5)));
            ("Exceptions", "file1", ((1, 7), (1, 17)))|]


//-----------------------------------------------------------------------------------------

module Project7 = 
    open System.IO

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1 = """
module NamedArgs

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


[<Test>]
let ``Test project7 whole project errors`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project7.options) |> Async.RunSynchronously
    wholeProjectResults.Errors.Length |> shouldEqual 0


[<Test>]
let ``Test project 7 all symbols`` () =

    let wholeProjectResults = checker.ParseAndCheckProject(Project7.options) |> Async.RunSynchronously

    let allUsesOfAllSymbols = 
        wholeProjectResults.GetAllUsesOfAllSymbols()
        |> Array.map (fun su -> su.Symbol.ToString(), su.Symbol.DisplayName, Project7.cleanFileName su.FileName, tups su.RangeAlternate)

    let arg1symbol = wholeProjectResults.GetAllUsesOfAllSymbols() |> Array.pick (fun x -> if x.Symbol.DisplayName = "arg1" then Some x.Symbol else None)
    let arg1uses = 
        wholeProjectResults.GetUsesOfSymbol(arg1symbol) 
        |> Array.map (fun su -> Option.map tups su.Symbol.DeclarationLocation, Project7.cleanFileName su.FileName, tups su.RangeAlternate)
    arg1uses |> shouldEqual
     [|(Some ((5, 20), (5, 24)), "file1", ((5, 20), (5, 24)));
       (Some ((5, 20), (5, 24)), "file1", ((5, 57), (5, 61)));
       (Some ((5, 20), (5, 24)), "file1", ((7, 13), (7, 17)));
       (Some ((5, 20), (5, 24)), "file1", ((9, 13), (9, 17)))|]


//-----------------------------------------------------------------------------------------
module Project8 = 
    open System.IO

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1 = """
module NamedUnionFields

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


[<Test>]
let ``Test project8 whole project errors`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project8.options) |> Async.RunSynchronously
    wholeProjectResults.Errors.Length |> shouldEqual 0


[<Test>]
let ``Test project 8 all symbols`` () =

    let wholeProjectResults = checker.ParseAndCheckProject(Project8.options) |> Async.RunSynchronously

    let allUsesOfAllSymbols = 
        wholeProjectResults.GetAllUsesOfAllSymbols()
        |> Array.map (fun su -> su.Symbol.ToString(), su.Symbol.DisplayName, Project8.cleanFileName su.FileName, tups su.RangeAlternate, attribsOfSymbolUse su)

    allUsesOfAllSymbols |> shouldEqual
          [|("int", "int", "file1", ((4, 19), (4, 22)), ["type"]);
            ("int", "int", "file1", ((4, 31), (4, 34)), ["type"]);
            ("int", "int", "file1", ((4, 19), (4, 22)), ["type"]);
            ("parameter xxx", "xxx", "file1", ((4, 14), (4, 17)), ["defn"]);
            ("int", "int", "file1", ((4, 31), (4, 34)), ["type"]);
            ("parameter yyy", "yyy", "file1", ((4, 25), (4, 28)), ["defn"]);
            ("B", "B", "file1", ((4, 9), (4, 10)), ["defn"]);
            ("A", "A", "file1", ((4, 5), (4, 6)), ["defn"]);
            ("B", "B", "file1", ((5, 8), (5, 9)), []);
            ("parameter xxx", "xxx", "file1", ((5, 10), (5, 13)), []);
            ("parameter yyy", "yyy", "file1", ((5, 17), (5, 20)), []);
            ("val b", "b", "file1", ((5, 4), (5, 5)), ["defn"]);
            ("val b", "b", "file1", ((8, 10), (8, 11)), []);
            ("parameter xxx", "xxx", "file1", ((10, 9), (10, 12)), ["pattern"]);
            ("parameter yyy", "yyy", "file1", ((10, 18), (10, 21)), ["pattern"]);
            ("val b", "b", "file1", ((10, 24), (10, 25)), ["defn"]);
            ("val a", "a", "file1", ((10, 15), (10, 16)), ["defn"]);
            ("B", "B", "file1", ((10, 6), (10, 7)), ["pattern"]);
            ("val x", "x", "file1", ((7, 4), (7, 5)), ["defn"]);
            ("NamedUnionFields", "NamedUnionFields", "file1", ((2, 7), (2, 23)),
             ["defn"])|]

    let arg1symbol = wholeProjectResults.GetAllUsesOfAllSymbols() |> Array.pick (fun x -> if x.Symbol.DisplayName = "xxx" then Some x.Symbol else None)
    let arg1uses = 
        wholeProjectResults.GetUsesOfSymbol(arg1symbol) 
        |> Array.map (fun su -> Option.map tups su.Symbol.DeclarationLocation, Project8.cleanFileName su.FileName, tups su.RangeAlternate)
    arg1uses |> shouldEqual
     [|(Some ((4, 14), (4, 17)), "file1", ((4, 14), (4, 17)));
       (Some ((4, 14), (4, 17)), "file1", ((5, 10), (5, 13)));
       (Some ((4, 14), (4, 17)), "file1", ((10, 9), (10, 12)))|]

//-----------------------------------------------------------------------------------------
module Project9 = 
    open System.IO

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1 = """
module Constraints

let inline check< ^T when ^T : (static member IsInfinity : ^T -> bool)> (num: ^T) : ^T option =
    if (^T : (static member IsInfinity: ^T -> bool) (num)) then None
    else Some num
    """
    File.WriteAllText(fileName1, fileSource1)

    let cleanFileName a = if a = fileName1 then "file1" else "??"

    let fileNames = [fileName1]
    let args = mkProjectCommandLineArgs (dllName, fileNames)
    let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)


[<Test>]
let ``Test project9 whole project errors`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project9.options) |> Async.RunSynchronously
    wholeProjectResults.Errors.Length |> shouldEqual 0


[<Test>]
let ``Test project 9 all symbols`` () =

    let wholeProjectResults = checker.ParseAndCheckProject(Project9.options) |> Async.RunSynchronously

    let allUsesOfAllSymbols = 
        wholeProjectResults.GetAllUsesOfAllSymbols()
        |> Array.map (fun su -> su.Symbol.ToString(), su.Symbol.DisplayName, Project9.cleanFileName su.FileName, tups su.RangeAlternate)

    allUsesOfAllSymbols |> shouldEqual
          [|("generic parameter T", "T", "file1", ((4, 18), (4, 20)));
            ("generic parameter T", "T", "file1", ((4, 26), (4, 28)));
            ("generic parameter T", "T", "file1", ((4, 59), (4, 61)));
            ("bool", "bool", "file1", ((4, 65), (4, 69)));
            ("parameter IsInfinity", "IsInfinity", "file1", ((4, 46), (4, 56)));
            ("generic parameter T", "T", "file1", ((4, 78), (4, 80)));
            ("val num", "num", "file1", ((4, 73), (4, 76)));
            ("option`1", "option", "file1", ((4, 87), (4, 93)));
            ("generic parameter T", "T", "file1", ((4, 84), (4, 86)));
            ("generic parameter T", "T", "file1", ((5, 8), (5, 10)));
            ("generic parameter T", "T", "file1", ((5, 40), (5, 42)));
            ("bool", "bool", "file1", ((5, 46), (5, 50)));
            ("parameter IsInfinity", "IsInfinity", "file1", ((5, 28), (5, 38)));
            ("val num", "num", "file1", ((5, 53), (5, 56)));
            ("None", "None", "file1", ((5, 64), (5, 68)));
            ("Some", "Some", "file1", ((6, 9), (6, 13)));
            ("val num", "num", "file1", ((6, 14), (6, 17)));
            ("val check", "check", "file1", ((4, 11), (4, 16)));
            ("Constraints", "Constraints", "file1", ((2, 7), (2, 18)))|]

    let arg1symbol = wholeProjectResults.GetAllUsesOfAllSymbols() |> Array.pick (fun x -> if x.Symbol.DisplayName = "IsInfinity" then Some x.Symbol else None)
    let arg1uses = 
        wholeProjectResults.GetUsesOfSymbol(arg1symbol) 
        |> Array.map (fun su -> Option.map tups su.Symbol.DeclarationLocation, Project9.cleanFileName su.FileName, tups su.RangeAlternate)
    arg1uses |> shouldEqual
     [|(Some ((4, 46), (4, 56)), "file1", ((4, 46), (4, 56)))|]

//-----------------------------------------------------------------------------------------
// see https://github.com/fsharp/FSharp.Compiler.Service/issues/95

module Project10 = 
    open System.IO

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1 = """
module NamedArgs

type C() = 
    static member M(url: string, query: int)  = ()

C.M("http://goo", query = 1)

    """
    File.WriteAllText(fileName1, fileSource1)

    let cleanFileName a = if a = fileName1 then "file1" else "??"

    let fileNames = [fileName1]
    let args = mkProjectCommandLineArgs (dllName, fileNames)
    let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)


[<Test>]
let ``Test Project10 whole project errors`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project10.options) |> Async.RunSynchronously
    wholeProjectResults.Errors.Length |> shouldEqual 0


[<Test>]
let ``Test Project10 all symbols`` () =

    let wholeProjectResults = checker.ParseAndCheckProject(Project10.options) |> Async.RunSynchronously

    let allUsesOfAllSymbols = 
        wholeProjectResults.GetAllUsesOfAllSymbols()
        |> Array.map (fun su -> su.Symbol.ToString(), su.Symbol.DisplayName, Project10.cleanFileName su.FileName, tups su.RangeAlternate)

    allUsesOfAllSymbols |> shouldEqual
          [|("C", "C", "file1", ((4, 5), (4, 6)));
            ("member ( .ctor )", "( .ctor )", "file1", ((4, 5), (4, 6)));
            ("string", "string", "file1", ((5, 25), (5, 31)));
            ("int", "int", "file1", ((5, 40), (5, 43)));
            ("member M", "M", "file1", ((5, 18), (5, 19)));
            ("string", "string", "file1", ((5, 25), (5, 31)));
            ("int", "int", "file1", ((5, 40), (5, 43)));
            ("val url", "url", "file1", ((5, 20), (5, 23)));
            ("val query", "query", "file1", ((5, 33), (5, 38)));
            ("C", "C", "file1", ((7, 0), (7, 1)));
            ("member M", "M", "file1", ((7, 0), (7, 3)));
            ("parameter query", "query", "file1", ((7, 18), (7, 23)));
            ("NamedArgs", "NamedArgs", "file1", ((2, 7), (2, 16)))|]

    let backgroundParseResults1, backgroundTypedParse1 = 
        checker.GetBackgroundCheckResultsForFileInProject(Project10.fileName1, Project10.options) 
        |> Async.RunSynchronously

    let querySymbol = backgroundTypedParse1.GetSymbolAtLocationAlternate(7,23,"",["query"]).Value
    querySymbol.ToString() |> shouldEqual "parameter query"

    let querySymbol2 = backgroundTypedParse1.GetSymbolAtLocationAlternate(7,22,"",["query"]).Value
    querySymbol2.ToString() |> shouldEqual "val query" // This is perhaps the wrong result, but not that the input location was wrong - was not the "column at end of names"

//-----------------------------------------------------------------------------------------
// see https://github.com/fsharp/FSharp.Compiler.Service/issues/92

module Project11 = 
    open System.IO

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1 = """
module NestedTypes

let enum = new System.Collections.Generic.Dictionary<int,int>.Enumerator()
let fff (x:System.Collections.Generic.Dictionary<int,int>.Enumerator) = ()

    """
    File.WriteAllText(fileName1, fileSource1)

    let cleanFileName a = if a = fileName1 then "file1" else "??"

    let fileNames = [fileName1]
    let args = mkProjectCommandLineArgs (dllName, fileNames)
    let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)


[<Test>]
let ``Test Project11 whole project errors`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project11.options) |> Async.RunSynchronously
    wholeProjectResults.Errors.Length |> shouldEqual 0


[<Test>]
let ``Test Project11 all symbols`` () =

    let wholeProjectResults = checker.ParseAndCheckProject(Project11.options) |> Async.RunSynchronously

    let allUsesOfAllSymbols = 
        wholeProjectResults.GetAllUsesOfAllSymbols()
        |> Array.map (fun su -> su.Symbol.ToString(), su.Symbol.DisplayName, Project11.cleanFileName su.FileName, tups su.RangeAlternate, attribsOfSymbolUse su)

    allUsesOfAllSymbols |> shouldEqual
          [|("Generic", "Generic", "file1", ((4, 34), (4, 41)), ["type"]);
            ("Collections", "Collections", "file1", ((4, 22), (4, 33)), ["type"]);
            ("System", "System", "file1", ((4, 15), (4, 21)), ["type"]);
            ("Dictionary`2", "Dictionary", "file1", ((4, 15), (4, 52)), ["type"]);
            ("int", "int", "file1", ((4, 53), (4, 56)), []);
            ("int", "int", "file1", ((4, 57), (4, 60)), []);
            ("Enumerator", "Enumerator", "file1", ((4, 62), (4, 72)), ["type"]);
            ("symbol Enumerator", "Enumerator", "file1", ((4, 15), (4, 72)), []);
            ("val enum", "enum", "file1", ((4, 4), (4, 8)), ["defn"]);
            ("Generic", "Generic", "file1", ((5, 30), (5, 37)), ["type"]);
            ("Collections", "Collections", "file1", ((5, 18), (5, 29)), ["type"]);
            ("System", "System", "file1", ((5, 11), (5, 17)), ["type"]);
            ("Dictionary`2", "Dictionary", "file1", ((5, 11), (5, 48)), ["type"]);
            ("int", "int", "file1", ((5, 49), (5, 52)), ["type"]);
            ("int", "int", "file1", ((5, 53), (5, 56)), ["type"]);
            ("Enumerator", "Enumerator", "file1", ((5, 58), (5, 68)), ["type"]);
            ("val x", "x", "file1", ((5, 9), (5, 10)), ["defn"]);
            ("val fff", "fff", "file1", ((5, 4), (5, 7)), ["defn"]);
            ("NestedTypes", "NestedTypes", "file1", ((2, 7), (2, 18)), ["defn"])|]

//-----------------------------------------------------------------------------------------
// see https://github.com/fsharp/FSharp.Compiler.Service/issues/92

module Project12 = 
    open System.IO

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1 = """
module ComputationExpressions

let x1 = seq { for i in 0 .. 100 -> i }
let x2 = query { for i in 0 .. 100 do
                 where (i = 0)
                 select (i,i) }

    """
    File.WriteAllText(fileName1, fileSource1)

    let cleanFileName a = if a = fileName1 then "file1" else "??"

    let fileNames = [fileName1]
    let args = mkProjectCommandLineArgs (dllName, fileNames)
    let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)


[<Test>]
let ``Test Project12 whole project errors`` () = 

    let wholeProjectResults = checker.ParseAndCheckProject(Project12.options) |> Async.RunSynchronously
    wholeProjectResults.Errors.Length |> shouldEqual 0


[<Test>]
let ``Test Project12 all symbols`` () =

    let wholeProjectResults = checker.ParseAndCheckProject(Project12.options) |> Async.RunSynchronously

    let allUsesOfAllSymbols = 
        wholeProjectResults.GetAllUsesOfAllSymbols()
        |> Array.map (fun su -> su.Symbol.ToString(), su.Symbol.DisplayName, Project12.cleanFileName su.FileName, tups su.RangeAlternate, attribsOfSymbolUse su)

    allUsesOfAllSymbols |> shouldEqual
          [|("val seq", "seq", "file1", ((4, 9), (4, 12)), ["compexpr"]);
            ("val ( .. )", "( .. )", "file1", ((4, 26), (4, 28)), []);
            ("val i", "i", "file1", ((4, 19), (4, 20)), ["defn"]);
            ("val i", "i", "file1", ((4, 36), (4, 37)), []);
            ("val x1", "x1", "file1", ((4, 4), (4, 6)), ["defn"]);
            ("val query", "query", "file1", ((5, 9), (5, 14)), []);   
            ("val query", "query", "file1", ((5, 9), (5, 14)), ["compexpr"]);
            ("member Where", "where", "file1", ((6, 17), (6, 22)), ["compexpr"]);
            ("member Select", "select", "file1", ((7, 17), (7, 23)), ["compexpr"]);
            ("val ( .. )", "( .. )", "file1", ((5, 28), (5, 30)), []);
            ("val i", "i", "file1", ((5, 21), (5, 22)), ["defn"]);
            ("val ( = )", "( = )", "file1", ((6, 26), (6, 27)), []);
            ("val i", "i", "file1", ((6, 24), (6, 25)), []);
            ("val i", "i", "file1", ((7, 25), (7, 26)), []);
            ("val i", "i", "file1", ((7, 27), (7, 28)), []);
            ("val x2", "x2", "file1", ((5, 4), (5, 6)), ["defn"]);
            ("ComputationExpressions", "ComputationExpressions", "file1",((2, 7), (2, 29)), ["defn"])|]
