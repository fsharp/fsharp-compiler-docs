
#if INTERACTIVE
#r "../../bin/v4.5/FSharp.Compiler.Service.dll"
#r "../../bin/v4.5/CSharp_Analysis.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "FsUnit.fs"
#load "Common.fs"
#else
module FSharp.Compiler.Service.Tests.CSharpProjectAnalysis
#endif


open NUnit.Framework
open FsUnit
open System
open System.IO
open System.Collections.Generic

open Microsoft.FSharp.Compiler
open FSharp.Compiler.Service.Tests
open Microsoft.FSharp.Compiler.SourceCodeServices

open FSharp.Compiler.Service.Tests.Common

let getProjectReferences (content, dllFiles, libDirs, otherFlags) = 
    let otherFlags = defaultArg otherFlags []
    let libDirs = defaultArg libDirs []
    let base1 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base1, ".dll")
    let fileName1 = Path.ChangeExtension(base1, ".fs")
    let projFileName = Path.ChangeExtension(base1, ".fsproj")
    File.WriteAllText(fileName1, content)
    let options =
        checker.GetProjectOptionsFromCommandLineArgs(projFileName,
            [| yield "--debug:full" 
               yield "--define:DEBUG" 
               yield "--optimize-" 
               yield "--out:" + dllName
               yield "--doc:test.xml" 
               yield "--warn:3" 
               yield "--fullpaths" 
               yield "--flaterrors" 
               yield "--target:library" 
               for dllFile in dllFiles do
                 yield "-r:"+dllFile
               for libDir in libDirs do
                 yield "-I:"+libDir
               yield! otherFlags
               yield fileName1 |])
    let results = checker.ParseAndCheckProject(options) |> Async.RunSynchronously
    if results.HasCriticalErrors then
        let builder = new System.Text.StringBuilder()
        for err in results.Errors do
            builder.AppendLine(sprintf "**** %s: %s" (if err.Severity = FSharpErrorSeverity.Error then "error" else "warning") err.Message)
            |> ignore
        failwith (builder.ToString())
    let assemblies =
        results.ProjectContext.GetReferencedAssemblies()
        |> List.map(fun x -> x.SimpleName, x)
        |> dict
    results, assemblies

[<Test>]
let ``Test that csharp references are recognized as such`` () = 
    let csharpAssembly = typeof<CSharpClass>.Assembly.Location
    let _, table = getProjectReferences("""module M""", [csharpAssembly], None, None)
    let ass = table.["CSharp_Analysis"]
    match ass.Contents.Entities |> Seq.tryFind (fun e -> e.DisplayName = "CSharpClass") with
    | Some found ->
        // this is no F# thing
        found.IsFSharp |> shouldEqual false
        
        // Check that we have members
        let members = found.MembersFunctionsAndValues |> Seq.map (fun e -> e.CompiledName, e) |> dict
        members.ContainsKey ".ctor" |> shouldEqual true
        members.ContainsKey "Method" |> shouldEqual true
        members.ContainsKey "Property" |> shouldEqual true
        members.ContainsKey "Event" |> shouldEqual true
        members.ContainsKey "InterfaceMethod" |> shouldEqual true
        members.ContainsKey "InterfaceProperty" |> shouldEqual true
        members.ContainsKey "InterfaceEvent" |> shouldEqual true

        //// Check that we get xml docs
        //String.IsNullOrWhiteSpace(members.[".ctor"].XmlDocSig) |> shouldEqual false
        //String.IsNullOrWhiteSpace(members.["Method"].XmlDocSig) |> shouldEqual false
        //String.IsNullOrWhiteSpace(members.["Property"].XmlDocSig) |> shouldEqual false
        //String.IsNullOrWhiteSpace(members.["Event"].XmlDocSig) |> shouldEqual false
        //String.IsNullOrWhiteSpace(members.["InterfaceMethod"].XmlDocSig) |> shouldEqual false
        //String.IsNullOrWhiteSpace(members.["InterfaceProperty"].XmlDocSig) |> shouldEqual false
        //String.IsNullOrWhiteSpace(members.["InterfaceEvent"].XmlDocSig) |> shouldEqual false

        ()
    | None -> 
        Assert.Fail ("CSharpClass was not found in CSharp_Analysis assembly!")

[<Test; Ignore("Failing test for https://github.com/fsharp/FSharp.Compiler.Service/issues/177")>]
let ``Test that symbols of csharp inner classes/enums are reported`` () = 
    let csharpAssembly = typeof<CSharpClass>.Assembly.Location
    let content = """
module NestedEnumClass
open FSharp.Compiler.Service.Tests

let _ = CSharpOuterClass.InnerEnum.Case1
let _ = CSharpOuterClass.InnerClass.StaticMember()
"""

    let results, _ = getProjectReferences(content, [csharpAssembly], None, None)
    results.GetAllUsesOfAllSymbols()
    |> Async.RunSynchronously
    |> Array.map (fun su -> su.Symbol.ToString())
    |> shouldEqual 
        [|"CSharpOuterClass"; "InnerEnum"; "symbol Case1"; 
          "CSharpOuterClass"; "InnerClass"; "val StaticMember"; 
          "NestedEnumClass"|]
