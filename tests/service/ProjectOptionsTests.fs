#if INTERACTIVE
#r "../../bin/v4.5/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "FsUnit.fs"
#load "Common.fs"
#else
module FSharp.Compiler.Service.Tests.ProjectOptionsTests
#endif

let runningOnMono = try System.Type.GetType("Mono.Runtime") <> null with e ->  false

open System
open System.IO
open NUnit.Framework
open FsUnit
open Microsoft.FSharp.Compiler.SourceCodeServices

open FSharp.Compiler.Service.Tests.Common

#if FX_ATLEAST_45

let normalizePath s = (new Uri(s)).LocalPath

let checkOption (opts:string[]) s = 
    let found = "Found '"+s+"'"
    (if opts |> Array.exists (fun o -> o.EndsWith(s)) then found else "Failed to find '"+s+"'")
       |> shouldEqual found

let checkOptionNotPresent (opts:string[]) s = 
    let found = "Found '"+s+"'"
    let notFound = "Did not expect to find '"+s+"'"
    (if opts |> Array.exists (fun o -> o.EndsWith(s)) then found else notFound)
       |> shouldEqual notFound

[<Test>]
let ``Project file parsing example 1 Default Configuration`` () = 
    let projectFile = __SOURCE_DIRECTORY__ + @"/FSharp.Compiler.Service.Tests.fsproj"
    let options = checker.GetProjectOptionsFromProjectFile(projectFile)

    printfn "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv" 
    printfn "PROJ FILE %s" projectFile
    printfn "%A" options.OtherOptions
    printfn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^" 

    checkOption options.OtherOptions "FSharp.Compiler.Service.dll"
    checkOption options.OtherOptions "FileSystemTests.fs"
    checkOption options.OtherOptions "--define:TRACE"
    checkOption options.OtherOptions "--define:DEBUG"
    checkOption options.OtherOptions "--flaterrors"
    checkOption options.OtherOptions "--simpleresolution"
    checkOption options.OtherOptions "--noframework"

[<Test>]
let ``Project file parsing example 1 Release Configuration`` () = 
    let projectFile = __SOURCE_DIRECTORY__ + @"/FSharp.Compiler.Service.Tests.fsproj"
    // Check with Configuration = Release
    let options = checker.GetProjectOptionsFromProjectFile(projectFile, [("Configuration", "Release")])

    printfn "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv" 
    printfn "PROJ FILE %s" projectFile
    printfn "%A" options.OtherOptions
    printfn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^" 
    checkOption options.OtherOptions "FSharp.Compiler.Service.dll"
    checkOption options.OtherOptions "FileSystemTests.fs"
    checkOption options.OtherOptions "--define:TRACE"
    checkOptionNotPresent options.OtherOptions "--define:DEBUG"
    checkOption options.OtherOptions "--debug:pdbonly"

[<Test>]
let ``Project file parsing example 1 Default configuration relative path`` () = 
    let projectFile = "FSharp.Compiler.Service.Tests.fsproj"
    Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

    let options = checker.GetProjectOptionsFromProjectFile(projectFile)

    printfn "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv" 
    printfn "PROJ FILE %s" projectFile
    printfn "%A" options.OtherOptions
    printfn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^" 

    checkOption options.OtherOptions "FSharp.Compiler.Service.dll"
    checkOption options.OtherOptions "FileSystemTests.fs"
    checkOption options.OtherOptions "--define:TRACE"
    checkOption options.OtherOptions "--define:DEBUG"
    checkOption options.OtherOptions "--flaterrors"
    checkOption options.OtherOptions "--simpleresolution"
    checkOption options.OtherOptions "--noframework"

[<Test>]
let ``Project file parsing VS2013_FSharp_Portable_Library_net45``() = 
  // BUG - see https://github.com/fsharp/FSharp.Compiler.Service/issues/237
  if not runningOnMono then 
    let projectFile = __SOURCE_DIRECTORY__ + @"/../projects/Sample_VS2013_FSharp_Portable_Library_net45/Sample_VS2013_FSharp_Portable_Library_net45.fsproj"
    let options = checker.GetProjectOptionsFromProjectFile(projectFile, [])

    printfn "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv" 
    printfn "PROJ FILE %s" projectFile
    printfn "%A" options.OtherOptions
    printfn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^" 

    checkOption options.OtherOptions "--targetprofile:netcore"
    checkOption options.OtherOptions "--tailcalls-"

    checkOption options.OtherOptions "FSharp.Core.dll"
    checkOption options.OtherOptions "Microsoft.CSharp.dll"
    checkOption options.OtherOptions "System.Runtime.dll"
    checkOption options.OtherOptions "System.Net.Requests.dll"
    checkOption options.OtherOptions "System.Xml.XmlSerializer.dll"

[<Test>]
let ``Project file parsing Sample_VS2013_FSharp_Portable_Library_net451_adjusted_to_profile78``() = 
  // BUG - see https://github.com/fsharp/FSharp.Compiler.Service/issues/237
  if not runningOnMono then 
    let projectFile = __SOURCE_DIRECTORY__ + @"/../projects/Sample_VS2013_FSharp_Portable_Library_net451_adjusted_to_profile78/Sample_VS2013_FSharp_Portable_Library_net451.fsproj"
    let options = checker.GetProjectOptionsFromProjectFile(projectFile, [])

    printfn "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv" 
    printfn "PROJ FILE %s" projectFile
    printfn "%A" options.OtherOptions
    printfn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^" 
    checkOption options.OtherOptions "--targetprofile:netcore"
    checkOption options.OtherOptions "--tailcalls-"

    checkOption options.OtherOptions "FSharp.Core.dll"
    checkOption options.OtherOptions "Microsoft.CSharp.dll"
    checkOption options.OtherOptions "System.Runtime.dll"
    checkOption options.OtherOptions "System.Net.Requests.dll"
    checkOption options.OtherOptions "System.Xml.XmlSerializer.dll"

[<Test>]
let ``Project file parsing -- compile files 1``() =
  let p = FSharpProjectFileInfo.Parse(__SOURCE_DIRECTORY__ + @"/data/Test1.fsproj")

  p.CompileFiles
  |> List.map Path.GetFileName
  |> set
  |> should equal (set [ "Test1File1.fs"; "Test1File2.fs" ])

[<Test>]
let ``Project file parsing -- compile files 2``() =
  let p = FSharpProjectFileInfo.Parse(__SOURCE_DIRECTORY__ + @"/data/Test2.fsproj")

  p.CompileFiles
  |> List.map Path.GetFileName
  |> set
  |> should equal (set [ "Test2File1.fs"; "Test2File2.fs" ])

[<Test>]
let ``Project file parsing -- bad project file``() =
  (fun () -> FSharpProjectFileInfo.Parse(__SOURCE_DIRECTORY__ + @"/data/Malformed.fsproj") |> ignore)
  |> should throw typeof<Microsoft.Build.Exceptions.InvalidProjectFileException>

[<Test>]
let ``Project file parsing -- non-existent project file``() =
  (fun () -> FSharpProjectFileInfo.Parse(__SOURCE_DIRECTORY__ + @"/data/DoesNotExist.fsproj") |> ignore)
  |> should throw typeof<System.IO.FileNotFoundException>

[<Test>]
let ``Project file parsing -- output file``() =
  let p = FSharpProjectFileInfo.Parse(__SOURCE_DIRECTORY__ + @"/data/Test1.fsproj")

  let expectedOutputPath =
    normalizePath (__SOURCE_DIRECTORY__ + "/data/Test1/bin/Debug/Test1.dll")

  p.OutputFile
  |> should equal (Some expectedOutputPath)

[<Test>]
let ``Project file parsing -- references``() =
  let p = FSharpProjectFileInfo.Parse(__SOURCE_DIRECTORY__ + @"/data/Test1.fsproj")

  checkOption (Array.ofList p.References) "FSharp.Core.dll"
  checkOption (Array.ofList p.References) "mscorlib.dll"
  checkOption (Array.ofList p.References) "System.Core.dll"
  checkOption (Array.ofList p.References) "System.dll"
  p.References |> should haveLength 4
  p.ProjectReferences |> should be Empty

[<Test>]
let ``Project file parsing -- 2nd level references``() =
  let p = FSharpProjectFileInfo.Parse(__SOURCE_DIRECTORY__ + @"/data/Test2.fsproj")

  checkOption (Array.ofList p.References) "FSharp.Core.dll"
  checkOption (Array.ofList p.References) "mscorlib.dll"
  checkOption (Array.ofList p.References) "System.Core.dll"
  checkOption (Array.ofList p.References) "System.dll"
  checkOption (Array.ofList p.References) "Test1.dll"
  p.References |> should haveLength 5
  p.ProjectReferences |> should haveLength 1
  p.ProjectReferences |> should contain (normalizePath (__SOURCE_DIRECTORY__ + @"/data/Test1.fsproj"))

[<Test>]
let ``Project file parsing -- Tools Version 12``() =
  let p = FSharpProjectFileInfo.Parse(__SOURCE_DIRECTORY__ + @"/data/ToolsVersion12.fsproj")

  checkOption (Array.ofList p.References) "System.Core.dll"

[<Test>]
let ``Project file parsing -- Logging``() =
  let p = FSharpProjectFileInfo.Parse(__SOURCE_DIRECTORY__ + @"/data/ToolsVersion12.fsproj", enableLogging=true)

  if runningOnMono then
    Assert.That(p.LogOutput, Is.StringContaining("Reference System.Core resolved"))
    Assert.That(p.LogOutput, Is.StringContaining("Target named 'ImplicitlyExpandTargetFramework'"))
    Assert.That(p.LogOutput, Is.StringContaining("Using task ResolveAssemblyReference from Microsoft.Build.Tasks.ResolveAssemblyReference, Microsoft.Build.Tasks.v12.0, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"))
  else
    Assert.That(p.LogOutput, Is.StringContaining("""Using "ResolveAssemblyReference" task from assembly "Microsoft.Build.Tasks.v12.0, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"."""))
    Assert.That(p.LogOutput, Is.StringContaining("""target "ImplicitlyExpandTargetFramework"""))

[<Test>]
let ``Project file parsing -- Full path``() =
  let f = normalizePath (__SOURCE_DIRECTORY__ + @"/data/ToolsVersion12.fsproj")
  let p = FSharpProjectFileInfo.Parse(f)

  p.FullPath |> should equal f

[<Test>]
let ``Project file parsing -- project references``() =
  let f1 = normalizePath (__SOURCE_DIRECTORY__ + @"/data/Test1.fsproj")
  let f2 = normalizePath (__SOURCE_DIRECTORY__ + @"/data/Test2.fsproj")
  let options = checker.GetProjectOptionsFromProjectFile(f2)

  options.ReferencedProjects |> should haveLength 1
  fst options.ReferencedProjects.[0] |> should endWith "Test1.fsproj"
  snd options.ReferencedProjects.[0] |> should equal (checker.GetProjectOptionsFromProjectFile(f1))

#endif

