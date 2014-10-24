// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#I "packages/FAKE/tools"
#r "packages/FAKE/tools/FakeLib.dll"
#load "packages/SourceLink.Fake/tools/SourceLink.fsx"
open System
open Fake 
open Fake.Git
open Fake.ReleaseNotesHelper
open Fake.AssemblyInfoFile
open SourceLink

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let project = "FSharp.Compiler.Service"
let authors = ["Microsoft Corporation, Dave Thomas, Anh-Dung Phan, Tomas Petricek"]
let summary = "F# compiler services for creating IDE tools, language extensions and for F# embedding"
let description = """
  The F# compiler services package contains a custom build of the F# compiler that
  exposes additional functionality for implementing F# language bindings, additional
  tools based on the compiler or refactoring tools. The package also includes F# 
  interactive service that can be used for embedding F# scripting into your applications."""
let tags = "F# fsharp interactive compiler editor"

let gitHome = "https://github.com/fsharp"
let gitName = "FSharp.Compiler.Service"
let gitRaw = environVarOrDefault "gitRaw" "https://raw.githubusercontent.com/fsharp"

let netFrameworks = ["v4.0"; "v4.5"]

// --------------------------------------------------------------------------------------
// The rest of the code is standard F# build script 
// --------------------------------------------------------------------------------------

// Read release notes & version info from RELEASE_NOTES.md
let release = LoadReleaseNotes "RELEASE_NOTES.md"
let isAppVeyorBuild = environVar "APPVEYOR" <> null
let nugetVersion = 
    if isAppVeyorBuild then sprintf "%s-a%s" release.NugetVersion (DateTime.UtcNow.ToString "yyMMddHHmm")
    else release.NugetVersion

Target "BuildVersion" (fun _ ->
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" nugetVersion) |> ignore
)

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
  let fileName = "src/assemblyinfo/assemblyinfo.shared.fs"
  CreateFSharpAssemblyInfo fileName
      [ Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion] 
)

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "RestorePackages" RestorePackages

Target "Clean" (fun _ ->
    CleanDirs ["bin" ]
)

Target "CleanDocs" (fun _ ->
    CleanDirs ["docs/output"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target "GenerateFSIStrings" (fun _ -> 
    // Generate FSIStrings using the FSSrGen tool
    execProcess (fun p -> 
      let dir = __SOURCE_DIRECTORY__ @@ "src/fsharp/fsi"
      p.Arguments <- "FSIstrings.txt FSIstrings.fs FSIstrings.resx"
      p.WorkingDirectory <- dir
      p.FileName <- !! "lib/bootstrap/2.0/fssrgen.exe" |> Seq.head ) TimeSpan.MaxValue
    |> ignore
)

Target "Build" (fun _ ->
    netFrameworks
    |> List.iter (fun framework -> 
        let outputPath = "bin/" + framework
        !! (project + ".sln")
        |> MSBuild outputPath "Build" ["Configuration","Release"; "TargetFrameworkVersion", framework]
        |> Log (".NET " + framework + " Build-Output: "))
)

Target "SourceLink" (fun _ ->
    #if MONO
    ()
    #else
    netFrameworks
    |> List.iter (fun framework -> 
        let f = !! "src/fsharp/FSharp.Compiler.Service/FSharp.Compiler.Service.fsproj" |> Seq.head
        use repo = new GitRepo(__SOURCE_DIRECTORY__)
        let outputPath = __SOURCE_DIRECTORY__ @@ "bin/" + framework
        let proj = VsProj.Load f ["Configuration","Release"; "TargetFrameworkVersion",framework; "OutputPath",outputPath]
        logfn "indexing %s" proj.OutputFilePdb
        let compiles = proj.Compiles.SetBaseDirectory __SOURCE_DIRECTORY__ 
        let gitFiles =
            compiles
            -- "src/assemblyinfo/assemblyinfo*.fs" // not source indexed
            // generated and in fsproj as Compile, but in .gitignore, not source indexed
            -- "src/fsharp/FSharp.Compiler.Service/illex.fs" // <FsLex Include="..\..\absil\illex.fsl">
            -- "src/fsharp/FSharp.Compiler.Service/ilpars.fs"
            -- "src/fsharp/FSharp.Compiler.Service/lex.fs"
            -- "src/fsharp/FSharp.Compiler.Service/pars.fs"
        repo.VerifyChecksums gitFiles
        let pdbFiles =  
            compiles
            // generated, not in the fsproj as Compile, not source indexed
            ++ "src/absil/illex.fsl"
            ++ "src/absil/ilpars.fsy"
            ++ "src/fsharp/fsharp.compiler.service/obj/x86/release/fscomp.fs"
            ++ "src/fsharp/fsharp.compiler.service/obj/x86/release/fsistrings.fs"
            ++ "src/fsharp/lex.fsl"
            ++ "src/fsharp/pars.fsy"
        proj.VerifyPdbChecksums pdbFiles
        proj.CreateSrcSrv (sprintf "%s/%s/{0}/%%var2%%" gitRaw gitName) repo.Revision (repo.Paths gitFiles)
        Pdbstr.exec proj.OutputFilePdb proj.OutputFilePdbSrcSrv
    )
    #endif
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target "RunTests" (fun _ ->
    !! (if isAppVeyorBuild then "./bin/v4.5/FSharp.Compiler.Service.Tests.dll" 
        else "./bin/**/FSharp.Compiler.Service.Tests.dll")
    |> NUnit (fun p ->
        { p with
            Framework = "v4.0.30319"
            DisableShadowCopy = true
            TimeOut = TimeSpan.FromMinutes 20.
            OutputFile = "TestResults.xml" })
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet" (fun _ ->
    NuGet (fun p -> 
        { p with   
            Authors = authors
            Project = project
            Summary = summary
            Description = description
            Version = nugetVersion
            ReleaseNotes = release.Notes |> toLines
            Tags = tags
            OutputPath = "bin"
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = hasBuildParam "nugetkey" })
        ("nuget/" + project + ".nuspec")
)

// --------------------------------------------------------------------------------------
// Generate the documentation

Target "GenerateDocs" (fun _ ->
    executeFSIWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"] [] |> ignore
)

Target "GenerateDocsJa" (fun _ ->
    executeFSIWithArgs "docs/tools" "generate.ja.fsx" ["--define:RELEASE"] [] |> ignore
)

// --------------------------------------------------------------------------------------
// Release Scripts

Target "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    if not (System.IO.Directory.Exists tempDocsDir) then 
        Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir

    fullclean tempDocsDir
    CopyRecursive "docs/output" "temp/gh-pages" true |> printfn "%A"
    StageAll tempDocsDir
    Commit tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Branches.push "temp/gh-pages"
)

Target "Release" DoNothing

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "Prepare" DoNothing
Target "PrepareRelease" DoNothing
Target "All" DoNothing

"Clean"
  =?> ("BuildVersion", isAppVeyorBuild)
  ==> "RestorePackages"
  ==> "AssemblyInfo"
  ==> "GenerateFSIStrings"
  ==> "Prepare"
  ==> "Build"
  ==> "RunTests"
  ==> "All"

"All"
  ==> "PrepareRelease" 
  ==> "SourceLink"
  ==> "NuGet"
  ==> "Release"

"Release"
  ==> "CleanDocs"

"GenerateDocsJa"
  ==> "GenerateDocs"
  ==> "ReleaseDocs"

RunTargetOrDefault "All"
