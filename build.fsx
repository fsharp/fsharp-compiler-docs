// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#I "packages/FAKE/tools"
#r "packages/FAKE/tools/FakeLib.dll"
#load "packages/SourceLink.Fake/tools/SourceLink.fsx"
open System
open Fake.AppVeyor
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
let isAppVeyorBuild = buildServer = BuildServer.AppVeyor
let isVersionTag tag = Version.TryParse tag |> fst
let hasRepoVersionTag = isAppVeyorBuild && AppVeyorEnvironment.RepoTag && isVersionTag AppVeyorEnvironment.RepoTagName
let assemblyVersion = if hasRepoVersionTag then AppVeyorEnvironment.RepoTagName else release.NugetVersion
let buildDate = DateTime.UtcNow
let buildVersion = 
    if hasRepoVersionTag then assemblyVersion
    else if isAppVeyorBuild then sprintf "%s-b%s" assemblyVersion AppVeyorEnvironment.BuildNumber
    else sprintf "%s-a%s" assemblyVersion (buildDate.ToString "yyMMddHHmm")

Target "BuildVersion" (fun _ ->
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" buildVersion) |> ignore
)

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
    let fileName = "src/assemblyinfo/assemblyinfo.shared.fs"
    // add json info to the informational version
    let iv = Text.StringBuilder() // json
    iv.Appendf "{\\\"buildVersion\\\":\\\"%s\\\"" buildVersion
    iv.Appendf ",\\\"buildDate\\\":\\\"%s\\\"" (buildDate.ToString "yyyy'-'MM'-'dd'T'HH':'mm':'sszzz")
    if isAppVeyorBuild then
        iv.Appendf ",\\\"gitCommit\\\":\\\"%s\\\"" AppVeyor.AppVeyorEnvironment.RepoCommit
        iv.Appendf ",\\\"gitBranch\\\":\\\"%s\\\"" AppVeyor.AppVeyorEnvironment.RepoBranch
    iv.Appendf "}"
    CreateFSharpAssemblyInfo fileName
          [ Attribute.Version assemblyVersion
            Attribute.FileVersion assemblyVersion
            Attribute.InformationalVersion iv.String ]
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
      p.FileName <- !! "lib/bootstrap/4.0/fssrgen.exe" |> Seq.head ) TimeSpan.MaxValue
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
        let outputPath = __SOURCE_DIRECTORY__ @@ "bin/" + framework
        let proj = VsProj.Load "src/fsharp/FSharp.Compiler.Service/FSharp.Compiler.Service.fsproj"
                        ["Configuration","Release"; "TargetFrameworkVersion",framework; "OutputPath",outputPath]
        let sourceFiles = 
            SetBaseDir __SOURCE_DIRECTORY__ proj.Compiles
            // generated and in fsproj as Compile, but in .gitignore, not source indexed
            -- "src/fsharp/FSharp.Compiler.Service/illex.fs" // <FsLex Include="..\..\absil\illex.fsl">
            -- "src/fsharp/FSharp.Compiler.Service/ilpars.fs"
            -- "src/fsharp/FSharp.Compiler.Service/pplex.fs" // <FsLex Include="..\..\absil\illex.fsl">
            -- "src/fsharp/FSharp.Compiler.Service/pppars.fs"
            -- "src/fsharp/FSharp.Compiler.Service/lex.fs"
            -- "src/fsharp/FSharp.Compiler.Service/pars.fs"
        let url = sprintf "%s/%s/{0}/%%var2%%" gitRaw gitName
        SourceLink.Index sourceFiles proj.OutputFilePdb __SOURCE_DIRECTORY__ url
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
            Version = buildVersion
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
    Commit tempDocsDir (sprintf "Update generated documentation for version %s" buildVersion)
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

"CleanDocs"
  ==> "GenerateDocsJa"
  ==> "GenerateDocs"
  ==> "ReleaseDocs"

RunTargetOrDefault "All"
