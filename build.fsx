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
open Fake.UserInputHelper
open Fake.AssemblyInfoFile
open SourceLink

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let project = "FSharp.Compiler.Service"
let authors = ["Microsoft Corporation, Dave Thomas, Anh-Dung Phan, Tomas Petricek"]

let gitOwner = "fsharp"
let gitHome = "https://github.com/" + gitOwner

let gitName = "FSharp.Compiler.Service"
let gitRaw = environVarOrDefault "gitRaw" "https://raw.githubusercontent.com/fsharp"

let netFrameworks = [(* "v4.0"; *) "v4.5"]

// --------------------------------------------------------------------------------------
// The rest of the code is standard F# build script 
// --------------------------------------------------------------------------------------

let buildDir = "bin"

// Read release notes & version info from RELEASE_NOTES.md
let release = LoadReleaseNotes (__SOURCE_DIRECTORY__ + "/RELEASE_NOTES.md")
let isAppVeyorBuild = buildServer = BuildServer.AppVeyor
let isVersionTag tag = Version.TryParse tag |> fst
let hasRepoVersionTag = isAppVeyorBuild && AppVeyorEnvironment.RepoTag && isVersionTag AppVeyorEnvironment.RepoTagName
let assemblyVersion = if hasRepoVersionTag then AppVeyorEnvironment.RepoTagName else release.NugetVersion
let buildDate = DateTime.UtcNow
let buildVersion = 
    if hasRepoVersionTag then assemblyVersion
    else if isAppVeyorBuild then sprintf "%s-b%s" assemblyVersion AppVeyorEnvironment.BuildNumber
    else assemblyVersion

Target "BuildVersion" (fun _ ->
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" buildVersion) |> ignore
)

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
    let fileName = "src/assemblyinfo/assemblyinfo.shared.fs"
    CreateFSharpAssemblyInfo fileName
          [ Attribute.Version assemblyVersion
            Attribute.FileVersion assemblyVersion
            Attribute.InformationalVersion assemblyVersion ]
)

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "Clean" (fun _ ->
    CleanDirs [ buildDir ]
)

Target "CleanDocs" (fun _ ->
    CleanDirs ["docs/output"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target "GenerateFSIStrings" (fun _ -> 
    // Generate FSIStrings using the FSSrGen tool
    execProcess (fun p -> 
      let dir = __SOURCE_DIRECTORY__ </> "src/fsharp/fsi"
      p.Arguments <- "FSIstrings.txt FSIstrings.fs FSIstrings.resx"
      p.WorkingDirectory <- dir
      p.FileName <- !! "packages/FsSrGen/lib/net46/fssrgen.exe" |> Seq.head ) TimeSpan.MaxValue
    |> ignore
//    execProcess (fun p -> 
 //     p.Arguments <- "u+x packages/FsSrGen/lib/net46/fssrgen.exe"
 //     p.WorkingDirectory <- __SOURCE_DIRECTORY__ 
 //     p.FileName <- "chmod") TimeSpan.MaxValue
 //   |> ignore
)

Target "Build.NetFx" (fun _ ->
    netFrameworks
    |> List.iter (fun framework -> 
        !! (project + ".sln")
        |> MSBuild "" "Build" ["Configuration","Release"; "TargetFrameworkVersion", framework]
        |> Log (".NET " + framework + " Build-Output: "))
)

Target "SourceLink" (fun _ ->
    #if MONO
    ()
    #else
    netFrameworks
    |> List.iter (fun framework -> 
        let outputPath = __SOURCE_DIRECTORY__ </> buildDir </> framework
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

Target "RunTests.NetFx" (fun _ ->
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

Target "NuGet.NetFx" (fun _ ->
    Paket.Pack (fun p -> 
        { p with 
            TemplateFile = "nuget/FSharp.Compiler.Service.template"
            Version = release.NugetVersion
            OutputPath = buildDir
            ReleaseNotes = toLines release.Notes })
    Paket.Pack (fun p -> 
        { p with 
            TemplateFile = "nuget/projectcracker.template"
            Version = release.NugetVersion
            OutputPath = buildDir
            ReleaseNotes = toLines release.Notes })
)


Target "PublishNuGet" (fun _ ->
    Paket.Push (fun p -> 
        { p with
            WorkingDir = buildDir }) 
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

#load "paket-files/fsharp/FAKE/modules/Octokit/Octokit.fsx"
open Octokit

Target "GitHubRelease" (fun _ ->
    let user =
        match getBuildParam "github-user" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserInput "Username: "
    let pw =
        match getBuildParam "github-pw" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserPassword "Password: "
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.pushBranch "" remote (Information.getBranchName "")

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" remote release.NugetVersion
    
    // release on github
    createClient user pw
    |> createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes 
    |> uploadFile (buildDir</>("FSharp.Compiler.Service." + release.NugetVersion + ".nupkg"))
    |> releaseDraft    
    |> Async.RunSynchronously
)

// --------------------------------------------------------------------------------------
// .NET Core and .NET Core SDK

let isDotnetSDKInstalled =
    match Fake.EnvironmentHelper.environVarOrNone "FCS_DNC" with
    | Some flag -> 
        match bool.TryParse flag with
        | true, result -> result
        | _ -> false
    | None -> 
        try 
            Shell.Exec("dotnet", "--info") = 0 
        with 
        _ -> false

let assertExitCodeZero x = if x = 0 then () else failwithf "Command failed with exit code %i" x
let runCmdIn workDir exe = Printf.ksprintf (fun args -> Shell.Exec(exe, args, workDir) |> assertExitCodeZero)
let run exe = runCmdIn "." exe

Target "CodeGen.NetCore" (fun _ ->
    let lexArgs = "--lexlib Internal.Utilities.Text.Lexing"
    let yaccArgs = "--internal --parslib Internal.Utilities.Text.Parsing"
    let module1 = "--module Microsoft.FSharp.Compiler.AbstractIL.Internal.AsciiParser"
    let module2 = "--module Microsoft.FSharp.Compiler.Parser"
    let module3 = "--module Microsoft.FSharp.Compiler.PPParser"
    let open1 = "--open Microsoft.FSharp.Compiler.AbstractIL"
    let open2 = "--open Microsoft.FSharp.Compiler"
    let open3 = "--open Microsoft.FSharp.Compiler"

    // dotnet restore
    run "dotnet" "restore -v Information"

    // run tools
    let runInDir exe = runCmdIn "src/fsharp/FSharp.Compiler.Service/" exe
    let fsLex fslFilePath outFilePath = runInDir "lib/bootstrap/4.0/fslex.exe" @"%s --unicode %s -o %s" fslFilePath lexArgs outFilePath
    let fsYacc = runInDir "lib/bootstrap/4.0/fsyacc.exe"

    runInDir "dotnet" "fssrgen ../FSComp.txt ./FSComp.fs ./FSComp.resx"
    runInDir "dotnet" "fssrgen ../fsi/FSIstrings.txt ./FSIstrings.fs ./FSIstrings.resx"
    fsLex "../lex.fsl" "lex.fs"
    fsLex "../pplex.fsl" "pplex.fs"
    fsLex "../../absil/illex.fsl" "illex.fs"
    fsYacc "../../absil/ilpars.fsy %s %s %s %s -o ilpars.fs" lexArgs yaccArgs module1 open1
    fsYacc "../pars.fsy %s %s %s %s -o pars.fs" lexArgs yaccArgs module2 open2
    fsYacc "../pppars.fsy %s %s %s %s -o pppars.fs" lexArgs yaccArgs module3 open3
)

Target "Build.NetCore" (fun _ ->
    [ "src/fsharp/FSharp.Compiler.Service/";
      "src/fsharp/FSharp.Compiler.Service.ProjectCracker/";
      "src/fsharp/FSharp.Compiler.Service.ProjectCrackerTool/" ]
    |> List.iter (run "dotnet" "-v pack %s -c Release")
)

Target "RunTests.NetCore" (fun _ ->
    runCmdIn "tests/service/" "dotnet" "test -c Release --result:TestResults.NetCore.xml;format=nunit3"
)


//use dotnet-mergenupkg to merge the .netcore nuget package into the default one
Target "Nuget.AddNetCore" (fun _ ->
    do
        let nupkg = sprintf "../../../%s/FSharp.Compiler.Service.%s.nupkg" buildDir (release.AssemblyVersion)
        let netcoreNupkg = sprintf "bin/Release/FSharp.Compiler.Service.%s.nupkg" (release.AssemblyVersion)
        runCmdIn "src/fsharp/FSharp.Compiler.Service" "dotnet" "mergenupkg --source %s --other %s --framework netstandard1.6" nupkg netcoreNupkg
    
    do
        let nupkg = sprintf "../../../%s/FSharp.Compiler.Service.ProjectCracker.%s.nupkg" buildDir (release.AssemblyVersion)
        let netcoreNupkg = sprintf "bin/Release/FSharp.Compiler.Service.ProjectCracker.%s.nupkg" (release.AssemblyVersion)
        runCmdIn "src/fsharp/FSharp.Compiler.Service.ProjectCracker" "dotnet" "mergenupkg --source %s --other %s --framework netstandard1.6" nupkg netcoreNupkg
)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "Release" DoNothing
Target "CreatePackage" DoNothing
Target "NuGet" DoNothing
Target "All" DoNothing
Target "All.NetCore" DoNothing
Target "All.NetFx" DoNothing

"Clean"
  =?> ("BuildVersion", isAppVeyorBuild)
  ==> "AssemblyInfo"
  ==> "GenerateFSIStrings"
  ==> "CodeGen.NetCore"
  ==> "Build.NetCore"
  ==> "RunTests.NetCore"
  ==> "All.NetCore"

"Clean"
  =?> ("BuildVersion", isAppVeyorBuild)
  ==> "AssemblyInfo"
  ==> "GenerateFSIStrings"
  ==> "Build.NetFx"
  ==> "RunTests.NetFx"
  ==> "All.NetFx"

"All.NetFx"
  =?> ("All.NetCore", isDotnetSDKInstalled)
  ==> "All"

"NuGet.NetFx"
  =?> ("Nuget.AddNetCore", isDotnetSDKInstalled)
  ==> "NuGet"

"All"
  ==> "SourceLink"
  ==> "NuGet"
  ==> "CreatePackage"
  ==> "GitHubRelease"
  ==> "PublishNuGet"
  ==> "Release"

"CleanDocs"
  ==> "GenerateDocsJa"
  ==> "GenerateDocs"
  ==> "ReleaseDocs"
  ==> "Release"

RunTargetOrDefault "All"
