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

#if MONO
// prevent incorrect output encoding (e.g. https://github.com/fsharp/FAKE/issues/1196)
System.Console.OutputEncoding <- System.Text.Encoding.UTF8
#endif

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
let releaseDir = "artefacts/release"
let debugDir = "artefacts/debug"


let publishDebugPackage = environVar "DEBUG_PACKAGE_PUBLISH" = "true"
let buildDebugPackage = publishDebugPackage || environVar "DEBUG_PACKAGE" <> "false"

// Read release notes & version info from RELEASE_NOTES.md
let release = LoadReleaseNotes (__SOURCE_DIRECTORY__ + "/RELEASE_NOTES.md")
let isAppVeyorBuild = buildServer = BuildServer.AppVeyor
let isVersionTag tag = Version.TryParse tag |> fst
let hasRepoVersionTag = isAppVeyorBuild && AppVeyorEnvironment.RepoTag && isVersionTag AppVeyorEnvironment.RepoTagName
let assemblyVersion = if hasRepoVersionTag then AppVeyorEnvironment.RepoTagName else release.NugetVersion
let nugetVersion = release.NugetVersion
open SemVerHelper
let nugetDebugVersion =
    let semVer = SemVerHelper.parse nugetVersion
    let debugPatch, debugPreRelease =
        match semVer.PreRelease with
        | None -> semVer.Patch + 1, { Origin = "alpha001"; Name = "alpha"; Number = Some 1; Parts = [AlphaNumeric "alpha001"] }
        | Some pre ->
            let num = match pre.Number with Some i -> i + 1 | None -> 1
            let name = pre.Name
            let newOrigin = sprintf "%s%03d" name num
            semVer.Patch, { Origin = newOrigin; Name = name; Number = Some num; Parts = [AlphaNumeric newOrigin] }
    let debugVer =
        { semVer with
            Patch = debugPatch
            PreRelease = Some debugPreRelease }
    debugVer.ToString()
let buildDate = DateTime.UtcNow
let buildVersion =
    if hasRepoVersionTag then assemblyVersion
    else if isAppVeyorBuild then sprintf "%s-b%s" assemblyVersion AppVeyorEnvironment.BuildNumber
    else assemblyVersion

let netcoresln = gitName + ".netcore.sln";

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
    CleanDirs [ buildDir; releaseDir; debugDir ]
)

Target "CleanDocs" (fun _ ->
    CleanDirs ["docs/output"]
)

Target "Build.NetFx" (fun _ ->
    netFrameworks
    |> List.iter (fun framework ->
        !! (project + ".sln")
        |> MSBuild "" "Build" ["Configuration","Release"; "TargetFrameworkVersion", framework]
        |> Log (".NET " + framework + " Build-Output: "))
    CopyDir releaseDir buildDir allFiles
    CleanDir buildDir
)

Target "Build.NetFx.Debug" (fun _ ->
    netFrameworks
    |> List.iter (fun framework ->
        !! (project + ".sln")
        |> MSBuild "" "Build" ["Configuration","Debug"; "TargetFrameworkVersion", framework]
        |> Log (".NET " + framework + " Build-Output: "))
    CopyDir debugDir buildDir allFiles
    CleanDir buildDir
)

Target "SourceLink" (fun _ ->
    #if MONO
    ()
    #else
    netFrameworks
    |> List.iter (fun framework ->
        let outputPath = __SOURCE_DIRECTORY__ </> releaseDir </> framework
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
    !! (if isAppVeyorBuild then sprintf "./%s/v4.5/FSharp.Compiler.Service.Tests.dll" releaseDir
        else sprintf "./%s/**/FSharp.Compiler.Service.Tests.dll" releaseDir)
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
            Version = nugetVersion
            OutputPath = releaseDir
            ReleaseNotes = toLines release.Notes })
    Paket.Pack (fun p ->
        { p with
            TemplateFile = "nuget/projectcracker.template"
            Version = nugetVersion
            OutputPath = releaseDir
            ReleaseNotes = toLines release.Notes })
)
Target "NuGet.NetFx.Debug" (fun _ ->
    Paket.Pack (fun p ->
        { p with
            TemplateFile = "nuget/FSharp.Compiler.Service.template"
            Version = nugetDebugVersion
            OutputPath = debugDir
            ReleaseNotes = toLines release.Notes })
    Paket.Pack (fun p ->
        { p with
            TemplateFile = "nuget/projectcracker.template"
            Version = nugetDebugVersion
            OutputPath = debugDir
            ReleaseNotes = toLines release.Notes })
)


Target "PublishNuGet" (fun _ ->
    Paket.Push (fun p ->
        let apikey =
            match getBuildParam "nuget-apikey" with
            | s when not (String.IsNullOrWhiteSpace s) -> s
            | _ -> getUserInput "Nuget API Key: "
        { p with
            ApiKey = apikey
            WorkingDir = releaseDir })
    if publishDebugPackage then
        Paket.Push (fun p ->
            let apikey =
                match getBuildParam "nuget-apikey" with
                | s when not (String.IsNullOrWhiteSpace s) -> s
                | _ -> getUserInput "Nuget API Key: "
            { p with
                ApiKey = apikey
                WorkingDir = debugDir })
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
        | _ -> getUserInput "GitHub Username: "
    let pw =
        match getBuildParam "github-pw" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserPassword "GitHub Password: "
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s (%s)" nugetVersion nugetDebugVersion)
    Branches.pushBranch "" remote (Information.getBranchName "")

    Branches.tag "" nugetVersion
    Branches.pushTag "" remote nugetVersion
    if publishDebugPackage then
        Branches.tag "" nugetDebugVersion
        Branches.pushTag "" remote nugetDebugVersion

    // release on github
    createClient user pw
    |> createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    |> uploadFile (releaseDir</>("FSharp.Compiler.Service." + nugetVersion + ".nupkg"))
    |> releaseDraft
    |> Async.RunSynchronously

    if publishDebugPackage then
        createClient user pw
        |> createDraft gitOwner gitName nugetDebugVersion true release.Notes
        |> uploadFile (debugDir</>("FSharp.Compiler.Service." + nugetDebugVersion + ".nupkg"))
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
let runCmdIn mono workDir exe = Printf.ksprintf (fun args ->
    if mono
    then
        printfn "mono %s/%s %s" workDir exe args
        Shell.Exec("mono", sprintf "%s %s" exe args, workDir)
        |> assertExitCodeZero
    else
        printfn "%s/%s %s" workDir exe args
        Shell.Exec(exe, args, workDir)
        |> assertExitCodeZero
)
let run mono exe = runCmdIn mono "." exe

Target "CodeGen.NetCore" (fun _ ->
    let lexArgs = "--lexlib Internal.Utilities.Text.Lexing"
    let yaccArgs = "--internal --parslib Internal.Utilities.Text.Parsing"
    let module1 = "--module Microsoft.FSharp.Compiler.AbstractIL.Internal.AsciiParser"
    let module2 = "--module Microsoft.FSharp.Compiler.Parser"
    let module3 = "--module Microsoft.FSharp.Compiler.PPParser"
    let open1 = "--open Microsoft.FSharp.Compiler.AbstractIL"
    let open2 = "--open Microsoft.FSharp.Compiler"
    let open3 = "--open Microsoft.FSharp.Compiler"

    // restore all the required tools, declared in each fsproj
    run false "dotnet" "restore %s" netcoresln
    run false "dotnet" "restore %s" "tools.fsproj"

    // run tools
    let toolDir = "packages/FsLexYacc/build"
    let fcsNetcore = "src/fsharp/FSharp.Compiler.Service/FSharp.Compiler.Service.netcore.fsproj"
    let fsLex fsl out = runCmdIn isMono "." (sprintf "%s/fslex.exe" toolDir) "%s --unicode %s -o %s" fsl lexArgs out
    let fsYacc fsy out m o = runCmdIn isMono "." (sprintf "%s/fsyacc.exe" toolDir) "%s %s %s %s %s -o %s" fsy lexArgs yaccArgs m o out

    run false "dotnet" "fssrgen src/fsharp/FSComp.txt src/fsharp/FSharp.Compiler.Service/FSComp.fs src/fsharp/FSharp.Compiler.Service/FSComp.resx"
    run false "dotnet" "fssrgen src/fsharp/fsi/FSIstrings.txt src/fsharp/FSharp.Compiler.Service/FSIstrings.fs src/fsharp/FSharp.Compiler.Service/FSIstrings.resx"
    fsLex "src/fsharp/lex.fsl" "src/fsharp/FSharp.Compiler.Service/lex.fs"
    fsLex "src/fsharp/pplex.fsl" "src/fsharp/FSharp.Compiler.Service/pplex.fs"
    fsLex "src/absil/illex.fsl" "src/fsharp/FSharp.Compiler.Service/illex.fs"
    fsYacc "src/absil/ilpars.fsy" "src/fsharp/FSharp.Compiler.Service/ilpars.fs" module1 open1
    fsYacc "src/fsharp/pars.fsy" "src/fsharp/FSharp.Compiler.Service/pars.fs" module2 open2
    fsYacc "src/fsharp/pppars.fsy" "src/fsharp/FSharp.Compiler.Service/pppars.fs" module3 open3
)

Target "Build.NetCore" (fun _ ->
    run false "dotnet" "pack %s -v n -c Release" netcoresln
)

Target "Build.NetCore.Debug" (fun _ ->
    run false "dotnet" "pack %s -v n -c Debug" netcoresln
)

Target "RunTests.NetCore" (fun _ ->
    run false "dotnet" "run -p tests/service/FSharp.Compiler.Service.Tests.netcore.fsproj -c Release -- --result:TestResults.NetCore.xml;format=nunit3"
)


//use dotnet-mergenupkg to merge the .netcore nuget package into the default one
Target "Nuget.AddNetCore" (fun _ ->
    do
        let nupkg = sprintf "%s/FSharp.Compiler.Service.%s.nupkg" releaseDir release.AssemblyVersion
        let netcoreNupkg = sprintf "src/fsharp/FSharp.Compiler.Service/bin/Release/FSharp.Compiler.Service.%s.nupkg" release.AssemblyVersion
        runCmdIn false "." "dotnet" "mergenupkg --source %s --other %s --framework netstandard1.6" nupkg netcoreNupkg

    do
        let nupkg = sprintf "%s/FSharp.Compiler.Service.ProjectCracker.%s.nupkg" releaseDir release.AssemblyVersion
        let netcoreNupkg = sprintf "src/fsharp/FSharp.Compiler.Service.ProjectCracker/bin/Release/FSharp.Compiler.Service.ProjectCracker.%s.nupkg" release.AssemblyVersion
        runCmdIn false "." "dotnet" "mergenupkg --source %s --other %s --framework netstandard1.6" nupkg netcoreNupkg
)

Target "Nuget.AddNetCore.Debug" (fun _ ->
    do
        let nupkg = sprintf "%s/FSharp.Compiler.Service.%s.nupkg" debugDir nugetDebugVersion
        let netcoreNupkg = sprintf "src/fsharp/FSharp.Compiler.Service/bin/Debug/FSharp.Compiler.Service.%s.nupkg" release.AssemblyVersion
        runCmdIn false "." "dotnet" "mergenupkg --source %s --other %s --framework netstandard1.6" nupkg netcoreNupkg

    do
        let nupkg = sprintf "%s/FSharp.Compiler.Service.ProjectCracker.%s.nupkg" debugDir nugetDebugVersion
        let netcoreNupkg = sprintf "src/fsharp/FSharp.Compiler.Service.ProjectCracker/bin/Debug/FSharp.Compiler.Service.ProjectCracker.%s.nupkg" release.AssemblyVersion
        runCmdIn false "." "dotnet" "mergenupkg --source %s --other %s --framework netstandard1.6" nupkg netcoreNupkg
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
  ==> "CodeGen.NetCore"
  ==> "Build.NetCore"
  =?> ("Build.NetCore.Debug", buildDebugPackage)
  ==> "RunTests.NetCore"
  ==> "All.NetCore"

"Clean"
  =?> ("BuildVersion", isAppVeyorBuild)
  ==> "AssemblyInfo"
  ==> "Build.NetFx"
  =?> ("Build.NetFx.Debug", buildDebugPackage)
  ==> "RunTests.NetFx"
  ==> "All.NetFx"

"All.NetFx"
  =?> ("All.NetCore", isDotnetSDKInstalled)
  ==> "All"

"All.NetCore"
  ==> "Nuget.AddNetCore"

"All.NetCore"
  =?> ("Nuget.AddNetCore.Debug", buildDebugPackage)

"All.NetFx"
  ==> "NuGet.NetFx"
  =?> ("Nuget.AddNetCore", isDotnetSDKInstalled)
  =?> ("Nuget.AddNetCore.Debug", buildDebugPackage && isDotnetSDKInstalled)
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
