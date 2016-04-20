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
// bootstrap helpers
// --------------------------------------------------------------------------------------

let run exe path args =
    let lkgPath = "lib/bootstrap/4.0/"
    traceImportant <| sprintf "Running '%s' with args '%s'" exe args
    execProcess (fun x ->
        x.WorkingDirectory <- __SOURCE_DIRECTORY__ </> path
        x.FileName <- __SOURCE_DIRECTORY__ </> lkgPath </> exe
        x.Arguments <- args) TimeSpan.MaxValue
    |> ignore
let fsLex  = run "fslex.exe"
let fsYacc = run "fsyacc.exe"
let fsSrGen = run "fssrgen.exe"

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
      p.FileName <- !! "lib/bootstrap/4.0/fssrgen.exe" |> Seq.head ) TimeSpan.MaxValue
    |> ignore
)

Target "Build" (fun _ ->
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
    Paket.Pack (fun p -> 
        { p with 
            TemplateFile = "nuget/paket.template"
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
    |> releaseDraft
    |> Async.RunSynchronously
)

// --------------------------------------------------------------------------------------
// .NET CLI and .NET Core

let isDotnetCLIInstalled = Shell.Exec("dotnet", "--version") = 0
let assertExitCodeZero x = if x = 0 then () else failwithf "Command failed with exit code %i" x
let netcoreFW = "netstandard1.5"
let options = sprintf "--verbose build --framework %s --configuration Release" netcoreFW

Target "DotnetCliBuild" (fun _ ->
    Shell.Exec("dotnet", "restore") |> ignore //assertExitCodeZero
    Shell.Exec("dotnet", options, "src/fsharp/FSharp.Compiler.Service.netcore/") |> assertExitCodeZero
)

//TODO: use xplat tools to generate (dotnet-fssrgen, dotnet-fslex, etc.)
Target "DotnetCliParse" (fun _ ->
    let path = @"src/fsharp/FSharp.Compiler.Service.netcore/"
    let lexArgs = @" --lexlib Internal.Utilities.Text.Lexing"
    let yaccArgs = @" --internal --parslib Internal.Utilities.Text.Parsing"
    let module1 = @" --module Microsoft.FSharp.Compiler.AbstractIL.Internal.AsciiParser"
    let module2 = @" --module Microsoft.FSharp.Compiler.Parser"
    let module3 = @" --module Microsoft.FSharp.Compiler.PPParser"
    let open1 = @" --open Microsoft.FSharp.Compiler.AbstractIL"
    let open2 = @" --open Microsoft.FSharp.Compiler"
    let open3 = @" --open Microsoft.FSharp.Compiler"
    fsSrGen path @"../FSComp.txt FSComp.fs FSComp.resx"
    fsSrGen path @"../fsi/FSIstrings.txt FSIstrings.fs FSIstrings.resx"
    fsLex path (@"../lex.fsl --unicode" + lexArgs + " -o lex.fs")
    fsLex path (@"../pplex.fsl --unicode" + lexArgs + " -o pplex.fs")
    fsLex path (@"../../absil/illex.fsl --unicode" + lexArgs + " -o illex.fs")
    fsYacc path (@"../../absil/ilpars.fsy" + lexArgs + yaccArgs + module1 + open1 + " -o ilpars.fs")
    fsYacc path (@"../pars.fsy" + lexArgs + yaccArgs + module2 + open2 + " -o pars.fs")
    fsYacc path (@"../pppars.fsy" + lexArgs + yaccArgs + module3 + open3 + " -o pppars.fs")
)

//TODO: proper nuget package
// Target "AddNetcoreToNupkg" (fun _ ->
//     let nupkg = sprintf "../../temp/FSharp.Compiler.Service.%s.nupkg" (release.AssemblyVersion)
//     Shell.Exec("dotnet", "--verbose pack --configuration Release", "src/fsharp/FSharp.Compiler.Service") |> assertExitCodeZero
//     let netcoreNupkg = sprintf "bin/Release/FSharp.Compiler.Service.%s.nupkg" (release.AssemblyVersion)
//     Shell.Exec("dotnet", sprintf """mergenupkg --source "%s" --other "%s" --framework "%s" """ nupkg netcoreNupkg netcoreFW, "src/fsharp/FSharp.Compiler.Service/") |> assertExitCodeZero
// )

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "Prepare" DoNothing
Target "PrepareRelease" DoNothing
Target "All" DoNothing
Target "Release" DoNothing

"Clean"
  =?> ("BuildVersion", isAppVeyorBuild)
  ==> "AssemblyInfo"
  ==> "GenerateFSIStrings"
  ==> "Prepare"
  =?> ("DotnetCliBuild", isDotnetCLIInstalled)      
  ==> "Build"
  ==> "RunTests"
  ==> "All"

"All"
  ==> "PrepareRelease" 
  ==> "SourceLink"
  ==> "NuGet"
  ==> "GitHubRelease"
  ==> "PublishNuGet"
  ==> "Release"

"CleanDocs"
  ==> "GenerateDocsJa"
  ==> "GenerateDocs"
  ==> "ReleaseDocs"
  ==> "Release"

RunTargetOrDefault "All"
