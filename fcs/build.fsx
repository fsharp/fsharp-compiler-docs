// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "paket: groupref Main //"
#load "./.fake/build.fsx/intellisense.fsx"

open System
open System.IO
open Paket
open Fake.BuildServer
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.Api
open Fake.Tools

BuildServer.install [ AppVeyor.Installer ]
// --------------------------------------------------------------------------------------
// Utilities
// --------------------------------------------------------------------------------------

let withDotnetExe =
    // Build.cmd normally downloads a dotnet cli to: <repo-root>\artifacts\toolset\dotnet
    // check if there is one there to avoid downloading an additional one here
    let pathToCli = Path.Combine(__SOURCE_DIRECTORY__, @"..\artifacts\toolset\dotnet\dotnet.exe")
    if File.Exists(pathToCli) then
        (fun opts -> { opts with DotNet.Options.DotNetCliPath = pathToCli })
    else
        DotNet.install (fun cliOpts -> { cliOpts with Version = DotNet.CliVersion.GlobalJson })

let runDotnet workingDir command args =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir >> withDotnetExe) command args

    if result.ExitCode <> 0 then failwithf "dotnet %s %s failed with code %d and errors:\n%s" command args result.ExitCode (result.Errors |> String.concat "\n")

// --------------------------------------------------------------------------------------
// The rest of the code is standard F# build script
// --------------------------------------------------------------------------------------

let releaseDir = Path.Combine(__SOURCE_DIRECTORY__, "../artifacts/bin/fcs/Release")
let packagesDir = Path.Combine(__SOURCE_DIRECTORY__, "../artifacts/packages/Release/Shipping")
let docsDir = Path.Combine(__SOURCE_DIRECTORY__, "docsrc", "_public")

// Read release notes & version info from RELEASE_NOTES.md
let release = ReleaseNotes.load (__SOURCE_DIRECTORY__ + "/RELEASE_NOTES.md")
let isVersionTag (tag: string) = Version.TryParse tag |> fst
let assemblyVersion = release.NugetVersion

Target.create "Clean" (fun _ ->
    Shell.cleanDir releaseDir
    Shell.cleanDir docsDir
)

Target.create "Restore" (fun _ ->
    // We assume a paket restore has already been run
    runDotnet __SOURCE_DIRECTORY__ "restore" "../src/buildtools/buildtools.proj -v n"
    runDotnet __SOURCE_DIRECTORY__ "restore" "FSharp.Compiler.Service/FSharp.Compiler.Service.fsproj -v n"
)

let outputs =[
  "FSharp.Compiler.Service/FSharp.Compiler.Service.fsproj"
  "../src/fsharp/FSharp.DependencyManager.Nuget/FSharp.DependencyManager.Nuget.fsproj"
  "FSharp.Compiler.Service.MSBuild.v12/FSharp.Compiler.Service.MSBuild.v12.fsproj"
]

Target.create "Build" (fun _ ->
    runDotnet __SOURCE_DIRECTORY__ "build" "../src/buildtools/buildtools.proj -v n -c Proto"
    let fslexPath = __SOURCE_DIRECTORY__ + "/../artifacts/bin/fslex/Proto/netcoreapp3.1/fslex.dll"
    let fsyaccPath = __SOURCE_DIRECTORY__ + "/../artifacts/bin/fsyacc/Proto/netcoreapp3.1/fsyacc.dll"
    
    let build relPath =
      let dir, proj = Path.getDirectory relPath, Path.GetFileName relPath
      runDotnet (Path.Combine(__SOURCE_DIRECTORY__, dir)) "build" (sprintf "%s -nodereuse:false -v n -c Release /p:DisableCompilerRedirection=true /p:FsLexPath=%s /p:FsYaccPath=%s" proj fslexPath fsyaccPath)
    
    outputs
    |> List.iter build
)

Target.create "Test" (fun _ ->
    // This project file is used for the netcoreapp2.0 tests to work out reference sets
    runDotnet __SOURCE_DIRECTORY__ "build" "../tests/projects/Sample_NETCoreSDK_FSharp_Library_netstandard2_0/Sample_NETCoreSDK_FSharp_Library_netstandard2_0.fsproj -nodereuse:false -v n /restore /p:DisableCompilerRedirection=true"

    // Now run the tests (different output files per TFM)
    let logFilePath = Path.Combine(__SOURCE_DIRECTORY__, "..", "artifacts", "TestResults", "Release", "FSharp.Compiler.Service.Test.{framework}.xml")
    runDotnet (Path.Combine(__SOURCE_DIRECTORY__, "FSharp.Compiler.Service.Tests")) "test" (sprintf "FSharp.Compiler.Service.Tests.fsproj -nodereuse:false -v n -c Release --logger \"nunit;LogFilePath=%s\"" logFilePath)
)

Target.create "NuGet" (fun _ ->
    outputs
    |> List.iter (
      DotNet.pack (fun packOpts ->
        { packOpts with
            Configuration = DotNet.BuildConfiguration.Release
            Common = packOpts.Common |> withDotnetExe |> DotNet.Options.withVerbosity (Some DotNet.Verbosity.Normal)
            MSBuildParams = { packOpts.MSBuildParams with
                                Properties = packOpts.MSBuildParams.Properties @ [ "Version", assemblyVersion; "VersionPrefix", assemblyVersion; "PackageReleaseNotes", release.Notes |> String.concat "\n" ] }
        })
    )
)

Target.create "GenerateDocs" (fun _ ->
    runDotnet "docsrc" "fornax" "build"
)


open Fake.IO.Globbing.Operators

let packagesPatterns =
  !! (sprintf "%s/*.%s.nupkg" releaseDir release.NugetVersion)
  ++ (sprintf "%s/FSharp.DependencyManager.Nuget.%s.nupkg" packagesDir release.NugetVersion)

Target.create "PublishNuGet" (fun _ ->
  let apikey = 
    Environment.environVarOrNone "NUGET_APIKEY" 
    |> Option.defaultWith (fun _ -> UserInput.getUserPassword "Nuget API Key: ")

  packagesPatterns
  |> Seq.iter (fun nupkg ->
    DotNet.nugetPush (fun p -> {
      p with
        PushParams = { p.PushParams with
                          ApiKey = Some apikey
                          Source = Some "https://api.nuget.org/v3/index.json" }
    }) nupkg
  )
)

let anchor (path: string) =
  System.IO.Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, path))

let bumpMajor (semver: Paket.SemVerInfo) =
  { semver with Major = semver.Major + 1u
                Minor = 0u
                Patch = 0u }

let bumpMinor (semver: Paket.SemVerInfo) =
  { semver with Minor = semver.Minor + 1u
                Patch = 0u }

let bumpPatch (semver: Paket.SemVerInfo) =
  { semver with Patch = semver.Patch + 1u }

let (|WithinRange|OutsideRange|) (leftSemver, (magnitude: SynVer.Version), rightSemver) =
  let allowedMin =
    match magnitude with
    | SynVer.Version.Major -> bumpMajor leftSemver
    | SynVer.Version.Minor -> bumpMinor leftSemver
    | SynVer.Version.Patch -> bumpPatch leftSemver

  if rightSemver < allowedMin || rightSemver < leftSemver then OutsideRange else WithinRange

Target.create "ValidateVersionBump" (fun _ ->
  let intendedVersion = Paket.PublicAPI.ParseSemVer release.NugetVersion
  let lockfile = Paket.LockFile.LoadFrom "paket.lock"
  let refGroup = lockfile.Groups.[Paket.Domain.GroupName "reference"]
  let oldPackage = refGroup.Resolution.[Paket.Domain.PackageName "FSharp.Compiler.Service"]
  let oldVersion = oldPackage.Version
  let oldSurfaceArea = SynVer.SurfaceArea.ofAssembly (System.Reflection.Assembly.LoadFile (anchor "packages/reference/FSharp.Compiler.Service/lib/netstandard2.0/FSharp.Compiler.Service.dll"))
  let newSurfaceArea = SynVer.SurfaceArea.ofAssembly (System.Reflection.Assembly.LoadFile (anchor "../artifacts/bin/fcs/Release/netstandard2.0/FSharp.Compiler.Service.dll"))
  let (computedVersion, computedMagnitude) = SynVer.SurfaceArea.bump (string oldVersion) oldSurfaceArea newSurfaceArea
  let parsedComputedVersion = Paket.PublicAPI.ParseSemVer computedVersion
  let apiDiffs = SynVer.SurfaceArea.diff oldSurfaceArea newSurfaceArea |> String.concat "\n"
  match oldVersion, computedMagnitude, intendedVersion with
  | WithinRange ->
    Trace.tracefn "Version %A is within the allowed range of %A from the prior version of %A" intendedVersion computedMagnitude oldVersion
  | OutsideRange ->
    failwithf """Version bump invalid.
  Version packaged was %A
  Version computed due to API diffs was %A
  Allowed version magnitude change is %A
  The full set of API diffs is:
%A
"""  intendedVersion parsedComputedVersion computedMagnitude apiDiffs
)

Target.create "CreateRelease" (fun _ ->
  async {
    let client = GitHub.createClientWithToken (Environment.environVarOrNone "GITHUB_TOKEN" |> Option.defaultWith (fun _ -> UserInput.getUserPassword "Github API Token: "))
    let currentSha = Git.Information.getCurrentSHA1 ""
    let releaseParams (input: GitHub.CreateReleaseParams) =
      { input with
          Name = sprintf "Release %s" release.NugetVersion
          Body = release.Notes |> String.concat "\n"
          TargetCommitish = currentSha }
    let artifacts = packagesPatterns
    let! release = GitHub.createRelease "fsharp" "FSharp.Compiler.Service" assemblyVersion releaseParams client
    let! _release = GitHub.uploadFiles artifacts (async.Return release)
    return ()
  }
  |> Async.RunSynchronously
)


// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override



Target.create "Start" ignore
Target.create "Release" ignore
Target.create "TestAndNuGet" ignore

open Fake.Core.TargetOperators

"Start"
  ==> "Restore"
  ==> "Clean"
  ==> "Build"

"Build"
  ==> "Test"

"Build"
  ==> "NuGet"

"Test"
  ==> "TestAndNuGet"

"NuGet"
  ==> "TestAndNuGet"

"Build"
  ==> "NuGet"
  ==> "ValidateVersionBump"
  ==> "PublishNuGet"
  ==> "Release"

"Build"
  ==> "GenerateDocs"

"GenerateDocs"
  ==> "Release"

Target.runOrDefaultWithArguments "Build"
