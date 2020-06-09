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

    if result.ExitCode <> 0 then failwithf "dotnet %s failed with errors: %s" args (result.Errors |> String.concat "\n")

// --------------------------------------------------------------------------------------
// The rest of the code is standard F# build script
// --------------------------------------------------------------------------------------

let releaseDir = Path.Combine(__SOURCE_DIRECTORY__, "../artifacts/bin/fcs/Release")
let packagesDir = Path.Combine(__SOURCE_DIRECTORY__, "../artifacts/packages/Release/Shipping")
let docsDir = Path.Combine(__SOURCE_DIRECTORY__, "docsrc", "_public")

// Read release notes & version info from RELEASE_NOTES.md
let release = ReleaseNotes.load (__SOURCE_DIRECTORY__ + "/RELEASE_NOTES.md")
let isAppVeyorBuild = AppVeyor.detect()
let isVersionTag (tag: string) = Version.TryParse tag |> fst
let hasRepoVersionTag = isAppVeyorBuild && AppVeyor.Environment.RepoTag && isVersionTag AppVeyor.Environment.RepoTagName
let assemblyVersion = if hasRepoVersionTag then AppVeyor.Environment.RepoTagName else release.NugetVersion

let buildVersion =
    if hasRepoVersionTag then assemblyVersion
    else if isAppVeyorBuild then sprintf "%s-b%s" assemblyVersion AppVeyor.Environment.BuildNumber
    else assemblyVersion

Target.create "Clean" (fun _ ->
    Shell.cleanDir releaseDir
    Shell.cleanDir docsDir
)

Target.create "Restore" (fun _ ->
    // We assume a paket restore has already been run
    runDotnet __SOURCE_DIRECTORY__ "restore" "../src/buildtools/buildtools.proj -v n"
    runDotnet __SOURCE_DIRECTORY__ "restore" "FSharp.Compiler.Service.sln -v n"
)

Target.create "BuildVersion" (fun _ ->
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" buildVersion) |> ignore
)

Target.create "Build" (fun _ ->
    runDotnet __SOURCE_DIRECTORY__ "build" "../src/buildtools/buildtools.proj -v n -c Proto"
    let fslexPath = Path.GetFullPath <| Path.Combine(__SOURCE_DIRECTORY__, "../artifacts/bin/fslex/Proto/netcoreapp3.1/fslex.dll")
    let fsyaccPath = Path.GetFullPath <| Path.Combine(__SOURCE_DIRECTORY__, "../artifacts/bin/fsyacc/Proto/netcoreapp3.1/fsyacc.dll")
    runDotnet __SOURCE_DIRECTORY__ "build" (sprintf "FSharp.Compiler.Service.sln -nodereuse:false -v n -c Release /p:DisableCompilerRedirection=true /p:FsLexPath=%s /p:FsYaccPath=%s" fslexPath fsyaccPath)
)

Target.create "Test" (fun _ ->
    // This project file is used for the netcoreapp2.0 tests to work out reference sets
    runDotnet __SOURCE_DIRECTORY__ "build" "../tests/projects/Sample_NETCoreSDK_FSharp_Library_netstandard2_0/Sample_NETCoreSDK_FSharp_Library_netstandard2_0.fsproj -nodereuse:false -v n /restore /p:DisableCompilerRedirection=true"

    // Now run the tests (different output files per TFM)
    let logFilePath = Path.Combine(__SOURCE_DIRECTORY__, "..", "artifacts", "TestResults", "Release", "FSharp.Compiler.Service.Test.{framework}.xml")
    runDotnet __SOURCE_DIRECTORY__ "test" (sprintf "FSharp.Compiler.Service.Tests/FSharp.Compiler.Service.Tests.fsproj --no-restore --no-build -nodereuse:false -v n -c Release --logger \"nunit;LogFilePath=%s\"" logFilePath)
)

Target.create "NuGet" (fun _ ->
    DotNet.pack (fun packOpts ->
      { packOpts with
          Configuration = DotNet.BuildConfiguration.Release
          Common = packOpts.Common |> withDotnetExe |> DotNet.Options.withVerbosity (Some DotNet.Verbosity.Normal)
          MSBuildParams = { packOpts.MSBuildParams with
                              Properties = packOpts.MSBuildParams.Properties @ [ "Version", assemblyVersion; "VersionPrefix", assemblyVersion; "PackageReleaseNotes", release.Notes |> String.concat "\n" ] }
      }) "FSharp.Compiler.Service.sln"
)

Target.create "GenerateDocs" (fun _ ->
    runDotnet "docsrc" "fornax" "build"
)


open Fake.IO.Globbing.Operators

Target.create "PublishNuGet" (fun _ ->
  let apikey = lazy(Environment.environVarOrDefault "nuget-apikey" (UserInput.getUserPassword "Nuget API Key: "))
  !! (sprintf "%s/*.%s.nupkg" releaseDir release.NugetVersion)
  ++ (sprintf "%s/FSharp.DependencyManager.Nuget.%s.nupkg" packagesDir release.NugetVersion)
  |> Seq.iter (fun nupkg ->
    DotNet.nugetPush (fun p -> {
      p with
        PushParams = { p.PushParams with
                          ApiKey = Some apikey.Value
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

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override



Target.create "Start" ignore
Target.create "Release" ignore
Target.create "TestAndNuGet" ignore

open Fake.Core.TargetOperators

"Start"
  =?> ("BuildVersion", isAppVeyorBuild)
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
