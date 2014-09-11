@echo off

if not exist packages\FAKE\tools\Fake.exe ( 
  ".nuget/NuGet.exe" "install" "FAKE" "-OutputDirectory" "packages" "-ExcludeVersion" 
  ".nuget/NuGet.exe" "install" "FSharp.Formatting" "-OutputDirectory" "packages" "-ExcludeVersion"
  ".nuget/NuGet.exe" "install" "SourceLink.Fake" "-OutputDirectory" "packages" "-ExcludeVersion"
)
packages\FAKE\tools\FAKE.exe build.fsx %*
