#!/bin/bash

mono .nuget/NuGet.exe install FAKE -OutputDirectory packages -ExcludeVersion
mono .nuget/NuGet.exe install FSharp.Formatting -OutputDirectory packages -ExcludeVersion
mono .nuget/NuGet.exe install SourceLink.Fake -OutputDirectory packages -ExcludeVersion

mono packages/FAKE/tools/FAKE.exe build.fsx -d:MONO $@
