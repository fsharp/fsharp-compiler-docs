#!/bin/bash

if [[ ! -e ~/.config/.mono/certs ]];
then
    mozroots --import --sync --quiet
fi

if [[ ! -e packages/FAKE/tools/FAKE.exe ]];
then
    mono .nuget/NuGet.exe install FAKE -OutputDirectory packages -ExcludeVersion
fi

if [[ ! -e packages/FSharp.Formatting/lib/net40/FSharp.Literate.dll ]];
then
    mono .nuget/NuGet.exe install FSharp.Formatting -OutputDirectory packages -ExcludeVersion
fi

if [[ ! -e packages/SourceLink.Fake/tools/SourceLink.dll ]];
then
    mono .nuget/NuGet.exe install SourceLink.Fake -OutputDirectory packages -ExcludeVersion
fi

mono packages/FAKE/tools/FAKE.exe build.fsx -d:MONO "$@"
