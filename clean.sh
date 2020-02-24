#! /usr/bin/env bash

DEAD_DIRS=(
    "azure-pipelines.yml"
    "DEVGUIDE.md"
    "eng/common/templates"
    "FSharp.sln"
    "src/fsharp/fsc/fsc.fsproj"
    "src/fsharp/FSharp.Compiler.nuget/Microsoft.FSharp.Compiler.nuspec"
    "src/fsharp/FSharp.Compiler.Private/FSharp.Compiler.Private.fsproj"
    "src/fsharp/fsi/fsi.fsproj"
    "src/fsharp/fsiAnyCpu/fsiAnyCpu.fsproj"
    "src/fsharp/Interactive.DependencyManager/xlf"
    "src/fsharp/xlf"
    "TESTGUIDE.md"
    "tests/EndToEndBuildTests"
    "tests/FSharp.Compiler.Private.Scripting.UnitTests"
    "tests/FSharp.Compiler.UnitTests"
    "tests/FSharp.Core.UnitTests"
    "tests/fsharp/Compiler"
    "tests/fsharp/conformance"
    "tests/fsharp/core"
    "tests/fsharp/test-framework.fs"
    "tests/fsharp/tests.fs"
    "tests/fsharp/typecheck"
    "tests/fsharpqa"
    "tests/scripts"
    "VisualFSharp.sln"
    "vsintegration"
)

git rm -rf --ignore-unmatch ${DEAD_DIRS[*]}