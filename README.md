# The F# Language, Library, and  Visual F# Tools Repository

You are invited to help producing future releases of the F# language compiler, library, and tools. This repository enables development on Linux, macOS and Windows, along with some automated CI testing for these.

* [About F#](http://fsharp.org)
* [Testimonials](http://fsharp.org/testimonials)
* [Contributing](#contributing)
* [Using](#using)

The F# Compiler and Tools are also mirrored in [the corresponding repository](http://github.com/fsharp/fsharp) of the F# Software Foundation.

Changes contributed here are eventually propagated to this repository and are included in all packagings of F# and open source F# editing tools. The process for doing this is explained in this guide by the [F# Core Engineering Group](https://fsharp.github.io/2014/06/18/fsharp-contributions.html). Currently, the F# community coordinates packaging [other editions of F#](https://github.com/fsharp/fsharp/) for use on Linux, macOS, Android, iOS, and other platforms, and Microsoft coordinates packaging this repository as part of the Visual F# Tools. 

For historical reasons this repository is called "visualfsharp" and currently also contains the Visual F# IDE Tools. The eventual plan is to split these repositories into "fsharp" and "visualfsharp".


.NET Framework:

|            | Ubuntu (Build) | Windows (Debug Build) | Windows (Release Tests 1) | Windows (Release Tests 2) | Windows  (Release Tests 3) |
|:----------:|:----------------:|:----------------:|:------------------:|:-----------------------:|:---------------------:|
|**master**  |[![Build Status](https://ci2.dot.net/buildStatus/icon?job=Microsoft_visualfsharp/master/release_ubuntu14.04)](https://ci2.dot.net/job/Microsoft_visualfsharp/job/master/job/release_ubuntu14.04/)|[![Build Status](https://ci2.dot.net/buildStatus/icon?job=Microsoft_visualfsharp/master/debug_windows_nt)](https://ci2.dot.net/job/Microsoft_visualfsharp/job/master/job/debug_windows_nt/)|[![Build Status](https://ci2.dot.net/buildStatus/icon?job=Microsoft_visualfsharp/master/release_ci_part1_windows_nt)](https://ci2.dot.net/job/Microsoft_visualfsharp/job/master/job/release_ci_part1_windows_nt/)|[![Build Status](https://ci2.dot.net/buildStatus/icon?job=Microsoft_visualfsharp/master/release_ci_part2_windows_nt)](https://ci2.dot.net/job/Microsoft_visualfsharp/job/master/job/release_ci_part2_windows_nt/)|[![Build Status](https://ci2.dot.net/buildStatus/icon?job=Microsoft_visualfsharp/master/release_ci_part3_windows_nt)](https://ci2.dot.net/job/Microsoft_visualfsharp/job/master/job/release_ci_part3_windows_nt/)|
|**vs2017-rtm**  |[![Build Status](https://ci2.dot.net/buildStatus/icon?job=Microsoft_visualfsharp/vs2017-rtm/release_ubuntu14.04)](https://ci2.dot.net/job/Microsoft_visualfsharp/job/vs2017-rtm/job/release_ubuntu14.04/)|[![Build Status](https://ci2.dot.net/buildStatus/icon?job=Microsoft_visualfsharp/vs2017-rtm/debug_windows_nt)](https://ci2.dot.net/job/Microsoft_visualfsharp/job/vs2017-rtm/job/debug_windows_nt/)|[![Build Status](https://ci2.dot.net/buildStatus/icon?job=Microsoft_visualfsharp/vs2017-rtm/release_ci_part1_windows_nt)](https://ci2.dot.net/job/Microsoft_visualfsharp/job/vs2017-rtm/job/release_ci_part1_windows_nt/)|[![Build Status](https://ci2.dot.net/buildStatus/icon?job=Microsoft_visualfsharp/vs2017-rtm/release_ci_part2_windows_nt)](https://ci2.dot.net/job/Microsoft_visualfsharp/job/vs2017-rtm/job/release_ci_part2_windows_nt/)|[![Build Status](https://ci2.dot.net/buildStatus/icon?job=Microsoft_visualfsharp/vs2017-rtm/release_ci_part3_windows_nt)](https://ci2.dot.net/job/Microsoft_visualfsharp/job/vs2017-rtm/job/release_ci_part3_windows_nt/)|


## Help improve the Quality of the Tools by Using the Nightly Releases of Visual F# Tools
To setup Visual Studio to use the latest nightly releases of the Visual F# Tools:
https://blogs.msdn.microsoft.com/dotnet/2017/03/14/announcing-nightly-releases-for-the-visual-f-tools/


    build.cmd All.NetFx 
    (unix: ./build.sh All.NetFx)

.NET Core

    build All.NetCore

Both:

    build All


Build Status
------------

Head (branch ``master``), Mono, Linux/OSX + unit tests (Travis) [![Build Status](https://travis-ci.org/fsharp/FSharp.Compiler.Service.png?branch=master)](https://travis-ci.org/fsharp/FSharp.Compiler.Service/branches)

Head (branch ``master``), Windows Server 2012 R2 + VS2015 + unit tests (AppVeyor)  [![Build status](https://ci.appveyor.com/api/projects/status/3yllu2qh19brk61d?svg=true)](https://ci.appveyor.com/project/fsgit/fsharp-compiler-service)

NuGet Feed  [![NuGet Badge](https://buildstats.info/nuget/FSharp.Compiler.Service)](https://www.nuget.org/packages/FSharp.Compiler.Service)

Stable builds are available in the NuGet Gallery:
[https://www.nuget.org/packages/FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service)

All AppVeyor builds are available using the NuGet feed: https://ci.appveyor.com/nuget/fsgit-fsharp-compiler-service

## Using

For typical installs of  F#, see http://fsharp.org.

### Using Nightly Releases of Visual F# Tools

To setup Visual Studio to use the latest nightly releases of the Visual F# Tools:

https://blogs.msdn.microsoft.com/dotnet/2017/03/14/announcing-nightly-releases-for-the-visual-f-tools/

### Using CI Builds

To install F#, see http://fsharp.org.

To download the bits for the latest CI builds see [these instructions](https://github.com/Microsoft/visualfsharp/wiki/Using-CI-Builds). This includes and ZIPs containing the F# compiler and VSIX installers for the Visual F# IDE Tools.

### Using F# on a build server or computer without an F# installation

If you wish to use the latest F# compiler on a computer without Visual Studio 2017 installed, you can add the nuget package ``FSharp.Compiler.Tools`` to your projects. This will replace the in-box compiler with the version contained in the package.
The actual package is built in https://github.com/fsharp/fsharp.

You will need to adjust the targets reference on your project file to use the targets file from the installed ``FSharp.Compiler.Tools`` package.
See https://github.com/fsharp/fsharp/issues/676 for how to modify your project file.

## Code of Conduct

This project has adopted the code of conduct defined by the [Contributor Covenant](http://contributor-covenant.org/) to clarify expected behavior in our community. This code of conduct has been [adopted by many other projects](http://contributor-covenant.org/adopters/). For more information see the [Code of conduct](https://github.com/Microsoft/visualfsharp/wiki/Code-of-Conduct).

## Get In Touch

Follow [@VisualFSharp](https://twitter.com/VisualFSharp) and [@fsharporg](https://twitter.com/fsharporg) on twitter and subscribe to the [.NET Blog](https://blogs.msdn.microsoft.com/dotnet/).

Members of the F# Software Foundation can be invited to the "F# Software Foundation" discussion rooms on slack. More details at http://fsharp.org/guides/slack/.
