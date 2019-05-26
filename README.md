# F# Compiler Service

The FSharp.Compiler.Service package contains a custom build of the F# compiler that
exposes additional functionality for implementing F# language bindings, additional
tools based on the compiler or refactoring tools. The package also includes F#
interactive service that can be used for embedding F# scripting into your applications. This repository exists entirely to create and publish that package.

## Purpose

> This repo is (mostly) **read only**

This repo exists as a downstream packaging repository for the [FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service) NuGet Package. It serves the following purposes _only_:

* Release packaging
* Hosting [public documentation](http://fsharp.github.io/FSharp.Compiler.Service/)
* Serving as a stable base for Fable

It is a fork of the official F# source repository, which is located at [microsoft/visualfsharp](https://github.com/microsoft/visualfsharp). **All** issues and contributions should be raised there. All feature development should be targeted there. Once contributions are accepted into [microsoft/visualfsharp](https://github.com/microsoft/visualfsharp), they will be integrated into this repository for packaging and release.

If you need to add customizations to FSharp.Compiler.Service for your own uses, you should clone [microsoft/visualfsharp](https://github.com/microsoft/visualfsharp) and build the FSharp.Compiler.Service binaries from there. The process is exactly the same as it is described below.

### No contribution is too small

Even if you find a single-character typo, we're happy to take the change! Although the codebase can feel daunting for beginners, we and other contributors are happy to help you along.

## Documentation

For more information about the project, see:

 * [F# Compiler Service API and usage documentation](http://fsharp.github.io/FSharp.Compiler.Service/)
 * [Description of the project structure](http://fsharp.github.io/FSharp.Compiler.Service/devnotes.html)

## Build and Test

On Windows:

    .\fcs\build.cmd Test

On Linux:

    ./fcs/build.sh Test

To build NuGet packages:

    build NuGet
    build TestAndNuGet

## Build Status

Branch | OS | Badge |
------ | ------ | - |
master | Linux/OSX | [![Build Status](https://travis-ci.org/fsharp/FSharp.Compiler.Service.svg?branch=master)](https://travis-ci.org/fsharp/FSharp.Compiler.Service) |
master | Windows | [![Build status](https://ci.appveyor.com/api/projects/status/3yllu2qh19brk61d?svg=true)](https://ci.appveyor.com/project/fsgit/fsharp-compiler-service)  |

## NuGet package

[![NuGet Badge](https://buildstats.info/nuget/FSharp.Compiler.Service)](https://www.nuget.org/packages/FSharp.Compiler.Service)

Stable builds are available in the NuGet Gallery:
[https://www.nuget.org/packages/FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service)

All AppVeyor builds are available using the NuGet feed: https://ci.appveyor.com/nuget/fsgit-fsharp-compiler-service

If using Paket, add the source at the top of `paket.dependencies`.

## License

This project is subject to the MIT License. A copy of this license can be found in [License.txt](License.txt) at the root of this repo.

## Maintainers

The maintainers of this repository are:

 - [Don Syme](http://github.com/dsyme)
 - [Tomas Petricek](http://github.com/tpetricek)
 - [Enrico Sada](http://github.com/enricosada)
 - [Chet Husk](http://github.com/baronfel)
 - Many people have helped including [Robin Neatherway](https://github.com/rneatherway), [Dave Thomas](http://github.com/7sharp9), [Lincoln Atkinson](http://github.com/latkin), [Kevin Ransom](http://github.com/KevinRansom), [Vladimir Matveev](http://github.com/vladima) and others