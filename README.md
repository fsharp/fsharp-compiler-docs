F# Compiler Service
===================

The F# compiler services package contains a custom build of the F# compiler that
exposes additional functionality for implementing F# language bindings, additional
tools based on the compiler or refactoring tools. The package also includes F#
interactive service that can be used for embedding F# scripting into your applications.

Documentation
-------------

For more information about the project, see:

 * [F# Compiler Service documentation](http://fsharp.github.io/FSharp.Compiler.Service/)
 * [Developer notes explain the project structure](http://fsharp.github.io/FSharp.Compiler.Service/devnotes.html)

Build and Test
-----

On Windows:

    .\fcs\build.cmd Test

On Linux:

    ./fcs/build.sh Test

Packages:

    build NuGet
    build TestAndNuGet


Build Status
------------

Head (branch ``master``):

 * Linux/OSX: [![Build Status](https://travis-ci.org/fsharp/FSharp.Compiler.Service.svg?branch=master)](https://travis-ci.org/fsharp/FSharp.Compiler.Service)
 * Windows: [![Build status](https://ci.appveyor.com/api/projects/status/3yllu2qh19brk61d?svg=true)](https://ci.appveyor.com/project/fsgit/fsharp-compiler-service) 

NuGet  [![NuGet Badge](https://buildstats.info/nuget/FSharp.Compiler.Service)](https://www.nuget.org/packages/FSharp.Compiler.Service)

Stable builds are available in the NuGet Gallery:
[https://www.nuget.org/packages/FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service)

All AppVeyor builds are available using the NuGet feed: https://ci.appveyor.com/nuget/fsgit-fsharp-compiler-service

If using Paket, add the source at the top of `paket.dependencies`.

License
-----------

This project is subject to the MIT License. A copy of this license can be found in [License.txt](License.txt) at the root of this repo.

Maintainers
-----------

The maintainers of this repository are:

 - [Don Syme](http://github.com/dsyme)
 - [Tomas Petricek](http://github.com/tpetricek)
 - [Enrico Sada](http://github.com/enricosada)
 - Many people have helped including [Robin Neatherway](https://github.com/rneatherway), [Dave Thomas](http://github.com/7sharp9), [Lincoln Atkinson](http://github.com/latkin), [Kevin Ransom](http://github.com/KevinRansom), [Vladimir Matveev](http://github.com/vladima) and others
