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
 * [FSharp.Compiler.Service package on NuGet](https://www.nuget.org/packages/FSharp.Compiler.Service)
 * [Developer notes explain the project structure](http://fsharp.github.io/FSharp.Compiler.Service/devnotes.html)

Build Status
------------

Head (branch ``master``), Mono 3.x, OSX + unit tests (Travis) [![Build Status](https://travis-ci.org/fsharp/FSharp.Compiler.Service.png?branch=master)](https://travis-ci.org/fsharp/FSharp.Compiler.Service/branches)

Head (branch ``master``), Windows Server 2012 R2 + unit tests (AppVeyor)  [![Build status](https://ci.appveyor.com/api/projects/status/3yllu2qh19brk61d)](https://ci.appveyor.com/project/fsgit/fsharp-compiler-service)

Dev Guide
----------

To integrate latest changes from http://github.com/fsharp/fsharp, use

git remote add fsc https://github.com/fsharp/fsharp
git pull fsc master

