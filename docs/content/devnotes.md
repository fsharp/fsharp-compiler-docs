Developer notes
===============

Modified clone of F# compiler exposing additional functionality for editing clients and embedding F# compiler
and F# interactive as services.

## Components

At the moment, the two main components are `FSharp.Compiler.Service.dll` and
`FSharp.Interactive.Service.dll`. The first one contains minor modifications in visibility 
to allow refactoring editing and other tools to have access to the full F# AST and parser.
The main aim is to have a stable and documented fork of the main compiler that allows various 
tools to share this common code.  

The second component allows embedding F# Interactive as a service and contains a number of
modifications to the source code of `fsi.exe` that adds `EvalExpression` and `EvalInteraction` functions.

This repo should be _identical_ to 'fsharp' except:

  - Changes for building `FSharp.Compiler.Service.dll`, notably
    - Change the assembly name
    - Only build `FSharp.Compiler.Service.dll`
    - No bootstrap or proto compiler is used - an installed F# compiler is assumed

  - Changes for building `FSharp.Interactive.Service.dll`, notably
    - New project `FSharp.Interactive.Service.fsproj` that references relevant files
      from `fsi.exe` source code. 

  - Build script using FAKE that builds everything, produces NuGet package and 
    generates documentation, files for publising NuGet packages etc.
    (following [F# project scaffold](https://github.com/fsprojects/FSharp.ProjectScaffold))

  - Changes to compiler source code to expose new functionality; Changes to the
    F# Interactive service to implement the evaluation functions.

  - Additions to compiler source code which improve the API for the use of F# editing clients

  - Additions to compiler source code which add new functionality used by all F# editing clients

If language or compiler addiitons are committed to `fsharp/fsharp`, they should be merged into 
this repo and a new NuGet package released.

## Building and NuGet

The build process follows the standard recommended by [F# project scaffold](https://github.com/fsprojects/FSharp.ProjectScaffold)
If you want to build the project yourself then you can follow these instructions:

    [lang=text]
    git clone https://github.com/fsharp/FSharp.Compiler.Service
    cd FSharp.Compiler.Service

Now follow build everything by running `build.cmd` (Windows) or `build.sh` (Linux + Mac OS).
The output will be located in the `bin` directory. If you also wish to build the documentation
and NuGet package, run `build Release` (this also attempts to publish the documentation to
GitHub, which only works if you have access to the GitHub repository).

## Clients

The known tools that use this component are:

 * [Fantomas](https://github.com/dungpa/fantomas) - F# code formatting tool
 * [Fsharp-Refactor](https://github.com/Lewix/fsharp-refactor) - Refactoring for F#
 * [FSharpbinding](https://github.com/fsharp/fsharpbinding) - Xamarin studio bindings
 * [F# Snippets web site](http://fssnip.net/) - smart F# pastebin

If you modify this component it is polite to check that these tools all build after your modifications.