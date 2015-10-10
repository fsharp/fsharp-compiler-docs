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

Build Status
------------

Head (branch ``master``), Mono 3.x, OSX + unit tests (Travis) [![Build Status](https://travis-ci.org/fsharp/FSharp.Compiler.Service.png?branch=master)](https://travis-ci.org/fsharp/FSharp.Compiler.Service/branches)

Head (branch ``master``), Windows Server 2012 R2 + unit tests (AppVeyor)  [![Build status](https://ci.appveyor.com/api/projects/status/3yllu2qh19brk61d)](https://ci.appveyor.com/project/fsgit/fsharp-compiler-service)

NuGet Feed
------------

Stable builds are available in the NuGet Gallery:
[https://www.nuget.org/packages/FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service)

All AppVeyor builds are available using the NuGet feed: https://ci.appveyor.com/nuget/fsgit-fsharp-compiler-service

If using Paket, add the source at the top of `paket.dependencies`.

```
source https://www.nuget.org/api/v2
source https://ci.appveyor.com/nuget/fsgit-fsharp
```

Add the dependency on `FSharp.Core` and run `paket update`. See the AppVeyor [build history](https://ci.appveyor.com/project/fsgit/fsharp/history) for a list of available versions. Here are some options for specifying the dependency:

```
nuget FSharp.Core
nuget FSharp.Core prerelease
nuget FSharp.Core 3.1.2.3
nuget FSharp.Core 3.1.2.3-b208
```

If using NuGet Package Manager, add the source to the list of available package sources.

![Available Package Sources](https://cloud.githubusercontent.com/assets/80104/8576204/3cf077f4-2555-11e5-80cc-5db185af7d1e.png)

## Build Requirements

Building F# on Unix-type platforms requires
[Mono](http://www.mono-project.com/download/) 3.0 or higher. If you
get a complaint in subsequent steps about `xbuild` being missing, it means
you don't have Mono installed.

Building on OS X requires several development tools that are not installed
by default. Most can be installed via [Homebrew](http://brew.sh/):

	brew install autoconf automake pkg-config

Building on OS X also requires Xcode. If you don't want to install
the full GUI development environment, the command line tools are sufficient.
At a shell prompt, say:

	xcode-select --install

## How to Build

### Linux and other Unix systems:
The usual:

	./autogen.sh --prefix=/usr
	make
	sudo make install

By default that makes optimized binaries. To make debug, use ```make CONFIG=debug```

### OS X

Use a prefix to your version of Mono:

	./autogen.sh --prefix=/Library/Frameworks/Mono.framework/Versions/Current/
	make
	sudo make install

By default that makes optimized binaries. To make debug, use ```make CONFIG=debug```

### Windows, using msbuild

Build using:

    build.bat

This build the proto compiler, then the library, then the final compiler.

You can also build these independently using:

    msbuild src\fsharp-proto-build.proj
    ngen install ..\lib\proto\fsc-proto.exe
    msbuild src\fsharp-library-build.proj /p:Configuration=Release
    msbuild src\fsharp-compiler-build.proj /p:Configuration=Release

You can also build FSharp.Core.dll for other profiles:

    msbuild src\fsharp-library-build.proj /p:TargetFramework=net20 /p:Configuration=Release
    msbuild src\fsharp-library-build.proj /p:TargetFramework=portable47 /p:Configuration=Release
    msbuild src\fsharp-library-build.proj /p:TargetFramework=portable7 /p:Configuration=Release
    msbuild src\fsharp-library-build.proj /p:TargetFramework=portable78 /p:Configuration=Release
    msbuild src\fsharp-library-build.proj /p:TargetFramework=portable259 /p:Configuration=Release
    msbuild src\fsharp-library-build.proj /p:TargetFramework=sl5 /p:Configuration=Release

    msbuild src\fsharp-library-build.proj /p:TargetFramework=monodroid /p:Configuration=Release
    msbuild src\fsharp-library-build.proj /p:TargetFramework=monotouch /p:Configuration=Release
    msbuild src\fsharp-library-build.proj /p:TargetFramework=net40-xna40-xbox360 /p:Configuration=Release

You can also build the FSharp.Core and FSharp.Compiler.Silverlight.dll for Silverlight 5.0:

    msbuild src\fsharp-library-build.proj /p:TargetFramework=sl5-compiler  /p:Configuration=Release

Change to ``` /p:Configuration=Debug``` for debug binaries.

Add ``` /p:FSharpCoreBackVersion=3.0``` to build a back version of FSharp.Core.dll with a
version number suitable for use when building libaries that have usable with both F# 3.0 and F# 3.1 libraries.

    msbuild src\fsharp-library-build.proj /p:TargetFramework=net20 /p:Configuration=Release /p:FSharpCoreBackVersion=3.0
    msbuild src\fsharp-library-build.proj /p:TargetFramework=net40 /p:Configuration=Release /p:FSharpCoreBackVersion=3.0
    msbuild src\fsharp-library-build.proj /p:TargetFramework=portable47 /p:Configuration=Release /p:FSharpCoreBackVersion=3.0
	msbuild src\fsharp-library-build.proj /p:TargetFramework=net40 /p:Configuration=Release /p:FSharpCoreBackVersion=3.0
	msbuild src\fsharp-library-build.proj /p:TargetFramework=portable47 /p:Configuration=Release /p:FSharpCoreBackVersion=3.0
	msbuild src\fsharp-library-build.proj /p:TargetFramework=net40 /p:Configuration=Release /p:FSharpCoreBackVersion=3.1
	msbuild src\fsharp-library-build.proj /p:TargetFramework=portable7 /p:Configuration=Release /p:FSharpCoreBackVersion=3.1
	msbuild src\fsharp-library-build.proj /p:TargetFramework=portable47 /p:Configuration=Release /p:FSharpCoreBackVersion=3.1
	msbuild src\fsharp-library-build.proj /p:TargetFramework=portable78 /p:Configuration=Release /p:FSharpCoreBackVersion=3.1
	msbuild src\fsharp-library-build.proj /p:TargetFramework=portable259 /p:Configuration=Release /p:FSharpCoreBackVersion=3.1

### Windows, using xbuild (e.g. if only Mono is installed):

    xbuild src\fsharp-proto-build.proj
    xbuild src\fsharp-library-build.proj
    xbuild src\fsharp-compiler-build.proj

Building using xbuild does not yet lay down a Mono-ready distribution (see src/fsharp/targets.make), so should only
be used for private development rather than preparing distributions.

## Build Note: Strong Names

The FSharp.Core.dll produced is only delay-signed (Mono does not require strong names).
If a strong-name signed FSharp.Core.dll is needed then use the one in

    lib\bootstrap\signed\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll
    lib\bootstrap\signed\.NETFramework\v4.0\4.3.1.0\FSharp.Core.dll

## How to Install

Built main compiler binaries go to
    lib/release/4.0

Additionally, versions of FSharp.Core for .NET 2.0, MonoAndroid, MonoTouch (Mono profile 2.1) go to
     lib/release/2.0
     lib/release/2.1
     lib/release/2.1monotouch

`make install` sends the binaries to the `prefix` location, e.g.

    /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/2.0/FSharp.Core.dll
    /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/2.1/FSharp.Core.dll
    /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/4.0/fsc.exe
    /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/4.0/FSharp.Compiler.dll
    ...
    /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/4.5/fsc.exe
    /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/4.5/FSharp.Compiler.dll
    ...
    /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/gac/.../FSharp.Compiler.dll
    /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/gac/.../FSharp.Compiler.dll
    ...

plus some files for xbuild support

    /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/Microsoft\ F#/v4.0/*
    /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/Microsoft\ SDKs/F#/3.0/Framework/*

(these names are the canonical names for Microsoft.FSharp.Targets used by project files coming from Visual Studio)

plus scripts

    /usr/bin/fsharpc   (F# compiler)
    /usr/bin/fsharpi   (F# Interactive)

## Development Notes

### Integrating changes from 'visualfsharp'

To integrate latest changes from https://github.com/Microsoft/visualfsharp, use
```
git remote add visualfsharp https://github.com/Microsoft/visualfsharp
git pull visualfsharp master
```

### Continuous Integration Build

A continuous integration build is set up with Travis. See above.

### Editing the Compiler with Visual Studio, Xamarin Studio or MonoDevelop

Open `all-vs2013.sln`, and edit in modes Debug or Release. The compiler takes a good while to compile and that
can be a bit invasive to the work flow, so it's normally better to do the actual compilation from
the command line, see above.

Historically it is difficult to edit the compiler with Xamarin Studio or MonoDevelop because of bugs in loading the hand-edited project files and targets used in the F# compiler build. These are generally in the process of being fixed, your mileage will vary.

## How to Test and Validate

### Linux and OSX

Only a subset of the tests are currently enabled.

After building and installing, run
```
cd tests/fsharp/core
./run-all.sh
```

### Windows

See the [TESTGUIDE.md](https://github.com/Microsoft/visualfsharp/blob/master/TESTGUIDE.md) for instructions for how to test on Windows. Use that repository
to develop and test on Windows.

## History

F# compiler sources as initially dropped are available from [fsharppowerpack.codeplex.com](http://fsharppowerpack.codeplex.com).

On 4 April 2014, Microsoft Open Tech published the F# compiler sources  at http://visualfsharp.codeplex.com and began
accepting contributions to the F# compiler/library and tools.  This repository is a modified version of that.

This repository uses bootstrapping libraries, tools and F# compiler. The `lib/bootstrap/X.0` directories contain mono-built libraries, compiler and tools that can be used to bootstrap a build. You can also supply your own via the `--with-bootstrap` option.

### Wheezy build

```
vagrant up
vagrant ssh
cd /vagrant
sudo apt-get install dos2unix autoconf
./autogen.sh --prefix=/usr
make
sudo make install
```