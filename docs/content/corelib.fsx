(*** hide ***)
#I "../../bin/v4.5/"
(**
Compiler Services: Notes on FSharp.Core.dll
=================================================

Shipping an FSharp.Core with your application
---------------------------------------------


When building applications or plug-in components which use ``FSharp.Compiler.Service.dll``, you will normally need
to ship a copy of ``FSharp.Core.dll`` as part of your application.  

For example, if you build a HostedCompiler.exe, you will normally place an ``FSharp.Core.dll`` (say 4.3.1.0) alongside
your HostedCompiler.exe.  

Sometimes you will also need to include an ``FSharp.Core.optdata`` and ``FSharp.Core.sigdata``, see below.

Binding redirects for your application
--------------------------------------

The ``FSharp.Compiler.Service.dll`` component depends on FSharp.Core 4.3.0.0.  Normally your application will target
a later version of FSharp.Core.  This means you will also need a binding-redirect file to make sure
that FSharp.Core 4.3.0.0 forwards to which ever final version of ``FSharp.Core.dll`` your application runs correctly.
Binding redirect files are normally generated automatically by build tooling. If not, you can use a 

``
<?xml version="1.0" encoding="utf-8" ?>
<configuration>
    <runtime>
      <assemblyBinding xmlns="urn:schemas-microsoft-com:asm.v1">
        <dependentAssembly>
          <assemblyIdentity name="FSharp.Core" publicKeyToken="b03f5f7f11d50a3a" culture="neutral"/>
          <bindingRedirect oldVersion="2.0.0.0-4.3.0.0" newVersion="4.3.1.0"/>
        </dependentAssembly>
      </assemblyBinding>
    </runtime>	
</configuration>
``

Which FSharp.Core and .NET Framework gets referenced in compilation?
--------------------------------------

The FSharp.Compiler.Service component can be used to do more or less any sort of F# compilation.
In particular you can reference an explicit FSharp.Core and/or framework
assemblies in the command line arguments (different to the FSharp.Core and a .NET Framework being used to run your tool).

To target a specific FSharp.Core and/or .NET Framework assemblies, use the ``--noframework`` argument
and the appropriate command-line arguments:

   let errors2, exitCode2 = 
        scs.Compile([| "fsc.exe"; "--noframework"; "-r"; @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.1.0\FSharp.Core.dll"; "-r"; @"C:\Windows\Microsoft.NET\Framework\v4.0.30319\mscorlib.dll"; "-o"; fn3; "-a"; fn2 |])



You will need to determine the location of these assemblies.  The easiest ways to locate these DLLs in a cross-platform way and
convert them to command-line arguments is to [crack an F# project file](http://fsharp.github.io/FSharp.Compiler.Service/project.html).  
Alternatively you can compute SDK paths yourself, and some helpers to do this are in [the tests for FSharp.Compiler.Service.dll](https://github.com/fsharp/FSharp.Compiler.Service/blob/8a943dd3b545648690cb3bed652a469bdb6dd869/tests/service/Common.fs#L54).


Do I need to include ``FSharp.Core.optdata`` and ``FSharp.Core.sigdata``?
--------------------------------------

If your compilation arguments explicitly reference an ``FSharp.Core.dll`` from an SDK location, then ``FSharp.Core.sigdata`` and ``FSharp.Core.optdata`` should be alongside the DLL
(if these files are not installed, then that's a bug in the F# SDK installation).  If your compilation
arguments are always making an explicit reference, then you should _not_ include ``FSharp.Core.optdata`` and ``FSharp.Core.sigdata`` as part of your application.


If you do _not_ explicitly reference an ``FSharp.Core.dll`` from an SDK location, then an implicit reference will be made
to whichever FSharp.Core.dll your tool is running.  This means your tool will almost certainly implicitly reference the FSharp.Core.dll
that is part of your application.  In this case, you may either get an error that ``FSharp.Core.optdata`` and ``FSharp.Core.sigdata`` are not
found alongside FSharp.Core.dll.  If you want to implicitly reference the ``FSharp.Core.dll`` you are including in your application,
then also add ``FSharp.Core.sigdata`` and ``FSharp.Core.optdata`` as two additional files to your application.  When using CompileToDynamicAssembly, this problem
can also manifest itself as [a stack overflow during assembly resolution](https://github.com/fsharp/FSharp.Compiler.Service/issues/258).

Tools that dynamically compile and execute code (e.g. a ``HostedExecution.exe``) normally make an implicit reference to FSharp.Core.dll.

Summary
-------

In this design note we've discussed three things

- which FSharp.Core.dll is used to run your compilation tool
- how  to configure binding redirects for the FSharp.Core.dll used to run your compilation tool
- which FSharp.Core.dll and/or framework assemblies are  referenced during the checking and compilations performed by your tool.

*)
