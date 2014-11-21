#if INTERACTIVE
#r "../../bin/v4.5/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#r "../../packages/Foq.1.6/Lib/net45/Foq.dll"
#load "FsUnit.fs"
#load "Common.fs"
#else
module FileSystemTests
#endif


open NUnit.Framework
open FsUnit
open Foq
open System
open System.IO
open System.Collections.Generic
open System.Text
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Interactive.Shell
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open FSharp.Compiler.Service.Tests.Common

module FrameworkReferenceResolver =
    let mutable cache = Map.empty

    let find name =
        match Map.tryFind name cache with
        | Some assemblyPath -> assemblyPath
        | None ->
            

let createFake () =
    Mock<IFileSystem>()
        .Create ()

