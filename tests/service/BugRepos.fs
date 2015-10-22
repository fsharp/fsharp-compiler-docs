#if INTERACTIVE
#r "../../bin/v4.5/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit/lib/nunit.framework.dll"
#load "FsUnit.fs"
#load "Common.fs"
#else
module FSharp.Compiler.Service.Tests.RugRepos
#endif


open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Interactive.Shell
open Microsoft.FSharp.Compiler.SourceCodeServices

open NUnit.Framework
open FsUnit
open System
open System.IO
open System.Text

// Intialize output and input streams
let inStream = new StringReader("")
let outStream = new CompilerOutputStream()
let errStream = new CompilerOutputStream()

// Build command line arguments & start FSI session
let argv = [| "C:\\fsi.exe" |]
let allArgs = Array.append argv [|"--noninteractive"|]

let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration(fsi)
let fsiSession = FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, new StreamWriter(outStream), new StreamWriter(errStream))  

/// Evaluate interaction & ignore the result
let evalInteraction text = 
  fsiSession.EvalInteraction(text)

[<Test>]
let ``FAKE  #985 - works``() = 
    let script = "

let configure () =
    let dlls = seq { yield \"\" } 
    let tabName = \"\" //new System.String([||]) // sprintf \"%s\" \"\"
    for _ in dlls do
        System.Console.WriteLine(tabName)"
    evalInteraction script

[<Test>]
let ``FAKE  #985 - fails``() = 
    let script = "

let configure () =
    let dlls = seq { yield \"\" } 
    let tabName = new System.String([||])
    for _ in dlls do
        System.Console.WriteLine(tabName)"
    evalInteraction script

