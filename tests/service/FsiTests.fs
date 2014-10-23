
#if INTERACTIVE
#r "../../bin/v4.5/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "FsUnit.fs"
#load "Common.fs"
#else
module FSharp.Compiler.Service.Tests.FsiTests
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

/// Evaluate expression & return the result
let evalExpression text =
    match fsiSession.EvalExpression(text) with
    | Some value -> sprintf "%A" value.ReflectionValue
    | None -> sprintf "null or no result"

/// Evaluate interaction & ignore the result
let evalInteraction text = 
  fsiSession.EvalInteraction(text)

// For some reason NUnit doesn't like running these FsiEvaluationSession tests. We need to work out why.
//#if INTERACTIVE
[<Test>]
let ``EvalExpression test 1``() = 
    evalExpression "42+1" |> shouldEqual "43"

[<Test>]
// 'fsi' can be evaluated because we passed it in explicitly up above
let ``EvalExpression fsi test``() = 
    evalExpression "fsi" |> shouldEqual "Microsoft.FSharp.Compiler.Interactive.InteractiveSession"

[<Test>]
// 'fsi' can be evaluated because we passed it in explicitly up above
let ``EvalExpression fsi test 2``() = 
    evalInteraction "fsi.AddPrinter |> ignore" 


[<Test>]
let ``EvalExpression typecheck failure``() = 
    (try evalExpression "42+1.0"  |> ignore
         false
     with e -> true)
    |> shouldEqual true

[<Test>]
let ``EvalExpression function value 1``() = 
    fsiSession.EvalExpression "(fun x -> x + 1)"  |> fun s -> s.IsSome
    |> shouldEqual true

[<Test>]
let ``EvalExpression function value 2``() = 
    fsiSession.EvalExpression "fun x -> x + 1"  |> fun s -> s.IsSome
    |> shouldEqual true

[<Test>]
let ``EvalExpression function value 3``() = 
    fsiSession.EvalExpression "incr"  |> fun s -> s.IsSome
    |> shouldEqual true

[<Test; Ignore("Failing test for #135")>]
let ``EvalExpression function value 4``() = 
    fsiSession.EvalInteraction  "let hello(s : System.IO.TextReader) = printfn \"Hello World\""
    fsiSession.EvalExpression "hello"  |> fun s -> s.IsSome
    |> shouldEqual true

[<Test>]
let ``EvalExpression runtime failure``() = 
    (try evalExpression """ (failwith "fail" : int) """  |> ignore
         false
     with e -> true)
    |> shouldEqual true

[<Test>]
let ``EvalExpression parse failure``() = 
    (try evalExpression """ let let let let x = 1 """  |> ignore
         false
     with e -> true)
    |> shouldEqual true

[<Test>]
let ``EvalInteraction typecheck failure``() = 
    (try evalInteraction "let x = 42+1.0"  |> ignore
         false
     with e -> true)
    |> shouldEqual true

[<Test>]
let ``EvalInteraction runtime failure``() = 
    (try evalInteraction """let x = (failwith "fail" : int) """  |> ignore
         false
     with e -> true)
    |> shouldEqual true

[<Test>]
let ``EvalInteraction parse failure``() = 
    (try evalInteraction """ let let let let x =  """  |> ignore
         false
     with e -> true)
    |> shouldEqual false  // EvalInteraction doesn't fail for parse failures, it just reports errors.

[<Test>]
let ``PartialAssemblySignatureUpdated test``() = 
    let count = ref 0 
    fsiSession.PartialAssemblySignatureUpdated.Add(fun x -> count := count.Value + 1)
    count.Value |> shouldEqual 0
    evalInteraction """ let x = 1 """  
    count.Value |> shouldEqual 1
    evalInteraction """ let x = 1 """  
    count.Value |> shouldEqual 2


[<Test>]
let ``ParseAndCheckInteraction test 1``() = 
    evalInteraction """ let xxxxxx = 1 """  
    evalInteraction """ type CCCC() = member x.MMMMM()  = 1 + 1 """  
    let untypedResults, typedResults, _ = fsiSession.ParseAndCheckInteraction("xxxxxx")
    untypedResults.FileName |> shouldEqual "stdin.fsx"
    untypedResults.Errors.Length |> shouldEqual 0
    untypedResults.ParseHadErrors |> shouldEqual false

    // Check we can't get a declaration location for text in the F# interactive state (because the file doesn't exist)
    // TODO: check that if we use # line directives, then the file will exist correctly
    let identToken = Parser.tagOfToken(Parser.token.IDENT("")) 
    typedResults.GetDeclarationLocationAlternate(1,6,"xxxxxx",["xxxxxx"]) |> Async.RunSynchronously |> shouldEqual (FSharpFindDeclResult.DeclNotFound  FSharpFindDeclFailureReason.NoSourceCode) 

    // Check we can get a tooltip for text in the F# interactive state
    let tooltip = 
        match typedResults.GetToolTipTextAlternate(1,6,"xxxxxx",["xxxxxx"],identToken)  |> Async.RunSynchronously with 
        | FSharpToolTipText [FSharpToolTipElement.Single(text, FSharpXmlDoc.None)] -> text
        | _ -> failwith "incorrect tool tip"

    Assert.True(tooltip.Contains("val xxxxxx : int"))

[<Test>]
let ``Bad arguments to session creation 1``() = 
    let inStream = new StringReader("")
    let outStream = new CompilerOutputStream()
    let errStream = new CompilerOutputStream()
    let errWriter = new StreamWriter(errStream)
    let fsiSession = 
        try 
           FsiEvaluationSession.Create(fsiConfig, [| "fsi.exe"; "-r:nonexistent.dll" |], inStream, new StreamWriter(outStream), errWriter) |> ignore
           false
        with _ -> true
    Assert.True fsiSession
    Assert.False (String.IsNullOrEmpty (errStream.Read())) // error stream contains some output
    Assert.True (String.IsNullOrEmpty (outStream.Read())) // output stream contains no output

[<Test>]
let ``Bad arguments to session creation 2``() = 
    let inStream = new StringReader("")
    let outStream = new CompilerOutputStream()
    let errStream = new CompilerOutputStream()
    let errWriter = new StreamWriter(errStream)
    let fsiSession = 
        try 
           FsiEvaluationSession.Create(fsiConfig, [| "fsi.exe"; "-badarg" |], inStream, new StreamWriter(outStream), errWriter) |> ignore
           false
        with _ -> true
    Assert.True fsiSession
    Assert.False (String.IsNullOrEmpty (errStream.Read())) // error stream contains some output
    Assert.True (String.IsNullOrEmpty (outStream.Read())) // output stream contains no output

[<Test>]
// Regression test for #184
let ``EvalScript accepts paths verbatim``() =
    // Path contains escape sequences (\b and \n)
    // Let's ensure the exception thrown (if any) is FileNameNotResolved
    (try
        let scriptPath = @"C:\bad\path\no\donut.fsx"
        fsiSession.EvalScript scriptPath |> ignore
        true
     with
        | e ->
            // Microsoft.FSharp.Compiler.Build is internal, so we can't access the exception class here
            String.Equals(e.InnerException.GetType().FullName,
                          "Microsoft.FSharp.Compiler.Build+FileNameNotResolved",
                          StringComparison.InvariantCultureIgnoreCase))
    |> shouldEqual true


[<Test>]
let ``Disposing interactive session (collectible)``() =

    let createSession i =
        let defaultArgs = [|"fsi.exe";"--noninteractive";"--nologo";"--gui-"|]
        let sbOut = StringBuilder()
        use inStream = new StringReader("")
        use outStream = new StringWriter(sbOut)
        let sbErr = StringBuilder("")
        use errStream = new StringWriter(sbErr)

        let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
        use session = FsiEvaluationSession.Create(fsiConfig, defaultArgs, inStream, outStream, errStream, collectible=true)
        
        session.EvalInteraction <| sprintf "let x%i = 42" i

    // Dynamic assemblies should be collected and handle count should not be increased
    for i in 1 .. 50 do
        printfn "iteration %d" i
        createSession i


let RunManually() = 
  ``EvalExpression test 1``() 
  ``EvalExpression fsi test``() 
  ``EvalExpression fsi test 2``() 
  ``EvalExpression typecheck failure``() 
  ``EvalExpression function value 1``() 
  ``EvalExpression function value 2``() 
  ``EvalExpression runtime failure``() 
  ``EvalExpression parse failure``() 
  ``EvalInteraction typecheck failure``() 
  ``EvalInteraction runtime failure``() 
  ``EvalInteraction parse failure``() 
  ``PartialAssemblySignatureUpdated test``() 
  ``ParseAndCheckInteraction test 1``() 
  ``Bad arguments to session creation 1``()
  ``Bad arguments to session creation 2``()
  ``EvalScript accepts paths verbatim``()
  ``Disposing interactive session (collectible)``() 

//#endif
