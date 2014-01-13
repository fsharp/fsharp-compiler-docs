module FSharp.Compiler.Service.Tests.FsiTests

#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "FsUnit.fs"
#load "Common.fs"
#endif

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Interactive.Shell
open Microsoft.FSharp.Compiler.SourceCodeServices

open NUnit.Framework
open FsUnit
open System
open System.IO

// Intialize output and input streams
let inStream = new StringReader("")
let outStream = new CompilerOutputStream()
let errStream = new CompilerOutputStream()

// Build command line arguments & start FSI session
let argv = [| "C:\\fsi.exe" |]
let allArgs = Array.append argv [|"--noninteractive"|]

let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration(fsi)
let fsiSession = FsiEvaluationSession(fsiConfig, allArgs, inStream, new StreamWriter(outStream), new StreamWriter(errStream))  

/// Evaluate expression & return the result
let evalExpression text =
    match fsiSession.EvalExpression(text) with
    | Some value -> sprintf "%A" value.ReflectionValue
    | None -> sprintf "null or no result"

/// Evaluate interaction & ignore the result
let evalInteraction text = 
  fsiSession.EvalInteraction(text)

// For some reason NUnit doesn't lik running these FsiEvaluationSession tests. We need to work out why.
#if INTERACTIVE
[<Test>]
let ``EvalExpression test 1``() = 
    evalExpression "42+1" |> shouldEqual "43"


[<Test>]
let ``EvalExpression typecheck failure``() = 
    (try evalExpression "42+1.0"  |> ignore
         false
     with e -> true)
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
    let untypedResults, typedResults = fsiSession.ParseAndCheckInteraction("xxxxxx")
    untypedResults.FileName |> shouldEqual "stdin.fsx"
    untypedResults.Errors.Length |> shouldEqual 0
    untypedResults.ParseHadErrors |> shouldEqual false

    // Check we can't get a declaration location for text in the F# interactive state (because the file doesn't exist)
    // TODO: check that if we use # line directives, then the file will exist correctly
    let identToken = Parser.tagOfToken(Parser.token.IDENT("")) 
    typedResults.GetDeclarationLocation(0,6,"xxxxxx",["xxxxxx"],identToken,false) |> shouldEqual (FindDeclResult.DeclNotFound  FindDeclFailureReason.NoSourceCode)

    // Check we can get a tooltip for text in the F# interactive state
    let tooltip = 
        match typedResults.GetToolTipText(0,6,"xxxxxx",["xxxxxx"],identToken)  with 
        | ToolTipText [ToolTipElement(text, XmlCommentNone)] -> text
        | _ -> failwith "incorrect tool tip"

    Assert.True(tooltip.Contains("val xxxxxx : int"))


#endif
