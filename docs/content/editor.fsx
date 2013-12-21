(*** hide ***)
#I "../../bin/"
(**
Compiler Services: Editor services
==================================

This tutorial demonstrates how to use the editor services provided by the F# compiler.
This API is used to provide auto-complete, tool-tips, parameter info help, matching of
brackets and other functions in F# editors including Visual Studio, Xamarin studio and Emacs 
(see [fsharpbindings](https://github.com/fsharp/fsharpbinding) project for more information).
Similarly to [the tutorial on using untyped AST](untypedtree.html), we start by 
getting the `InteractiveChecker` object. 

Type checking F# source code
----------------------------

As in the [previous tutorial (using untyped AST)](untypedtree.html), we start by referencing
`FSharp.Compiler.Service.dll`, opening the relevant namespace and creating an instance
of `InteractiveChecker`:

*)
// Reference F# compiler API
#r "FSharp.Compiler.Service.dll"
open System
open Microsoft.FSharp.Compiler.SourceCodeServices

// Create an interactive checker instance 
// (and do not provide file changed notifications)
let notify = NotifyFileTypeCheckStateIsDirty ignore
let checker = InteractiveChecker.Create(notify)
(**

The untyped parse operation is relatively fast, but type checking of F# code can take
longer time. The F# compiler performs the work in background (automatically) and when
we call `TypeCheckSource` method, it returns a value that may contain the result (if the
type checking has completed) or it can report that the work is still running in background.

One way of using the API is to write a simple polling using F# asynchronous workflows that
calls the function in a loop (for some limited number of times) until it successfully 
completes. 

> The [fsharpbinding](https://github.com/fsharp/fsharpbinding) project has more advanced
example of handling the background work where all requests are sent through an F# agent.
This is more appropriate for implementing editor support. Here we look at the case when
we want to trigger the operation and wait for the result (even if that takes a long time).

As [previously](untypedtree.html), we use `GetCheckOptionsFromScriptRoot` to get a context 
where the specified input is the only file passed to the compiler (and it is treated as a
script file or stand-alone F# source code):
*)

/// Perform type checking on the specified input and 
/// return asynchronous workflow that eventually returns
/// the untyped parse info & type check result.
let parseWithTypeInfo (file, input) = async {
  // Get context representing a stand-alone (script) file
  let checkOptions = 
    checker.GetCheckOptionsFromScriptRoot
      (file, input, DateTime.Now, [| |])
  // Perform untyped parsing  (reasonably quick)
  let untypedRes = 
    checker.UntypedParse(file, input, checkOptions)

  // Recursive loop waiting for type-checking result
  let rec waitForTypeCheck(n) = async {
    // Get the reuslt for specified file & input
    // Also specify that result is never invalidated
    let typedRes = 
      checker.TypeCheckSource
        ( untypedRes, file, 0, input, checkOptions, 
          IsResultObsolete(fun _ -> false), null )
    // Wait until type checking succeeds (or 100 attempts)
    match typedRes with
    | TypeCheckAnswer.TypeCheckSucceeded(res) -> 
        return untypedRes, res
    | res when n > 100 -> 
        return failwithf "Parsing did not finish... (%A)" res
    | _ -> 
        do! Async.Sleep(100)
        return! waitForTypeCheck(n + 1) }
  // Start the recursive waiting loop
  return! waitForTypeCheck 0 }

(**
To perform type checking, we first need to parse the input using 
`UntypedParse`, which gives us access to the [untyped AST](untypedtree.html). However,
then we need to call `TypeCheckSource` to perform the full type checking. This function
also requires the result of `UntypedParse`, so the two functions are often called 
together. 

Waiting for type checking result is done using a simple recursive function
`waitForTypeCheck` that calls itself recursively until the result is available,
or until it iterated 100 times (this is sufficient for reasonably sized F# files).
The function returns both the untyped parse result (which we do not use in this
tutoril), but also a `TypeCheckResults` value, which gives us access to all
the interesting functionality...

Using type checking results
---------------------------

This is where the fun starts.
*)
open Microsoft.FSharp.Compiler

let input = 
  """
  let foo() = 
    let msg = "Hello world"
    if true then 
      printfn "%s" msg.
  """
let inputLines = input.Split('\n')
let file = "/home/user/Test.fsx"

let identToken = Parser.tagOfToken(Parser.token.IDENT("")) 
let untyped, parsed = 
  parseWithTypeInfo(file, input)
  |> Async.RunSynchronously

// Get tool tip at the specified location
let tip = parsed.GetDataTipText((1, 7), inputLines.[1], ["foo"], identToken)
printfn "%A" tip

// Get declarations (autocomplete) for a location
let decls = 
  parsed.GetDeclarations(Some untyped, (4, 23), inputLines.[4], ([], "msg"), fun _ -> false)
  |> Async.RunSynchronously

for item in decls.Items do
  printfn " - %s" item.Name
(**

Fin!
*)