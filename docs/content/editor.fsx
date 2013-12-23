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

Waiting for type checking results
---------------------------------

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

Type checking sample source code
--------------------------------

Before we look at the interesting operations provided by `TypeCheckResults`, we 
need to run the type checker on a sample input. This operation would fail on code
that cannot be parsed. On F# code with errors, you would get some type checking
result (but it may contain incorrectly "guessed" results).

Here, we type check a simple function that (conditionally) prints "Hello world".
On the last line, we leave an additional dot in `msg.` so that we can get the
completion list on the `msg` value (we expect to see various methods on the string
type there).
*)
// Sample input as a multi-line string
let input = 
  """
  open System

  let foo() = 
    let msg = String.Concat("Hello"," ","world")
    if true then 
      printfn "%s" msg.
  """
// Split the input & define file name
let inputLines = input.Split('\n')
let file = "/home/user/Test.fsx"
(**
Now we can call `parseWithTypeInfo`, synchronously wait for the result (in F# interactive)
and look at the various operations for working with the result.

Using type checking results
---------------------------

Let's now look at some of the API that is exposed by the `TypeCheckResults` type. In general,
this is the type that lets you implement most of the interesting F# source code editor services.
First, we call `parseWithTypeInfo` and wait for the asynchronous workflow to complete:
*)
let untyped, parsed = 
  parseWithTypeInfo(file, input)
  |> Async.RunSynchronously
(**
### Getting tool tip

To get a tool tip, you can use `GetDataTipText` method. The method takes a line number and character
offset. Both of the numbers are zero-based. In the sample code, we want to get tooltip for the `foo`
function that is defined on line 3 (line 0 is blank) and the letter `f` starts at index 7 (the tooltip
would work anywhere inside the identifier).

In addition, the method takes a tag of token which is typically `IDENT`, when getting tooltip for an
identifier (the other option lets you get tooltip with full assembly location when using `#r "..."`).
*)
// Get tag of the IDENT token to be used as the last argument
open Microsoft.FSharp.Compiler
let identToken = Parser.tagOfToken(Parser.token.IDENT("")) 

// Get tool tip at the specified location
let tip = parsed.GetDataTipText((3, 7), inputLines.[1], ["foo"], identToken)
printfn "%A" tip
(**
Aside from the location and token kind, the function also requires the current contents of the line
(useful when the source code changes) and a `Names` value, which is a list of strings representing
the current long name. For example to get tooltip for the `Random` identifier in a long name
`System.Random`, you would use location somewhere in the string `Random` and you would pass 
`["System"; "Random"]` as the `Names` value.

The returned value is of type `DataTipText` which contains a discriminated union `DataTipElement`.
The union represents different kinds of tool tips that you can get from the compiler.

### Getting auto-complete lists

The next method exposed by `TypeCheckResults` lets us perform auto-complete on a given location.
This can be called on any identifier or in any scope (in which case you get a list of names visible
in the scope) or immediately after `.` to get a list of members of some object. Here, we get a 
list of members of the string value `msg`.

To do this, we call `GetDeclarations` with the location of the `.` symbol on the last line 
(ending with `printfn "%s" msg.`). The offsets are zero-based, so the location is `6, 23`.
We also need to specify a function that says that the text has not changed and the current identifer
where we need to perform the completion.
*)
// Get declarations (autocomplete) for a location
let decls = 
  parsed.GetDeclarations
    ( Some untyped, (6, 23), inputLines.[6], 
      ([], "msg"), fun _ -> false)
  |> Async.RunSynchronously

// Print the names of available items
for item in decls.Items do
  printfn " - %s" item.Name
(**
When you run the code, you should get a list containing the usual string methods such as 
`Substring`, `ToUpper`, `ToLower` etc. The fourt argument of `GetDeclarations`, here `([], "msg")`, 
specifies the context for the auto-completion. Here, we want a completion on a complete name
`msg`, but you could for example use `(["System"; "Collections"], "Generic")` to get a completion list
for a fully qualified namespace.

### Getting parameter information

The next common feature of editors is to provide information about overloads of a method. In our 
sample code, we use `String.Contact` which has a number of overloads. We can get the list using
`GetMethods` operation. As previously, this takes zero-indexed offset of the location that we are
interested in (here, right at the end of the `String.Contact` identifier) and we also need to provide
the identifier again (so that the compiler can provide up-to-date information when the source code
changes):

*)
// Get overloads of the String.Contact method
let methods = 
  parsed.GetMethods
    ((4, 27), inputLines.[4], Some ["String"; "Contat"])

// Print concatenated parameter lists
for mi in methods.Methods do
  [ for p in mi.Parameters -> p.Display ]
  |> String.concat ", " 
  |> printfn "%s: %s" methods.Name 
(**
The code uses the `Display` property to get the annotation for each parameter. This returns information
such as `arg0: obj` or `params args: obj[]` or `str0: string, str1: string`. We concatenate the parameters
and print a type annotation with the method name.

Summary
-------

The `TypeCheckResults` object contains other useful methods that were not covered in this tutorial. You
can use it to get location of a declaration for a given identifier, additional colorization information
(the F# 3.1 colorizes computation builder identifiers & query operators) and others.

Finally, if you are implementing an editor support for an editor that cannot directly call .NET API,
you can call many of the methods discussed here via a command line interface that is available in the
[FSharp.AutoComplete](https://github.com/fsharp/fsharpbinding/tree/master/FSharp.AutoComplete) project.

*)
