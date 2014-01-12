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

> **NOTE:** The API used below is experimental and subject to change. In particular, the 
services in FSharp.Compiler.Service.dll are overlapping and will in the future be made more regular.
This will involve breaking changes to the APIs used for these services.


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
let checker = InteractiveChecker.Create()
(**
As [previously](untypedtree.html), we use `GetCheckOptionsFromScriptRoot` to get a context 
where the specified input is the only file passed to the compiler (and it is treated as a
script file or stand-alone F# source code).
*)

/// Perform type checking on the specified input and 
/// return asynchronous workflow that eventually returns
/// the untyped parse info & type check result.
let parseAndTypeCheckFileInProject (file, input) = 
    // Get context representing a stand-alone (script) file
    let checkOptions = checker.GetProjectOptionsFromScriptRoot(file, input)

    // Perform parsing  
    let untypedRes = checker.ParseFileInProject(file, input, checkOptions)
        
    // Perform type checking
    let typedRes = checker.CheckFileInProject(untypedRes, file, 0, input, checkOptions) |> Async.RunSynchronously

    match typedRes with
    | CheckFileAnswer.Succeeded(res) -> untypedRes, res
    | res -> failwithf "Parsing did not finish... (%A)" res

(**
To perform type checking, we first need to parse the input using 
`ParseFileInProject`, which gives us access to the [untyped AST](untypedtree.html). However,
then we need to call `CheckFileInProject` to perform the full type checking. This function
also requires the result of `ParseFileInProject`, so the two functions are often called 
together. 

The function returns both the untyped parse result (which we do not use in this
tutorial), but also a `CheckFileResults` value, which gives us access to all
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
Now we can call `parseAndTypeCheckFileInProject`, synchronously wait for the result (in F# interactive)
and look at the various operations for working with the result.

Using type checking results
---------------------------

Let's now look at some of the API that is exposed by the `TypeCheckResults` type. In general,
this is the type that lets you implement most of the interesting F# source code editor services.
First, we call `parseAndTypeCheckFileInProject` and wait for the asynchronous workflow to complete:
*)
let untyped, typeCheckResults = parseAndTypeCheckFileInProject(file, input)
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
let tip = typeCheckResults.GetToolTipText(3, 7, inputLines.[1], ["foo"], identToken)
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
    typeCheckResults.GetDeclarations
      (Some untyped, 6, 23, inputLines.[6], [], "msg", fun _ -> false)
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
// Get overloads of the String.Concat method
let methods = 
    typeCheckResults.GetMethods(4, 27, inputLines.[4], Some ["String"; "Concat"])

// Print concatenated parameter lists
for mi in methods.Methods do
    [ for p in mi.Parameters -> p.Display ]
    |> String.concat ", " 
    |> printfn "%s(%s)" methods.MethodName
(**
The code uses the `Display` property to get the annotation for each parameter. This returns information
such as `arg0: obj` or `params args: obj[]` or `str0: string, str1: string`. We concatenate the parameters
and print a type annotation with the method name.
*)
(** 

## Asynchronous and immediate operations

The astute reader will have noticed that CheckFileInProject is an asynchronous operation.
This indicates that type checking of F# code can take some time. 
The F# compiler performs the work in background (automatically) and when
we call `CheckFileInProject` method, it returns an asynchronous operation.

There is also the `CheckFileInProjectIfReady` method. This returns immediately if the
type checking operation can't be started immediately, e.g. if other files in the project
are not yet type-checked. In this case, a background worker might choose to do other
work in the meantime, or give up on type checking the file until the FileTypeCheckStateIsDirty event
is raised.

> The [fsharpbinding](https://github.com/fsharp/fsharpbinding) project has more advanced
example of handling the background work where all requests are sent through an F# agent.
This may be a more appropriate for implementing editor support. 

*)

(**

Checking a project 
------------------

*)

open System.IO

let base1 = Path.GetTempFileName()
let fileName1 = Path.ChangeExtension(base1, ".fs")
let base2 = Path.GetTempFileName()
let fileName2 = Path.ChangeExtension(base2, ".fs")
let dll = Path.ChangeExtension(base2, ".dll")
let proj = Path.ChangeExtension(base2, ".fsproj")

File.WriteAllText(fileName1, """
module M

type C() = 
   member x.P = 1

let x = 3 + 4
""")

File.WriteAllText(fileName2, """
module N

type D1() = 
   member x.P = M.x

type D2() = 
   member x.P = M.x

let y = 3 + 4

// Generate a warning
let y2 = match 1 with 1 -> 1
""")

let projectOptions = 
    let allFlags = 
        [| yield "--simpleresolution" 
           yield "--simpleresolution" 
           yield "--noframework" 
           yield "--noframework" 
           yield "--debug:full" 
           yield "--define:DEBUG" 
           yield "--optimize-" 
           yield "--out:" + dll 
           yield "--doc:test.xml" 
           yield "--warn:3" 
           yield "--fullpaths" 
           yield "--flaterrors" 
           yield "--target:library" 
           for r in [ @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\mscorlib.dll" 
                      @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\System.dll" 
                      @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\System.Core.dll" 
                      @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll"] do 
                 yield "-r:" + r |]
 
    { ProjectFileName = proj // A name that is unique to this project
      ProjectFileNames = [| fileName1; fileName2 |]
      ProjectOptions = allFlags 
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = true 
      LoadTime = System.DateTime.Now 
      UnresolvedReferences = None }

(**
Now check individual files in the project context: 
*)

let untypedParse1 = checker.ParseFileInProject(fileName1, File.ReadAllText(fileName1), projectOptions) 
let untypedParse2 = checker.ParseFileInProject(fileName2, File.ReadAllText(fileName2), projectOptions) 
let typedParse1 = checker.CheckFileInProject(untypedParse1, fileName1, 0, File.ReadAllText(fileName1), projectOptions)  |> Async.RunSynchronously
let typedParse2 = checker.CheckFileInProject(untypedParse2, fileName2, 0, File.ReadAllText(fileName2), projectOptions)  |> Async.RunSynchronously

(**
In this case, we checked the same file contents as are
already saved on disk - however you can also use this API to check new file contents from the editor.
The "prior type checking state" used for checking these updated contents will be based on the files
saved on disk - see also the [File System Tutorial](filesystem.html)

You can also check the entire project (using the files saved on disk):
*)

let results = checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously
results.Errors.Length // 1
results.Errors.[0].Message.Contains("Incomplete pattern matches on this expression") // yes it does
results.Errors.[0].StartLine // 12
results.Errors.[0].EndLine // 12
results.Errors.[0].StartColumn // 15
results.Errors.[0].EndColumn // 16

[ for x in results.AssemblySignature.Entities -> x.DisplayName ] // ["N"; "M"]
[ for x in results.AssemblySignature.Entities.[0].NestedEntities -> x.DisplayName ] // ["D1"; "D2"]
[ for x in results.AssemblySignature.Entities.[1].NestedEntities -> x.DisplayName ] // ["C"]
[ for x in results.AssemblySignature.Entities.[0].MembersOrValues -> x.DisplayName ] // ["y"; "y2"]

(*
Summary
-------

The `CheckFileResults` object contains other useful methods that were not covered in this tutorial. You
can use it to get location of a declaration for a given identifier, additional colorization information
(the F# 3.1 colorizes computation builder identifiers & query operators) and others.

Finally, if you are implementing an editor support for an editor that cannot directly call .NET API,
you can call many of the methods discussed here via a command line interface that is available in the
[FSharp.AutoComplete](https://github.com/fsharp/fsharpbinding/tree/master/FSharp.AutoComplete) project.

*)
