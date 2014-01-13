(*** hide ***)
#I "../../bin/"
(**
Compiler Services: Project analysis services
============================================

This tutorial demonstrates how to use the symbol-provision services provided by the F# compiler.

> **NOTE:** The API used below is experimental and subject to change. In particular, the 
services in FSharp.Compiler.Service.dll are overlapping and will in the future be made more regular.
This will involve breaking changes to the APIs used for these services.

We start by referencing `FSharp.Compiler.Service.dll`, opening the relevant namespace and creating an instance
of `InteractiveChecker`:

*)
// Reference F# compiler API
#r "FSharp.Compiler.Service.dll"

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

// Create an interactive checker instance 
let checker = InteractiveChecker.Create()
(**
Perform type checking on the specified input:

*)

let parseAndTypeCheckSingleFile (file, input) = 
    // Get context representing a stand-alone (script) file
    let projectOptions = checker.GetProjectOptionsFromScriptRoot(file, input)

    // Perform untyped parsing  (reasonably quick)
    let untypedRes = checker.ParseFileInProject(file, input, projectOptions)
        
    let typedRes = 
        checker.CheckFileInProject(untypedRes, file, 0, input, projectOptions) 
        |> Async.RunSynchronously

    // Wait until type checking succeeds (or 100 attempts)
    match typedRes with
    | CheckFileAnswer.Succeeded(res) -> untypedRes, res
    | res -> failwithf "Parsing did not finish... (%A)" res

let file = "/home/user/Test.fsx"

(**
## Getting resolved signature information about the file

After type checking a file, you can access the inferred signature of a project up to and including the
checking of the given file through the `PartialAssemblySignature` property of the `TypeCheckResults`.

The full signature information is available for modules, types, attributes, members, values, functions, 
union cases, record types, units of measure and other F# language constructs.

The typed expression tree is not available via this route (as yet).

*)

let input2 = 
      """
[<System.CLSCompliant(true)>]
let foo(x, y) = 
    let msg = String.Concat("Hello"," ","world")
    if true then 
        printfn "x = %d, y = %d" x y 
        printfn "%s" msg

type C() = 
    member x.P = 1
      """
let untyped2, typeCheckResults2 = 
    parseAndTypeCheckSingleFile(file, input2)

(**
Now get the partial assembly signature for the code:
*)
let partialAssemblySignature = typeCheckResults2.PartialAssemblySignature
    
partialAssemblySignature.Entities.Count = 1  // one entity
    

(**
Now get the entity that corresponds to the module containing the code:
*)
let moduleEntity = partialAssemblySignature.Entities.[0]

moduleEntity.DisplayName = "Test"

(**
Now get the entity that corresponds to the type definition in the code:
*)
let classEntity = moduleEntity.NestedEntities.[0]

(**
Now get the value that corresponds to the function defined in the code:
*)
let fnVal = moduleEntity.MembersOrValues.[0]

(**
Now look around at the properties describing the function value. All fo the following evaluate to `true`:
*)
fnVal.Attributes.Count = 1
fnVal.CurriedParameterGroups.Count // 1
fnVal.CurriedParameterGroups.[0].Count // 2
fnVal.CurriedParameterGroups.[0].[0].Name // "x"
fnVal.CurriedParameterGroups.[0].[1].Name // "y"
fnVal.DeclarationLocation.StartLine // 3
fnVal.DisplayName // "foo"
fnVal.EnclosingEntity.DisplayName // "Test"
fnVal.EnclosingEntity.DeclarationLocation.StartLine // 1
fnVal.GenericParameters.Count // 0
fnVal.InlineAnnotation // FSharpInlineAnnotation.OptionalInline
fnVal.IsActivePattern // false
fnVal.IsCompilerGenerated // false
fnVal.IsDispatchSlot // false
fnVal.IsExtensionMember // false
fnVal.IsGetterMethod // false
fnVal.IsImplicitConstructor // false
fnVal.IsInstanceMember // false
fnVal.IsMember // false
fnVal.IsModuleValueOrMember // true
fnVal.IsMutable // false
fnVal.IsSetterMethod // false
fnVal.IsTypeFunction // false

(**
Now look at the type of the function if used as a first class value. (Aside: the `CurriedParameterGroups` property contains
more information like the names of the arguments.)
*)
fnVal.FullType // int * int -> unit
fnVal.FullType.IsFunctionType // int * int -> unit
fnVal.FullType.GenericArguments.[0] // int * int 
fnVal.FullType.GenericArguments.[0].IsTupleType // int * int 
let argTy1 = fnVal.FullType.GenericArguments.[0].GenericArguments.[0]

argTy1.NamedEntity.DisplayName // int

(**
OK, so we got an object representation of the type `int * int -> unit`, and we have seen the first 'int'. We can find out more about the
type 'int' as follows, determining that it is a named type, which is an F# type abbreviation, `type int = int32`
*)

argTy1.IsNamedType
argTy1.NamedEntity.IsFSharpAbbreviation // "int"

(**
We can now look at the right-hand-side of the type abbreviation, which is the type `int32`
*)

let argTy1b = argTy1.NamedEntity.AbbreviatedType
argTy1b.NamedEntity.Namespace // Some "Microsoft.FSharp.Core" 
argTy1b.NamedEntity.CompiledName // "int32" 

(**
Again we can now look through the type abbreviation `type int32 = System.Int32` to get the 
full information about the type:
*)
let argTy1c = argTy1b.NamedEntity.AbbreviatedType
argTy1c.NamedEntity.Namespace // Some "SystemCore" 
argTy1c.NamedEntity.CompiledName // "Int32" 

(**
The type checking results for a file also contain information extracted from the project (or script) options
used in the compilation, called the `ProjectContext`:
*)
let typeCheckContext = typeCheckResults2.ProjectContext
    
for ass in typeCheckContext.GetReferencedAssemblies() do
    match ass.FileName with 
    | None -> printfn "compilation referenced an assembly without a file" 
    | Some s -> printfn "compilation references assembly '%s'" s
    

(**
**Notes:**

  - If incomplete code is present, some or all of the attirbutes may not be quite as expected.
  - If some assembly references are missing (which is actually very, very common), then 'IsUnresolved'  may
    be true on values, members and/or entites related to external assemblies.  You should be sure to make your
    code robust against IsUnresolved exceptions.

*)

(**

## Getting symbolic informaion about whole projects

To check whole projects, create a checker, then call `parseAndCheckScript`. In this case, we just check 
the project for a single script. By specifying a different "projectOptions" you can create 
a specification of a larger project.
*)
let parseAndCheckScript (file, input) = 
    let projectOptions = checker.GetProjectOptionsFromScriptRoot(file, input)
    checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously

(**
Now do it for a particular input:
*)

let tmpFile = Path.ChangeExtension(System.IO.Path.GetTempFileName() , "fs")
File.WriteAllText(tmpFile, input2)

let projectResults = parseAndCheckScript(tmpFile, input2)


(**
Now look at the results:
*)

let assemblySig = projectResults.AssemblySignature
    
assemblySig.Entities.Count = 1  // one entity
assemblySig.Entities.[0].Namespace  // one entity
assemblySig.Entities.[0].DisplayName // "Tmp28D0"
assemblySig.Entities.[0].MembersOrValues.Count // 1 
assemblySig.Entities.[0].MembersOrValues.[0].DisplayName // "foo" 
    
