
#if INTERACTIVE
#r "../../bin/v4.5/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "FsUnit.fs"
#load "Common.fs"
#else
module FSharp.Compiler.Service.Tests.Editor
#endif

open NUnit.Framework
open FsUnit
open System
open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
open FSharp.Compiler.Service.Tests.Common


let input = 
  """
  open System

  let foo() = 
    let msg = String.Concat("Hello"," ","world")
    if true then 
      printfn "%s" msg.
  """

[<Test>]
let ``Intro test`` () = 

    // Split the input & define file name
    let inputLines = input.Split('\n')
    let file = "/home/user/Test.fsx"
    let untyped, typeCheckResults =  parseAndTypeCheckFileInProject(file, input) 
    let identToken = Parser.tagOfToken(Parser.token.IDENT("")) 

    // We only expect one reported error. However,
    // on Unix, using filenames like /home/user/Test.fsx gives a second copy of all parse errors due to the
    // way the load closure for scripts is generated. So this returns two identical errors
    (match typeCheckResults.Errors.Length with 1 | 2 -> true | _ -> false)  |> shouldEqual true

    // So we check that the messages are the same
    for msg in typeCheckResults.Errors do 
        printfn "Good! got an error, hopefully with the right text: %A" msg
        msg.Message.Contains("Missing qualification after '.'") |> shouldEqual true

    // Get tool tip at the specified location
    let tip = typeCheckResults.GetToolTipTextAlternate(4, 7, inputLines.[1], ["foo"], identToken) |> Async.RunSynchronously
    (sprintf "%A" tip).Replace("\n","") |> shouldEqual """ToolTipText  [ToolTipElement ("val foo : unit -> unitFull name: Test.foo",XmlCommentNone)]"""
    // Get declarations (autocomplete) for a location
    let decls =  typeCheckResults.GetDeclarationsAlternate(Some untyped, 7, 23, inputLines.[6], [], "msg", fun _ -> false)|> Async.RunSynchronously
    [ for item in decls.Items -> item.Name ] |> shouldEqual
          ["Chars"; "Clone"; "CompareTo"; "Contains"; "CopyTo"; "EndsWith"; "Equals";
           "GetEnumerator"; "GetHashCode"; "GetType"; "GetTypeCode"; "IndexOf";
           "IndexOfAny"; "Insert"; "IsNormalized"; "LastIndexOf"; "LastIndexOfAny";
           "Length"; "Normalize"; "PadLeft"; "PadRight"; "Remove"; "Replace"; "Split";
           "StartsWith"; "Substring"; "ToCharArray"; "ToLower"; "ToLowerInvariant";
           "ToString"; "ToUpper"; "ToUpperInvariant"; "Trim"; "TrimEnd"; "TrimStart"]
    // Get overloads of the String.Concat method
    let methods = typeCheckResults.GetMethodsAlternate(5, 27, inputLines.[4], Some ["String"; "Concat"]) |> Async.RunSynchronously

    methods.MethodName  |> shouldEqual "Concat"

    // Print concatenated parameter lists
    [ for mi in methods.Methods do
        yield methods.MethodName , [ for p in mi.Parameters do yield p.Display ] ]
        |> shouldEqual
              [("Concat", ["params args: obj []"]);
               ("Concat", ["params values: string []"]);
               ("Concat", ["values: Collections.Generic.IEnumerable<'T>"]);
               ("Concat", ["values: Collections.Generic.IEnumerable<string>"]);
               ("Concat", ["arg0: obj"]); ("Concat", ["arg0: obj"; "arg1: obj"]);
               ("Concat", ["str0: string"; "str1: string"]);
               ("Concat", ["arg0: obj"; "arg1: obj"; "arg2: obj"]);
               ("Concat", ["str0: string"; "str1: string"; "str2: string"]);
               ("Concat", ["arg0: obj"; "arg1: obj"; "arg2: obj"; "arg3: obj"]);
               ("Concat", ["str0: string"; "str1: string"; "str2: string"; "str3: string"])]



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

[<Test>]
let ``Symbols basic test`` () = 

    let file = "/home/user/Test.fsx"
    let untyped2, typeCheckResults2 = parseAndTypeCheckFileInProject(file, input2)

    let partialAssemblySignature = typeCheckResults2.PartialAssemblySignature
    
    partialAssemblySignature.Entities.Count |> shouldEqual 1  // one entity

[<Test>]
let ``Symbols many tests`` () = 

    let file = "/home/user/Test.fsx"
    let untyped2, typeCheckResults2 = parseAndTypeCheckFileInProject(file, input2)

    let partialAssemblySignature = typeCheckResults2.PartialAssemblySignature
    
    partialAssemblySignature.Entities.Count |> shouldEqual 1  // one entity
    let moduleEntity = partialAssemblySignature.Entities.[0]

    moduleEntity.DisplayName |> shouldEqual "Test"

    let classEntity = moduleEntity.NestedEntities.[0]

    let fnVal = moduleEntity.MembersFunctionsAndValues.[0]

    fnVal.Accessibility.IsPublic |> shouldEqual true
    fnVal.Attributes.Count |> shouldEqual 1
    fnVal.CurriedParameterGroups.Count |> shouldEqual 1
    fnVal.CurriedParameterGroups.[0].Count |> shouldEqual 2
    fnVal.CurriedParameterGroups.[0].[0].Name.IsSome |> shouldEqual true
    fnVal.CurriedParameterGroups.[0].[1].Name.IsSome |> shouldEqual true
    fnVal.CurriedParameterGroups.[0].[0].Name.Value |> shouldEqual "x"
    fnVal.CurriedParameterGroups.[0].[1].Name.Value |> shouldEqual "y"
    fnVal.DeclarationLocation.StartLine |> shouldEqual 3
    fnVal.DisplayName |> shouldEqual "foo"
    fnVal.EnclosingEntity.DisplayName |> shouldEqual "Test"
    fnVal.EnclosingEntity.DeclarationLocation.StartLine |> shouldEqual 1
    fnVal.GenericParameters.Count |> shouldEqual 0
    fnVal.InlineAnnotation |> shouldEqual FSharpInlineAnnotation.OptionalInline
    fnVal.IsActivePattern |> shouldEqual false
    fnVal.IsCompilerGenerated |> shouldEqual false
    fnVal.IsDispatchSlot |> shouldEqual false
    fnVal.IsExtensionMember |> shouldEqual false
    fnVal.IsPropertyGetterMethod |> shouldEqual false
    fnVal.IsImplicitConstructor |> shouldEqual false
    fnVal.IsInstanceMember |> shouldEqual false
    fnVal.IsMember |> shouldEqual false
    fnVal.IsModuleValueOrMember |> shouldEqual true
    fnVal.IsMutable |> shouldEqual false
    fnVal.IsPropertySetterMethod |> shouldEqual false
    fnVal.IsTypeFunction |> shouldEqual false

    fnVal.FullType.IsFunctionType |> shouldEqual true // int * int -> unit
    fnVal.FullType.GenericArguments.[0].IsTupleType |> shouldEqual true // int * int 
    let argTy1 = fnVal.FullType.GenericArguments.[0].GenericArguments.[0]

    argTy1.TypeDefinition.DisplayName |> shouldEqual "int" // int

    argTy1.HasTypeDefinition |> shouldEqual true
    argTy1.TypeDefinition.IsFSharpAbbreviation |> shouldEqual true // "int"

    let argTy1b = argTy1.TypeDefinition.AbbreviatedType
    argTy1b.TypeDefinition.Namespace |> shouldEqual (Some "Microsoft.FSharp.Core")
    argTy1b.TypeDefinition.CompiledName |> shouldEqual "int32" 

    let argTy1c = argTy1b.TypeDefinition.AbbreviatedType
    argTy1c.TypeDefinition.Namespace |> shouldEqual (Some "System")
    argTy1c.TypeDefinition.CompiledName |> shouldEqual "Int32" 

    let typeCheckContext = typeCheckResults2.ProjectContext
    
    typeCheckContext.GetReferencedAssemblies() |> List.exists (fun s -> s.FileName.Value.Contains("mscorlib")) |> shouldEqual true
    

let input3 = 
  """
let date = System.DateTime.Now.ToString().PadRight(25)
  """

[<Test>]
let ``Expression typing test`` () = 

    // Split the input & define file name
    let inputLines = input3.Split('\n')
    let file = "/home/user/Test.fsx"
    let untyped, typeCheckResults =  parseAndTypeCheckFileInProject(file, input3) 
    let identToken = Parser.tagOfToken(Parser.token.IDENT("")) 

    // We only expect one reported error. However,
    // on Unix, using filenames like /home/user/Test.fsx gives a second copy of all parse errors due to the
    // way the load closure for scripts is generated. So this returns two identical errors
    typeCheckResults.Errors.Length |> shouldEqual 0

    // Get declarations (autocomplete) for a location
    //
    // Getting the declarations at columns 42 to 43 with [], "" for the names and residue 
    // gives the results for the string type. 
    // 
    for col in 42..43 do 
        let decls =  typeCheckResults.GetDeclarationsAlternate(Some untyped, 2, col, inputLines.[1], [], "", fun _ -> false)|> Async.RunSynchronously
        set [ for item in decls.Items -> item.Name ] |> shouldEqual
           (set
              ["Chars"; "Clone"; "CompareTo"; "Contains"; "CopyTo"; "EndsWith"; "Equals";
               "GetEnumerator"; "GetHashCode"; "GetType"; "GetTypeCode"; "IndexOf";
               "IndexOfAny"; "Insert"; "IsNormalized"; "LastIndexOf"; "LastIndexOfAny";
               "Length"; "Normalize"; "PadLeft"; "PadRight"; "Remove"; "Replace"; "Split";
               "StartsWith"; "Substring"; "ToCharArray"; "ToLower"; "ToLowerInvariant";
               "ToString"; "ToUpper"; "ToUpperInvariant"; "Trim"; "TrimEnd"; "TrimStart"])

[<Test; Ignore("Currently failing, see #139")>]
let ``Find function from member 1`` () = 
    let input = 
      """
type Test() = 
    let abc a b c = a + b + c
    member x.Test = """ 

    // Split the input & define file name
    let inputLines = input.Split('\n')
    let file = "/home/user/Test.fsx"
    let untyped, typeCheckResults =  parseAndTypeCheckFileInProject(file, input) 

    let decls = typeCheckResults.GetDeclarationsAlternate(Some untyped, 4, 21, inputLines.[3], [], "", fun _ -> false)|> Async.RunSynchronously
    let item = decls.Items |> Array.tryFind (fun d -> d.Name = "abc")
    match item with
    | Some item -> 
       printf "%s" item.Name
    | _ -> ()
    decls.Items |> Seq.exists (fun d -> d.Name = "abc") |> shouldEqual true

[<Test>]
let ``Find function from member 2`` () = 
    let input = 
      """
type Test() = 
    let abc a b c = a + b + c
    member x.Test = a""" 

    // Split the input & define file name
    let inputLines = input.Split('\n')
    let file = "/home/user/Test.fsx"
    let untyped, typeCheckResults =  parseAndTypeCheckFileInProject(file, input) 

    let decls = typeCheckResults.GetDeclarationsAlternate(Some untyped, 4, 22, inputLines.[3], [], "", fun _ -> false)|> Async.RunSynchronously
    let item = decls.Items |> Array.tryFind (fun d -> d.Name = "abc")
    match item with
    | Some item -> 
       printf "%s" item.Name
    | _ -> ()
    decls.Items |> Seq.exists (fun d -> d.Name = "abc") |> shouldEqual true
 
[<Test>]
let ``Find function from var`` () = 
    let input = 
      """
type Test() = 
    let abc a b c = a + b + c
    let test = """ 

    // Split the input & define file name
    let inputLines = input.Split('\n')
    let file = "/home/user/Test.fsx"
    let untyped, typeCheckResults =  parseAndTypeCheckFileInProject(file, input) 

    let decls = typeCheckResults.GetDeclarationsAlternate(Some untyped, 4, 15, inputLines.[3], [], "", fun _ -> false)|> Async.RunSynchronously
    decls.Items |> Seq.exists (fun d -> d.Name = "abc") |> shouldEqual true

[<Test; Ignore("Currently failing, see #139")>]
let ``Symbol based find function from member 1`` () = 
    let input = 
      """
type Test() = 
    let abc a b c = a + b + c
    member x.Test = """ 

    // Split the input & define file name
    let inputLines = input.Split('\n')
    let file = "/home/user/Test.fsx"
    let untyped, typeCheckResults =  parseAndTypeCheckFileInProject(file, input) 

    let decls = typeCheckResults.GetDeclarationSymbols(Some untyped, 4, 21, inputLines.[3], [], "", fun _ -> false)|> Async.RunSynchronously
    let item = decls |> List.tryFind (fun d -> d.Head.DisplayName = "abc")
    match item with
    | Some items -> 
       for symbol in items do
           printf "%s" symbol.DisplayName
    | _ -> ()
    decls |> Seq.exists (fun d -> d.Head.DisplayName = "abc") |> shouldEqual true

[<Test>]
let ``Symbol based find function from member 2`` () = 
    let input = 
      """
type Test() = 
    let abc a b c = a + b + c
    member x.Test = a""" 

    // Split the input & define file name
    let inputLines = input.Split('\n')
    let file = "/home/user/Test.fsx"
    let untyped, typeCheckResults =  parseAndTypeCheckFileInProject(file, input) 

    let decls = typeCheckResults.GetDeclarationSymbols(Some untyped, 4, 22, inputLines.[3], [], "", fun _ -> false)|> Async.RunSynchronously
    let item = decls |> List.tryFind (fun d -> d.Head.DisplayName = "abc")
    match item with
    | Some items -> 
       for symbol in items do
           printf "%s" symbol.DisplayName
    | _ -> ()
    decls |> Seq.exists (fun d -> d.Head.DisplayName = "abc") |> shouldEqual true
    true |> should equal true

[<Test>]
let ``Symbol based find function from var`` () = 
    let input = 
      """
type Test() = 
    let abc a b c = a + b + c
    let test = """ 

    // Split the input & define file name
    let inputLines = input.Split('\n')
    let file = "/home/user/Test.fsx"
    let untyped, typeCheckResults =  parseAndTypeCheckFileInProject(file, input) 

    let decls = typeCheckResults.GetDeclarationSymbols(Some untyped, 4, 15, inputLines.[3], [], "", fun _ -> false)|> Async.RunSynchronously
    decls|> Seq .exists (fun d -> d.Head.DisplayName = "abc") |> shouldEqual true
 
