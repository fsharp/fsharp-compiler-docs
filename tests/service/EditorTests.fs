module FSharp.Compiler.Service.Tests.Editor

#if INTERACTIVE
#r "../../bin/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "FsUnit.fs"
#load "Common.fs"
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

    // Get tool tip at the specified location
    let tip = typeCheckResults.GetToolTipText(3, 7, inputLines.[1], ["foo"], identToken)
    (sprintf "%A" tip).Replace("\n","") |> shouldEqual """ToolTipText  [ToolTipElement ("val foo : unit -> unitFull name: Test.foo",XmlCommentNone)]"""
    // Get declarations (autocomplete) for a location
    let decls =  typeCheckResults.GetDeclarations(Some untyped, 6, 23, inputLines.[6], [], "msg", fun _ -> false)|> Async.RunSynchronously
    [ for item in decls.Items -> item.Name ] |> shouldEqual
          ["Chars"; "Clone"; "CompareTo"; "Contains"; "CopyTo"; "EndsWith"; "Equals";
           "GetEnumerator"; "GetHashCode"; "GetType"; "GetTypeCode"; "IndexOf";
           "IndexOfAny"; "Insert"; "IsNormalized"; "LastIndexOf"; "LastIndexOfAny";
           "Length"; "Normalize"; "PadLeft"; "PadRight"; "Remove"; "Replace"; "Split";
           "StartsWith"; "Substring"; "ToCharArray"; "ToLower"; "ToLowerInvariant";
           "ToString"; "ToUpper"; "ToUpperInvariant"; "Trim"; "TrimEnd"; "TrimStart"]
    // Get overloads of the String.Concat method
    let methods = typeCheckResults.GetMethods(4, 27, inputLines.[4], Some ["String"; "Concat"])

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

    let fnVal = moduleEntity.MembersOrValues.[0]

    fnVal.Accessibility.IsPublic |> shouldEqual true
    fnVal.Attributes.Count |> shouldEqual 1
    fnVal.CurriedParameterGroups.Count |> shouldEqual 1
    fnVal.CurriedParameterGroups.[0].Count |> shouldEqual 2
    fnVal.CurriedParameterGroups.[0].[0].Name |> shouldEqual "x"
    fnVal.CurriedParameterGroups.[0].[1].Name |> shouldEqual "y"
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
    fnVal.IsGetterMethod |> shouldEqual false
    fnVal.IsImplicitConstructor |> shouldEqual false
    fnVal.IsInstanceMember |> shouldEqual false
    fnVal.IsMember |> shouldEqual false
    fnVal.IsModuleValueOrMember |> shouldEqual true
    fnVal.IsMutable |> shouldEqual false
    fnVal.IsSetterMethod |> shouldEqual false
    fnVal.IsTypeFunction |> shouldEqual false

    fnVal.FullType.IsFunctionType |> shouldEqual true // int * int -> unit
    fnVal.FullType.GenericArguments.[0].IsTupleType |> shouldEqual true // int * int 
    let argTy1 = fnVal.FullType.GenericArguments.[0].GenericArguments.[0]

    argTy1.NamedEntity.DisplayName |> shouldEqual "int" // int

    argTy1.IsNamedType |> shouldEqual true
    argTy1.NamedEntity.IsFSharpAbbreviation |> shouldEqual true // "int"

    let argTy1b = argTy1.NamedEntity.AbbreviatedType
    argTy1b.NamedEntity.Namespace |> shouldEqual (Some "Microsoft.FSharp.Core")
    argTy1b.NamedEntity.CompiledName |> shouldEqual "int32" 

    let argTy1c = argTy1b.NamedEntity.AbbreviatedType
    argTy1c.NamedEntity.Namespace |> shouldEqual (Some "System")
    argTy1c.NamedEntity.CompiledName |> shouldEqual "Int32" 

    let typeCheckContext = typeCheckResults2.ProjectContext
    
    typeCheckContext.GetReferencedAssemblies() |> List.exists (fun s -> s.FileName.Value.Contains("mscorlib")) |> shouldEqual true
    

