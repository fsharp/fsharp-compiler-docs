
#if INTERACTIVE
#r "../../bin/v4.5/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit/lib/nunit.framework.dll"
#load "FsUnit.fs"
#load "Common.fs"
#else
module FSharp.Compiler.Service.Tests.ExprTests
#endif


open NUnit.Framework
open FsUnit
open System
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Service.Tests.Common

// Create an interactive checker instance 
let checker = FSharpChecker.Create(keepAssemblyContents=true)


[<AutoOpen>]
module Utils = 
    let rec printExpr low (e:FSharpExpr) = 
        match e with 
        | BasicPatterns.AddressOf(e1) -> "&"+printExpr 0 e1
        | BasicPatterns.AddressSet(e1,e2) -> printExpr 0 e1 + " <- " + printExpr 0 e2
        | BasicPatterns.Application(f,tyargs,args) -> quote low (printExpr 10 f + printTyargs tyargs + " " + printCurriedArgs args)
        | BasicPatterns.BaseValue(_) -> "base"
        | BasicPatterns.Call(Some obj,v,tyargs1,tyargs2,argsL) -> printObjOpt (Some obj) + v.CompiledName  + printTyargs tyargs2 + printTupledArgs argsL
        | BasicPatterns.Call(None,v,tyargs1,tyargs2,argsL) -> v.EnclosingEntity.CompiledName + printTyargs tyargs1 + "." + v.CompiledName  + printTyargs tyargs2 + " " + printTupledArgs argsL
        | BasicPatterns.Coerce(ty1,e1) -> quote low (printExpr 10 e1 + " :> " + printTy ty1)
        | BasicPatterns.DefaultValue(ty1) -> "dflt"
        | BasicPatterns.FastIntegerForLoop _ -> "for-loop"
        | BasicPatterns.ILAsm(s,tyargs,args) -> s + printTupledArgs args 
        | BasicPatterns.ILFieldGet _ -> "ILFieldGet"
        | BasicPatterns.ILFieldSet _ -> "ILFieldSet"
        | BasicPatterns.IfThenElse (a,b,c) -> "(if " + printExpr 0 a + " then " + printExpr 0 b + " else " + printExpr 0 c + ")"
        | BasicPatterns.Lambda(v,e1) -> "fun " + v.CompiledName + " -> " + printExpr 0 e1
        | BasicPatterns.Let((v,e1),b) -> "let " + (if v.IsMutable then "mutable " else "") + v.CompiledName + ": " + printTy v.FullType + " = " + printExpr 0 e1 + " in " + printExpr 0 b
        | BasicPatterns.LetRec(vse,b) -> "let rec ... in " + printExpr 0 b
        | BasicPatterns.NewArray(ty,es) -> "[| ... |]" 
        | BasicPatterns.NewDelegate(ty,es) -> "new-delegate" 
        | BasicPatterns.NewObject(v,tys,args) -> "new " + v.EnclosingEntity.CompiledName + printTupledArgs args 
        | BasicPatterns.NewRecord(v,args) -> "{ ... }" 
        | BasicPatterns.NewTuple(v,args) -> printTupledArgs args 
        | BasicPatterns.NewUnionCase(ty,uc,args) -> uc.CompiledName + printTupledArgs args 
        | BasicPatterns.Quote(e1) -> "quote" + printTupledArgs [e1]
        | BasicPatterns.FSharpFieldGet(obj, ty,f) -> printObjOpt obj + f.Name 
        | BasicPatterns.FSharpFieldSet(obj, ty,f,arg) -> printObjOpt obj + f.Name + " <- " + printExpr 0 arg
        | BasicPatterns.Sequential(e1,e2) -> "(" + printExpr 0 e1 + "; " + printExpr 0 e2 + ")"
        | BasicPatterns.ThisValue _ -> "this"
        | BasicPatterns.TryFinally(e1,e2) -> "try " + printExpr 0 e1 + " finally " + printExpr 0 e2
        | BasicPatterns.TryWith(e1,_,_,vC,eC) -> "try " + printExpr 0 e1 + " with " + vC.CompiledName + " -> " + printExpr 0 eC
        | BasicPatterns.TupleGet(ty,n,e1) -> printExpr 10 e1 + ".Item" + string n
        | BasicPatterns.DecisionTree(dtree,targets) -> "match " + printExpr 10 dtree + " targets ..."
        | BasicPatterns.DecisionTreeSuccess (tg,es) -> "$" + string tg
        | BasicPatterns.TypeLambda(gp1,e1) -> "FUN ... -> " + printExpr 0 e1 
        | BasicPatterns.TypeTest(ty,e1) -> printExpr 10 e1 + " :? " + printTy ty
        | BasicPatterns.UnionCaseSet(obj,ty,uc,f1,e1) -> printExpr 10 obj + "." + f1.Name + " <- " + printExpr 0 e1
        | BasicPatterns.UnionCaseGet(obj,ty,uc,f1) -> printExpr 10 obj + "." + f1.Name
        | BasicPatterns.UnionCaseTest(obj,ty,f1) -> printExpr 10 obj + ".Is" + f1.Name
        | BasicPatterns.UnionCaseTag(obj,ty) -> printExpr 10 obj + ".Tag" 
        | BasicPatterns.ObjectExpr(ty,basecall,overrides,iimpls) -> "{ new " + printTy ty + " with ... }"
        | BasicPatterns.TraitCall(tys,nm,argtys,tinst,args) -> "trait call " + nm + printTupledArgs args
        | BasicPatterns.Const(obj,ty) -> 
            match obj with 
            | :? string  as s -> "\"" + s + "\""
            | null -> "()"
            | _ -> string obj
        | BasicPatterns.Value(v) -> v.CompiledName
        | BasicPatterns.ValueSet(v,e1) -> quote low (v.CompiledName + " <- " + printExpr 0 e1)
        | BasicPatterns.WhileLoop(e1,e2) -> "while " + printExpr 0 e1 + " do " + printExpr 0 e2 + " done"
        | _ -> failwith (sprintf "unrecognized %+A" e)

    and quote low s = if low > 0 then "(" + s + ")" else s
    and printObjOpt e = match e with None -> "" | Some e -> printExpr 10 e + "."
    and printTupledArgs args = "(" + String.concat "," (List.map (printExpr 0) args) + ")"
    and printCurriedArgs args = String.concat " " (List.map (printExpr 10) args)
    and printParams (vs: FSharpMemberOrFunctionOrValue list) = "(" + String.concat "," (vs |> List.map (fun v -> v.CompiledName)) + ")"
    and printCurriedParams (vs: FSharpMemberOrFunctionOrValue list list) = String.concat " " (List.map printParams vs)
    and printTy ty = ty.Format(FSharpDisplayContext.Empty)
    and printTyargs tyargs = match tyargs with [] -> "" | args -> "<" + String.concat "," (List.map printTy tyargs) + ">"


    let rec printDeclaration (excludes:HashSet<_> option) (d: FSharpImplementationFileDeclaration) = 
        seq {
           match d with 
            | FSharpImplementationFileDeclaration.Entity(e,ds) ->
                yield sprintf "type %s" e.LogicalName
                yield! printDeclarations excludes ds
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v,vs,e) ->
            
               if not v.IsCompilerGenerated && 
                  not (match excludes with None -> false | Some t -> t.Contains v.CompiledName) then
                let text = 
                    printfn "%s" v.CompiledName
                 //try
                    if v.IsMember then 
                        sprintf "member %s%s = %s @ %s" v.CompiledName (printCurriedParams vs)  (printExpr 0 e) (e.Range.ToShortString())
                    else 
                        sprintf "let %s%s = %s @ %s" v.CompiledName (printCurriedParams vs) (printExpr 0 e) (e.Range.ToShortString())
                 //with e -> 
                 //    printfn "FAILURE STACK: %A" e
                 //    sprintf "!!!!!!!!!! FAILED on %s @ %s, message: %s" v.CompiledName (v.DeclarationLocation.ToString()) e.Message
                yield text
            | FSharpImplementationFileDeclaration.InitAction(e) ->
                yield sprintf "do %s" (printExpr 0 e) }
    and printDeclarations excludes ds = 
        seq { for d in ds do 
                yield! printDeclaration excludes d }

    let rec exprsOfDecl (d: FSharpImplementationFileDeclaration) = 
        seq {
           match d with 
            | FSharpImplementationFileDeclaration.Entity(e,ds) ->
                yield! exprsOfDecls ds
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v,vs,e) ->
               if not v.IsCompilerGenerated then
                  yield e, e.Range
            | FSharpImplementationFileDeclaration.InitAction(e) ->
                yield e, e.Range }
    and exprsOfDecls ds = 
        seq { for d in ds do 
                yield! exprsOfDecl d }

//---------------------------------------------------------------------------------------------------------
// This project is a smoke test for a whole range of standard and obscure expressions

module Project1 = 
    open System.IO

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base2 = Path.GetTempFileName()
    let fileName2 = Path.ChangeExtension(base2, ".fs")
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1 = """
module M

type IntAbbrev = int

let boolEx1 = true
let intEx1 = 1
let int64Ex1 = 1L
let tupleEx1 = (1, 1L)
let tupleEx2 = (1, 1L, 1u)
let tupleEx3 = (1, 1L, 1u, 1s)

let localExample = 
   let y = 1
   let z = 1
   y, z

let localGenericFunctionExample() = 
   let y = 1
   let compiledAsLocalGenericFunction x = x
   compiledAsLocalGenericFunction y, compiledAsLocalGenericFunction 1.0

let funcEx1 (x:int) =  x
let genericFuncEx1 (x:'T) =  x
let (topPair1a, topPair1b) = (1,2)
let tyfuncEx1<'T> = typeof<'T>
let testILCall1 = new obj()
let testILCall2 = System.Console.WriteLine(176)

// Test recursive values in a module
let rec recValNeverUsedAtRuntime = recFuncIgnoresFirstArg (fun _ -> recValNeverUsedAtRuntime) 1
and recFuncIgnoresFirstArg g v = v

let testFun4() = 
    // Test recursive values in expression position
    let rec recValNeverUsedAtRuntime = recFuncIgnoresFirstArg (fun _ -> recValNeverUsedAtRuntime) 1
    and recFuncIgnoresFirstArg g v = v

    recValNeverUsedAtRuntime

type ClassWithImplicitConstructor(compiledAsArg: int) = 
    inherit obj()
    let compiledAsField = 1
    let compiledAsLocal = 1
    let compiledAsLocal2 = compiledAsLocal + compiledAsLocal
    let compiledAsInstanceMethod () = compiledAsField + compiledAsField
    let compiledAsGenericInstanceMethod x = x

    static let compiledAsStaticField = 1
    static let compiledAsStaticLocal = 1
    static let compiledAsStaticLocal2 = compiledAsStaticLocal + compiledAsStaticLocal
    static let compiledAsStaticMethod () = compiledAsStaticField + compiledAsStaticField
    static let compiledAsGenericStaticMethod x = x

    member __.M1() = compiledAsField + compiledAsGenericInstanceMethod compiledAsField + compiledAsArg
    member __.M2() = compiledAsInstanceMethod()
    static member SM1() = compiledAsStaticField + compiledAsGenericStaticMethod compiledAsStaticField 
    static member SM2() = compiledAsStaticMethod()
    override __.ToString() = base.ToString() + string 999
    member this.TestCallinToString() = this.ToString()

exception Error of int * int

let err = Error(3,4)

let matchOnException err = match err with Error(a,b) -> 3  | e -> raise e

let upwardForLoop () = 
    let mutable a = 1
    for i in 0 .. 10 do a <- a + 1
    a

let upwardForLoop2 () = 
    let mutable a = 1
    for i = 0 to 10 do a <- a + 1
    a

let downwardForLoop () = 
    let mutable a = 1
    for i = 10 downto 1 do a <- a + 1
    a

let quotationTest1() =  <@ 1 + 1 @>
let quotationTest2 v =  <@ %v + 1 @>

type RecdType = { Field1: int; Field2: int }
type UnionType = Case1 of int | Case2 | Case3 of int * string 

type ClassWithEventsAndProperties() = 
    let ev = new Event<_>()
    static let sev = new Event<_>()
    member x.InstanceProperty = ev.Trigger(1); 1
    static member StaticProperty = sev.Trigger(1); 1
    member x.InstanceEvent = ev.Publish
    member x.StaticEvent = sev.Publish

let c = ClassWithEventsAndProperties()
let v = c.InstanceProperty

System.Console.WriteLine(777) // do a top-levl action

let functionWithSubmsumption(x:obj)  =  x :?> string
let functionWithCoercion(x:string)  =  (x :> obj) :?> string |> functionWithSubmsumption |> functionWithSubmsumption

type MultiArgMethods(c:int,d:int) = 
   member x.Method(a:int, b : int) = 1
   member x.CurriedMethod(a1:int, b1: int)  (a2:int, b2:int) = 1

let testFunctionThatCallsMultiArgMethods() = 
    let m = MultiArgMethods(3,4)
    (m.Method(7,8) + m.CurriedMethod (9,10) (11,12))

let functionThatUsesObjectExpression() = 
   { new obj() with  member x.ToString() = string 888 } 

let functionThatUsesObjectExpressionWithInterfaceImpl() = 
   { new obj() with  
       member x.ToString() = string 888 
     interface System.IComparable with 
       member x.CompareTo(y:obj) = 0 } 

let testFunctionThatUsesUnitsOfMeasure (x : float<_>) (y: float<_>) = x + y

let testFunctionThatUsesAddressesAndByrefs (x: byref<int>) = 
    let mutable w = 4
    let y1 = &x  // address-of
    let y2 = &w  // address-of
    let arr = [| 3;4 |]  // address-of
    let r = ref 3  // address-of
    let y3 = &arr.[0] // address-of array
    let y4 = &r.contents // address-of field
    let z = x + y1 + y2 + y3 // dereference      
    w <- 3 // assign to pointer
    x <- 4 // assign to byref
    y2 <- 4 // assign to byref
    y3 <- 5 // assign to byref
    z + x + y1 + y2 + y3 + y4 + arr.[0] + r.contents

let testFunctionThatUsesStructs1 (dt:System.DateTime) =  dt.AddDays(3.0)

let testFunctionThatUsesStructs2 () = 
   let dt1 = System.DateTime.Now
   let mutable dt2 = System.DateTime.Now
   let dt3 = dt1 - dt2
   let dt4 = dt1.AddDays(3.0)
   let dt5 = dt1.Millisecond
   let dt6 = &dt2
   let dt7 = dt6 - dt4
   dt7

let testFunctionThatUsesWhileLoop() = 
   let mutable x = 1
   while x  < 100 do
      x <- x + 1
   x

let testFunctionThatUsesTryWith() = 
   try 
     testFunctionThatUsesWhileLoop()
   with :? System.ArgumentException as e -> e.Message.Length


let testFunctionThatUsesTryFinally() = 
   try 
     testFunctionThatUsesWhileLoop()
   finally
     System.Console.WriteLine(8888)

type System.Console with
    static member WriteTwoLines() = System.Console.WriteLine(); System.Console.WriteLine()

type System.DateTime with
    member x.TwoMinute = x.Minute + x.Minute

let testFunctionThatUsesExtensionMembers() = 
   System.Console.WriteTwoLines()
   let v = System.DateTime.Now.TwoMinute
   System.Console.WriteTwoLines()

let testFunctionThatUsesOptionMembers() = 
   let x = Some(3)
   (x.IsSome, x.IsNone)

let testFunctionThatUsesOverAppliedFunction() = 
   id id 3

let testFunctionThatUsesPatternMatchingOnLists(x) = 
    match x with 
    | [] -> 1
    | [h] -> 2
    | [h;h2] -> 3
    | _ -> 4

let testFunctionThatUsesPatternMatchingOnOptions(x) = 
    match x with 
    | None -> 1
    | Some h -> 2 + h

let testFunctionThatUsesPatternMatchingOnOptions2(x) = 
    match x with 
    | None -> 1
    | Some _ -> 2

let testFunctionThatUsesConditionalOnOptions2(x: int option) = 
    if x.IsSome then 1 else 2
    """
    File.WriteAllText(fileName1, fileSource1)

    let fileSource2 = """
module N

type IntAbbrev = int


let bool2 = false

    """
    File.WriteAllText(fileName2, fileSource2)

    let fileNames = [fileName1; fileName2]
    let args = mkProjectCommandLineArgs (dllName, fileNames)
    let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)

//<@ let x = Some(3) in x.IsSome @>

[<Test>]
let ``Test Declarations project1`` () =
    let wholeProjectResults = checker.ParseAndCheckProject(Project1.options) |> Async.RunSynchronously
    
    wholeProjectResults.Errors.Length |> shouldEqual 2 // recursive value warning
    wholeProjectResults.Errors.[0].Severity |> shouldEqual FSharpErrorSeverity.Warning
    wholeProjectResults.Errors.[1].Severity |> shouldEqual FSharpErrorSeverity.Warning

    wholeProjectResults.AssemblyContents.ImplementationFiles.Length |> shouldEqual 2
    let file1 = wholeProjectResults.AssemblyContents.ImplementationFiles.[0]
    let file2 = wholeProjectResults.AssemblyContents.ImplementationFiles.[1]

    printDeclarations None (List.ofSeq file1.Declarations) 
      |> Seq.toList 
      |> shouldEqual
              ["type M"; "type IntAbbrev"; "let boolEx1 = True @ (6,14--6,18)";
               "let intEx1 = 1 @ (7,13--7,14)"; "let int64Ex1 = 1 @ (8,15--8,17)";
               "let tupleEx1 = (1,1) @ (9,16--9,21)";
               "let tupleEx2 = (1,1,1) @ (10,16--10,25)";
               "let tupleEx3 = (1,1,1,1) @ (11,16--11,29)";
               "let localExample = let y: Microsoft.FSharp.Core.int = 1 in let z: Microsoft.FSharp.Core.int = 1 in (y,z) @ (14,7--14,8)";
               "let localGenericFunctionExample(unitVar0) = let y: Microsoft.FSharp.Core.int = 1 in let compiledAsLocalGenericFunction: 'a -> 'a = FUN ... -> fun x -> x in (compiledAsLocalGenericFunction<Microsoft.FSharp.Core.int> y,compiledAsLocalGenericFunction<Microsoft.FSharp.Core.float> 1) @ (19,7--19,8)";
               "let funcEx1(x) = x @ (23,23--23,24)";
               "let genericFuncEx1(x) = x @ (24,29--24,30)";
               "let topPair1b = patternInput@25.Item1 @ (25,4--25,26)";
               "let topPair1a = patternInput@25.Item0 @ (25,4--25,26)";
               "let tyfuncEx1 = Operators.TypeOf<'T> () @ (26,20--26,26)";
               "let testILCall1 = new Object() @ (27,18--27,27)";
               "let testILCall2 = Console.WriteLine (176) @ (28,18--28,47)";
               "let recFuncIgnoresFirstArg(g) (v) = v @ (32,33--32,34)";
               "let recValNeverUsedAtRuntime = recValNeverUsedAtRuntime@31.Force<Microsoft.FSharp.Core.int>(()) @ (31,8--31,32)";
               "let testFun4(unitVar0) = let rec ... in recValNeverUsedAtRuntime @ (36,4--39,28)";
               "type ClassWithImplicitConstructor";
               "member .ctor(compiledAsArg) = (Object..ctor (); (this.compiledAsArg <- compiledAsArg; (this.compiledAsField <- 1; let compiledAsLocal: Microsoft.FSharp.Core.int = 1 in let compiledAsLocal2: Microsoft.FSharp.Core.int = Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (compiledAsLocal,compiledAsLocal) in ()))) @ (41,5--41,33)";
               "member .cctor(unitVar) = (compiledAsStaticField <- 1; let compiledAsStaticLocal: Microsoft.FSharp.Core.int = 1 in let compiledAsStaticLocal2: Microsoft.FSharp.Core.int = Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (compiledAsStaticLocal,compiledAsStaticLocal) in ()) @ (49,11--49,40)";
               "member M1(__) (unitVar1) = Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (__.compiledAsField,let x: Microsoft.FSharp.Core.int = __.compiledAsField in __.compiledAsGenericInstanceMethod<Microsoft.FSharp.Core.int>(x)),__.compiledAsArg) @ (55,21--55,102)";
               "member M2(__) (unitVar1) = __.compiledAsInstanceMethod(()) @ (56,21--56,47)";
               "member SM1(unitVar0) = Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (compiledAsStaticField,let x: Microsoft.FSharp.Core.int = compiledAsStaticField in ClassWithImplicitConstructor.compiledAsGenericStaticMethod<Microsoft.FSharp.Core.int> (x)) @ (57,26--57,101)";
               "member SM2(unitVar0) = ClassWithImplicitConstructor.compiledAsStaticMethod (()) @ (58,26--58,50)";
               "member ToString(__) (unitVar1) = Operators.op_Addition<Microsoft.FSharp.Core.string,Microsoft.FSharp.Core.string,Microsoft.FSharp.Core.string> (base.ToString(),Operators.ToString<Microsoft.FSharp.Core.int> (999)) @ (59,29--59,57)";
               "member TestCallinToString(this) (unitVar1) = this.ToString() @ (60,39--60,54)";
               "type Error"; "let err = { ... } @ (64,10--64,20)";
               "let matchOnException(err) = match (if err :? M.Error then $0 else $1) targets ... @ (66,33--66,36)";
               "let upwardForLoop(unitVar0) = let mutable a: Microsoft.FSharp.Core.int = 1 in (for-loop; a) @ (69,16--69,17)";
               "let upwardForLoop2(unitVar0) = let mutable a: Microsoft.FSharp.Core.int = 1 in (for-loop; a) @ (74,16--74,17)";
               "let downwardForLoop(unitVar0) = let mutable a: Microsoft.FSharp.Core.int = 1 in (for-loop; a) @ (79,16--79,17)";
               "let quotationTest1(unitVar0) = quote(Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (1,1)) @ (83,24--83,35)";
               "let quotationTest2(v) = quote(Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (ExtraTopLevelOperators.SpliceExpression<Microsoft.FSharp.Core.int> (v),1)) @ (84,24--84,36)";
               "type RecdType"; "type UnionType"; "type ClassWithEventsAndProperties";
               "member .ctor(unitVar0) = (Object..ctor (); (this.ev <- new FSharpEvent`1(()); ())) @ (89,5--89,33)";
               "member .cctor(unitVar) = (sev <- new FSharpEvent`1(()); ()) @ (91,11--91,35)";
               "member get_InstanceProperty(x) (unitVar1) = (x.ev.Trigger(1); 1) @ (92,32--92,48)";
               "member get_StaticProperty(unitVar0) = (sev.Trigger(1); 1) @ (93,35--93,52)";
               "member get_InstanceEvent(x) (unitVar1) = x.ev.get_Publish(()) @ (94,29--94,39)";
               "member get_StaticEvent(x) (unitVar1) = sev.get_Publish(()) @ (95,27--95,38)";
               "let c = new ClassWithEventsAndProperties(()) @ (97,8--97,38)";
               "let v = c.get_InstanceProperty(()) @ (98,8--98,26)";
               "do Console.WriteLine (777)";
               "let functionWithSubmsumption(x) = IntrinsicFunctions.UnboxGeneric<Microsoft.FSharp.Core.string> (x) @ (102,40--102,52)";
               "let functionWithCoercion(x) = Operators.op_PipeRight<Microsoft.FSharp.Core.string,Microsoft.FSharp.Core.string> (Operators.op_PipeRight<Microsoft.FSharp.Core.string,Microsoft.FSharp.Core.string> (IntrinsicFunctions.UnboxGeneric<Microsoft.FSharp.Core.string> (x :> Microsoft.FSharp.Core.obj),fun x -> M.functionWithSubmsumption (x :> Microsoft.FSharp.Core.obj)),fun x -> M.functionWithSubmsumption (x :> Microsoft.FSharp.Core.obj)) @ (103,39--103,116)";
               "type MultiArgMethods";
               "member .ctor(c,d) = (Object..ctor (); ()) @ (105,5--105,20)";
               "member Method(x) (a,b) = 1 @ (106,37--106,38)";
               "member CurriedMethod(x) (a1,b1) (a2,b2) = 1 @ (107,63--107,64)";
               "let testFunctionThatCallsMultiArgMethods(unitVar0) = let m: M.MultiArgMethods = new MultiArgMethods(3,4) in Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (m.Method(7,8),fun tupledArg -> let arg00: Microsoft.FSharp.Core.int = tupledArg.Item0 in let arg01: Microsoft.FSharp.Core.int = tupledArg.Item1 in fun tupledArg -> let arg10: Microsoft.FSharp.Core.int = tupledArg.Item0 in let arg11: Microsoft.FSharp.Core.int = tupledArg.Item1 in m.CurriedMethod(arg00,arg01,arg10,arg11) (9,10) (11,12)) @ (110,8--110,9)";
               "let functionThatUsesObjectExpression(unitVar0) = { new Microsoft.FSharp.Core.obj with ... } @ (114,3--114,55)";
               "let functionThatUsesObjectExpressionWithInterfaceImpl(unitVar0) = { new Microsoft.FSharp.Core.obj with ... } :> System.IComparable @ (117,3--120,38)";
               "let testFunctionThatUsesUnitsOfMeasure(x) (y) = Operators.op_Addition<Microsoft.FSharp.Core.float<'u>,Microsoft.FSharp.Core.float<'u>,Microsoft.FSharp.Core.float<'u>> (x,y) @ (122,70--122,75)";
               "let testFunctionThatUsesAddressesAndByrefs(x) = let mutable w: Microsoft.FSharp.Core.int = 4 in let y1: Microsoft.FSharp.Core.byref<Microsoft.FSharp.Core.int> = x in let y2: Microsoft.FSharp.Core.byref<Microsoft.FSharp.Core.int> = &w in let arr: Microsoft.FSharp.Core.int Microsoft.FSharp.Core.[] = [| ... |] in let r: Microsoft.FSharp.Core.int Microsoft.FSharp.Core.ref = Operators.Ref<Microsoft.FSharp.Core.int> (3) in let y3: Microsoft.FSharp.Core.byref<Microsoft.FSharp.Core.int> = [I_ldelema (NormalAddress,false,ILArrayShape [(Some 0, null)],TypeVar 0us)](arr,0) in let y4: Microsoft.FSharp.Core.byref<Microsoft.FSharp.Core.int> = &r.contents in let z: Microsoft.FSharp.Core.int = Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (x,y1),y2),y3) in (w <- 3; (x <- 4; (y2 <- 4; (y3 <- 5; Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (z,x),y1),y2),y3),y4),IntrinsicFunctions.GetArray<Microsoft.FSharp.Core.int> (arr,0)),r.contents))))) @ (125,16--125,17)";
               "let testFunctionThatUsesStructs1(dt) = dt.AddDays(3) @ (139,57--139,72)";
               "let testFunctionThatUsesStructs2(unitVar0) = let dt1: System.DateTime = DateTime.get_Now () in let mutable dt2: System.DateTime = DateTime.get_Now () in let dt3: System.TimeSpan = Operators.op_Subtraction<System.DateTime,System.DateTime,System.TimeSpan> (dt1,dt2) in let dt4: System.DateTime = dt1.AddDays(3) in let dt5: Microsoft.FSharp.Core.int = dt1.get_Millisecond() in let dt6: Microsoft.FSharp.Core.byref<System.DateTime> = &dt2 in let dt7: System.TimeSpan = Operators.op_Subtraction<System.DateTime,System.DateTime,System.TimeSpan> (dt6,dt4) in dt7 @ (142,7--142,10)";
               "let testFunctionThatUsesWhileLoop(unitVar0) = let mutable x: Microsoft.FSharp.Core.int = 1 in (while Operators.op_LessThan<Microsoft.FSharp.Core.int> (x,100) do x <- Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (x,1) done; x) @ (152,15--152,16)";
               "let testFunctionThatUsesTryWith(unitVar0) = try M.testFunctionThatUsesWhileLoop (()) with matchValue -> match (if matchValue :? System.ArgumentException then $0 else $1) targets ... @ (158,3--160,60)";
               "let testFunctionThatUsesTryFinally(unitVar0) = try M.testFunctionThatUsesWhileLoop (()) finally Console.WriteLine (8888) @ (164,3--167,35)";
               "member Console.WriteTwoLines.Static(unitVar0) = (Console.WriteLine (); Console.WriteLine ()) @ (170,36--170,90)";
               "member DateTime.get_TwoMinute(x) (unitVar1) = Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (x.get_Minute(),x.get_Minute()) @ (173,25--173,44)";
               "let testFunctionThatUsesExtensionMembers(unitVar0) = (M.Console.WriteTwoLines.Static (()); let v: Microsoft.FSharp.Core.int = DateTime.get_Now ().DateTime.get_TwoMinute(()) in M.Console.WriteTwoLines.Static (())) @ (176,3--178,33)";
               "let testFunctionThatUsesOptionMembers(unitVar0) = let x: Microsoft.FSharp.Core.int Microsoft.FSharp.Core.option = Some(3) in (x.get_IsSome() (),x.get_IsNone() ()) @ (181,7--181,8)";
               "let testFunctionThatUsesOverAppliedFunction(unitVar0) = Operators.Identity<Microsoft.FSharp.Core.int -> Microsoft.FSharp.Core.int> (fun x -> Operators.Identity<Microsoft.FSharp.Core.int> (x)) 3 @ (185,3--185,10)";
               "let testFunctionThatUsesPatternMatchingOnLists(x) = match (if x.Isop_ColonColon then (if x.Tail.Isop_ColonColon then (if x.Tail.Tail.Isop_Nil then $2 else $3) else $1) else $0) targets ... @ (188,10--188,11)";
               "let testFunctionThatUsesPatternMatchingOnOptions(x) = match (if x.IsSome then $1 else $0) targets ... @ (195,10--195,11)";
               "let testFunctionThatUsesPatternMatchingOnOptions2(x) = match (if x.IsSome then $1 else $0) targets ... @ (200,10--200,11)";
               "let testFunctionThatUsesConditionalOnOptions2(x) = (if x.get_IsSome() () then 1 else 2) @ (205,4--205,29)"]
    ()

//---------------------------------------------------------------------------------------------------------
// This big list expression was causing us trouble

module ProjectStressBigExpressions = 
    open System.IO

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1 = """
module StressBigExpressions 


let BigListExpression = 

   [("C", "M.C", "file1", ((3, 5), (3, 6)), ["class"]);
    ("( .ctor )", "M.C.( .ctor )", "file1", ((3, 5), (3, 6)),["member"; "ctor"]);
    ("P", "M.C.P", "file1", ((4, 13), (4, 14)), ["member"; "getter"]);
    ("x", "x", "file1", ((4, 11), (4, 12)), []);
    ("( + )", "Microsoft.FSharp.Core.Operators.( + )", "file1",((6, 12), (6, 13)), ["val"]);
    ("xxx", "M.xxx", "file1", ((6, 4), (6, 7)), ["val"]);
    ("( + )", "Microsoft.FSharp.Core.Operators.( + )", "file1",((7, 17), (7, 18)), ["val"]);
    ("xxx", "M.xxx", "file1", ((7, 13), (7, 16)), ["val"]);
    ("xxx", "M.xxx", "file1", ((7, 19), (7, 22)), ["val"]);
    ("fff", "M.fff", "file1", ((7, 4), (7, 7)), ["val"]);
    ("C", "M.C", "file1", ((9, 15), (9, 16)), ["class"]);
    ("C", "M.C", "file1", ((9, 15), (9, 16)), ["class"]);
    ("C", "M.C", "file1", ((9, 15), (9, 16)), ["class"]);
    ("C", "M.C", "file1", ((9, 15), (9, 16)), ["class"]);
    ("CAbbrev", "M.CAbbrev", "file1", ((9, 5), (9, 12)), ["abbrev"]);
    ("M", "M", "file1", ((1, 7), (1, 8)), ["module"]);
    ("D1", "N.D1", "file2", ((5, 5), (5, 7)), ["class"]);
    ("( .ctor )", "N.D1.( .ctor )", "file2", ((5, 5), (5, 7)),["member"; "ctor"]);
    ("SomeProperty", "N.D1.SomeProperty", "file2", ((6, 13), (6, 25)),["member"; "getter"]); 
    ("x", "x", "file2", ((6, 11), (6, 12)), []);
    ("M", "M", "file2", ((6, 28), (6, 29)), ["module"]);
    ("xxx", "M.xxx", "file2", ((6, 28), (6, 33)), ["val"]);
    ("D2", "N.D2", "file2", ((8, 5), (8, 7)), ["class"]);
    ("( .ctor )", "N.D2.( .ctor )", "file2", ((8, 5), (8, 7)),["member"; "ctor"]);
    ("SomeProperty", "N.D2.SomeProperty", "file2", ((9, 13), (9, 25)),["member"; "getter"]); ("x", "x", "file2", ((9, 11), (9, 12)), []);
    ("( + )", "Microsoft.FSharp.Core.Operators.( + )", "file2",((9, 36), (9, 37)), ["val"]);
    ("M", "M", "file2", ((9, 28), (9, 29)), ["module"]);
    ("fff", "M.fff", "file2", ((9, 28), (9, 33)), ["val"]);
    ("D1", "N.D1", "file2", ((9, 38), (9, 40)), ["member"; "ctor"]);
    ("M", "M", "file2", ((12, 27), (12, 28)), ["module"]);
    ("xxx", "M.xxx", "file2", ((12, 27), (12, 32)), ["val"]);
    ("y2", "N.y2", "file2", ((12, 4), (12, 6)), ["val"]);
    ("DefaultValueAttribute", "Microsoft.FSharp.Core.DefaultValueAttribute","file2", ((18, 6), (18, 18)), ["class"]);
    ("DefaultValueAttribute", "Microsoft.FSharp.Core.DefaultValueAttribute","file2", ((18, 6), (18, 18)), ["class"]);
    ("DefaultValueAttribute", "Microsoft.FSharp.Core.DefaultValueAttribute","file2", ((18, 6), (18, 18)), ["member"]);
    ("int", "Microsoft.FSharp.Core.int", "file2", ((19, 20), (19, 23)),["abbrev"]);
    ("DefaultValueAttribute", "Microsoft.FSharp.Core.DefaultValueAttribute","file2", ((18, 6), (18, 18)), ["class"]);
    ("DefaultValueAttribute", "Microsoft.FSharp.Core.DefaultValueAttribute","file2", ((18, 6), (18, 18)), ["class"]);
    ("DefaultValueAttribute", "Microsoft.FSharp.Core.DefaultValueAttribute","file2", ((18, 6), (18, 18)), ["member"]);
    ("x", "N.D3.x", "file2", ((19, 16), (19, 17)),["field"; "default"; "mutable"]);
    ("D3", "N.D3", "file2", ((15, 5), (15, 7)), ["class"]);
    ("int", "Microsoft.FSharp.Core.int", "file2", ((15, 10), (15, 13)),["abbrev"]); ("a", "a", "file2", ((15, 8), (15, 9)), []);
    ("( .ctor )", "N.D3.( .ctor )", "file2", ((15, 5), (15, 7)),["member"; "ctor"]);
    ("SomeProperty", "N.D3.SomeProperty", "file2", ((21, 13), (21, 25)),["member"; "getter"]);
    ("( + )", "Microsoft.FSharp.Core.Operators.( + )", "file2",((16, 14), (16, 15)), ["val"]);
    ("a", "a", "file2", ((16, 12), (16, 13)), []);
    ("b", "b", "file2", ((16, 8), (16, 9)), []);
    ("x", "x", "file2", ((21, 11), (21, 12)), []);
    ("( + )", "Microsoft.FSharp.Core.Operators.( + )", "file2",((21, 30), (21, 31)), ["val"]);
    ("a", "a", "file2", ((21, 28), (21, 29)), []);
    ("b", "b", "file2", ((21, 32), (21, 33)), []);
    ("( + )", "Microsoft.FSharp.Core.Operators.( + )", "file2",((23, 25), (23, 26)), ["val"]);
    ("( + )", "Microsoft.FSharp.Core.Operators.( + )", "file2",((23, 21), (23, 22)), ["val"]);
    ("int32", "Microsoft.FSharp.Core.Operators.int32", "file2",((23, 27), (23, 32)), ["val"]);
    ("DateTime", "System.DateTime", "file2", ((23, 40), (23, 48)),["valuetype"]);
    ("System", "System", "file2", ((23, 33), (23, 39)), ["namespace"]);
    ("Now", "System.DateTime.Now", "file2", ((23, 33), (23, 52)),["member"; "prop"]);
    ("Ticks", "System.DateTime.Ticks", "file2", ((23, 33), (23, 58)),["member"; "prop"]);
    ("( + )", "Microsoft.FSharp.Core.Operators.( + )", "file2",((23, 62), (23, 63)), ["val"]);
    ("pair2", "N.pair2", "file2", ((23, 10), (23, 15)), ["val"]);
    ("pair1", "N.pair1", "file2", ((23, 4), (23, 9)), ["val"]);
    ("None", "N.SaveOptions.None", "file2", ((27, 4), (27, 8)),["field"; "static"; "0"]);
    ("DisableFormatting", "N.SaveOptions.DisableFormatting", "file2",((28, 4), (28, 21)), ["field"; "static"; "1"]);
    ("SaveOptions", "N.SaveOptions", "file2", ((26, 5), (26, 16)),["enum"; "valuetype"]);
    ("SaveOptions", "N.SaveOptions", "file2", ((30, 16), (30, 27)),["enum"; "valuetype"]);
    ("DisableFormatting", "N.SaveOptions.DisableFormatting", "file2",((30, 16), (30, 45)), ["field"; "static"; "1"]);
    ("enumValue", "N.enumValue", "file2", ((30, 4), (30, 13)), ["val"]);
    ("x", "x", "file2", ((32, 9), (32, 10)), []);
    ("y", "y", "file2", ((32, 11), (32, 12)), []);
    ("( + )", "Microsoft.FSharp.Core.Operators.( + )", "file2",((32, 17), (32, 18)), ["val"]);
    ("x", "x", "file2", ((32, 15), (32, 16)), []);
    ("y", "y", "file2", ((32, 19), (32, 20)), []);
    ("( ++ )", "N.( ++ )", "file2", ((32, 5), (32, 7)), ["val"]);
    ("( ++ )", "N.( ++ )", "file2", ((34, 11), (34, 13)), ["val"]);
    ("c1", "N.c1", "file2", ((34, 4), (34, 6)), ["val"]);
    ("( ++ )", "N.( ++ )", "file2", ((36, 11), (36, 13)), ["val"]);
    ("c2", "N.c2", "file2", ((36, 4), (36, 6)), ["val"]);
    ("M", "M", "file2", ((38, 12), (38, 13)), ["module"]);
    ("C", "M.C", "file2", ((38, 12), (38, 15)), ["class"]);
    ("M", "M", "file2", ((38, 22), (38, 23)), ["module"]);
    ("C", "M.C", "file2", ((38, 22), (38, 25)), ["class"]);
    ("C", "M.C", "file2", ((38, 22), (38, 25)), ["member"; "ctor"]);
    ("mmmm1", "N.mmmm1", "file2", ((38, 4), (38, 9)), ["val"]);
    ("M", "M", "file2", ((39, 12), (39, 13)), ["module"]);
    ("CAbbrev", "M.CAbbrev", "file2", ((39, 12), (39, 21)), ["abbrev"]);
    ("M", "M", "file2", ((39, 28), (39, 29)), ["module"]);
    ("CAbbrev", "M.CAbbrev", "file2", ((39, 28), (39, 37)), ["abbrev"]);
    ("C", "M.C", "file2", ((39, 28), (39, 37)), ["member"; "ctor"]);
    ("mmmm2", "N.mmmm2", "file2", ((39, 4), (39, 9)), ["val"]);
    ("N", "N", "file2", ((1, 7), (1, 8)), ["module"])]

let BigSequenceExpression(outFileOpt,docFileOpt,baseAddressOpt) =    
        [   yield "--simpleresolution"
            yield "--noframework"
            match outFileOpt with
            | None -> ()
            | Some outFile -> yield "--out:" + outFile
            match docFileOpt with
            | None -> ()
            | Some docFile -> yield "--doc:" + docFile
            match baseAddressOpt with
            | None -> ()
            | Some baseAddress -> yield "--baseaddress:" + baseAddress
            match baseAddressOpt with
            | None -> ()
            | Some keyFile -> yield "--keyfile:" + keyFile
            match baseAddressOpt with
            | None -> ()
            | Some sigFile -> yield "--sig:" + sigFile
            match baseAddressOpt with
            | None -> ()
            | Some pdbFile -> yield "--pdb:" + pdbFile
            match baseAddressOpt with
            | None -> ()
            | Some versionFile -> yield "--versionfile:" + versionFile
            match baseAddressOpt with
            | None -> ()
            | Some warnLevel -> yield "--warn:" + warnLevel
            match baseAddressOpt with
            | None -> ()
            | Some s -> yield "--subsystemversion:" + s
            if true then yield "--highentropyva+"
            match baseAddressOpt with
            | None -> ()
            | Some win32Res -> yield "--win32res:" + win32Res
            match baseAddressOpt with
            | None -> ()
            | Some win32Manifest -> yield "--win32manifest:" + win32Manifest
            match baseAddressOpt with
            | None -> ()
            | Some targetProfile -> yield "--targetprofile:" + targetProfile
            yield "--fullpaths"
            yield "--flaterrors"
            if true then yield "--warnaserror"
            yield 
                if true then "--target:library"
                else "--target:exe"
            for symbol in [] do
                if not (System.String.IsNullOrWhiteSpace symbol) then yield "--define:" + symbol
            for nw in [] do
                if not (System.String.IsNullOrWhiteSpace nw) then yield "--nowarn:" + nw
            for nw in [] do
                if not (System.String.IsNullOrWhiteSpace nw) then yield "--warnaserror:" + nw
            yield if true then "--debug+"
                    else "--debug-"
            yield if true then "--optimize+"
                    else "--optimize-"
            yield if true then "--tailcalls+"
                    else "--tailcalls-"
            match baseAddressOpt with
            | None -> ()
            | Some debugType -> 
                match "" with
                | "NONE" -> ()
                | "PDBONLY" -> yield "--debug:pdbonly"
                | "FULL" -> yield "--debug:full"
                | _ -> ()
            match baseAddressOpt |> Option.map (fun o -> ""), true, baseAddressOpt |> Option.map (fun o -> "") with
            | Some "ANYCPU", true, Some "EXE" | Some "ANYCPU", true, Some "WINEXE" -> yield "--platform:anycpu32bitpreferred"
            | Some "ANYCPU", _, _ -> yield "--platform:anycpu"
            | Some "X86", _, _ -> yield "--platform:x86"
            | Some "X64", _, _ -> yield "--platform:x64"
            | Some "ITANIUM", _, _ -> yield "--platform:Itanium"
            | _ -> ()
            match baseAddressOpt |> Option.map (fun o -> "") with
            | Some "LIBRARY" -> yield "--target:library"
            | Some "EXE" -> yield "--target:exe"
            | Some "WINEXE" -> yield "--target:winexe"
            | Some "MODULE" -> yield "--target:module"
            | _ -> ()
            yield! []
            for f in [] do
                yield "--resource:" + f
            for i in [] do
                yield "--lib:" 
            for r in []  do
                yield "-r:" + r 
            yield! [] ]

    
    """
    File.WriteAllText(fileName1, fileSource1)

    let fileNames = [fileName1]
    let args = mkProjectCommandLineArgs (dllName, fileNames)
    let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)



[<Test>]
let ``Test expressions of declarations stress big expressions`` () =
    let wholeProjectResults = checker.ParseAndCheckProject(ProjectStressBigExpressions.options) |> Async.RunSynchronously
    
    wholeProjectResults.Errors.Length |> shouldEqual 0

    wholeProjectResults.AssemblyContents.ImplementationFiles.Length |> shouldEqual 1
    let file1 = wholeProjectResults.AssemblyContents.ImplementationFiles.[0]

    // This should not stack overflow
    printDeclarations None (List.ofSeq file1.Declarations) |> Seq.toList |> ignore


#if SELF_HOST_STRESS

[<Test>]
let ``Test Declarations selfhost`` () =
    let projectFile = __SOURCE_DIRECTORY__ + @"/FSharp.Compiler.Service.Tests.fsproj"
    // Check with Configuration = Release
    let options = checker.GetProjectOptionsFromProjectFile(projectFile, [("Configuration", "Debug")])
    let wholeProjectResults = checker.ParseAndCheckProject(options) |> Async.RunSynchronously
    
    wholeProjectResults.Errors.Length |> shouldEqual 0 

    wholeProjectResults.AssemblyContents.ImplementationFiles.Length |> shouldEqual 13

    let textOfAll = [ for file in wholeProjectResults.AssemblyContents.ImplementationFiles -> Array.ofSeq (printDeclarations None (List.ofSeq file.Declarations))   ]

    ()


[<Test>]
let ``Test Declarations selfhost whole compiler`` () =
    
    Environment.CurrentDirectory <-  __SOURCE_DIRECTORY__ +  @"/../../src/fsharp/FSharp.Compiler.Service"
    let projectFile = __SOURCE_DIRECTORY__ + @"/../../src/fsharp/FSharp.Compiler.Service/FSharp.Compiler.Service.fsproj"

    //let v = FSharpProjectFileInfo.Parse(projectFile, [("Configuration", "Debug"); ("NoFsSrGenTask", "true")],enableLogging=true)
    let options = checker.GetProjectOptionsFromProjectFile(projectFile, [("Configuration", "Debug"); ("NoFsSrGenTask", "true")])

    // For subsets of the compiler:
    //let options = { options with OtherOptions = options.OtherOptions.[0..51] }

    //for x in options.OtherOptions do printfn "%s" x

    let wholeProjectResults = checker.ParseAndCheckProject(options) |> Async.RunSynchronously
    
    (wholeProjectResults.Errors |> Array.filter (fun x -> x.Severity = FSharpErrorSeverity.Error)).Length |> shouldEqual 0 

    for file in (wholeProjectResults.AssemblyContents.ImplementationFiles |> List.toArray) do
        for d in file.Declarations do 
           for s in printDeclaration None d do 
              () //printfn "%s" s

    // Very Quick (1 sec - expressions are computed on demand)
    for file in (wholeProjectResults.AssemblyContents.ImplementationFiles |> List.toArray) do
        for d in file.Declarations do 
           for s in exprsOfDecl d do 
              () 

    // Quickish (~4.5 seconds for all of FSharp.Compiler.Service.dll)
    #time "on"
    for file in (wholeProjectResults.AssemblyContents.ImplementationFiles |> List.toArray) do
        for d in file.Declarations do 
           for (e,m) in exprsOfDecl d do 
              // This forces the computation of the expression
              match e with
              | BasicPatterns.Const _ -> () //printfn "%s" s
              | _ -> () //printfn "%s" s

[<Test>]
let ``Test Declarations selfhost FSharp.Core`` () =
    
    Environment.CurrentDirectory <-  __SOURCE_DIRECTORY__ +  @"/../../../fsharp/src/fsharp/FSharp.Core"
    let projectFile = __SOURCE_DIRECTORY__ + @"/../../../fsharp/src/fsharp/FSharp.Core/FSharp.Core.fsproj"

    let options = checker.GetProjectOptionsFromProjectFile(projectFile, [("Configuration", "Debug")])

    let wholeProjectResults = checker.ParseAndCheckProject(options) |> Async.RunSynchronously
    
    //(wholeProjectResults.Errors |> Array.filter (fun x -> x.Severity = FSharpErrorSeverity.Error)).Length |> shouldEqual 0 

    for file in (wholeProjectResults.AssemblyContents.ImplementationFiles |> List.toArray) do
        for d in file.Declarations do 
           for s in printDeclaration (Some (HashSet [])) d do 
              printfn "%s" s

    #time "on"

    for file in (wholeProjectResults.AssemblyContents.ImplementationFiles |> List.toArray) do
        for d in file.Declarations do 
           for (e,m) in exprsOfDecl d do 
              // This forces the computation of the expression
              match e with
              | BasicPatterns.Const _ -> () 
              | _ -> () 

#endif

