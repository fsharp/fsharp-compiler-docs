
#if INTERACTIVE
#r "../../bin/v4.5/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "FsUnit.fs"
#load "Common.fs"
#else
module FSharp.Compiler.Service.Tests.ExpeTests
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
        | BasicPatterns.Application(f,tyargs,args) -> quote low (printExpr 10 f + printTyargs tyargs + " " + printArgs args)
        | BasicPatterns.BaseValue(_) -> "base"
        | BasicPatterns.Call(Some obj,v,tyargs1,tyargs2,argsL) -> printObjOpt (Some obj) + v.CompiledName  + printTyargs tyargs2 + " " + printCurriedArgs argsL
        | BasicPatterns.Call(None,v,tyargs1,tyargs2,argsL) -> v.EnclosingEntity.CompiledName + printTyargs tyargs1 + "." + v.CompiledName  + printTyargs tyargs2 + " " + printCurriedArgs argsL
        | BasicPatterns.Coerce(ty1,e1) -> quote low (printExpr 10 e1 + " :> " + printTy ty1)
        | BasicPatterns.DefaultValue(ty1) -> "dflt"
        | BasicPatterns.FastIntegerForLoop _ -> "for-loop"
        | BasicPatterns.ILAsm(s,tyargs,args) -> s + printTupledArgs args 
        | BasicPatterns.ILFieldGet _ -> "ILFieldGet"
        | BasicPatterns.ILFieldSet _ -> "ILFieldSet"
        | BasicPatterns.IfThenElse (a,b,c) -> "(if " + printExpr 0 a + " then " + printExpr 0 b + " else " + printExpr 0 c + ")"
        | BasicPatterns.Lambda(v,e1) -> "fun " + v.CompiledName + " -> " + printExpr 0 e1
        | BasicPatterns.Let((v,e1),b) -> "let " + v.CompiledName + " = " + printExpr 0 e1 + " in " + printExpr 0 b
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
        | BasicPatterns.TypeLambda(gp1,e1) -> "FUN ... -> " + printExpr 0 e1 
        | BasicPatterns.TypeTest(ty,e1) -> printExpr 10 e1 + " :? " + printTy ty
        | BasicPatterns.UnionCaseGet(obj,ty,f1) -> printExpr 10 obj + "." + f1.Name
        | BasicPatterns.UnionCaseTest(obj,ty,f1) -> printExpr 10 obj + ".Is" + f1.Name
        | BasicPatterns.ObjectExpr(ty,basecall,overrides,iimpls) -> "{ new " + printTy ty + " with ... }"
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
    and printArgs args = String.concat " " (List.map (printExpr 10) args)
    and printParams (vs: FSharpMemberOrFunctionOrValue list) = "(" + String.concat "," (vs |> List.map (fun v -> v.CompiledName)) + ")"
    and printCurriedParams (vs: FSharpMemberOrFunctionOrValue list list) = String.concat " " (List.map printParams vs)
    and printCurriedArgs argsL = String.concat " " (argsL |> List.map printTupledArgs)
    and printTy ty = ty.Format(FSharpDisplayContext.Empty)
    and printTyargs tyargs = match tyargs with [] -> "" | args -> "<" + String.concat "," (List.map printTy tyargs) + ">"

    let rec printDeclaration (d: FSharpImplementationFileDeclaration) = 
        [ match d with 
            | FSharpImplementationFileDeclaration.Entity(e,ds) ->
                yield sprintf "type %s" e.LogicalName
                yield! printDeclarations ds
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v,vs,e) ->
            
                if not v.IsCompilerGenerated  then
                    if v.IsMember then 
                        yield sprintf "member %s%s = %s @ %s" v.CompiledName (printCurriedParams vs)  (printExpr 0 e) (e.Range.ToShortString())
                    else 
                        yield sprintf "let %s%s = %s @ %s" v.CompiledName (printCurriedParams vs) (printExpr 0 e) (e.Range.ToShortString())
            | FSharpImplementationFileDeclaration.InitAction(e) ->
                yield sprintf "do %s" (printExpr 0 e) ]
    and printDeclarations ds = 
        [ for d in ds do 
            yield! printDeclaration d ]

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


let functionThatUsesObjectExpression() = 
   { new obj() with  member x.ToString() = string 888 } 

let functionThatUsesObjectExpressionWithInterfaceImpl() = 
   { new obj() with  
       member x.ToString() = string 888 
     interface System.IComparable with 
       member x.CompareTo(y:obj) = 0 } 

let testFunctionThatUsesUnitsOfMeasure (x : float<_>) (y: float<_>) = x + y
      
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


[<Test>]
let ``Test Declarations project1`` () =
    let wholeProjectResults = checker.ParseAndCheckProject(Project1.options) |> Async.RunSynchronously
    
    wholeProjectResults.Errors.Length |> shouldEqual 2 // recursive value warning
    wholeProjectResults.Errors.[0].Severity |> shouldEqual FSharpErrorSeverity.Warning
    wholeProjectResults.Errors.[1].Severity |> shouldEqual FSharpErrorSeverity.Warning

    wholeProjectResults.AssemblyContents.ImplementationFiles.Count |> shouldEqual 2
    let file1 = wholeProjectResults.AssemblyContents.ImplementationFiles.[0]
    let file2 = wholeProjectResults.AssemblyContents.ImplementationFiles.[1]

    printDeclarations (List.ofSeq file1.Declarations) 
      |> shouldEqual
              ["type M"; "type IntAbbrev"; "let boolEx1 = True @ (6,14--6,18)";
               "let intEx1 = 1 @ (7,13--7,14)"; "let int64Ex1 = 1 @ (8,15--8,17)";
               "let tupleEx1 = (1,1) @ (9,16--9,21)";
               "let tupleEx2 = (1,1,1) @ (10,16--10,25)";
               "let tupleEx3 = (1,1,1,1) @ (11,16--11,29)";
               "let localExample = let y = 1 in let z = 1 in (y,z) @ (14,7--14,8)";
               "let localGenericFunctionExample(unitVar0) = let y = 1 in let compiledAsLocalGenericFunction = FUN ... -> fun x -> x in (compiledAsLocalGenericFunction<Microsoft.FSharp.Core.int> y,compiledAsLocalGenericFunction<Microsoft.FSharp.Core.float> 1) @ (19,7--19,8)";
               "let funcEx1(x) = x @ (23,23--23,24)";
               "let genericFuncEx1(x) = x @ (24,29--24,30)";
               "let topPair1b = patternInput@25.Item1 @ (25,4--25,26)";
               "let topPair1a = patternInput@25.Item0 @ (25,4--25,26)";
               "let tyfuncEx1 = Operators.TypeOf<'T>  @ (26,20--26,26)";
               "let testILCall1 = new Object() @ (27,18--27,27)";
               "let testILCall2 = Console.WriteLine (176) @ (28,18--28,47)";
               "let recFuncIgnoresFirstArg(g) (v) = v @ (32,33--32,34)";
               "let recValNeverUsedAtRuntime = recValNeverUsedAtRuntime@31.Force<Microsoft.FSharp.Core.int> (()) @ (31,8--31,32)";
               "let testFun4(unitVar0) = let rec ... in recValNeverUsedAtRuntime @ (36,4--39,28)";
               "type ClassWithImplicitConstructor";
               "member .ctor(compiledAsArg) = (Object..ctor (); (this.compiledAsArg <- compiledAsArg; (this.compiledAsField <- 1; let compiledAsLocal = 1 in let compiledAsLocal2 = Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (compiledAsLocal) (compiledAsLocal) in ()))) @ (41,5--41,33)";
               "member .cctor(unitVar) = (compiledAsStaticField <- 1; let compiledAsStaticLocal = 1 in let compiledAsStaticLocal2 = Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (compiledAsStaticLocal) (compiledAsStaticLocal) in ()) @ (49,11--49,40)";
               "member M1(__) (unitVar1) = Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (__.compiledAsField) (let x = __.compiledAsField in __.compiledAsGenericInstanceMethod<Microsoft.FSharp.Core.int> (x))) (__.compiledAsArg) @ (55,21--55,102)";
               "member M2(__) (unitVar1) = __.compiledAsInstanceMethod (()) @ (56,21--56,47)";
               "member SM1(unitVar0) = Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (compiledAsStaticField) (let x = compiledAsStaticField in ClassWithImplicitConstructor.compiledAsGenericStaticMethod<Microsoft.FSharp.Core.int> () (x)) @ (57,26--57,101)";
               "member SM2(unitVar0) = ClassWithImplicitConstructor.compiledAsStaticMethod () (()) @ (58,26--58,50)";
               "member ToString(__) (unitVar1) = Operators.op_Addition<Microsoft.FSharp.Core.string,Microsoft.FSharp.Core.string,Microsoft.FSharp.Core.string> (base.ToString ) (Operators.ToString<Microsoft.FSharp.Core.int> (999)) @ (59,29--59,57)";
               "member TestCallinToString(this) (unitVar1) = this.ToString  @ (60,39--60,54)";
               "type Error"; "let err = { ... } @ (64,10--64,20)";
               "let matchOnException(err) = (if err :? M.Error then let a = (err :> M.Error).Data0 in let b = (err :> M.Error).Data1 in 3 else let e = err in Operators.Raise<Microsoft.FSharp.Core.int> (e)) @ (66,33--66,36)";
               "let upwardForLoop(unitVar0) = let a = 1 in (for-loop; a) @ (69,16--69,17)";
               "let upwardForLoop2(unitVar0) = let a = 1 in (for-loop; a) @ (74,16--74,17)";
               "let downwardForLoop(unitVar0) = let a = 1 in (for-loop; a) @ (79,16--79,17)";
               "let quotationTest1(unitVar0) = quote(Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (1) (1)) @ (83,24--83,35)";
               "let quotationTest2(v) = quote(Operators.op_Addition<Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int,Microsoft.FSharp.Core.int> (ExtraTopLevelOperators.SpliceExpression<Microsoft.FSharp.Core.int> (v)) (1)) @ (84,24--84,36)";
               "type RecdType"; "type UnionType"; "type ClassWithEventsAndProperties";
               "member .ctor(unitVar0) = (Object..ctor (); (this.ev <- new FSharpEvent`1(()); ())) @ (89,5--89,33)";
               "member .cctor(unitVar) = (sev <- new FSharpEvent`1(()); ()) @ (91,11--91,35)";
               "member get_InstanceProperty(x) (unitVar1) = (x.ev.Trigger (1); 1) @ (92,32--92,48)";
               "member get_StaticProperty(unitVar0) = (sev.Trigger (1); 1) @ (93,35--93,52)";
               "member get_InstanceEvent(x) (unitVar1) = x.ev.get_Publish (()) @ (94,29--94,39)";
               "member get_StaticEvent(x) (unitVar1) = sev.get_Publish (()) @ (95,27--95,38)";
               "let c = new ClassWithEventsAndProperties(()) @ (97,8--97,38)";
               "let v = c.get_InstanceProperty (()) @ (98,8--98,26)";
               "do Console.WriteLine (777)";
               "let functionWithSubmsumption(x) = IntrinsicFunctions.UnboxGeneric<Microsoft.FSharp.Core.string> (x) @ (102,40--102,52)";
               "let functionWithCoercion(x) = Operators.op_PipeRight<Microsoft.FSharp.Core.string,Microsoft.FSharp.Core.string> (Operators.op_PipeRight<Microsoft.FSharp.Core.string,Microsoft.FSharp.Core.string> (IntrinsicFunctions.UnboxGeneric<Microsoft.FSharp.Core.string> (x :> Microsoft.FSharp.Core.obj)) (fun x -> M.functionWithSubmsumption (x :> Microsoft.FSharp.Core.obj))) (fun x -> M.functionWithSubmsumption (x :> Microsoft.FSharp.Core.obj)) @ (103,39--103,116)";
               "type MultiArgMethods";
               "member .ctor(c,d) = (Object..ctor (); ()) @ (105,5--105,20)";
               "member Method(x) (a,b) = 1 @ (106,37--106,38)";
               "member CurriedMethod(x) (a1,b1) (a2,b2) = 1 @ (107,63--107,64)";
               "let functionThatUsesObjectExpression(unitVar0) = { new Microsoft.FSharp.Core.obj with ... } @ (111,3--111,55)";
               "let functionThatUsesObjectExpressionWithInterfaceImpl(unitVar0) = { new Microsoft.FSharp.Core.obj with ... } :> System.IComparable @ (114,3--117,38)"
               "let testFunctionThatUsesUnitsOfMeasure(x) (y) = Operators.op_Addition<Microsoft.FSharp.Core.float<'u>,Microsoft.FSharp.Core.float<'u>,Microsoft.FSharp.Core.float<'u>> (x) (y) @ (119,70--119,75)"]
    ()


