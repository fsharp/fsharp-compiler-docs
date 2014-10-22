//----------------------------------------------------------------------------
// Copyright (c) 2002-2012 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------

namespace Microsoft.FSharp.Compiler.SourceCodeServices

open Internal.Utilities
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Infos
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.QuotationTranslator
open Microsoft.FSharp.Compiler.Typrelns


[<AutoOpen>]
module ExprTranslationImpl = 
    type ExprTranslationEnv = 
        { //Map from Val to binding index
          vs: ValMap<unit>; 
          nvs: int;
          //Map from typar stamps to binding index
          tyvs: StampMap<FSharpGenericParameter>;
          // Map for values bound by the 
          //     'let v = isinst e in .... if nonnull v then ...v .... ' 
          // construct arising out the compilation of pattern matching. We decode these back to the form
          //     'if istype v then ...unbox v .... ' 
          isinstVals: ValMap<TType * Expr> 
          substVals: ValMap<Expr> }

        static member Empty = 
            { vs=ValMap<_>.Empty; 
              nvs=0;
              tyvs = Map.empty ;
              isinstVals = ValMap<_>.Empty 
              substVals = ValMap<_>.Empty }

        member env.BindTypar (v:Typar, gp) = 
            { env with tyvs = env.tyvs.Add(v.Stamp,gp ) }

        member env.BindTypars vs = 
            (env, vs) ||> List.fold (fun env v -> env.BindTypar v) // fold left-to-right because indexes are left-to-right 

    let bindVal env v = 
        { env with vs = env.vs.Add v (); nvs = env.nvs + 1 }

    let bindIsInstVal env v (ty,e) = 
        { env with isinstVals =  env.isinstVals.Add v (ty,e) }

    let bindSubstVal env v e = 
        { env with substVals = env.substVals.Add v e  }


    let bindVals env vs = List.fold bindVal env vs // fold left-to-right because indexes are left-to-right 
    let bindFlatVals env vs = FlatList.fold bindVal env vs // fold left-to-right because indexes are left-to-right 

    exception IgnoringPartOfQuotedTermWarning of string * Range.range

    let wfail ((_,msg),m) = failwith (msg + sprintf " at %A" m)


type FSharpValue = FSharpMemberFunctionOrValue
and [<Sealed>]  FSharpObjectExprOverride(gps: FSharpGenericParameter list, args:FSharpMemberFunctionOrValue list list, body: FSharpExpr) = 
    member __.GenericParameters = gps
    member __.CurriedParameterGroups = args
    member __.Body = body

and E =

    | Value  of FSharpValue
    | ThisValue  of FSharpType 
    | BaseValue  of FSharpType 
    | Application of FSharpExpr * FSharpType list * FSharpExpr list  
    | Lambda of FSharpValue * FSharpExpr  
    | TypeLambda of FSharpGenericParameter list * FSharpExpr  
    | Quote  of FSharpExpr  
    | IfThenElse   of FSharpExpr * FSharpExpr * FSharpExpr  
    | Call of FSharpExpr option * FSharpMemberOrFunctionOrValue * FSharpType list * FSharpType list * FSharpExpr list list  
    | NewObject of FSharpMemberOrFunctionOrValue * FSharpType list * FSharpExpr list 
    | LetRec of ( FSharpValue * FSharpExpr) list * FSharpExpr  
    | Let of (FSharpValue * FSharpExpr) * FSharpExpr 
    | NewRecord of FSharpType * FSharpExpr list 
    | ObjectExpr of FSharpType * FSharpExpr * FSharpObjectExprOverride list * (FSharpType * FSharpObjectExprOverride list) list
    | FSharpFieldGet of  FSharpExpr option * FSharpType * FSharpField 
    | FSharpFieldSet of  FSharpExpr option * FSharpType * FSharpField * FSharpExpr 
    | NewUnionCase of FSharpType * FSharpUnionCase * FSharpExpr list  
    | UnionCaseGet of FSharpExpr * FSharpType * FSharpField 
    | UnionCaseTest of FSharpExpr  * FSharpType * FSharpUnionCase 
    | NewTuple of FSharpType * FSharpExpr list  
    | TupleGet of FSharpType * int * FSharpExpr 
    | Coerce of FSharpType * FSharpExpr  
    | NewArray of FSharpType * FSharpExpr list  
    | TypeTest of FSharpType * FSharpExpr  
    | AddressSet of FSharpExpr * FSharpExpr  
    | ValueSet of FSharpMemberOrFunctionOrValue * FSharpExpr  
    | Unit of unit  
    | DefaultValue of FSharpType  
    | Const of obj * FSharpType
    | AddressOf of FSharpExpr 
    | Sequential of FSharpExpr * FSharpExpr  
    | FastIntegerForLoop of FSharpExpr * FSharpExpr * FSharpExpr * bool
    | WhileLoop of FSharpExpr * FSharpExpr  
    | TryFinally of FSharpExpr * FSharpExpr  
    | TryWith of FSharpExpr * FSharpValue * FSharpExpr * FSharpValue * FSharpExpr  
    | NewDelegate of FSharpType * FSharpExpr  
    | ILFieldGet of FSharpExpr option * FSharpType * string 
    | ILFieldSet of FSharpExpr option * FSharpType * string  * FSharpExpr 
    | ILAsm of string * FSharpType list * FSharpExpr list

and FSharpExprConvert (cenv) =
    let ConvType typ = FSharpType(cenv, typ)
    let ConvTypes typs = List.map ConvType typs
    let ConvILTypeRefApp m tref tyargs = 
        let tcref = Import.ImportILTypeRef cenv.amap m tref
        ConvType (mkAppTy tcref tyargs)

    let ConvUnionCaseRef (ucref:UnionCaseRef) = FSharpUnionCase(cenv, ucref)
    let ConvRecdFieldRef (rfref:RecdFieldRef) = FSharpField(cenv,rfref )

    let rec exprOfExprAddr expr = 
        match expr with 
        | Expr.Op(op,tyargs,args,m) -> 
            match op, args, tyargs  with
            | TOp.LValueOp(LGetAddr,vref),_,_ -> exprForValRef m vref
            | TOp.ValFieldGetAddr(rfref),[],_ -> mkStaticRecdFieldGet(rfref,tyargs,m)
            | TOp.ValFieldGetAddr(rfref),[arg],_ -> mkRecdFieldGetViaExprAddr(exprOfExprAddr arg,rfref,tyargs,m)
            | TOp.ILAsm([ I_ldflda(fspec) ],rtys),[arg],_  -> mkAsmExpr([ mkNormalLdfld(fspec) ],tyargs, [exprOfExprAddr arg], rtys, m)
            | TOp.ILAsm([ I_ldsflda(fspec) ],rtys),_,_  -> mkAsmExpr([ mkNormalLdsfld(fspec) ],tyargs, args, rtys, m)
            | TOp.ILAsm(([ I_ldelema(_ro,_isNativePtr,shape,_tyarg) ] ),_), (arr::idxs), [elemty]  -> 
                match shape.Rank, idxs with 
                | 1, [idx1] -> mkCallArrayGet cenv.g m elemty arr idx1
                | 2, [idx1; idx2] -> mkCallArray2DGet cenv.g m elemty arr idx1 idx2
                | 3, [idx1; idx2; idx3] -> mkCallArray3DGet cenv.g m elemty arr idx1 idx2 idx3
                | 4, [idx1; idx2; idx3; idx4] -> mkCallArray4DGet cenv.g m elemty arr idx1 idx2 idx3 idx4
                | _ -> expr
            | _ -> expr
        | _ -> expr

    let rec ConvLValueExpr env expr = ConvExpr env (exprOfExprAddr expr)

    and ConvExpr env expr = FSharpExpr(cenv,Lazy.CreateFromValue(FSharpExprConvert(cenv).ConvertExprEager(env, expr)),expr.Range)
    and Mk m e = FSharpExpr(cenv,Lazy.CreateFromValue e,m)
    and ConvExprPrim env expr = 
        // Eliminate integer 'for' loops 
        let expr = DetectFastIntegerForLoops cenv.g expr

        // Eliminate subsumption coercions for functions. This must be done post-typechecking because we need
        // complete inference types.
        let expr = NormalizeAndAdjustPossibleSubsumptionExprs cenv.g expr

        // Remove TExpr_ref nodes
        let expr = stripExpr expr 

        match expr with 
        // Uses of possibly-polymorphic values which were not polymorphic in the end
        | Expr.App(InnerExprPat(Expr.Val _ as ve),_fty,[],[],_) -> 
            ConvExprPrim env ve

        | Expr.Val(vref,_vFlags,m) -> 
            ConvValRef env m vref 

        | ModuleValueOrMemberUse cenv.g (vref,vFlags,_f,_fty,tyargs,curriedArgs) when (nonNil tyargs || nonNil curriedArgs) && vref.IsMemberOrModuleBinding ->
            let m = expr.Range 

            let (numEnclTypeArgs,_,isNewObj,_valUseFlags,_isSelfInit,takesInstanceArg,_isPropGet,_isPropSet) = 
                GetMemberCallInfo cenv.g (vref,vFlags)

            let isMember,curriedArgInfos = 

                match vref.MemberInfo with 
                | Some _ when not vref.IsExtensionMember -> 
                    // This is an application of a member method
                    // We only count one argument block for these.
                    let _tps,curriedArgInfos,_,_ = GetTypeOfMemberInFSharpForm cenv.g vref 
                    true,curriedArgInfos
                | _ -> 
                    // This is an application of a module value or extension member
                    let arities = arityOfVal vref.Deref 
                    let _tps,curriedArgInfos,_,_ = GetTopValTypeInFSharpForm cenv.g arities vref.Type m
                    false,curriedArgInfos

            // Compute the object arguments as they appear in a compiled call
            // Strip off the object argument, if any. The curriedArgInfos are already adjusted to compiled member form
            let objArgs,curriedArgs = 
                match takesInstanceArg,curriedArgs with 
                | false,curriedArgs -> [],curriedArgs
                | true,(objArg::curriedArgs) -> [objArg],curriedArgs
                | true,[] -> failwith ("warning: unexpected missing object argument when generating quotation for call to F# object member "+vref.LogicalName)

            // Check to see if there aren't enough arguments or if there is a tuple-arity mismatch
            // If so, adjust and try again
            if curriedArgs.Length < curriedArgInfos.Length ||
                ((List.take curriedArgInfos.Length curriedArgs,curriedArgInfos) ||> List.exists2 (fun arg argInfo -> (argInfo.Length > (tryDestTuple arg).Length))) then

                // Too few arguments or incorrect tupling? Convert to a lambda and beta-reduce the 
                // partially applied arguments to 'let' bindings 
                let topValInfo = 
                    match vref.ValReprInfo with 
                    | None -> failwith ("no arity information found for F# value "+vref.LogicalName)
                    | Some a -> a 

                let expr,exprty = AdjustValForExpectedArity cenv.g m vref vFlags topValInfo 
                ConvExprPrim env (MakeApplicationAndBetaReduce cenv.g (expr,exprty,[tyargs],curriedArgs,m)) 

            elif curriedArgs.Length > curriedArgInfos.Length then
                //let curriedArgs,laterArgs = List.chop curriedArgInfos.Length curriedArgs 
                failwith "nyi - over-application of members"

            else        
                // detuple the args
                let untupledCurriedArgs = 
                    (curriedArgs,curriedArgInfos) ||> List.map2 (fun arg curriedArgInfo -> 
                        let numUntupledArgs = curriedArgInfo.Length 
                        (if numUntupledArgs = 0 then [] 
                            elif numUntupledArgs = 1 then [arg] 
                            else tryDestTuple arg))

                if isMember then 
                    // This is an application of a member method
                    // We only count one argument block for these.
                    let callArgs = (objArgs::untupledCurriedArgs) 
                    let enclTyArgs, methTyArgs = List.splitAfter numEnclTypeArgs tyargs
                    ConvObjectModelCall env (isNewObj, FSharpMemberFunctionOrValue(cenv,vref), enclTyArgs, methTyArgs, callArgs)
                else
                    // This is an application of the module value. 
                    ConvModuleFunctionCall env vref tyargs untupledCurriedArgs

        // Simple applications 
        | Expr.App(f,_fty,tyargs,args,_m) -> 
            E.Application (ConvExpr env f, ConvTypes tyargs, ConvExprs env args) 
    
        | Expr.Const(c,m,ty) -> 
            ConvConst env m c ty


        | Expr.Let(bind,body,_,_) -> 
            // The binding may be a compiler-generated binding that gets removed here (instead being bound into the environment)
            match ConvLetBind env bind with 
            | None, env -> ConvExprPrim env body
            | Some(bindR),env -> E.Let(bindR,ConvExpr env body) 
        
        | Expr.LetRec(binds,body,_,_) -> 
                let vs = valsOfBinds binds
                let vsR = vs |> FlatList.map ConvVal
                let env = bindFlatVals env vs
                let bodyR = ConvExpr env body 
                let bindsR = FlatList.zip vsR (binds |> FlatList.map (fun b -> b.Expr |> ConvExpr env))
                E.LetRec(FlatList.toList bindsR,bodyR) 
  
        | Expr.Lambda(_,_,_,vs,b,_,_) -> 
            let v,b = MultiLambdaToTupledLambda vs b 
            let vR = ConvVal v 
            let bR  = ConvExpr (bindVal env v) b 
            E.Lambda(vR, bR) 

        | Expr.Quote(ast,_,_,_,_) -> 
            E.Quote(ConvExpr env ast) 

        | Expr.TyLambda (_,tps,b,_,_) -> 
            let gps = [ for tp in tps -> FSharpGenericParameter(cenv,tp) ]
            let env = env.BindTypars (Seq.zip tps gps |> Seq.toList)
            E.TypeLambda(gps, ConvExpr env b) 

        | Expr.Match (_spBind,_m,dtree,tgs,_,retTy) ->
            let typR = ConvType retTy 
            ConvDecisionTreePrim env tgs typR dtree 
    
        // Remove initialization checks
        | Expr.Sequential(ObjectInitializationCheck cenv.g, x1, NormalSeq, _, _) -> ConvExprPrim env x1

        // Remove static initialization counter updates
        // Remove static initialization counter checks
        // TODO: allow clients to see static initialization checks if they want to

        // Match "init@41 <- 6"
        // Match "if [AI_clt](init@41,6) then IntrinsicFunctions.FailStaticInit () else ()"
        | Expr.Sequential  (Expr.Op(TOp.ValFieldSet rfref,_,_,_),x1,NormalSeq,_,_)              
        | Expr.Sequential  (Expr.Match (_,_,TDSwitch(Expr.Op(TOp.ILAsm ([ AI_clt ],_),_,[Expr.Op(TOp.ValFieldGet rfref,_,_,_) ;_],_),_,_,_),_,_,_),x1,NormalSeq,_,_) 
            when 
                rfref.RecdField.IsCompilerGenerated && 
                rfref.RecdField.IsStatic &&
                rfref.RecdField.IsMutable &&
                rfref.RecdField.Name.StartsWith "init" -> 
            ConvExprPrim env x1


        | Expr.Sequential (x0,x1,NormalSeq,_,_)  -> E.Sequential(ConvExpr env x0, ConvExpr env x1) 

        | Expr.Obj (_,typ,_,_,[TObjExprMethod(TSlotSig(_,ctyp, _,_,_,_),_,tps,[tmvs],e,_) as tmethod],_,m) when isDelegateTy cenv.g typ -> 
                let f = mkLambdas m tps tmvs (e,GetFSharpViewOfReturnType cenv.g (returnTyOfMethod cenv.g tmethod))
                let fR = ConvExpr env f 
                let tyargR = ConvType ctyp 
                E.NewDelegate(tyargR, fR) 

        | Expr.StaticOptimization (_,_,x,_) -> 
            ConvExprPrim env x

        | Expr.TyChoose _  -> 
            ConvExprPrim env (Typrelns.ChooseTyparSolutionsForFreeChoiceTypars cenv.g cenv.amap expr)

        | Expr.Sequential  (x0,x1,ThenDoSeq,_,_) ->  E.Sequential(ConvExpr env x0, ConvExpr env x1) 

        | Expr.Obj (_lambdaId,typ,_basev,basecall,overrides, iimpls,_m)      -> 
            let basecallR = ConvExpr env basecall
            let ConvertMethods methods = 
                [ for (TObjExprMethod(_slotsig,_,tps,tmvs,body,_)) in methods -> 
                    let conv = FSharpExprConvert(cenv)
                    let vslR = List.map (List.map conv.ConvertVal) tmvs 
                    let tpsR = [ for tp in tps -> FSharpGenericParameter(cenv,tp) ]
                    let env = ExprTranslationEnv.Empty.BindTypars (Seq.zip tps tpsR |> Seq.toList)
                    let env = (env, tmvs) ||> List.fold bindVals
                    let bodyR = FSharpExprConvert(cenv).ConvertOnDemand(env, body)
                    FSharpObjectExprOverride(tpsR, vslR, bodyR) ]
            let overridesR = ConvertMethods overrides 
            let iimplsR = List.map (fun (ty,impls) -> ConvType ty, ConvertMethods impls) iimpls

            E.ObjectExpr(ConvType typ, basecallR, overridesR, iimplsR)

        | Expr.Op(op,tyargs,args,m) -> 
            match op,tyargs,args with 
            | TOp.UnionCase ucref,_,_ -> 
                let mkR = ConvUnionCaseRef ucref 
                let typR = ConvType (mkAppTy ucref.TyconRef tyargs)
                let argsR = ConvExprs env args
                E.NewUnionCase(typR, mkR, argsR) 

            | TOp.Tuple,tyargs,_ -> 
                let tyR = ConvType (mkTupledTy cenv.g tyargs)
                let argsR = ConvExprs env args
                E.NewTuple(tyR, argsR) 

            | TOp.Recd (_,tcref),_,_  -> 
                let typR = ConvType (mkAppTy tcref tyargs)
                let argsR = ConvExprs env args
                E.NewRecord(typR, argsR) 

            | TOp.UnionCaseFieldGet (ucref,n),tyargs,[e] -> 
                let typR = ConvType (mkAppTy ucref.TyconRef tyargs)
                let projR = FSharpField(cenv, ucref, n)
                E.UnionCaseGet(ConvExpr env e, typR, projR) 

            | TOp.ValFieldGetAddr(_rfref),_tyargs,_ -> 
                E.AddressOf(ConvLValueExpr env expr)

            | TOp.ValFieldGet(rfref),tyargs,[] ->
                let projR = ConvRecdFieldRef rfref 
                let typR = ConvType (mkAppTy rfref.TyconRef tyargs)
                E.FSharpFieldGet(None, typR, projR) 

            | TOp.ValFieldGet(rfref),tyargs,[obj] ->
                let objR = ConvLValueExpr env obj
                let projR = ConvRecdFieldRef rfref 
                let typR = ConvType (mkAppTy rfref.TyconRef tyargs)
                E.FSharpFieldGet(Some objR, typR, projR) 

            | TOp.TupleFieldGet(n),tyargs,[e] -> 
                let tyR = ConvType (mkTupledTy cenv.g tyargs)
                E.TupleGet(tyR, n, ConvExpr env e) 

            | TOp.ILAsm([ I_ldfld(_,_,fspec) ],_), enclTypeArgs, [obj] -> 
                let typR = ConvILTypeRefApp m fspec.EnclosingTypeRef enclTypeArgs 
                let objR = ConvLValueExpr env obj
                E.ILFieldGet(Some objR, typR, fspec.Name) 

            | TOp.ILAsm(( [ I_ldsfld (_,fspec) ] | [ I_ldsfld (_,fspec); AI_nop ]),_),enclTypeArgs,[]  -> 
                let typR = ConvILTypeRefApp m fspec.EnclosingTypeRef enclTypeArgs 
                E.ILFieldGet(None, typR, fspec.Name) 

            | TOp.ILAsm([ I_stfld(_,_,fspec) ],_),enclTypeArgs,[obj;arg]  -> 
                let typR = ConvILTypeRefApp m fspec.EnclosingTypeRef enclTypeArgs 
                let objR = ConvLValueExpr env obj
                let argR = ConvExpr env arg
                E.ILFieldSet(Some objR, typR, fspec.Name, argR) 

            | TOp.ILAsm([ I_stsfld(_,fspec) ],_),enclTypeArgs,[arg]  -> 
                let typR = ConvILTypeRefApp m fspec.EnclosingTypeRef enclTypeArgs 
                let argR = ConvExpr env arg
                E.ILFieldSet(None, typR, fspec.Name, argR) 


            | TOp.ILAsm([ AI_ceq ],_),_,[arg1;arg2]  -> 
                let ty = tyOfExpr cenv.g arg1
                let eq = mkCallEqualsOperator cenv.g m ty arg1 arg2
                ConvExprPrim env eq

            | TOp.ILAsm([ I_throw ],_),_,[arg1]  -> 
                let raiseExpr = mkCallRaise cenv.g m (tyOfExpr cenv.g expr) arg1 
                ConvExprPrim env raiseExpr        

            | TOp.ILAsm(il,_),tyargs,args                         -> 
                E.ILAsm(sprintf "%+A" il, ConvTypes tyargs, ConvExprs env args)

            | TOp.ExnConstr tcref,tyargs,args              -> 
                E.NewRecord(ConvType (mkAppTy tcref tyargs), ConvExprs env args) 

            | TOp.ValFieldSet rfref, _tinst,[obj;arg]     -> 
                let objR = ConvLValueExpr env obj
                let argR = ConvExpr env arg
                let typR = ConvType (mkAppTy rfref.TyconRef tyargs)
                let projR = ConvRecdFieldRef rfref 
                E.FSharpFieldSet(Some objR, typR, projR, argR) 

            | TOp.ValFieldSet rfref, _tinst,[arg]     -> 
                let argR = ConvExpr env arg
                let typR = ConvType (mkAppTy rfref.TyconRef tyargs)
                let projR = ConvRecdFieldRef rfref 
                E.FSharpFieldSet(None, typR, projR, argR) 

            | TOp.ExnFieldGet(tcref,i),[],[obj] -> 
                let exnc = stripExnEqns tcref
                let fspec = exnc.TrueInstanceFieldsAsList.[i]
                let fref = mkRecdFieldRef tcref fspec.Name
                let typR = ConvType (mkAppTy tcref tyargs)
                let objR = ConvExpr env (mkCoerceExpr (obj, mkAppTy tcref [], m, cenv.g.exn_ty))
                E.FSharpFieldGet(Some objR, typR, ConvRecdFieldRef fref) 

            | TOp.Coerce,[tgtTy;srcTy],[x]  -> 
                if typeEquiv cenv.g tgtTy srcTy then 
                    ConvExprPrim env x
                else
                    E.Coerce(ConvType tgtTy,ConvExpr env x) 

            | TOp.Reraise,[toTy],[]         -> 
                // rebuild reraise<T>() and Convert 
                mkReraiseLibCall cenv.g toTy m |> ConvExprPrim env 

            | TOp.LValueOp(LGetAddr,vref),[],[] -> 
                E.AddressOf(ConvExpr env (exprForValRef m vref)) 

            | TOp.LValueOp(LByrefSet,vref),[],[e] -> 
                E.AddressSet(ConvExpr env (exprForValRef m vref), ConvExpr env e) 

            | TOp.LValueOp(LSet,vref),[],[e] -> 
                E.ValueSet(FSharpMemberOrFunctionOrValue(cenv, vref), ConvExpr env e) 

            | TOp.LValueOp(LByrefGet,vref),[],[] -> 
                ConvValRef env m vref 

            | TOp.Array,[ty],xa -> 
                    E.NewArray(ConvType ty,ConvExprs env xa)                             

            | TOp.While _,[],[Expr.Lambda(_,_,_,[_],test,_,_);Expr.Lambda(_,_,_,[_],body,_,_)]  -> 
                    E.WhileLoop(ConvExpr env test, ConvExpr env body) 
        
            | TOp.For(_, (FSharpForLoopUp |FSharpForLoopDown as dir) ), [], [Expr.Lambda(_,_,_,[_], lim0,_,_); Expr.Lambda(_,_,_,[_], SimpleArrayLoopUpperBound, lm,_); SimpleArrayLoopBody cenv.g (arr, elemTy, body)] ->
                let lim1 = 
                    let len = mkCallArrayLength cenv.g lm elemTy arr // Array.length arr
                    mkCallSubtractionOperator cenv.g lm cenv.g.int32_ty len (Expr.Const(Const.Int32 1, m, cenv.g.int32_ty)) // len - 1
                E.FastIntegerForLoop(ConvExpr env lim0, ConvExpr env lim1, ConvExpr env body, (dir = FSharpForLoopUp)) 

            | TOp.For(_,dir),[],[Expr.Lambda(_,_,_,[_],lim0,_,_);Expr.Lambda(_,_,_,[_],lim1,_,_);body]  -> 
                match dir with 
                | FSharpForLoopUp -> E.FastIntegerForLoop(ConvExpr env lim0,ConvExpr env lim1, ConvExpr env body,true) 
                | FSharpForLoopDown -> E.FastIntegerForLoop(ConvExpr env lim0,ConvExpr env lim1, ConvExpr env body,false) 
                | _ -> failwith "unexpected for-loop form"

            | TOp.ILCall(_,_,_,isNewObj,_valUseFlags,_isProp,_,ilMethRef,enclTypeArgs,methTypeArgs,_tys),[],callArgs -> 
                let tcref = Import.ImportILTypeRef cenv.amap m ilMethRef.EnclosingTypeRef
                let mdef = 
                    try resolveILMethodRefWithRescope (rescopeILType (p13 tcref.ILTyconInfo)) tcref.ILTyconRawMetadata ilMethRef
                    with _ -> failwith (sprintf "A call to '%s' could not be resolved" (ilMethRef.ToString()))
                let minfo = MethInfo.CreateILMeth(cenv.amap, m, generalizedTyconRef tcref, mdef) 
                let v = FSharpMemberFunctionOrValue(cenv, minfo)
                ConvObjectModelCall env (isNewObj, v, enclTypeArgs, methTypeArgs, [callArgs])

            | TOp.TryFinally _,[_resty],[Expr.Lambda(_,_,_,[_],e1,_,_); Expr.Lambda(_,_,_,[_],e2,_,_)] -> 
                E.TryFinally(ConvExpr env e1,ConvExpr env e2) 

            | TOp.TryCatch _,[_resty],[Expr.Lambda(_,_,_,[_],e1,_,_); Expr.Lambda(_,_,_,[vf],ef,_,_); Expr.Lambda(_,_,_,[vh],eh,_,_)] -> 
                let vfR = ConvVal vf
                let envf = bindVal env vf
                let vhR = ConvVal vh
                let envh = bindVal env vh
                E.TryWith(ConvExpr env e1,vfR,ConvExpr envf ef,vhR,ConvExpr envh eh) 

            | TOp.Bytes bytes,[],[] -> 
                    ConvExprPrim env (Expr.Op(TOp.Array, [cenv.g.byte_ty], List.ofArray (Array.map (mkByte cenv.g m) bytes), m))

            | TOp.UInt16s arr,[],[] -> 
                    ConvExprPrim env (Expr.Op(TOp.Array, [cenv.g.uint16_ty], List.ofArray (Array.map (mkUInt16 cenv.g m) arr), m))
              
            | TOp.UnionCaseProof _,_,[e]       -> ConvExprPrim env e  // Note: we erase the union case proof conversions when converting to quotations
            | TOp.UnionCaseTagGet _tycr,_tinst,[_cx]          -> wfail((FSComp.SR.crefQuotationsCantFetchUnionIndexes(), m))
            | TOp.UnionCaseFieldSet (_c,_i),_tinst,[_cx;_x]     -> wfail((FSComp.SR.crefQuotationsCantSetUnionFields(), m))
            | TOp.ExnFieldSet(_tcref,_i),[],[_ex;_x] -> wfail((FSComp.SR.crefQuotationsCantSetExceptionFields(), m))
            | TOp.RefAddrGet,_,_                       -> wfail((FSComp.SR.crefQuotationsCantRequireByref(), m))
            | TOp.TraitCall (_ss),_,_                    -> wfail((FSComp.SR.crefQuotationsCantCallTraitMembers(), m))
            | _ -> 
                wfail((0, "Unexpected expression shape"),m)
        | _ -> 
            failwith (sprintf "unhandled construct in AST: %A" expr)


    and ConvLetBind env (bind : Binding) = 
        match bind.Expr with 
        // Map for values bound by the 
        //     'let v = isinst e in .... if nonnull v then ...v .... ' 
        // construct arising out the compilation of pattern matching. We decode these back to the form
        //     'if istype e then ...unbox e .... ' 
        // It's bit annoying that pattern matching does this tranformation. Like all premature optimization we pay a 
        // cost here to undo it.
        | Expr.Op(TOp.ILAsm([ I_isinst _ ],_),[ty],[e],_) -> 
            None, bindIsInstVal env bind.Var (ty,e)
    
        // Remove let <compilerGeneratedVar> = <var> from quotation tree
        | Expr.Val _ when bind.Var.IsCompilerGenerated -> 
            None, bindSubstVal env bind.Var bind.Expr

        // Remove let <compilerGeneratedVar> = () from quotation tree
        | Expr.Const(Const.Unit,_,_) when bind.Var.IsCompilerGenerated -> 
            None, bindSubstVal env bind.Var bind.Expr

        // Remove let unionCase = ... from quotation tree
        | Expr.Op(TOp.UnionCaseProof _,_,[e],_) -> 
            None, bindSubstVal env bind.Var e

        | _ ->
            let v = bind.Var
            let vR = ConvVal v 
            let rhsR = ConvExpr env bind.Expr
            let envinner = bindVal env v
            Some(vR,rhsR),envinner

    and ConvObjectModelCall env (isNewObj, v:FSharpMemberFunctionOrValue, enclTyArgs, methTyArgs,callArgs) =
        let enclTyArgsR = ConvTypes enclTyArgs
        let methTyArgsR = ConvTypes methTyArgs
        let obj, callArgs = 
            if v.IsInstanceMember then 
                match callArgs with 
                | [obj] :: rest -> Some obj, rest
                | _ -> failwith "unexpected shape of arguments"
            else
                None, callArgs
        let objR = Option.map (ConvLValueExpr env) obj
        let callArgsR = List.map (ConvExprs env) callArgs
        if isNewObj then 
            E.NewObject(v, enclTyArgsR, List.concat callArgsR) 
        else 
            E.Call(objR, v, enclTyArgsR, methTyArgsR, callArgsR) 

    and ConvModuleFunctionCall env (vref:ValRef) methTyArgs (args: Expr list list) =
        let v = FSharpMemberOrFunctionOrValue(cenv, vref)
        ConvObjectModelCall env (false, v, [], methTyArgs, args)

    and ConvExprs env args = List.map (ConvExpr env) args 

    and ConvValRef env m (vref:ValRef) =
        let v = vref.Deref
        if env.isinstVals.ContainsVal v then 
            let (ty,e) = env.isinstVals.[v]
            ConvExprPrim env (mkCallUnbox cenv.g m ty e)
        elif env.substVals.ContainsVal v then 
            let e = env.substVals.[v]
            ConvExprPrim env e
        elif v.BaseOrThisInfo = CtorThisVal then 
            E.ThisValue(ConvType v.Type) 
        elif v.BaseOrThisInfo = BaseVal then 
            E.BaseValue(ConvType v.Type) 
        else 
            E.Value(FSharpMemberFunctionOrValue(cenv, vref)) 

    and ConvVal (v:Val) =  
        let vref = mkLocalValRef v 
        FSharpMemberFunctionOrValue(cenv,  vref) 

    and ConvConst env m c ty =
        match TryEliminateDesugaredConstants cenv.g m c with 
        | Some e -> ConvExprPrim env e
        | None ->
            let tyR = ConvType ty
            match c with 
            | Const.Bool    i ->  E.Const(box i, tyR)
            | Const.SByte   i ->  E.Const(box i, tyR)
            | Const.Byte    i ->  E.Const(box i, tyR)
            | Const.Int16   i ->  E.Const(box i, tyR)
            | Const.UInt16  i ->  E.Const(box i, tyR)
            | Const.Int32   i ->  E.Const(box i, tyR)
            | Const.UInt32  i ->  E.Const(box i, tyR)
            | Const.Int64   i ->  E.Const(box i, tyR)
            | Const.UInt64  i ->  E.Const(box i, tyR)
            | Const.Double   i ->  E.Const(box i, tyR)
            | Const.Single i ->  E.Const(box i, tyR)
            | Const.String  i ->  E.Const(box i, tyR)
            | Const.Char    i ->  E.Const(box i, tyR)
            | Const.Unit      ->  E.Const(box (), tyR)
            | Const.Zero      ->  E.DefaultValue (ConvType ty)
            | _ -> 
                wfail( (FSComp.SR.crefQuotationsCantContainThisConstant(), m))

    and ConvDecisionTree env tgs typR x m = 
        ConvDecisionTreePrim env tgs typR x |> Mk m

    and ConvDecisionTreePrim env tgs typR x = 
        match x with 
        | TDSwitch(e1,csl,dfltOpt,m) -> 
            let acc = 
                match dfltOpt with 
                | Some d -> ConvDecisionTreePrim env tgs typR d 
                | None -> wfail( (FSComp.SR.crefQuotationsCantContainThisPatternMatch(), m))
            let converted = 
                (csl,acc) ||> List.foldBack (fun (TCase(discrim,dtree)) acc -> 
                        let acc = acc |> Mk m
                        match discrim with 
                        | Test.UnionCase (ucref, tyargs) -> 
                            let objR = ConvExpr env e1
                            let ucR = ConvUnionCaseRef ucref 
                            let typR = ConvType (mkAppTy ucref.TyconRef tyargs)
                            E.IfThenElse (E.UnionCaseTest (objR, typR, ucR) |> Mk m, ConvDecisionTree env tgs typR dtree m, acc) 
                        | Test.Const (Const.Bool true) -> 
                            let e1R = ConvExpr env e1
                            E.IfThenElse (e1R, ConvDecisionTree env tgs typR dtree m, acc) 
                        | Test.Const (Const.Bool false) -> 
                            let e1R = ConvExpr env e1
                            // Note, reverse the branches
                            E.IfThenElse (e1R, acc, ConvDecisionTree env tgs typR dtree m) 
                        | Test.Const c -> 
                            let ty = tyOfExpr cenv.g e1
                            let eq = mkCallEqualsOperator cenv.g m ty e1 (Expr.Const (c, m, ty))
                            let eqR = ConvExpr env eq 
                            E.IfThenElse (eqR, ConvDecisionTree env tgs typR dtree m, acc) 
                        | Test.IsNull -> 
                            // Decompile cached isinst tests
                            match e1 with 
                            | Expr.Val(vref,_,_) when env.isinstVals.ContainsVal vref.Deref  ->
                                let (ty,e) =  env.isinstVals.[vref.Deref]
                                let tyR = ConvType ty
                                let eR = ConvExpr env e
                                // note: reverse the branches - a null test is a failure of an isinst test
                                E.IfThenElse (E.TypeTest (tyR,eR) |> Mk m, acc, ConvDecisionTree env tgs typR dtree m) 
                            | _ -> 
                                let ty = tyOfExpr cenv.g e1
                                let eq = mkCallEqualsOperator cenv.g m ty e1 (Expr.Const (Const.Zero, m, ty))
                                let eqR = ConvExpr env eq 
                                E.IfThenElse (eqR, ConvDecisionTree env tgs typR dtree m, acc) 
                        | Test.IsInst (_srcty, tgty) -> 
                            let e1R = ConvExpr env e1
                            E.IfThenElse (E.TypeTest (ConvType tgty, e1R)  |> Mk m, ConvDecisionTree env tgs typR dtree m, acc) 
                        | Test.ActivePatternCase _ -> wfail((1,"Test.ActivePatternCase test in quoted expression"),m)
                        | Test.ArrayLength _ -> wfail( (FSComp.SR.crefQuotationsCantContainArrayPatternMatching(), m))
                        )
            converted

        | TDSuccess (args,n) -> 
                let (TTarget(vars,rhs,_)) = tgs.[n] 
                // TAST stores pattern bindings in reverse order for some reason
                // Reverse them here to give a good presentation to the user
                let args = List.rev (FlatList.toList args)
                let vars = List.rev (FlatList.toList vars)
          
                let varsR = vars |> List.map ConvVal 
                let targetR = ConvExprPrim (bindVals env vars) rhs
                (varsR,args,targetR) |||> List.foldBack2 (fun vR arg acc -> E.Let((vR,ConvExpr env arg), acc |> Mk arg.Range) ) 
          
        | TDBind(bind,rest) -> 
                // The binding may be a compiler-generated binding that gets removed in the quotation presentation
                match ConvLetBind env bind with 
                | None, env -> ConvDecisionTreePrim env tgs typR rest 
                | Some(bindR),env -> E.Let(bindR,ConvDecisionTree env tgs typR rest bind.Var.Range) 

    // Written as a member to capture 'cenv'
    member __.ConvertExprEager(env,expr) = ConvExprPrim env expr
    member __.ConvertVal v = ConvVal v
    member __.ConvertOnDemand (env, expr) = FSharpExpr(cenv, Lazy.Create(fun () -> FSharpExprConvert(cenv).ConvertExprEager(env, expr)), expr.Range)


and [<Sealed>] FSharpExpr (cenv, e: Lazy<E>, m:range) =

    member x.Range = m
    member x.cenv = cenv
    member x.E = e.Force()
    override __.ToString() = sprintf "%+A" e

and FSharpAssemblyContents(cenv: Impl.cenv, mimpls: TypedImplFile list) = 

    new (g, thisCcu, tcImports, mimpls) = FSharpAssemblyContents(Impl.cenv(g,thisCcu,tcImports), mimpls)

    member __.ImplementationFiles = 
        [ for mimpl in mimpls -> FSharpImplementationFileContents(cenv, mimpl)]

and FSharpImplementationFileDeclaration = 
    | Entity of FSharpEntity * FSharpImplementationFileDeclaration list
    | MemberOrFunctionOrValue  of FSharpMemberOrFunctionOrValue * FSharpMemberOrFunctionOrValue list list * FSharpExpr
    | InitAction of FSharpExpr

and FSharpImplementationFileContents(cenv, mimpl) = 
    let (TImplFile(_qname,_pragmas,ModuleOrNamespaceExprWithSig(_mty,mdef,_),hasExplicitEntryPoint,isScript)) = mimpl 
    let rec getDecls2 (ModuleOrNamespaceExprWithSig(_mty,def,_m)) = getDecls def
    and getBind (bind: Binding) = 
        let v = bind.Var
        assert v.IsCompiledAsTopLevel
        let topValInfo = InferArityOfExprBinding cenv.g v bind.Expr
        let tps,_ctorThisValOpt,_baseValOpt,vsl,body,_bodyty = IteratedAdjustArityOfLambda cenv.g cenv.amap topValInfo bind.Expr
        //let env = { env with functionVal = (match vspec with None -> None | Some v -> Some (v,topValInfo)) }
(*
        let env = Option.foldBack (BindInternalValToUnknown cenv) ctorThisValOpt env
        let env = Option.foldBack (BindInternalValToUnknown cenv) baseValOpt env
        let env = bindT tps env
        let env = List.foldBack (BindInternalValsToUnknown cenv) vsl env
*)
        let v = FSharpMemberOrFunctionOrValue(cenv, mkLocalValRef v)
        let gps = v.GenericParameters
        let conv = FSharpExprConvert(cenv)
        let vslR = List.map (List.map conv.ConvertVal) vsl 
        let env = ExprTranslationEnv.Empty.BindTypars (Seq.zip tps gps |> Seq.toList)
        let env = (env, vsl ) ||> List.fold bindVals
        //printfn "%d gps"
        let e = FSharpExprConvert(cenv).ConvertOnDemand(env, body)
        FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, vslR, e) 

    and getDecls mdef = 
        match mdef with 
        | TMDefRec(tycons,binds,mbinds,_m) -> 
            [ for tycon in tycons do 
                  let entity = FSharpEntity(cenv, mkLocalEntityRef tycon)
                  yield FSharpImplementationFileDeclaration.Entity(entity, []) 
              for bind in binds do 
                  yield getBind bind
              for (ModuleOrNamespaceBinding(mspec, def)) in mbinds do 
                  let entity = FSharpEntity(cenv, mkLocalEntityRef mspec)
                  yield FSharpImplementationFileDeclaration.Entity (entity, getDecls def) ]
        | TMAbstract(mexpr) -> getDecls2 mexpr
        | TMDefLet(bind,_m)  ->
            [ yield getBind bind  ]
        | TMDefDo(e,_m)  ->
            [ let expr = FSharpExprConvert(cenv).ConvertOnDemand(ExprTranslationEnv.Empty, e)
              yield FSharpImplementationFileDeclaration.InitAction(expr)  ]
        | TMDefs(mdefs) -> 
            [ for mdef in mdefs do yield! getDecls mdef ]

    member __.Declarations = getDecls mdef 
    member __.HasExplicitEntryPoint = hasExplicitEntryPoint
    member __.IsScript = isScript


module BasicPatterns = 
    let (|Value|_|) (e:FSharpExpr) = match e.E with E.Value (v) -> Some (v) | _ -> None
    let (|Const|_|) (e:FSharpExpr) = match e.E with E.Const (v,ty) -> Some (v,ty) | _ -> None
    let (|TypeLambda|_|) (e:FSharpExpr) = match e.E with E.TypeLambda (v,e) -> Some (v,e) | _ -> None
    let (|Lambda|_|) (e:FSharpExpr) = match e.E with E.Lambda (v,e) -> Some (v,e) | _ -> None
    let (|Application|_|) (e:FSharpExpr) = match e.E with E.Application (f,tys,e) -> Some (f,tys,e) | _ -> None
    let (|IfThenElse|_|) (e:FSharpExpr) = match e.E with E.IfThenElse (e1,e2,e3) -> Some (e1,e2,e3) | _ -> None
    let (|Let|_|) (e:FSharpExpr) = match e.E with E.Let ((v,e),b) -> Some ((v,e),b) | _ -> None
    let (|LetRec|_|) (e:FSharpExpr) = match e.E with E.LetRec (ves,b) -> Some (ves,b) | _ -> None
    let (|NewRecord|_|) (e:FSharpExpr) = match e.E with E.NewRecord (ty,es) -> Some (ty,es) | _ -> None
    let (|NewUnionCase|_|) (e:FSharpExpr) = match e.E with E.NewUnionCase (e,tys,es) -> Some (e,tys,es) | _ -> None
    let (|NewTuple|_|) (e:FSharpExpr) = match e.E with E.NewTuple (ty,es) -> Some (ty,es) | _ -> None
    let (|TupleGet|_|) (e:FSharpExpr) = match e.E with E.TupleGet (ty,n,es) -> Some (ty,n,es) | _ -> None
    let (|Call|_|) (e:FSharpExpr) = match e.E with E.Call (a,b,c,d,e) -> Some (a,b,c,d,e) | _ -> None
    let (|NewObject|_|) (e:FSharpExpr) = match e.E with E.NewObject (a,b,c) -> Some (a,b,c) | _ -> None
    let (|FSharpFieldGet|_|) (e:FSharpExpr) = match e.E with E.FSharpFieldGet (a,b,c) -> Some (a,b,c) | _ -> None
    let (|FSharpFieldSet|_|) (e:FSharpExpr) = match e.E with E.FSharpFieldSet (a,b,c,d) -> Some (a,b,c,d) | _ -> None
    let (|UnionCaseGet|_|) (e:FSharpExpr) = match e.E with E.UnionCaseGet (a,b,c) -> Some (a,b,c) | _ -> None
    let (|UnionCaseTest|_|) (e:FSharpExpr) = match e.E with E.UnionCaseTest (a,b,c) -> Some (a,b,c) | _ -> None
    let (|NewArray|_|) (e:FSharpExpr) = match e.E with E.NewArray (a,b) -> Some (a,b) | _ -> None
    let (|Coerce|_|) (e:FSharpExpr) = match e.E with E.Coerce (a,b) -> Some (a,b) | _ -> None
    let (|Quote|_|) (e:FSharpExpr) = match e.E with E.Quote (a) -> Some (a) | _ -> None
    let (|TypeTest|_|) (e:FSharpExpr) = match e.E with E.TypeTest (a,b) -> Some (a,b) | _ -> None
    let (|Sequential|_|) (e:FSharpExpr) = match e.E with E.Sequential (a,b) -> Some (a,b) | _ -> None
    let (|FastIntegerForLoop|_|) (e:FSharpExpr) = match e.E with E.FastIntegerForLoop (a,b,c,d) -> Some (a,b,c,d) | _ -> None
    let (|WhileLoop|_|) (e:FSharpExpr) = match e.E with E.WhileLoop (a,b) -> Some (a,b) | _ -> None
    let (|TryFinally|_|) (e:FSharpExpr) = match e.E with E.TryFinally (a,b) -> Some (a,b) | _ -> None
    let (|TryWith|_|) (e:FSharpExpr) = match e.E with E.TryWith (a,b,c,d,e) -> Some (a,b,c,d,e) | _ -> None
    let (|NewDelegate|_|) (e:FSharpExpr) = match e.E with E.NewDelegate (ty,e) -> Some (ty,e) | _ -> None
    let (|DefaultValue|_|) (e:FSharpExpr) = match e.E with E.DefaultValue (ty) -> Some (ty) | _ -> None
    let (|AddressSet|_|) (e:FSharpExpr) = match e.E with E.AddressSet (a,b) -> Some (a,b) | _ -> None
    let (|ValueSet|_|) (e:FSharpExpr) = match e.E with E.ValueSet (a,b) -> Some (a,b) | _ -> None
    let (|AddressOf|_|) (e:FSharpExpr) = match e.E with E.AddressOf (a) -> Some (a) | _ -> None
    let (|ThisValue|_|) (e:FSharpExpr) = match e.E with E.ThisValue (a) -> Some (a) | _ -> None
    let (|BaseValue|_|) (e:FSharpExpr) = match e.E with E.BaseValue (a) -> Some (a) | _ -> None
    let (|ILAsm|_|) (e:FSharpExpr) = match e.E with E.ILAsm (a,b,c) -> Some (a,b,c) | _ -> None
    let (|ILFieldGet|_|) (e:FSharpExpr) = match e.E with E.ILFieldGet (a,b,c) -> Some (a,b,c) | _ -> None
    let (|ILFieldSet|_|) (e:FSharpExpr) = match e.E with E.ILFieldSet (a,b,c,d) -> Some (a,b,c,d) | _ -> None
    let (|ObjectExpr|_|) (e:FSharpExpr) = match e.E with E.ObjectExpr (a,b,c,d) -> Some (a,b,c,d) | _ -> None


