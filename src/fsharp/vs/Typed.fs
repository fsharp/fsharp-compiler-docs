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

open System.IO
open System.Collections.Generic
open System.Reflection
open Internal.Utilities
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Pickle

[<AutoOpen>]
module Impl = 
    let makeReadOnlyCollection (arr : seq<'T>) = 
        System.Collections.ObjectModel.ReadOnlyCollection<_>(Seq.toArray arr) :> IList<_>
    let makeXmlDoc (XmlDoc x) = makeReadOnlyCollection (x)
    
    let rescopeEntity viewedCcu (entity : Entity) = 
        match tryRescopeEntity viewedCcu entity with
        | None -> mkLocalEntityRef entity
        | Some eref -> eref

type Env(typars : Typar list) = 
    let typars = Array.ofList typars
    member __.Typars = typars


type FSharpAssembly internal (g: TcGlobals, ccu: CcuThunk) = 

    member __.RawCcuThunk = ccu

    member __.QualifiedName = match ccu.QualifiedName with None -> "" | Some s -> s
      
    member __.CodeLocation = ccu.SourceCodeDirectory

    member __.FileName = ccu.FileName

    member __.SimpleName = ccu.AssemblyName 

    member __.IsProviderGenerated = ccu.IsProviderGenerated

    member __.Entities = 
        let rec loop (entity : Entity) = 
            [| if entity.IsNamespace then 
                   for entity in entity.ModuleOrNamespaceType.AllEntities do
                       yield! loop entity
               else 
                   yield FSharpEntity(g, rescopeEntity ccu entity) |]
        
        [| for entity in ccu.RootModulesAndNamespaces do
               yield! loop entity 
           for entity in ccu.RootTypeAndExceptionDefinitions do
               yield! loop entity 
            |]
        |> makeReadOnlyCollection
                 

and FSharpAssemblySignature internal (g: TcGlobals, mtyp: ModuleOrNamespaceType) = 

    member __.Entities = 

        let rec loop (rmtyp : ModuleOrNamespaceType) = 
            [| for entity in rmtyp.AllEntities do
                   if entity.IsNamespace then 
                       yield! loop entity.ModuleOrNamespaceType
                   else 
                       yield FSharpEntity(g, mkLocalEntityRef entity) |]
        
        loop mtyp |> makeReadOnlyCollection

and FSharpEntity(g:TcGlobals, entity:EntityRef) = 

    let isFSharp() = 
        match entity with
        | ERefNonLocal(NonLocalEntityRef(ccu, _)) -> not ccu.IsUnresolvedReference && ccu.IsFSharp
        | _ -> false

    let isUnresolved() = 
        match entity with
        | ERefNonLocal(NonLocalEntityRef(ccu, _)) -> 
            ccu.IsUnresolvedReference && entity.TryDeref.IsNone
        | _ -> false

    let poorAssembly() = 
        System.Reflection.Assembly.LoadWithPartialName entity.nlr.AssemblyName
    

    let checkIsResolved() = 
        if isUnresolved() then 
            let poorQualifiedName =
                if entity.nlr.AssemblyName = "mscorlib" then 
                    entity.nlr.DisplayName + ", mscorlib"
                else 
                    let ass = poorAssembly()
                    entity.nlr.DisplayName + ", " + ass.FullName
            invalidOp (sprintf "The entity '%s' does not exist or is in an unresolved assembly." poorQualifiedName)

    member __.Entity = entity
        
    member __.LogicalName = 
        checkIsResolved()
        entity.LogicalName 

    member __.CompiledName = 
        checkIsResolved()
        entity.CompiledName 

    member __.DisplayName = 
        checkIsResolved()
        if entity.IsModuleOrNamespace then entity.DemangledModuleOrNamespaceName
        else entity.DisplayName 

    member __.Namespace = 
        checkIsResolved()
        match entity.CompilationPathOpt with 
        | None -> None 
        | Some cp when cp.AccessPath |> List.forall (function (_,ModuleOrNamespaceKind.Namespace) -> true | _  -> false) -> 
            Some (buildAccessPath (Some cp))
        | Some _ -> None
    
    member x.QualifiedName = 
        checkIsResolved()
        let fail() = invalidOp (sprintf "the type '%s' does not have a qualified name" x.LogicalName)
        if entity.IsTypeAbbrev then fail()
        match entity.CompiledRepresentation with 
        | CompiledTypeRepr.ILAsmNamed(tref,_,_) -> tref.QualifiedName
        | CompiledTypeRepr.ILAsmOpen _ -> fail()
        

    member __.DeclarationLocation = 
        checkIsResolved()
        entity.Range

    member this.GenericParameters = 
        checkIsResolved()
        let env = Env(entity.TyparsNoRange)
        entity.TyparsNoRange |> List.map (fun tp -> FSharpGenericParameter(g, env,tp)) |> List.toArray |> makeReadOnlyCollection

    member __.IsMeasure = 
        checkIsResolved()
        (entity.TypeOrMeasureKind = TyparKind.Measure)

    member __.IsFSharpModule = 
        checkIsResolved()
        isFSharp() && entity.IsModule

    member __.HasFSharpModuleSuffix = 
        checkIsResolved()
        isFSharp() && entity.IsModule && (entity.ModuleOrNamespaceType.ModuleOrNamespaceKind = ModuleOrNamespaceKind.FSharpModuleWithSuffix)

    member __.IsValueType  = 
        checkIsResolved()
        entity.IsStructOrEnumTycon 

    member __.IsClass = 
        checkIsResolved()
        match metadataOfTycon entity.Deref with 
        | ProvidedTypeMetadata info -> info.IsClass
        | ILTypeMetadata (_,td) -> (td.tdKind = ILTypeDefKind.Class)
        | FSharpOrArrayOrByrefOrTupleOrExnTypeMetadata -> entity.Deref.IsFSharpClassTycon

    member __.IsInterface = 
        checkIsResolved()
        isInterfaceTyconRef entity

    member __.IsDelegate = 
        checkIsResolved()
        match metadataOfTycon entity.Deref with 
        | ProvidedTypeMetadata info -> info.IsDelegate ()
        | ILTypeMetadata (_,td) -> (td.tdKind = ILTypeDefKind.Delegate)
        | FSharpOrArrayOrByrefOrTupleOrExnTypeMetadata -> entity.IsFSharpDelegateTycon

    member __.IsEnum = 
        checkIsResolved()
        entity.IsEnumTycon
    
    member __.IsFSharpExceptionDeclaration = 
        checkIsResolved()
        isFSharp() && entity.IsExceptionDecl

    member __.IsUnresolved = 
        isUnresolved()

    member __.IsFSharp = 
        isFSharp()

    member __.IsFSharpAbbreviation = 
        checkIsResolved()
        isFSharp() && entity.IsTypeAbbrev 

    member __.IsFSharpRecord = 
        checkIsResolved()
        isFSharp() && entity.IsRecordTycon

    member __.IsFSharpUnion = 
        checkIsResolved()
        isFSharp() && entity.IsUnionTycon

    member __.HasAssemblyCodeRepresentation = 
        checkIsResolved()
        isFSharp() && (entity.IsAsmReprTycon || entity.IsMeasureableReprTycon)


    member __.FSharpDelegateSignature =
        checkIsResolved()
        match entity.TypeReprInfo with 
        | TFsObjModelRepr r when entity.IsFSharpDelegateTycon -> 
            match r.fsobjmodel_kind with 
            | TTyconDelegate ss -> FSharpDelegateSignature(g, ss)
            | _ -> invalidOp "not a delegate type"
        | _ -> invalidOp "not a delegate type"
      

    member __.Accessibility = 
        checkIsResolved()
        FSharpAccessibility(entity.Accessibility) 

    member __.RepresentationAccessibility = 
        checkIsResolved()
        FSharpAccessibility(entity.TypeReprAccessibility)

    member this.DeclaredInterfaces = 
        checkIsResolved()
        let env = Env(entity.TyparsNoRange)
        entity.ImmediateInterfaceTypesOfFSharpTycon |> List.map (fun ty -> FSharpType(g, env,ty)) |> makeReadOnlyCollection

    member this.BaseType = 
        checkIsResolved()
        let env = Env(entity.TyparsNoRange)
        match entity.TypeContents.tcaug_super with 
        | None -> invalidOp "this entity has no base type"
        | Some ty -> FSharpType(g, env,ty)
        
    member __.UsesPrefixDisplay = 
        checkIsResolved()
        not (isFSharp()) || entity.Deref.IsPrefixDisplay

    member this.MembersOrValues = 
        checkIsResolved()
        ((entity.MembersOfFSharpTyconSorted
            |> List.filter (fun v -> 
                 not v.IsOverrideOrExplicitImpl && 
                 not v.Deref.IsClassConstructor)
            |> List.map (fun v -> FSharpMemberOrVal(g, this, v)))
        @
            (entity.ModuleOrNamespaceType.AllValsAndMembers
            |> Seq.toList
            |> List.filter (fun v -> v.IsExtensionMember || not v.IsMember) 
            |> List.map (fun v -> FSharpMemberOrVal(g,this,mkNestedValRef entity v))))
               
            |> makeReadOnlyCollection

    member __.XmlDocSig = 
        checkIsResolved()
        entity.XmlDocSig 

    member __.XmlDoc = 
        checkIsResolved()
        entity.XmlDoc |> makeXmlDoc

    member __.NestedEntities = 
        checkIsResolved()
        entity.ModuleOrNamespaceType.AllEntities 
        |> QueueList.toList
        |> List.map (fun x -> FSharpEntity(g, entity.MkNestedTyconRef x))
        |> makeReadOnlyCollection

    member this.UnionCases = 
        checkIsResolved()
        let env = Env(entity.TyparsNoRange)
        entity.UnionCasesAsRefList
        |> List.map (fun x -> FSharpUnionCase(g, env,x)) 
        |> makeReadOnlyCollection

    member this.RecordFields =
        checkIsResolved()

        let env = Env(entity.TyparsNoRange)
        
        entity.AllFieldsAsList
        |> List.map (fun x -> FSharpRecordField(g, env, mkRecdFieldRef entity x.Name))
        |> makeReadOnlyCollection

    member this.AbbreviatedType   = 
        checkIsResolved()

        let env = Env(entity.TyparsNoRange)
        
        match entity.TypeAbbrev with
        | None -> invalidOp "not a type abbreviation"
        | Some ty -> FSharpType(g, env, ty)

    member __.Attributes = 
        checkIsResolved()
        entity.Attribs
        |> List.map (fun a -> FSharpAttribute(g, a))
        |> makeReadOnlyCollection

    override this.Equals(other : obj) =
        box this === other ||
        match other with
        |   :? FSharpEntity as otherEntity -> tyconRefEq g entity otherEntity.Entity
        |   _ -> false

    override this.GetHashCode() =
        checkIsResolved()
        ((hash entity.Stamp) <<< 1) + 1

    static member op_Equality (left:FSharpEntity,right:FSharpEntity) = (left = right)
    static member op_Inequality (left:FSharpEntity,right:FSharpEntity) = (left <> right)

and FSharpUnionCase(g:TcGlobals, env:Env, v: UnionCaseRef) =

    member __.Name = v.UnionCase.DisplayName
    member __.DeclarationLocation = v.Range
    member __.UnionCaseFields = v.UnionCase.RecdFields |> List.map (fun r ->  FSharpRecordField(g, env,mkRecdFieldRef v.TyconRef r.Name)) |> List.toArray |> makeReadOnlyCollection
    member __.ReturnType = FSharpType(g, env, v.ReturnType)
    member __.CompiledName = v.UnionCase.CompiledName
    member __.XmlDocSig = v.UnionCase.XmlDocSig
    member __.XmlDoc = v.UnionCase.XmlDoc |> makeXmlDoc
    member __.Attributes = v.Attribs |> List.map (fun a -> FSharpAttribute(g, a)) |> makeReadOnlyCollection
    member __.Accessibility =  FSharpAccessibility(v.UnionCase.Accessibility)

    member private this.V = v
    override this.Equals(other : obj) =
        box this === other ||
        match other with
        |   :? FSharpUnionCase as uc -> v === uc.V
        |   _ -> false
    
    override this.GetHashCode() = (hash (box v))

    static member op_Equality (left:FSharpUnionCase,right:FSharpUnionCase) = (left = right)
    static member op_Inequality (left:FSharpUnionCase,right:FSharpUnionCase) = (left <> right)


and 
    [<RequireQualifiedAccess>]
    RecordFieldContainer = Entity of FSharpEntity | UnionCase of FSharpUnionCase

and FSharpRecordField(g:TcGlobals, env:Env, v: RecdFieldRef) =
    member __.IsMutable = v.RecdField.IsMutable
    member __.XmlDocSig = v.RecdField.XmlDocSig
    member __.XmlDoc = v.RecdField.XmlDoc |> makeXmlDoc
    member __.FieldType = FSharpType(g, env, v.RecdField.FormalType)
    member __.IsStatic = v.RecdField.IsStatic
    member __.Name = v.RecdField.Name
    member __.IsCompilerGenerated = v.RecdField.IsCompilerGenerated
    member __.DeclarationLocation = v.Range
    member __.FieldAttributes = v.RecdField.FieldAttribs |> List.map (fun a -> FSharpAttribute(g, a)) |> makeReadOnlyCollection
    member __.PropertyAttributes = v.PropertyAttribs |> List.map (fun a -> FSharpAttribute(g, a)) |> makeReadOnlyCollection
    //member __.LiteralValue = v.Is
    member __.Accessibility =  FSharpAccessibility(v.RecdField.Accessibility) 
    member private this.V = v
    override this.Equals(other : obj) =
        box this === other ||
        match other with
        |   :? FSharpRecordField as uc -> v === uc.V
        |   _ -> false

    override this.GetHashCode() = hash (box v)
    static member op_Equality (left:FSharpRecordField,right:FSharpRecordField) = (left = right)
    static member op_Inequality (left:FSharpRecordField,right:FSharpRecordField) = (left <> right)


and FSharpAccessibility(a:Accessibility) = 
    let isInternalCompPath x = 
        match x with 
        | CompPath(ILScopeRef.Local,[]) -> true 
        | _ -> false
    let (|Public|Internal|Private|) (TAccess p) = 
        match p with 
        | [] -> Public 
        | _ when List.forall isInternalCompPath p  -> Internal 
        | _ -> Private
    member __.IsPublic = match a with Public -> true | _ -> false
    member __.IsPrivate = match a with Private -> true | _ -> false
    member __.IsInternal = match a with Internal -> true | _ -> false

and FSharpGenericParameter(g:TcGlobals, env:Env, v:Typar) = 

    member __.Name = v.DisplayName
    member __.DeclarationLocation = v.Range
       
    member __.IsMeasure = (v.Kind = TyparKind.Measure)

    member __.XmlDoc = v.Data.typar_xmldoc |> makeXmlDoc

    member __.IsSolveAtCompileTime = (v.StaticReq = TyparStaticReq.HeadTypeStaticReq)
       
    member __.Attributes = v.Attribs |> List.map (fun a -> FSharpAttribute(g, a)) |> makeReadOnlyCollection

    member __.Constraints = v.Constraints |> List.map (fun a -> FSharpGenericParameterConstraint(g, env,a)) |> makeReadOnlyCollection
    
    member private this.V = v

    override this.Equals(other : obj) =
        box this === other ||
        match other with
        |   :? FSharpGenericParameter as p -> typarRefEq v p.V
        |   _ -> false

    override this.GetHashCode() = (hash v.Stamp)

    static member op_Equality (left:FSharpGenericParameter,right:FSharpGenericParameter) = (left = right)
    static member op_Inequality (left:FSharpGenericParameter,right:FSharpGenericParameter) = (left <> right)

and FSharpDelegateSignature(g: TcGlobals, info : SlotSig) = 

    let env = Env(info.ClassTypars)
    member __.DelegateArguments = 
        info.FormalParams.Head
        |> List.map (fun (TSlotParam(nm, ty, _, _, _, _)) -> nm, FSharpType(g, env, ty))
        |> makeReadOnlyCollection

    member __.DelegateReturnType = 
        match info.FormalReturnType with
        | None -> FSharpType(g, env, g.unit_ty)
        | Some ty -> FSharpType(g, env, ty)

and FSharpGenericParameterMemberConstraint(g: TcGlobals, env : Env, info : TraitConstraintInfo) = 
    let (TTrait(tys,nm,flags,atys,rty,_)) = info 
    member __.MemberSources = 
        tys   |> List.map (fun ty -> FSharpType(g, env, ty)) |> makeReadOnlyCollection

    member __.MemberName = nm

    member __.MemberIsStatic = not flags.IsInstance

    member __.MemberArgumentTypes = atys   |> List.map (fun ty -> FSharpType(g, env,ty)) |> makeReadOnlyCollection

    member this.MemberReturnType =
        match rty with 
        | None -> FSharpType(g, env,  g.unit_ty) 
        | Some ty -> FSharpType(g, env, ty) 


and FSharpGenericParameterDelegateConstraint(g: TcGlobals, env : Env, tupledArgTyp: TType, rty: TType) = 
    member __.DelegateTupledArgumentType = FSharpType(g, env,tupledArgTyp)
    member __.DelegateReturnType =  FSharpType(g, env,rty)

and FSharpGenericParameterDefaultsToConstraint(g: TcGlobals, env : Env, pri:int, ty:TType) = 
    member __.DefaultsToPriority = pri 
    member __.DefaultsToTarget = FSharpType(g, env, ty) 

and FSharpGenericParameterConstraint(g: TcGlobals, env : Env, cx : TyparConstraint) = 

    member __.IsCoercesToConstraint = 
        match cx with 
        | TyparConstraint.CoercesTo _ -> true 
        | _ -> false

    member __.CoercesToTarget = 
        match cx with 
        | TyparConstraint.CoercesTo(ty,_) -> FSharpType(g, env, ty) 
        | _ -> invalidOp "not a coerces-to constraint"

    member __.IsDefaultsToConstraint = 
        match cx with 
        | TyparConstraint.DefaultsTo _ -> true 
        | _ -> false

    member __.DefaultsToConstraintData = 
        match cx with 
        | TyparConstraint.DefaultsTo(pri, ty, _) ->  FSharpGenericParameterDefaultsToConstraint(g, env, pri, ty) 
        | _ -> invalidOp "not a 'defaults-to' constraint"

    member __.IsSupportsNullConstraint  = match cx with TyparConstraint.SupportsNull _ -> true | _ -> false

    member __.IsMemberConstraint = 
        match cx with 
        | TyparConstraint.MayResolveMember _ -> true 
        | _ -> false

    member __.MemberConstraintData =  
        match cx with 
        | TyparConstraint.MayResolveMember(info, _) ->  FSharpGenericParameterMemberConstraint(g, env, info) 
        | _ -> invalidOp "not a member constraint"

    member __.IsNonNullableValueTypeConstraint = 
        match cx with 
        | TyparConstraint.IsNonNullableStruct _ -> true 
        | _ -> false
    
    member __.IsReferenceTypeConstraint  = 
        match cx with 
        | TyparConstraint.IsReferenceType _ -> true 
        | _ -> false

    member __.IsSimpleChoiceConstraint = 
        match cx with 
        | TyparConstraint.SimpleChoice _ -> true 
        | _ -> false

    member __.SimpleChoices = 
        match cx with 
        | TyparConstraint.SimpleChoice (tys,_) -> 
            tys   |> List.map (fun ty -> FSharpType(g, env,ty)) |> makeReadOnlyCollection
        | _ -> invalidOp "incorrect constraint kind"

    member __.IsRequiresDefaultConstructorConstraint  = 
        match cx with 
        | TyparConstraint.RequiresDefaultConstructor _ -> true 
        | _ -> false

    member __.IsEnumConstraint = 
        match cx with 
        | TyparConstraint.IsEnum _ -> true 
        | _ -> false

    member __.EnumConstraintTarget = 
        match cx with 
        | TyparConstraint.IsEnum(ty,_) -> FSharpType(g, env,ty)
        | _ -> invalidOp "incorrect constraint kind"
    
    member __.IsComparisonConstraint = 
        match cx with 
        | TyparConstraint.SupportsComparison _ -> true 
        | _ -> false

    member __.IsEqualityConstraint = 
        match cx with 
        | TyparConstraint.SupportsEquality _ -> true 
        | _ -> false

    member __.IsUnmanagedConstraint = 
        match cx with 
        | TyparConstraint.IsUnmanaged _ -> true 
        | _ -> false

    member __.IsDelegateConstraint = 
        match cx with 
        | TyparConstraint.IsDelegate _ -> true 
        | _ -> false

    member __.DelegateConstraintData =  
        match cx with 
        | TyparConstraint.IsDelegate(ty1,ty2, _) ->  FSharpGenericParameterDelegateConstraint(g, env, ty1, ty2) 
        | _ -> invalidOp "not a member constraint"


and FSharpInlineAnnotation = 
   | PsuedoValue 
   | AlwaysInline 
   | OptionalInline 
   | NeverInline 

and FSharpMemberOrVal(g:TcGlobals,e:FSharpEntity,v:ValRef) = 

    let isUnresolved() = v.TryDeref.IsNone

    let checkIsResolved() = 
        if isUnresolved() then invalidOp (sprintf "The value or member '%s' does not exist or is in an unresolved assembly." (match v with VRefNonLocal n -> n.ItemKey.PartialKey.LogicalName | _ -> "<local>"))

    member __.IsUnresolved = 
        isUnresolved()

    member __.DeclarationLocation = 
        checkIsResolved()
        v.Range

    member __.LogicalEnclosingEntity = 
        checkIsResolved()
        match v.ApparentParent with 
        | ParentNone -> invalidOp "the value or member doesn't have a logical parent" 
        | Parent p -> FSharpEntity(g, p)

    member this.GenericParameters = 
        checkIsResolved()
        let env = Env(v.Typars)
        v.Typars |> List.map (fun tp -> FSharpGenericParameter(g, env,tp)) |> List.toArray |> makeReadOnlyCollection

    member this.FullType = 
        checkIsResolved()
        FSharpType(g, Env(v.Typars),v.TauType)

    member __.EnclosingEntity = e

    member __.IsCompilerGenerated = 
        checkIsResolved()
        v.IsCompilerGenerated

    member __.InlineAnnotation = 
        checkIsResolved()
        match v.InlineInfo with 
        | ValInline.PseudoVal -> FSharpInlineAnnotation.PsuedoValue
        | ValInline.Always -> FSharpInlineAnnotation.AlwaysInline
        | ValInline.Optional -> FSharpInlineAnnotation.OptionalInline
        | ValInline.Never -> FSharpInlineAnnotation.NeverInline

    member __.IsMutable = 
        checkIsResolved()
        v.IsMutable

    member __.IsModuleValueOrMember = 
        checkIsResolved()
        v.IsMember || v.IsModuleBinding

    member __.IsMember = 
        checkIsResolved()
        v.IsMember 
    
    member __.IsDispatchSlot = 
        checkIsResolved()
        v.IsDispatchSlot

    member __.IsGetterMethod = 
        checkIsResolved()
        match v.MemberInfo with 
        | None -> false 
        | Some memInfo -> memInfo.MemberFlags.MemberKind = MemberKind.PropertyGet

    member __.IsSetterMethod = 
        checkIsResolved()
        match v.MemberInfo with 
        | None -> false 
        | Some memInfo -> memInfo.MemberFlags.MemberKind = MemberKind.PropertySet

    member __.IsInstanceMember = 
        checkIsResolved()
        v.IsInstanceMember

    member __.IsExtensionMember = 
        checkIsResolved()
        v.IsExtensionMember

    member __.IsImplicitConstructor = 
        checkIsResolved()
        v.IsIncrClassConstructor
    
    member __.IsTypeFunction = 
        checkIsResolved()
        v.IsTypeFunction

    member __.IsActivePattern =  
        checkIsResolved()
        v.CoreDisplayName |> PrettyNaming.ActivePatternInfoOfValName |> isSome

    member __.CompiledName = 
        checkIsResolved()
        v.CompiledName

    member __.LogicalName = 
        checkIsResolved()
        v.LogicalName

    member __.DisplayName = 
        checkIsResolved()
        v.DisplayName

    member __.XmlDocSig = 
        checkIsResolved()
        v.XmlDocSig

    member __.XmlDoc = 
        checkIsResolved()
        v.XmlDoc |> makeXmlDoc

    member __.CurriedParameterGroups = 
        checkIsResolved()
        let env = Env(v.Typars)
        match v.ValReprInfo with 
        | None -> failwith "not a module let binding or member"
        | Some (ValReprInfo(_typars,curriedArgInfos,_retInfo)) -> 
            let tau = v.TauType
            let argtysl,_ = GetTopTauTypeInFSharpForm g curriedArgInfos tau range0
            let argtysl = if v.IsInstanceMember then argtysl.Tail else argtysl
            
            [ for argtys in argtysl do 
                 yield 
                   [ for argty, argInfo in argtys do 
                        yield FSharpParameter(g, env, argty, argInfo) ] 
                   |> makeReadOnlyCollection ]
             |> makeReadOnlyCollection

    member this.ReturnParameter  = 
        checkIsResolved()
        let env = Env(v.Typars)
        match v.ValReprInfo with 
        | None -> failwith "not a module let binding or member" 
        | Some (ValReprInfo(_typars,argInfos,retInfo)) -> 
        
            let tau = v.TauType
            let _,rty = GetTopTauTypeInFSharpForm g argInfos tau range0
            
            FSharpParameter(g, env, rty, retInfo) 


    member __.Attributes = 
        checkIsResolved()
        v.Attribs |> List.map (fun a -> FSharpAttribute(g, a)) |> makeReadOnlyCollection
     
(*
    /// Is this "base" in "base.M(...)"
    member __.IsBaseValue : bool

    /// Is this the "x" in "type C() as x = ..."
    member __.IsConstructorThisValue : bool

    /// Is this the "x" in "member __.M = ..."
    member __.IsMemberThisValue : bool

    /// Is this a [<Literal>] value, and if so what value?
    member __.LiteralValue : obj // may be null

*)

      /// How visible is this? 
    member __.Accessibility = 
        checkIsResolved()
        FSharpAccessibility(v.Accessibility)

    member private this.V = v

    override this.Equals(other : obj) =
        box this === other ||
        match other with
        |   :? FSharpMemberOrVal as other -> v === other.V
        |   _ -> false

    override this.GetHashCode() = hash (box v)

    static member op_Equality (left:FSharpMemberOrVal,right:FSharpMemberOrVal) = (left = right)
    static member op_Inequality (left:FSharpMemberOrVal,right:FSharpMemberOrVal) = (left <> right)


and FSharpType(g:TcGlobals, env:Env, typ:TType) =
    new (g:TcGlobals, typ:TType) = FSharpType(g, Env [], typ)
    member __.IsNamedType = (match stripTyparEqns typ with TType_app _ | TType_measure (MeasureCon _ | MeasureProd _ | MeasureInv _ | MeasureOne _) -> true | _ -> false)
    member __.IsTupleType = (match stripTyparEqns typ with TType_tuple _ -> true | _ -> false)

    member __.NamedEntity = 
        match stripTyparEqns typ with 
        | TType_app (tcref,_) -> FSharpEntity(g, tcref) 
        | TType_measure (MeasureCon tcref) ->  FSharpEntity(g, tcref) 
        | TType_measure (MeasureProd _) ->  FSharpEntity(g, g.measureproduct_tcr) 
        | TType_measure MeasureOne ->  FSharpEntity(g, g.measureone_tcr) 
        | TType_measure (MeasureInv _) ->  FSharpEntity(g, g.measureinverse_tcr) 
        | _ -> invalidOp "not a named type"

    member __.GenericArguments = 
        match stripTyparEqns typ with 
        | TType_app (_,tyargs) 
        | TType_tuple (tyargs) -> (tyargs |> List.map (fun ty -> FSharpType(g, env,ty)) |> makeReadOnlyCollection) 
        | TType_fun(d,r) -> [| FSharpType(g, env,d); FSharpType(g, env,r) |] |> makeReadOnlyCollection
        | TType_measure (MeasureCon _) ->  [| |] |> makeReadOnlyCollection
        | TType_measure (MeasureProd (t1,t2)) ->  [| FSharpType(g, env,TType_measure t1); FSharpType(g, env,TType_measure t2) |] |> makeReadOnlyCollection
        | TType_measure MeasureOne ->  [| |] |> makeReadOnlyCollection
        | TType_measure (MeasureInv t1) ->  [| FSharpType(g, env,TType_measure t1) |] |> makeReadOnlyCollection
        | _ -> invalidOp "not a named type"


    member __.IsFunctionType = (match stripTyparEqns typ with TType_fun _ -> true | _ -> false)

    member __.IsGenericParameter = 
        match stripTyparEqns typ with 
        | TType_var _ -> true 
        | TType_measure (MeasureVar _) -> true 
        | _ -> false

    member __.GenericParameter = 
        match stripTyparEqns typ with 
        | TType_var tp 
        | TType_measure (MeasureVar tp) -> 
            FSharpGenericParameter (g, env, env.Typars |> Array.find (fun tp2 -> typarRefEq tp tp2)) 
        | _ -> invalidOp "not a generic parameter type"

    member __.GenericParameterIndex = 
        match stripTyparEqns typ with 
        | TType_var tp 
        | TType_measure (MeasureVar tp) -> 
            env.Typars |> Array.findIndex (fun tp2 -> typarRefEq tp tp2)
        | _ -> invalidOp "not a generic parameter type"

    member private x.Typ = typ

    override this.Equals(other : obj) =
        box this === other ||
        match other with
        |   :? FSharpType as t -> typeEquiv g typ t.Typ
        |   _ -> false

    override this.GetHashCode() = hash this

    static member op_Equality (left:FSharpType,right:FSharpType) = (left = right)
    static member op_Inequality (left:FSharpType,right:FSharpType) = (left <> right)


and FSharpAttribute(g: TcGlobals, attrib) = 

    let (Attrib(tcref,_kind,unnamedArgs,propVals,_,_,_)) = attrib
    let fail() = failwith "This custom attribute has an argument that can not yet be converted using this API"
    let evalArg e = 
        match e with
        | Expr.Const(c,_,_) -> 
            match c with 
            | Const.Bool b -> box b
            | Const.SByte i  -> box i
            | Const.Int16 i  -> box  i
            | Const.Int32 i   -> box i
            | Const.Int64 i   -> box i  
            | Const.Byte i    -> box i
            | Const.UInt16 i  -> box i
            | Const.UInt32 i  -> box i
            | Const.UInt64 i  -> box i
            | Const.Single i   -> box i
            | Const.Double i -> box i
            | Const.Char i    -> box i
            | Const.Zero -> null
            | Const.String s ->  box s
            | _ -> fail()
        | _ -> fail()

    member __.AttributeType =  FSharpEntity(g, tcref)

    member __.ConstructorArguments = 
        unnamedArgs |> List.map (fun (AttribExpr(_,e)) -> evalArg e) |> makeReadOnlyCollection

    member __.NamedArguments = 
        propVals |> List.map (fun (AttribNamedArg(nm,_,isField,AttribExpr(_, e))) -> (nm, isField, evalArg e)) |> makeReadOnlyCollection

    member private this.Attrib = attrib

    override this.Equals(other : obj) =
        box this === other ||
        match other with
        |   :? FSharpAttribute as other -> attrib === other.Attrib
        |   _ -> false

    override this.GetHashCode() = hash (box attrib)

    static member op_Equality (left:FSharpAttribute,right:FSharpAttribute) = (left = right)
    static member op_Inequality (left:FSharpAttribute,right:FSharpAttribute) = (left <> right)
    
and FSharpParameter(g, env:Env,typ:TType,topArgInfo:ArgReprInfo) = 
    let attribs = topArgInfo.Attribs
    let idOpt = topArgInfo.Name
    member __.Name = match idOpt with None -> null | Some v -> v.idText
    member __.Type = FSharpType(g, env,typ)
    member __.DeclarationLocation = match idOpt with None -> range0 | Some v -> v.idRange
    member __.Attributes = attribs |> List.map (fun a -> FSharpAttribute(g, a)) |> makeReadOnlyCollection
    
    member private x.ValReprInfo = topArgInfo

    override this.Equals(other : obj) =
        box this === other || 
        match other with
        |   :? FSharpParameter as p -> topArgInfo === p.ValReprInfo
        |   _ -> false

    override this.GetHashCode() = hash (box topArgInfo)

    static member op_Equality (left:FSharpParameter,right:FSharpParameter) = (left = right)
    static member op_Inequality (left:FSharpParameter,right:FSharpParameter) = (left <> right)
