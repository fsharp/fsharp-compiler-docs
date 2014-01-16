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
open Microsoft.FSharp.Compiler.Infos
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Nameres
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

    let entityIsUnresolved(entity:EntityRef) = 
        match entity with
        | ERefNonLocal(NonLocalEntityRef(ccu, _)) -> 
            ccu.IsUnresolvedReference && entity.TryDeref.IsNone
        | _ -> false

    let checkEntityIsResolved(entity:EntityRef) = 
        if entityIsUnresolved(entity) then 
            let poorQualifiedName =
                if entity.nlr.AssemblyName = "mscorlib" then 
                    entity.nlr.DisplayName + ", mscorlib"
                else 
                    entity.nlr.DisplayName + ", " + entity.nlr.Ccu.AssemblyName
            invalidOp (sprintf "The entity '%s' does not exist or is in an unresolved assembly." poorQualifiedName)

type Env(typars : Typar list) = 
    let typars = Array.ofList typars
    member __.Typars = typars

// delay the realization of 'item' in case it is unresolved
type FSharpSymbol(g:TcGlobals, item: (unit -> Item)) =
    member x.DeclarationLocation = ItemDescriptionsImpl.rangeOfItem g true (item())
    member x.ImplementationLocation = ItemDescriptionsImpl.rangeOfItem g false (item())
    member internal x.Item = item()
    member x.DisplayName = item().DisplayName(g)
(*
    member x.Kind = 
        match item() with 
        | Item.Value v -> if v.IsModuleBinding then Modu
        | Item.ActivePatternCase apref -> apref.Name
        | Item.UnionCase uinfo -> DecompileOpName uinfo.UnionCase.DisplayName
        | Item.ExnCase tcref -> tcref.LogicalName
        | Item.RecdField rfinfo -> DecompileOpName rfinfo.RecdField.Name
        | Item.NewDef id -> id.idText
        | Item.ILField finfo -> finfo.FieldName
        | Item.Event einfo -> einfo.EventName
        | Item.Property(nm,_) -> nm
        | Item.MethodGroup(nm,_) -> nm
        | Item.CtorGroup(nm,_) -> DemangleGenericTypeName nm
        | Item.FakeInterfaceCtor typ 
        | Item.DelegateCtor typ -> DemangleGenericTypeName (tcrefOfAppTy g typ).LogicalName
        | Item.Types(nm,_) -> DemangleGenericTypeName nm
        | Item.TypeVar nm -> nm
        | Item.ModuleOrNamespaces(modref :: _) ->  modref.DemangledModuleOrNamespaceName
        | Item.ArgName (id,_)  -> id.idText
        | Item.SetterArg (id, _) -> id.idText
        | Item.CustomOperation (customOpName,_,_) -> customOpName
        | Item.CustomBuilder (nm,_) -> nm
*)
        
    override x.ToString() = "symbol " + (try item().DisplayName(g) with _ -> "?")

type FSharpEntity(g:TcGlobals, entity:EntityRef) = 
    inherit FSharpSymbol(g, (fun () -> checkEntityIsResolved(entity); 
                                       if entity.IsModule then Item.ModuleOrNamespaces [entity] 
                                       else Item.UnqualifiedType [entity]))

    // If an entity is in an assembly not available to us in the resolution set,
    // we generally return "false" from predicates like IsClass, since we know
    // nothing about that type.
    let isResolvedAndFSharp() = 
        match entity with
        | ERefNonLocal(NonLocalEntityRef(ccu, _)) -> not ccu.IsUnresolvedReference && ccu.IsFSharp
        | _ -> true

    let isUnresolved() = entityIsUnresolved entity
    let isResolved() = not (isUnresolved())
    let checkIsResolved() = checkEntityIsResolved entity

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

    member __.AccessPath  = 
        checkIsResolved()
        match entity.CompilationPathOpt with 
        | None -> "global" 
        | Some (CompPath(_,[])) -> "global" 
        | Some cp -> buildAccessPath (Some cp)
    
    member __.Namespace  = 
        checkIsResolved()
        match entity.CompilationPathOpt with 
        | None -> None
        | Some (CompPath(_,[])) -> None
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
        isResolvedAndFSharp() && (entity.TypeOrMeasureKind = TyparKind.Measure)

    member __.IsFSharpModule = 
        isResolvedAndFSharp() && entity.IsModule

    member __.HasFSharpModuleSuffix = 
        isResolvedAndFSharp() && entity.IsModule && (entity.ModuleOrNamespaceType.ModuleOrNamespaceKind = ModuleOrNamespaceKind.FSharpModuleWithSuffix)

    member __.IsValueType  = 
        isResolved() &&
        entity.IsStructOrEnumTycon 

    member __.IsProvided  = 
        isResolved() &&
        entity.IsProvided

    member __.IsProvidedAndErased  = 
        isResolved() &&
        entity.IsProvidedErasedTycon

    member __.IsProvidedAndGenerated  = 
        isResolved() &&
        entity.IsProvidedGeneratedTycon

    member __.IsClass = 
        isResolved() &&
        match metadataOfTycon entity.Deref with 
        | ProvidedTypeMetadata info -> info.IsClass
        | ILTypeMetadata (_,td) -> (td.tdKind = ILTypeDefKind.Class)
        | FSharpOrArrayOrByrefOrTupleOrExnTypeMetadata -> entity.Deref.IsFSharpClassTycon

    member __.IsInterface = 
        isResolved() &&
        isInterfaceTyconRef entity

    member __.IsDelegate = 
        isResolved() &&
        match metadataOfTycon entity.Deref with 
        | ProvidedTypeMetadata info -> info.IsDelegate ()
        | ILTypeMetadata (_,td) -> (td.tdKind = ILTypeDefKind.Delegate)
        | FSharpOrArrayOrByrefOrTupleOrExnTypeMetadata -> entity.IsFSharpDelegateTycon

    member __.IsEnum = 
        isResolved() &&
        entity.IsEnumTycon
    
    member __.IsFSharpExceptionDeclaration = 
        isResolvedAndFSharp() && entity.IsExceptionDecl

    member __.IsUnresolved = 
        isUnresolved()

    member __.IsFSharp = 
        isResolvedAndFSharp()

    member __.IsFSharpAbbreviation = 
        isResolvedAndFSharp() && entity.IsTypeAbbrev 

    member __.IsFSharpRecord = 
        isResolvedAndFSharp() && entity.IsRecordTycon

    member __.IsFSharpUnion = 
        isResolvedAndFSharp() && entity.IsUnionTycon

    member __.HasAssemblyCodeRepresentation = 
        isResolvedAndFSharp() && (entity.IsAsmReprTycon || entity.IsMeasureableReprTycon)


    member __.FSharpDelegateSignature =
        checkIsResolved()
        match entity.TypeReprInfo with 
        | TFsObjModelRepr r when entity.IsFSharpDelegateTycon -> 
            match r.fsobjmodel_kind with 
            | TTyconDelegate ss -> FSharpDelegateSignature(g, ss)
            | _ -> invalidOp "not a delegate type"
        | _ -> invalidOp "not a delegate type"
      

    member __.Accessibility = 
        if isUnresolved() then FSharpAccessibility(taccessPublic) else
        FSharpAccessibility(entity.Accessibility) 

    member __.RepresentationAccessibility = 
        if isUnresolved() then FSharpAccessibility(taccessPublic) else
        FSharpAccessibility(entity.TypeReprAccessibility)

    member this.DeclaredInterfaces = 
        if isUnresolved() then makeReadOnlyCollection [] else
        let env = Env(entity.TyparsNoRange)
        entity.ImmediateInterfaceTypesOfFSharpTycon |> List.map (fun ty -> FSharpType(g, env,ty)) |> makeReadOnlyCollection

    member this.BaseType = 
        checkIsResolved()
        let env = Env(entity.TyparsNoRange)
        match entity.TypeContents.tcaug_super with 
        | None -> invalidOp "this entity has no base type"
        | Some ty -> FSharpType(g, env,ty)
        
    member __.UsesPrefixDisplay = 
        if isUnresolved() then true else
        not (isResolvedAndFSharp()) || entity.Deref.IsPrefixDisplay

    member this.MembersOrValues = 
        if isUnresolved() then makeReadOnlyCollection[] else
        ((entity.MembersOfFSharpTyconSorted
            |> List.filter (fun v -> 
                 not v.IsOverrideOrExplicitImpl && 
                 not v.Deref.IsClassConstructor)
            |> List.map (fun v -> FSharpMemberOrVal(g, v)))
        @
            (entity.ModuleOrNamespaceType.AllValsAndMembers
            |> Seq.toList
            |> List.filter (fun v -> v.IsExtensionMember || not v.IsMember) 
            |> List.map (fun v -> FSharpMemberOrVal(g,mkNestedValRef entity v))))
               
            |> makeReadOnlyCollection

    member __.XmlDocSig = 
        checkIsResolved()
        entity.XmlDocSig 

    member __.XmlDoc = 
        if isUnresolved() then XmlDoc.Empty  |> makeXmlDoc else
        entity.XmlDoc |> makeXmlDoc

    member __.NestedEntities = 
        if isUnresolved() then makeReadOnlyCollection[] else
        entity.ModuleOrNamespaceType.AllEntities 
        |> QueueList.toList
        |> List.map (fun x -> FSharpEntity(g, entity.MkNestedTyconRef x))
        |> makeReadOnlyCollection

    member this.UnionCases = 
        if isUnresolved() then makeReadOnlyCollection[] else
        let env = Env(entity.TyparsNoRange)
        entity.UnionCasesAsRefList
        |> List.map (fun x -> FSharpUnionCase(g, env,x)) 
        |> makeReadOnlyCollection

    member this.RecordFields =
        if isUnresolved() then makeReadOnlyCollection[] else

        let env = Env(entity.TyparsNoRange)
        
        entity.AllFieldsAsList
        |> List.map (fun x -> FSharpRecordField(g, env, RecdFieldData.Recd (mkRecdFieldRef entity x.Name)))
        |> makeReadOnlyCollection

    member this.AbbreviatedType   = 
        checkIsResolved()

        let env = Env(entity.TyparsNoRange)
        
        match entity.TypeAbbrev with
        | None -> invalidOp "not a type abbreviation"
        | Some ty -> FSharpType(g, env, ty)

    member __.Attributes = 
        if isUnresolved() then makeReadOnlyCollection[] else
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

    override x.ToString() = x.CompiledName

and FSharpUnionCase(g:TcGlobals, env:Env, v: UnionCaseRef) =
    inherit FSharpSymbol (g, (fun () -> checkEntityIsResolved v.TyconRef
                                        Item.UnionCase(UnionCaseInfo(generalizeTypars v.TyconRef.TyparsNoRange,v))))

    let isUnresolved() = 
        entityIsUnresolved v.TyconRef || v.TryUnionCase.IsNone 
    let checkIsResolved() = 
        checkEntityIsResolved v.TyconRef
        if v.TryUnionCase.IsNone then 
            invalidOp (sprintf "The union case '%s' could not be found in the target type" v.CaseName)

    member __.Name = 
        checkIsResolved()
        v.UnionCase.DisplayName

    member __.DeclarationLocation = 
        checkIsResolved()
        v.Range

    member __.UnionCaseFields = 
        if isUnresolved() then makeReadOnlyCollection [] else
        v.UnionCase.RecdFields |> List.mapi (fun i _ ->  FSharpRecordField(g, env, RecdFieldData.Union (v, i))) |> List.toArray |> makeReadOnlyCollection

    member __.ReturnType = 
        checkIsResolved()
        FSharpType(g, env, v.ReturnType)

    member __.CompiledName = 
        checkIsResolved()
        v.UnionCase.CompiledName

    member __.XmlDocSig = 
        checkIsResolved()
        v.UnionCase.XmlDocSig

    member __.XmlDoc = 
        if isUnresolved() then XmlDoc.Empty  |> makeXmlDoc else
        v.UnionCase.XmlDoc |> makeXmlDoc

    member __.Attributes = 
        if isUnresolved() then makeReadOnlyCollection [] else
        v.Attribs |> List.map (fun a -> FSharpAttribute(g, a)) |> makeReadOnlyCollection

    member __.Accessibility =  
        if isUnresolved() then FSharpAccessibility(taccessPublic) else
        FSharpAccessibility(v.UnionCase.Accessibility)

    member private this.V = v
    override this.Equals(other : obj) =
        box this === other ||
        match other with
        |   :? FSharpUnionCase as uc -> v === uc.V
        |   _ -> false
    
    override this.GetHashCode() = hash v.CaseName

    override x.ToString() = x.CompiledName


and RecdFieldData = 
    | Recd of RecdFieldRef
    | Union of UnionCaseRef * int
    member x.RecdField =
        match x with 
        | Recd v -> v.RecdField
        | Union (v,n) -> v.FieldByIndex(n)

and FSharpRecordField(g:TcGlobals, env:Env, d: RecdFieldData) =
    inherit FSharpSymbol (g, (fun () -> 
             match d with 
             | Recd v -> 
                 checkEntityIsResolved v.TyconRef
                 Item.RecdField(RecdFieldInfo(generalizeTypars v.TyconRef.TyparsNoRange,v))
             | Union (v,_) -> 
                 // This is not correct: there is no "Item" for a named union case field
                 Item.UnionCase(UnionCaseInfo(generalizeTypars v.TyconRef.TyparsNoRange,v))

             ))
    let isUnresolved() = 
        match d with 
        | Recd v -> entityIsUnresolved v.TyconRef || v.TryRecdField.IsNone 
        | Union (v,_) -> entityIsUnresolved v.TyconRef || v.TryUnionCase.IsNone 

    let checkIsResolved() = 
        match d with 
        | Recd v -> 
            checkEntityIsResolved v.TyconRef
            if v.TryRecdField.IsNone then 
                invalidOp (sprintf "The record field '%s' could not be found in the target type" v.FieldName)
        | Union (v,_) -> 
            checkEntityIsResolved v.TyconRef
            if v.TryUnionCase.IsNone then 
                invalidOp (sprintf "The union case '%s' could not be found in the target type" v.CaseName)

    member __.IsMutable = 
        if isUnresolved() then false else 
        d.RecdField.IsMutable

    member __.XmlDocSig = 
        checkIsResolved()
        d.RecdField.XmlDocSig

    member __.XmlDoc = 
        if isUnresolved() then XmlDoc.Empty  |> makeXmlDoc else
        d.RecdField.XmlDoc |> makeXmlDoc

    member __.FieldType = 
        checkIsResolved()
        FSharpType(g, env, d.RecdField.FormalType)

    member __.IsStatic = 
        if isUnresolved() then false else 
        d.RecdField.IsStatic

    member __.Name = 
        checkIsResolved()
        d.RecdField.Name

    member __.IsCompilerGenerated = 
        if isUnresolved() then false else 
        d.RecdField.IsCompilerGenerated

    member __.DeclarationLocation = 
        checkIsResolved()
        d.RecdField.Range

    member __.FieldAttributes = 
        if isUnresolved() then makeReadOnlyCollection [] else 
        d.RecdField.FieldAttribs |> List.map (fun a -> FSharpAttribute(g, a)) |> makeReadOnlyCollection

    member __.PropertyAttributes = 
        if isUnresolved() then makeReadOnlyCollection [] else 
        d.RecdField.PropertyAttribs |> List.map (fun a -> FSharpAttribute(g, a)) |> makeReadOnlyCollection

    member __.Accessibility =  
        if isUnresolved() then FSharpAccessibility(taccessPublic) else 
        FSharpAccessibility(d.RecdField.Accessibility) 

    member private this.V = d
    override this.Equals(other : obj) =
        box this === other ||
        match other with
        |   :? FSharpRecordField as uc -> 
            match d, uc.V with 
            | Recd r1, Recd r2 -> recdFieldRefOrder.Compare(r1, r2) = 0
            | Union (u1,n1), Union (u2,n2) -> g.unionCaseRefEq u1 u2 && n1 = n2
            | _ -> false
        |   _ -> false

    override x.GetHashCode() = hash x.Name
    override x.ToString() = "entity " + x.Name


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
    override x.ToString() = match a with Public -> "public" | Internal -> "internal" | Private -> "private"

and FSharpGenericParameter(g:TcGlobals, env:Env, v:Typar) = 

    inherit FSharpSymbol (g, (fun () -> Item.TypeVar(v.Name)))
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

    override x.ToString() = "generic parameter " + x.Name

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
    override this.ToString() = "<delegate signature>"

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
    override this.ToString() = "<member constraint info>"


and FSharpGenericParameterDelegateConstraint(g: TcGlobals, env : Env, tupledArgTyp: TType, rty: TType) = 
    member __.DelegateTupledArgumentType = FSharpType(g, env,tupledArgTyp)
    member __.DelegateReturnType =  FSharpType(g, env,rty)
    override this.ToString() = "<delegate constraint info>"

and FSharpGenericParameterDefaultsToConstraint(g: TcGlobals, env : Env, pri:int, ty:TType) = 
    member __.DefaultsToPriority = pri 
    member __.DefaultsToTarget = FSharpType(g, env, ty) 
    override this.ToString() = "<defaults-to constraint info>"

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
        | _ -> invalidOp "not a delegate constraint"

    override this.ToString() = "<type constraint>"

and FSharpInlineAnnotation = 
   | PsuedoValue 
   | AlwaysInline 
   | OptionalInline 
   | NeverInline 

and FSharpMemberOrVal(g:TcGlobals,v:ValRef) = 

    inherit FSharpSymbol (g, (fun () -> Item.Value(v)))

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

    member __.EnclosingEntity = 
        checkIsResolved()
        match v.ActualParent with 
        | ParentNone -> invalidOp "the value or member doesn't have an enclosing entity" 
        | Parent p -> FSharpEntity(g, p)

    member __.IsCompilerGenerated = 
        if isUnresolved() then false else 
        v.IsCompilerGenerated

    member __.InlineAnnotation = 
        if isUnresolved() then FSharpInlineAnnotation.OptionalInline else 
        match v.InlineInfo with 
        | ValInline.PseudoVal -> FSharpInlineAnnotation.PsuedoValue
        | ValInline.Always -> FSharpInlineAnnotation.AlwaysInline
        | ValInline.Optional -> FSharpInlineAnnotation.OptionalInline
        | ValInline.Never -> FSharpInlineAnnotation.NeverInline

    member __.IsMutable = 
        if isUnresolved() then false else 
        v.IsMutable

    member __.IsModuleValueOrMember = 
        if isUnresolved() then false else 
        v.IsMember || v.IsModuleBinding

    member __.IsMember = 
        if isUnresolved() then false else 
        v.IsMember 
    
    member __.IsDispatchSlot = 
        if isUnresolved() then false else 
        v.IsDispatchSlot

    member __.IsGetterMethod = 
        if isUnresolved() then false else 
        match v.MemberInfo with 
        | None -> false 
        | Some memInfo -> memInfo.MemberFlags.MemberKind = MemberKind.PropertyGet

    member __.IsSetterMethod = 
        if isUnresolved() then false else 
        match v.MemberInfo with 
        | None -> false 
        | Some memInfo -> memInfo.MemberFlags.MemberKind = MemberKind.PropertySet

    member __.IsInstanceMember = 
        if isUnresolved() then false else 
        v.IsInstanceMember

    member __.IsExtensionMember = 
        if isUnresolved() then false else 
        v.IsExtensionMember

    member __.IsImplicitConstructor = 
        if isUnresolved() then false else 
        v.IsIncrClassConstructor
    
    member __.IsTypeFunction = 
        if isUnresolved() then false else 
        v.IsTypeFunction

    member __.IsActivePattern =  
        if isUnresolved() then false else 
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
        if isUnresolved() then XmlDoc.Empty  |> makeXmlDoc else
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
        if isUnresolved() then makeReadOnlyCollection [] else 
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
        if isUnresolved() then FSharpAccessibility(taccessPublic) else 
        FSharpAccessibility(v.Accessibility)

    member private this.V = v

    override this.Equals(other : obj) =
        box this === other ||
        match other with
        |   :? FSharpMemberOrVal as other -> v === other.V
        |   _ -> false

    override this.GetHashCode() = hash (box v)
    override this.ToString() = try  (if v.IsMember then "member " else "val ") + v.DisplayName with _  -> "??"

and FSharpType(g:TcGlobals, env:Env, typ:TType) =
    let protect f = 
       ErrorLogger.protectAssemblyExplorationF  (fun () -> invalidOp "The type could not be resolved due to a missing assembly reference") f

    new (g:TcGlobals, typ:TType) = FSharpType(g, Env [], typ)

    member __.IsNamedType = 
       protect <| fun () -> 
         match stripTyparEqns typ with 
         | TType_app _ | TType_measure (MeasureCon _ | MeasureProd _ | MeasureInv _ | MeasureOne _) -> true 
         | _ -> false

    member __.IsTupleType = 
       protect <| fun () -> 
        match stripTyparEqns typ with 
        | TType_tuple _ -> true 
        | _ -> false

    member __.NamedEntity = 
       protect <| fun () -> 
        match stripTyparEqns typ with 
        | TType_app (tcref,_) -> FSharpEntity(g, tcref) 
        | TType_measure (MeasureCon tcref) ->  FSharpEntity(g, tcref) 
        | TType_measure (MeasureProd _) ->  FSharpEntity(g, g.measureproduct_tcr) 
        | TType_measure MeasureOne ->  FSharpEntity(g, g.measureone_tcr) 
        | TType_measure (MeasureInv _) ->  FSharpEntity(g, g.measureinverse_tcr) 
        | _ -> invalidOp "not a named type"

    member __.GenericArguments = 
       protect <| fun () -> 
        match stripTyparEqns typ with 
        | TType_app (_,tyargs) 
        | TType_tuple (tyargs) -> (tyargs |> List.map (fun ty -> FSharpType(g, env,ty)) |> makeReadOnlyCollection) 
        | TType_fun(d,r) -> [| FSharpType(g, env,d); FSharpType(g, env,r) |] |> makeReadOnlyCollection
        | TType_measure (MeasureCon _) ->  [| |] |> makeReadOnlyCollection
        | TType_measure (MeasureProd (t1,t2)) ->  [| FSharpType(g, env,TType_measure t1); FSharpType(g, env,TType_measure t2) |] |> makeReadOnlyCollection
        | TType_measure MeasureOne ->  [| |] |> makeReadOnlyCollection
        | TType_measure (MeasureInv t1) ->  [| FSharpType(g, env,TType_measure t1) |] |> makeReadOnlyCollection
        | _ -> invalidOp "not a named type"

    member __.IsFunctionType = 
       protect <| fun () -> 
        match stripTyparEqns typ with 
        | TType_fun _ -> true 
        | _ -> false

    member __.IsGenericParameter = 
       protect <| fun () -> 
        match stripTyparEqns typ with 
        | TType_var _ -> true 
        | TType_measure (MeasureVar _) -> true 
        | _ -> false

    member __.GenericParameter = 
       protect <| fun () -> 
        match stripTyparEqns typ with 
        | TType_var tp 
        | TType_measure (MeasureVar tp) -> 
            FSharpGenericParameter (g, env, env.Typars |> Array.find (fun tp2 -> typarRefEq tp tp2)) 
        | _ -> invalidOp "not a generic parameter type"

    member __.GenericParameterIndex = 
       protect <| fun () -> 
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

    override this.ToString() = 
       protect <| fun () -> 
        "type " + NicePrint.stringOfTy (DisplayEnv.Empty(g)) typ 

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

    member __.AttributeType =  
        checkEntityIsResolved tcref
        FSharpEntity(g, tcref)

    member __.ConstructorArguments = 
        checkEntityIsResolved tcref
        unnamedArgs |> List.map (fun (AttribExpr(_,e)) -> evalArg e) |> makeReadOnlyCollection

    member __.NamedArguments = 
        checkEntityIsResolved tcref
        propVals |> List.map (fun (AttribNamedArg(nm,_,isField,AttribExpr(_, e))) -> (nm, isField, evalArg e)) |> makeReadOnlyCollection

    override this.ToString() = 
        if entityIsUnresolved tcref then "attribute ???" else "attribute " + tcref.CompiledName + "(...)" 

    
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

type FSharpAssemblySignature internal (g: TcGlobals, mtyp: ModuleOrNamespaceType) = 

    member __.Entities = 

        let rec loop (rmtyp : ModuleOrNamespaceType) = 
            [| for entity in rmtyp.AllEntities do
                   if entity.IsNamespace then 
                       yield! loop entity.ModuleOrNamespaceType
                   else 
                       yield FSharpEntity(g, mkLocalEntityRef entity) |]
        
        loop mtyp |> makeReadOnlyCollection

    override this.ToString() = "<assembly signature>"

type FSharpAssembly internal (g: TcGlobals, ccu: CcuThunk) = 

    member __.RawCcuThunk = ccu
    member __.QualifiedName = match ccu.QualifiedName with None -> "" | Some s -> s
    member __.CodeLocation = ccu.SourceCodeDirectory
    member __.FileName = ccu.FileName
    member __.SimpleName = ccu.AssemblyName 
    member __.IsProviderGenerated = ccu.IsProviderGenerated
    member __.Contents = FSharpAssemblySignature(g, ccu.Contents.ModuleOrNamespaceType)
                 
    override this.ToString() = this.QualifiedName

type FSharpSymbol with 
    // TODO: there are several cases where we may need to report more interesting
    // symbol information below. By default we return a vanilla symbol.
    static member Create(g, item) : FSharpSymbol = 
        let dflt = FSharpSymbol(g, (fun () -> item)) 
        match item with 
        | Item.Value v -> FSharpMemberOrVal(g, v) :> _
        | Item.UnionCase uinfo -> FSharpUnionCase(g,Env(uinfo.TyconRef.TyparsNoRange),uinfo.UnionCaseRef) :> _
        | Item.ExnCase tcref -> FSharpEntity(g, tcref) :>_
        | Item.RecdField rfinfo -> FSharpRecordField(g,Env(rfinfo.TyconRef.TyparsNoRange),Recd rfinfo.RecdFieldRef) :> _
        
        | Item.Event einfo -> 
            match einfo.ArbitraryValRef with 
            | Some vref ->  FSharpMemberOrVal(g, vref) :> _
            | None -> dflt 
            
        | Item.Property(_,pinfo :: _) -> 
            match pinfo.ArbitraryValRef with 
            | Some vref ->  FSharpMemberOrVal(g, vref) :> _
            | None -> dflt 
            
        | Item.MethodGroup(_,minfo :: _) -> 
            match minfo.ArbitraryValRef with 
            | Some vref ->  FSharpMemberOrVal(g, vref) :> _
            | None -> dflt

        | Item.CtorGroup(_,cinfo :: _) -> 
            match cinfo.ArbitraryValRef with 
            | Some vref ->  FSharpMemberOrVal(g, vref) :> _
            | None -> dflt 

        | Item.DelegateCtor typ -> 
            FSharpEntity(g, tcrefOfAppTy g typ) :>_ 

        | Item.Types(_,typ :: _) -> 
            if isAppTy g  typ then FSharpEntity(g, tcrefOfAppTy g typ) :>_  else dflt

        | Item.ModuleOrNamespaces(modref :: _) ->  
            FSharpEntity(g, modref) :> _

        | Item.SetterArg (_id, item) -> FSharpSymbol.Create(g,item)
        | Item.CustomOperation (_customOpName,_, Some minfo) -> 
            match minfo.ArbitraryValRef with 
            | Some vref ->  FSharpMemberOrVal(g, vref) :> _
            | None -> dflt

        | Item.CustomBuilder (_,vref) -> 
            FSharpMemberOrVal(g, vref) :> _

        // TODO: the following don't currently return any interesting subtype
        | Item.ArgName _  
        | Item.TypeVar _
        | Item.ActivePatternCase _
        | Item.ILField _ 
        | Item.FakeInterfaceCtor _
        | Item.NewDef _ -> dflt
        | _ ->  dflt
