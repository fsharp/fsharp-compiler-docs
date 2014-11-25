// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

module internal Microsoft.FSharp.Compiler.AbstractIL.Internal.Pdb 

open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL.IL

open System.Runtime.InteropServices
open System.Security

// -------------------------------------------------------------------- 
// PDB data
// --------------------------------------------------------------------  

type PdbDocumentData = ILSourceDocument

type PdbLocalVar = 
    { Name: string;
      Signature: byte[]; 
      /// the local index the name corresponds to
      Index: int32  }

type PdbMethodScope = 
    { Children: PdbMethodScope array;
      StartOffset: int;
      EndOffset: int;
      Locals: PdbLocalVar array;
      (* REVIEW open_namespaces: pdb_namespace array; *) }

type PdbSourceLoc = 
    { Document: int;
      Line: int;
      Column: int; }
      
type PdbSequencePoint = 
    { Document: int;
      Offset: int;
      Line: int;
      Column: int;
      EndLine: int;
      EndColumn: int; }
    override x.ToString() = sprintf "(%d,%d)-(%d,%d)" x.Line x.Column x.EndLine x.EndColumn

type PdbMethodData = 
    { MethToken: int32;
      MethName:string;
      Params: PdbLocalVar array;
      RootScope: PdbMethodScope;
      Range: (PdbSourceLoc * PdbSourceLoc) option;
      SequencePoints: PdbSequencePoint array; }

module SequencePoint = 
    let orderBySource sp1 sp2 = 
        let c1 = compare sp1.Document sp2.Document
        if c1 <> 0 then c1 else 
        let c1 = compare sp1.Line sp2.Line
        if c1 <> 0 then c1 else 
        compare sp1.Column sp2.Column 
        
    let orderByOffset sp1 sp2 = 
        compare sp1.Offset sp2.Offset 

/// 28 is the size of the IMAGE_DEBUG_DIRECTORY in ntimage.h 
let sizeof_IMAGE_DEBUG_DIRECTORY = 28 

[<NoEquality; NoComparison>]
type PdbData = 
    { EntryPoint: int32 option;
      // MVID of the generated .NET module (used by MDB files to identify debug info)
      ModuleID: byte[];
      Documents: PdbDocumentData[];
      Methods: PdbMethodData[] }

// COM interfaces
type u32 = uint32 // helper shortcut
type sb = System.Text.StringBuilder

[<Struct>]
type COR_FIELD_OFFSET =
    val RidOfField : u32
    val UlOffset : u32

[<ComImport; Interface>]
[<InterfaceType(ComInterfaceType.InterfaceIsIUnknown); Guid("BA3FEE4C-ECB9-4E41-83B7-183FA41CD859")>]
type IMetadataEmit =
    abstract Placeholder : unit -> unit

[<ComImport; Interface>]
[<InterfaceType(ComInterfaceType.InterfaceIsIUnknown); Guid("7DAC8207-D3AE-4c75-9B67-92801A497D44")>]
[<SuppressUnmanagedCodeSecurity>]
type IMetadataImport =
    [<PreserveSig>]
    abstract CloseEnum : handleEnum: u32 -> unit
    abstract CountEnum : handleEnum: u32 -> u32
    abstract ResetEnum : handleEnum: u32 * ulongPos: u32 -> unit
    abstract EnumTypeDefs : 
        handlePointerEnum : byref<u32> *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2s)>] arrayTypeDefs : u32 array *
        countMax : u32 -> u32
    abstract EnumInterfaceImpls :
        handlePointerEnum : byref<u32> *
        td : u32 *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3s)>] arrayTypeDefs : u32 array *
        countMax : u32 -> u32
    abstract EnumTypeRefs :
        handlePointerEnum : byref<u32> *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2s)>] arrayTypeDefs : u32 array *
        countMax : u32 -> u32
    abstract FindTypeDefByName :
        stringTypeDef : string *
        tokenEnclosingClass : u32 -> u32
    abstract GetScopeProps :
        stringName : sb *
        cchName : u32 *
        [<Out>] pchName : byref<u32> -> Guid
    abstract GetModuleFromScope : unit -> u32
    abstract GetTypeDefProps :
        td : u32 *
        stringTypeDef : nativeint *
        cchTypeDef : u32 *
        [<Out>] pchTypeDef : byref<u32> *
        pdwTypeDefFlags : nativeint -> u32
    abstract GetInterfaceImplProps :
        impl : u32 *
        [<Out>] pointerClass : byref<u32> -> u32
    abstract GetTypeRefProps :
        tr : u32 *
        [<Out>] ptkResolutionScope : byref<u32> *
        stringName : sb *
        cchName : u32 -> u32
    abstract ResolveTypeRef :
        tr : u32 *
        [<In>] riid : byref<Guid> *
        [<Out; MarshalAs(UnmanagedType.Interface)>] ppIScope : byref<obj> -> u32
    abstract EnumMembers :
        handlePointerEnum : byref<u32> *
        cl : u32 *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3s)>] arrayMembers : u32 array *
        countMax : u32 -> u32
    abstract EnumMembersWithName :
        handlePointerEnum : byref<u32> *
        cl : u32 *
        stringName : string *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4s)>] arrayMembers : u32 array *
        countMax : u32 -> u32
    abstract EnumMethods :
        handlePointerEnum : byref<u32> *
        cl : u32 *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3s)>] arrayMethods : u32 array *
        countMax : u32 -> u32
    abstract EnumMethodsWithName :
        handlePointerEnum : byref<u32> *
        cl : u32 *
        stringName : string *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4s)>] arrayMethods : u32 array *
        countMax : u32 -> u32
    abstract EnumFields :
        handlePointerEnum : byref<u32> *
        cl : u32 *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3s)>] arrayFields : u32 array *
        countMax : u32 -> u32
    abstract EnumFieldsWithName :
        handlePointerEnum : byref<u32> *
        cl : u32 *
        stringName : string *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4s)>] arrayFields : u32 array *
        countMax : u32 -> u32
    abstract EnumParams :
        handlePointerEnum : byref<u32> *
        mb : u32 *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3s)>] arrayParams : u32 array *
        countMax : u32 -> u32
    abstract EnumMemberRefs :
        handlePointerEnum : byref<u32> *
        tokenParent : u32 *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3s)>] arrayMemberRefs : u32 array *
        countMax : u32 -> u32
    abstract EnumMethodImpls :
        handlePointerEnum : byref<u32> *
        td : u32 *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4s)>] arrayMethodBody : u32 array *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4s)>] arrayMethodDecl : u32 array *
        countMax : u32 -> u32
    abstract EnumPermissionSets :
        handlePointerEnum : byref<u32> *
        tk : u32 *
        dwordActions : u32 *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4s)>] arrayPermission : u32 array *
        countMax : u32 -> u32
    abstract FindMember :
        td : u32 *
        stringName : string *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3s)>] voidPointerSigBlob : byte array *
        byteCountSigBlob : u32 -> u32
    abstract FindMethod :
        td : u32 *
        stringName : string *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3s)>] voidPointerSigBlob : byte array *
        byteCountSigBlob : u32 -> u32
    abstract FindField :
        td : u32 *
        stringName : string *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3s)>] voidPointerSigBlob : byte array *
        byteCountSigBlob : u32 -> u32
    abstract FindMemberRef :
        td : u32 *
        stringName : string *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3s)>] voidPointerSigBlob : byte array *
        byteCountSigBlob : u32 -> u32
    abstract GetMethodProps :
        mb : u32 *
        [<Out>] pointerClass : byref<u32> *
        stringMethod : nativeint *
        cchMethod : u32 *
        [<Out>] pchMethod : byref<u32> *
        pdwAttr : nativeint *
        ppvSigBlob : nativeint *
        pcbSigBlob : nativeint *
        pulCodeRVA : nativeint -> u32
    abstract GetMemberRefProps :
        mr : u32 *
        ptk : byref<u32> *
        stringMember : sb *
        cchMember : u32 *
        [<Out>] pchMember : byref<u32> *
        [<Out>] ppvSigBlob : byref<nativeint> -> u32 // nativeint = byte*
    abstract EnumProperties :
        handlePointerEnum : byref<u32> *
        td : u32 *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3s)>] arrayProperties : u32 array *
        maxCount : u32 -> u32
    abstract EnumEvents :
        handlePointerEnum : byref<u32> *
        td : u32 *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3s)>] arrayEvents : u32 array *
        maxCount : u32 -> u32
    abstract GetEventProps :
        ev : u32 *
        [<Out>] pointerClass : byref<u32> *
        stringEvent : sb *
        cchEvent : u32 *
        [<Out>] pchEvent : byref<u32> *
        [<Out>] pdwEventFlags : byref<u32> *
        [<Out>] ptkEventType : byref<u32> *
        [<Out>] pmdAddOn : byref<u32> *
        [<Out>] pmdRemoveOn : byref<u32> *
        [<Out>] pmdFire : byref<u32> *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 11s)>] rmdOtherMethod : u32 array *
        countMax : u32 -> u32
    abstract EnumMethodSemantics :
        handlePointerEnum : byref<u32> *
        mb : u32 *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3s)>] arrayEventProp : u32 array *
        maxCount : u32 -> u32
    abstract GetMethodSemantics :
        mb : u32 *
        tokenEventProp : u32 -> u32
    abstract GetClassLayout :
        td : u32 *
        [<Out>] pdwPackSize : byref<u32> *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3s)>] arrayFieldOffset : COR_FIELD_OFFSET array *
        countMax : u32 *
        [<Out>] countPointerFieldOffset : byref<u32> -> u32
    abstract GetFieldMarshal :
        tk : u32 *
        [<Out>] ppvNativeType : byref<nativeint> -> u32 // nativeint = byte*
    abstract GetRVA :
        tk : u32 *
        [<Out>] pulCodeRVA : byref<u32> -> u32
    abstract GetPermissionSetProps :
        pm : u32 *
        [<Out>] pdwAction : byref<u32> *
        [<Out>] ppvPermission : byref<nativeint> -> u32
    abstract GetSigFromToken :
        memberDefSig : u32 *
        [<Out>] ppvSig : byref<nativeint> *
        [<Out>] pcbSig : byref<u32> -> u32
    abstract GetModuleRefProps :
        mur : u32 *
        stringName : sb *
        cchName : u32 -> u32
    abstract EnumModuleRefs :
        handlePointerEnum : byref<u32> *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2s)>] arrayModuleRefs : u32 array *
        countMax : u32 -> u32
    abstract GetTypeSpecFromToken :
        typespec : u32 *
        [<Out>] pvvSig : byref<nativeint> -> u32 // nativeint = byte*
    abstract GetNameFromToken :
        tk : u32 -> u32
    abstract EnumUnresolvedMethods :
        handlePointerEnum : byref<u32> *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2s)>] arrayMethods : u32 array *
        countMax : u32 -> u32
    abstract GetUserString :
        stk : u32 *
        stringString : sb *
        cchString : u32 -> u32
    abstract GetPinvokeMap :
        tk : u32 *
        [<Out>] pdwMappingFlags : byref<u32> *
        stringImportName : sb *
        cchImportName : u32 *
        [<Out>] pchImportName : byref<u32> -> u32
    abstract EnumSignatures :
        handlePointerEnum : byref<u32> *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2s)>] arraySignatures : u32 array *
        countMax : u32 -> u32
    abstract EnumTypeSpecs :
        handlePointerEnum : byref<u32> *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2s)>] arraySignatures : u32 array *
        countMax : u32 -> u32
    abstract EnumUserStrings :
        handlePointerEnum : byref<u32> *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2s)>] arraySignatures : u32 array *
        countMax : u32 -> u32
    [<PreserveSig>]
    abstract GetParamForMethodIndex :
        md : u32 *
        ulongParamSeq : u32 *
        [<Out>] pointerParam : byref<u32> -> int32
    abstract EnumCustomAttributes :
        handlePointerEnum : byref<u32> *
        tk : u32 *
        tokenType: u32 *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4s)>] arrayCustomAttributes : u32 array *
        countMax : u32 -> u32
    abstract GetCustomAttributeProps :
        cv : u32 *
        [<Out>] ptkObj : byref<u32> *
        [<Out>] ptkType : byref<u32> *
        [<Out>] ppBlob : byref<nativeint> -> u32 // nativeint = byte*
    abstract FindTypeRef :
        tokenResolutionScope : u32 *
        stringName : string -> u32
    abstract GetMemberProps :
        mb : u32 *
        [<Out>] pointerClass : byref<u32> *
        stringMember : sb *
        cchMember : u32 *
        [<Out>] pchMember : byref<u32> *
        [<Out>] pdwAttr : byref<u32> *
        [<Out>] ppvSigBlob : byref<nativeint> * // nativeint = byte*
        [<Out>] pcbSigBlob : byref<u32> *
        [<Out>] pulCodeRVA : byref<u32> *
        [<Out>] pdwImplFlags : byref<u32> *
        [<Out>] pdwCPlusTypeFlag : byref<u32> *
        [<Out>] ppValue : byref<nativeint> -> u32 // nativeint = void*
    abstract GetFieldProps :
        mb : u32 *
        [<Out>] pointerClass : byref<u32> *
        stringField : sb *
        cchField : u32 *
        [<Out>] pchField : byref<u32> *
        [<Out>] pdwAttr : byref<u32> *
        [<Out>] ppvSigBlob : byref<nativeint> * // nativeint = byte*
        [<Out>] pcbSigBlob : byref<u32> *
        [<Out>] pdwCPlusTypeFlag : byref<u32> *
        [<Out>] ppValue : byref<nativeint> -> u32 // nativeint = void*
    abstract GetPropertyProps :
        prop : u32 *
        [<Out>] pointerClass : byref<u32> *
        stringProperty : sb *
        cchProperty : u32 *
        [<Out>] pchProperty : byref<u32> *
        [<Out>] pdwPropFlags : byref<u32> *
        [<Out>] ppvSig : byref<nativeint> * // nativeint = byte*
        [<Out>] bytePointerSig : byref<u32> *
        [<Out>] pdwCPlusTypeFlag : byref<u32> *
        [<Out>] ppDefaultValue : byref<nativeint> * // nativeint = void*
        [<Out>] pcchDefaultValue : byref<u32> *
        [<Out>] pmdSetter : byref<u32> *
        [<Out>] pmdGetter : byref<u32> *
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 14s)>] rmdOtherMethod : u32 array *
        countMax : u32 -> u32
    abstract GetParamProps :
        tk : u32 *
        [<Out>] pmd : byref<u32> *
        [<Out>] pulSequence : byref<u32> *
        stringName : sb *
        cchName : u32 *
        [<Out>] pchName : byref<u32> *
        [<Out>] pdwAttr : byref<u32> *
        [<Out>] pdwCPlusTypeFlag : byref<u32> *
        [<Out>] ppValue : byref<nativeint> -> u32 // nativeint = void*
    abstract GetCustomAttributeByName :
        tokenObj : u32 *
        stringName : string *
        [<Out>] ppData : byref<nativeint> -> u32 // nativeint = void*
    [<PreserveSig>]
    abstract IsValidToken : tk : u32 -> [<return: MarshalAs(UnmanagedType.Bool)>] bool
    abstract GetNestedClassProps : typeDefNestedClass : u32 -> u32
    abstract GetNativeCallConvFromSig :
        voidPointerSig : nativeint * // nativeint = void*
        byteCountSig : u32 -> u32
    abstract IsGlobal : pd : u32 -> int32

let inline debug () =
    match System.Diagnostics.Debugger.IsAttached with
    | true -> System.Diagnostics.Debugger.Break ()
    | false -> System.Diagnostics.Debugger.Launch () |> ignore
let inline ni () = 
    debug ()
    raise (new System.NotImplementedException ())

let ofNativeInt = Microsoft.FSharp.NativeInterop.NativePtr.ofNativeInt
let nAdd = Microsoft.FSharp.NativeInterop.NativePtr.add
let nWrite = Microsoft.FSharp.NativeInterop.NativePtr.write

// The emit interface is only needed because the unmanaged pdb writer does a QueryInterface for it and fails if the wrapper does not implement it.
// None of its methods are called.
type PdbMetadataWrapper (info: PdbData) =
//    let mutable lastTypeDef = 0u
//    let mutable lastTypeDefName = ""

    interface IMetadataEmit with
        member x.Placeholder () = ni ()

    interface IMetadataImport with
        // The only purpose of this method is to get type name and "is nested" flag, everything else is ignored by the SymWriter.
        // "td" is token returned by GetMethodProps or GetNestedClassProps
        member x.GetTypeDefProps (td, stringTypeDef, cchTypeDef, pchTypeDef, pdwTypeDefFlags) = 
            match td with 
            | 0u -> 0u
            | _ -> ni ()
        
        // The only purpose of this method is to get type name of the method and declaring type token (opaque for SymWriter), everything else is ignored by the SymWriter.
        // "mb" is the token passed to OpenMethod. The token is remembered until the corresponding CloseMethod, which passes it to GetMethodProps.
        // It's opaque for SymWriter.
        member x.GetMethodProps (mb, pointerClass, stringMethod, cchMethod, pchMethod, pdwAttr, ppvSigBlog, pcbSigBlob, pulCodeRVA) =
            printfn "get props for method %d" (int32 mb)
            let meth = 
                match info.Methods |> Array.tryFind (fun m -> m.MethToken = int32 mb) with
                | Some m -> m
                | None -> ni ()
            pchMethod <- 0u
            // This should be the class token....
            pointerClass <- 0u
            
            let methName = meth.MethName
            pchMethod <- uint32 methName.Length
            if pchMethod > cchMethod then pchMethod <- cchMethod - 1u

            let pointerMethName = stringMethod |> ofNativeInt
            for i = 0 to (int32 pchMethod) - 1 do
                nWrite (nAdd pointerMethName i) (methName.[i])
            nWrite (nAdd pointerMethName (int32 pchMethod)) (char 0)
            0u


        member x.GetNestedClassProps (typeDefNestedClass) = ni ()

        [<PreserveSig>]
        member x.CloseEnum (handleEnum) = ni ()
        member x.CountEnum (handleEnum) = ni ()
        member x.ResetEnum (handleEnum, ulongPos) = ni ()
        member x.EnumTypeDefs (handlePointEnum, arrayTypeDefs, countMax) = ni ()
        member x.EnumInterfaceImpls (handlePointerEnum, td, arrayImpls, countMax) = ni ()
        member x.EnumTypeRefs (handlePointerEnum, arrayTypeRefs, countMax) = ni ()
        member x.FindTypeDefByName (stringTypeDef, tokenEnclosingClass) = ni ()
        member x.GetScopeProps (stringName, cchName, pchName) = ni ()
        member x.GetModuleFromScope () = ni ()
        member x.GetInterfaceImplProps (impl, pointerClass) = ni ()
        member x.GetTypeRefProps (tr, ptkResolutionScope, stringName, cchName) = ni ()
        member x.ResolveTypeRef (tr, riid, ppIScope) = ni ()
        member x.EnumMembers (handlePointerEnum, cl, arrayMembers, countMax) = ni ()
        member x.EnumMembersWithName (handlePointerEnum, cl, stringName, arrayMembers, countMax) = ni ()
        member x.EnumMethods (handlePointerEnum, cl, arrayMembers, countMax) = ni ()
        member x.EnumMethodsWithName (handlePointerEnum, cl, stringName, arrayMembers, countMax) = ni ()
        member x.EnumFields (handlePointerEnum, cl, arrayMembers, countMax) = ni ()
        member x.EnumFieldsWithName (handlePointerEnum, cl, stringName, arrayMembers, countMax) = ni ()
        member x.EnumParams (handlePointerEnum, tokenParent, arrayMemberRef, countMax) = ni ()
        member x.EnumMemberRefs (handlePointerEnum, tokenParent, arrayMemberRefs, countMax) = ni ()
        member x.EnumMethodImpls (handlePointerEnum, td, arrayMethodBody, arrayMethodImpl, countMax) = ni ()
        member x.EnumPermissionSets (handlePointerEnum, tk, dwordActions, arrayPermission, countMax) = ni ()
        member x.FindMember (td, stringName, voidPointerSigBlob, byteCountSigBlob) = ni ()
        member x.FindMethod (td, stringName, voidPointerSigBlob, byteCountSigBlob) = ni ()
        member x.FindField (td, stringName, voidPointerSigBlob, byteCountSigBlob) = ni ()
        member x.FindMemberRef (td, stringName, voidPointerSigBlob, byteCountSigBlob) = ni ()
        member x.GetMemberRefProps (mr, ptk, stringMember, cchMember, pchMember, ppvSigBlob) = ni ()
        member x.EnumProperties (handlePointerEnum, td, arrayProperties, countMax) = ni ()
        member x.EnumEvents (handlePointerEnum, td, arrayEvents, countMax) = ni ()
        member x.GetEventProps (ev, pointerClass, stringEvent, cchEvent, pchEvent, pdwEventFlags, ptkEventType, pmdAddOn, pmdRemoveOn, pmdFire, rmdOtherMethod, countMax) = ni ()
        member x.EnumMethodSemantics (handlePointerEnum, mb, arrayEventProp, countMax) = ni ()
        member x.GetMethodSemantics (mb, tokenEventProp) = ni ()
        member x.GetClassLayout (td, pdwPackSize, arrayFieldOffset, countMax, countPointerFieldOffset) = ni ()
        member x.GetFieldMarshal (tk, ppvNativeType) = ni ()
        member x.GetRVA (tk, pulCodeRVA) = ni ()
        member x.GetPermissionSetProps (pm, pdwAction, ppvPermission) = ni ()
        member x.GetSigFromToken (memberDefSig, ppvSig, pcbSig) = ni ()
        member x.GetModuleRefProps (mur, stringName, cchName) = ni ()
        member x.EnumModuleRefs (handlePointerEnum, arrayModuleRefs, countMax) = ni ()
        member x.GetTypeSpecFromToken (typespec, ppvSig) = ni ()
        member x.GetNameFromToken (tk) = ni ()
        member x.EnumUnresolvedMethods (handlePointerEnum, arrayMethods, countMax) = ni ()
        member x.GetUserString (stk, stringString, cchString) = ni ()
        member x.GetPinvokeMap (tk, pdwMappingFlags, stringImportName, cchImportName, pchImportName) = ni ()
        member x.EnumSignatures (handlePointerEnum, arraySignatures, countMax) = ni ()
        member x.EnumTypeSpecs (handlePointerEnum, arraySignatures, countMax) = ni ()
        member x.EnumUserStrings (handlePointerEnum, arraySignatures, countMax) = ni ()
        member x.GetParamForMethodIndex (md, ulongParamSeq, pointerParam) = ni ()
        member x.EnumCustomAttributes (handlePointerEnum, tk, tokenType, arrayCustomAttributes, countMax) = ni ()
        member x.GetCustomAttributeProps (cv, ptkObj, ptkType, ppBlob) = ni ()
        member x.FindTypeRef (tokenResolutionScope, stringName) = ni ()
        member x.GetMemberProps (mb, pointerClass, stringMember, cchMember, pchMember, pdwAttr, ppvSigBlob, pchSigBlob, pulCodeRVA, pdwImplFlags, pdwCPlusTypeFlag, ppValue) = ni ()
        member x.GetFieldProps (mb, pointerClass, stringField, cchField, pchField, pdwAttr, ppvSigBlog, pchSigBlob, pdwCPlusTypeFlag, ppValue) = ni ()
        member x.GetPropertyProps (prop, pointerClass, stringProperty, cchProperty, pchProperty, pdwPropFlags, ppvSig, bytePointerSig, pdwCPlusTypeFlag, ppDefaultValue, pcchDefaultValue, pmdSetter, pmdGetter, rmdOtherMethod, countMax) = ni ()
        member x.GetParamProps (tk, pmd, pulSequence, stringName, cchName, pchName, pdwAttr, pdwCPlusTypeFlag, ppValue) = ni ()
        member x.GetCustomAttributeByName (tokenObj, stringName, ppData) = ni ()
        member x.IsValidToken (tk) = ni ()
        member x.GetNativeCallConvFromSig (voidPointerSig, byteCountSig) = ni ()
        member x.IsGlobal (pd) = ni ()
