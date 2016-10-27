// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

//----------------------------------------------------------------------------
// Open up the compiler as an incremental service for parsing,
// type checking and intellisense-like environment-reporting.
//--------------------------------------------------------------------------

namespace Microsoft.FSharp.Compiler.SourceCodeServices

open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices.ItemDescriptionIcons 

/// Represents the differnt kinds of items that can appear in the navigation bar
type FSharpNavigationDeclarationItemKind =
    | NamespaceDecl
    | ModuleFileDecl
    | ExnDecl
    | ModuleDecl
    | TypeDecl
    | MethodDecl
    | PropertyDecl
    | FieldDecl
    | OtherDecl

[<RequireQualifiedAccess>]
type FSharpEnclosingEntityKind =
    | Namespace
    | Module
    | Class
    | Exception
    | Interface
    | Record
    | Enum
    | DU

/// Represents an item to be displayed in the navigation bar
[<Sealed>]
type FSharpNavigationDeclarationItem
    (
        uniqueName: string, 
        name: string, 
        kind: FSharpNavigationDeclarationItemKind, 
        glyph: int, 
        range: range, 
        bodyRange: range, 
        singleTopLevel: bool,
        enclosingEntityKind: FSharpEnclosingEntityKind,
        isAbstract: bool
    ) = 
    
    member x.bodyRange = bodyRange
    member x.UniqueName = uniqueName
    member x.Name = name
    member x.Glyph = glyph
    member x.Kind = kind
    member x.Range = range
    member x.BodyRange = bodyRange 
    member x.IsSingleTopLevel = singleTopLevel
    member x.EnclosingEntityKind = enclosingEntityKind
    member x.IsAbstract = isAbstract
    
    member x.WithUniqueName(uniqueName: string) =
      FSharpNavigationDeclarationItem (uniqueName, name, kind, glyph, range, bodyRange, singleTopLevel, enclosingEntityKind, isAbstract)
    
    static member Create(name: string, kind, glyph: int, range: range, bodyRange: range, singleTopLevel: bool, enclosingEntityKind, isAbstract) = 
      FSharpNavigationDeclarationItem("", name, kind, glyph, range, bodyRange, singleTopLevel, enclosingEntityKind, isAbstract)

/// Represents top-level declarations (that should be in the type drop-down)
/// with nested declarations (that can be shown in the member drop-down)
[<NoEquality; NoComparison>]
type FSharpNavigationTopLevelDeclaration = 
    { Declaration: FSharpNavigationDeclarationItem
      Nested: FSharpNavigationDeclarationItem[] }
      
/// Represents result of 'GetNavigationItems' operation - this contains
/// all the members and currently selected indices. First level correspond to
/// types & modules and second level are methods etc.
[<Sealed>]
type FSharpNavigationItems(declarations:FSharpNavigationTopLevelDeclaration[]) =
    member x.Declarations = declarations

module NavigationImpl =

    let unionRangesChecked r1 r2 = if r1 = range.Zero then r2 elif r2 = range.Zero then r1 else unionRanges r1 r2
    
    let rangeOfDecls2 f decls = 
      match (decls |> List.map (f >> (fun (d:FSharpNavigationDeclarationItem) -> d.bodyRange))) with 
      | hd::tl -> tl |> List.fold (unionRangesChecked) hd
      | [] -> range.Zero
    
    let rangeOfDecls = rangeOfDecls2 fst

    let moduleRange (idm:range) others = 
      unionRangesChecked idm.EndRange (rangeOfDecls2 (fun (a, _, _) -> a) others)
    
    let fldspecRange fldspec =
      match fldspec with
      | UnionCaseFields(flds) -> flds |> List.fold (fun st (Field(_, _, _, _, _, _, _, m)) -> unionRangesChecked m st) range.Zero
      | UnionCaseFullType(ty, _) -> ty.Range
      
    let bodyRange mb decls =
      unionRangesChecked (rangeOfDecls decls) mb
          
    /// Get information for implementation file        
    let getNavigationFromImplFile (modules:SynModuleOrNamespace list) =

        // Map for dealing with name conflicts
        let nameMap = ref Map.empty 
        let addItemName name = 
            let count = defaultArg (!nameMap |> Map.tryFind name) 0
            nameMap := (Map.add name (count + 1) (!nameMap))
            (count + 1)
        let uniqueName name idx = 
            let total = Map.find name (!nameMap)
            sprintf "%s_%d_of_%d" name idx total

        // Create declaration (for the left dropdown)                
        let createDeclLid(baseName, lid, kind, baseGlyph, m, bodym, nested, enclosingEntityKind, isAbstract) =
            let name = (if baseName <> "" then baseName + "." else "") + (textOfLid lid)
            FSharpNavigationDeclarationItem.Create
              (name, kind, baseGlyph * 6, m, bodym, false, enclosingEntityKind, isAbstract), (addItemName name), nested
            
        let createDecl(baseName, (id:Ident), kind, baseGlyph, m, bodym, nested, enclosingEntityKind, isAbstract) =
            let name = (if baseName <> "" then baseName + "." else "") + (id.idText)
            FSharpNavigationDeclarationItem.Create
              (name, kind, baseGlyph * 6, m, bodym, false, enclosingEntityKind, isAbstract), (addItemName name), nested
         
        // Create member-kind-of-thing for the right dropdown
        let createMemberLid(lid, kind, baseGlyph, m, enclosingEntityKind, isAbstract) =
            FSharpNavigationDeclarationItem.Create(textOfLid lid, kind, baseGlyph * 6, m, m, false, enclosingEntityKind, isAbstract), 
                (addItemName(textOfLid lid))

        let createMember((id:Ident), kind, baseGlyph, m, enclosingEntityKind, isAbstract) =
            FSharpNavigationDeclarationItem.Create(id.idText, kind, baseGlyph * 6, m, m, false, enclosingEntityKind, isAbstract), 
                (addItemName(id.idText))

        // Process let-binding
        let processBinding isMember enclosingEntityKind isAbstract 
            (Binding(_, _, _, _, _, _, SynValData(memebrOpt, _, _), synPat, _, synExpr, _, _)) =

            let m = match synExpr with 
                    | SynExpr.Typed(e, _, _) -> Some e.Range // fix range for properties with type annotations
                    | _ -> None
            match synPat, memebrOpt with
            | SynPat.LongIdent(LongIdentWithDots(lid,_), _,_, _, _, _), Some(flags) when isMember -> 
                let icon, kind =
                  match flags.MemberKind with
                  | MemberKind.ClassConstructor
                  | MemberKind.Constructor
                  | MemberKind.Member -> 
                        (if flags.IsOverrideOrExplicitImpl then iIconGroupMethod2 else iIconGroupMethod), MethodDecl
                  | MemberKind.PropertyGetSet
                  | MemberKind.PropertySet
                  | MemberKind.PropertyGet -> iIconGroupProperty, PropertyDecl
                let lidShowAndrangeMerge = 
                  match lid with 
                  | _thisVar::nm::_ -> Some (List.tail lid, nm.idRange) 
                  | hd::_ -> Some (lid, hd.idRange) 
                  | _ -> m |> Option.map (fun m -> lid, m)
                match lidShowAndrangeMerge with
                | Some (lidShow, rangeMerge) ->
                    let range = 
                        match m with
                        | Some m -> unionRanges rangeMerge m
                        | None -> rangeMerge
                    [ createMemberLid(lidShow, kind, icon, range, enclosingEntityKind, isAbstract) ]
                | None -> []
            | SynPat.LongIdent(LongIdentWithDots(lid,_), _,_, _, _, _), _ -> 
                let range =
                    match m with
                    | Some m -> unionRanges (List.head lid).idRange m
                    | None -> (List.head lid).idRange
                [ createMemberLid(lid, FieldDecl, iIconGroupConstant, range, enclosingEntityKind, isAbstract) ]
            | SynPat.Named (_,ident,_,_,r), _ ->
                [ createMemberLid([ident], FieldDecl, iIconGroupConstant, r, enclosingEntityKind, isAbstract)]
            | _ -> []
        
        // Process a class declaration or F# type declaration
        let rec processExnDefnRepr baseName nested (SynExceptionDefnRepr(_, (UnionCase(_, id, fldspec, _, _, _)), _, _, _, m)) =
            // Exception declaration
            [ createDecl(baseName, id, ExnDecl, iIconGroupException, m, fldspecRange fldspec, nested, 
                FSharpEnclosingEntityKind.Exception, false) ] 

        // Process a class declaration or F# type declaration
        and processExnDefn baseName (SynExceptionDefn(repr, membDefns, _)) =  
            let nested = processMembers membDefns FSharpEnclosingEntityKind.Exception |> snd
            processExnDefnRepr baseName nested repr

        and processTycon baseName (TypeDefn(ComponentInfo(_, _, _, lid, _, _, _, _), repr, membDefns, m)) =
            let topMembers = processMembers membDefns FSharpEnclosingEntityKind.Class |> snd
            match repr with
            | SynTypeDefnRepr.Exception repr -> processExnDefnRepr baseName [] repr
            | SynTypeDefnRepr.ObjectModel(_, membDefns, mb) ->
                // F# class declaration
                let members = processMembers membDefns FSharpEnclosingEntityKind.Class |> snd
                let nested = members@topMembers
                ([ createDeclLid(baseName, lid, TypeDecl, iIconGroupClass, m, bodyRange mb nested, nested,
                        FSharpEnclosingEntityKind.Class, false) ]: ((FSharpNavigationDeclarationItem * int * _) list))
            | SynTypeDefnRepr.Simple(simple, _) ->
                // F# type declaration
                match simple with
                | SynTypeDefnSimpleRepr.Union(_, cases, mb) ->
                    let cases = 
                        [ for (UnionCase(_, id, fldspec, _, _, _)) in cases -> 
                            createMember(id, OtherDecl, iIconGroupValueType, unionRanges (fldspecRange fldspec) id.idRange,
                                FSharpEnclosingEntityKind.DU, false) ]
                    let nested = cases@topMembers              
                    [ createDeclLid(baseName, lid, TypeDecl, iIconGroupUnion, m, bodyRange mb nested, nested,
                        FSharpEnclosingEntityKind.DU, false) ]
                | SynTypeDefnSimpleRepr.Enum(cases, mb) -> 
                    let cases = 
                        [ for (EnumCase(_, id, _, _, m)) in cases ->
                            createMember(id, FieldDecl, iIconGroupEnumMember, m, FSharpEnclosingEntityKind.Enum, false) ]
                    let nested = cases@topMembers
                    [ createDeclLid(baseName, lid, TypeDecl, iIconGroupEnum, m, bodyRange mb nested, nested,
                        FSharpEnclosingEntityKind.Enum, false) ]
                | SynTypeDefnSimpleRepr.Record(_, fields, mb) ->
                    let fields = 
                        [ for (Field(_, _, id, _, _, _, _, m)) in fields do
                            if (id.IsSome) then
                              yield createMember(id.Value, FieldDecl, iIconGroupFieldBlue, m, FSharpEnclosingEntityKind.Record, false) ]
                    let nested = fields@topMembers
                    [ createDeclLid(baseName, lid, TypeDecl, iIconGroupType, m, bodyRange mb nested, nested, 
                        FSharpEnclosingEntityKind.Record, false) ]
                | SynTypeDefnSimpleRepr.TypeAbbrev(_, _, mb) ->
                    [ createDeclLid(baseName, lid, TypeDecl, iIconGroupTypedef, m, bodyRange mb topMembers, topMembers,
                        FSharpEnclosingEntityKind.Class, false) ]
                          
                //| SynTypeDefnSimpleRepr.General of TyconKind * (SynType * range * ident option) list * (valSpfn * MemberFlags) list * fieldDecls * bool * bool * range 
                //| SynTypeDefnSimpleRepr.LibraryOnlyILAssembly of ILType * range
                //| TyconCore_repr_hidden of range
                | _ -> [] 
                  
        // Returns class-members for the right dropdown                  
        and processMembers (members: SynMemberDefns) (enclosingEntityKind: FSharpEnclosingEntityKind)
            : (range * list<FSharpNavigationDeclarationItem * int>) = 

            let members = members |> List.map (fun memb ->
               (memb.Range,
                match memb with
                | SynMemberDefn.LetBindings(binds, _, _, _) -> List.collect (processBinding false enclosingEntityKind false) binds
                | SynMemberDefn.Member(bind, _) -> processBinding true enclosingEntityKind false bind
                | SynMemberDefn.ValField(Field(_, _, Some(rcid), _, _, _, _, _), _) ->
                    [ createMember(rcid, FieldDecl, iIconGroupFieldBlue, rcid.idRange, enclosingEntityKind, false) ]
                | SynMemberDefn.AutoProperty(_attribs,_isStatic,id,_tyOpt,_propKind,_,_xmlDoc,_access,_synExpr, _, _) -> 
                    [ createMember(id, FieldDecl, iIconGroupFieldBlue, id.idRange, enclosingEntityKind, false) ]
                | SynMemberDefn.AbstractSlot(ValSpfn(_, id, _, ty, _, _, _, _, _, _, _), _, _) ->
                    [ createMember(id, MethodDecl, iIconGroupMethod2, ty.Range, enclosingEntityKind, true) ]
                | SynMemberDefn.NestedType _ -> failwith "tycon as member????" //processTycon tycon                
                | SynMemberDefn.Interface(_, Some(membs), _) ->
                    processMembers membs FSharpEnclosingEntityKind.Interface |> snd
                | _ -> []  )) 
            ((members |> Seq.map fst |> Seq.fold unionRangesChecked range.Zero),
             (members |> List.map snd |> List.concat))

        // Process declarations in a module that belong to the right drop-down (let bindings)
        let processNestedDeclarations decls = decls |> List.collect (function
            | SynModuleDecl.Let(_, binds, _) -> List.collect (processBinding false FSharpEnclosingEntityKind.Module false) binds
            | _ -> [] )        

        // Process declarations nested in a module that should be displayed in the left dropdown
        // (such as type declarations, nested modules etc.)                            
        let rec processFSharpNavigationTopLevelDeclarations(baseName, decls) = decls |> List.collect (function
            | SynModuleDecl.ModuleAbbrev(id, lid, m) ->
                [ createDecl(baseName, id, ModuleDecl, iIconGroupModule, m, rangeOfLid lid, [], FSharpEnclosingEntityKind.Namespace, false) ]
                
            | SynModuleDecl.NestedModule(ComponentInfo(_, _, _, lid, _, _, _, _), _isRec, decls, _, m) ->                
                // Find let bindings (for the right dropdown)
                let nested = processNestedDeclarations(decls)
                let newBaseName = (if (baseName = "") then "" else baseName+".") + (textOfLid lid)
                
                // Get nested modules and types (for the left dropdown)
                let other = processFSharpNavigationTopLevelDeclarations(newBaseName, decls)
                createDeclLid(baseName, lid, ModuleDecl, iIconGroupModule, m, unionRangesChecked (rangeOfDecls nested) 
                    (moduleRange (rangeOfLid lid) other), nested, FSharpEnclosingEntityKind.Module, false)::other
                  
            | SynModuleDecl.Types(tydefs, _) -> tydefs |> List.collect (processTycon baseName)                                    
            | SynModuleDecl.Exception (defn,_) -> processExnDefn baseName defn
            | _ -> [] )            
                  
        // Collect all the items  
        let items = 
            // Show base name for this module only if it's not the root one
            let singleTopLevel = (modules.Length = 1)
            modules |> List.collect (fun (SynModuleOrNamespace(id, _isRec, isModule, decls, _, _, _, m)) ->
                let baseName = if (not singleTopLevel) then textOfLid id else ""
                // Find let bindings (for the right dropdown)
                let nested = processNestedDeclarations(decls)
                // Get nested modules and types (for the left dropdown)
                let other = processFSharpNavigationTopLevelDeclarations(baseName, decls)
                
                // Create explicitly - it can be 'single top level' thing that is hidden
                let decl =
                    FSharpNavigationDeclarationItem.Create
                        (textOfLid id, (if isModule then ModuleFileDecl else NamespaceDecl),
                            iIconGroupModule * 6, m, 
                            unionRangesChecked (rangeOfDecls nested) (moduleRange (rangeOfLid id) other), 
                            singleTopLevel, FSharpEnclosingEntityKind.Module, false), (addItemName(textOfLid id)), nested
                decl::other )
                  
        let items = 
            items 
            |> Array.ofList 
            |> Array.map (fun (d, idx, nest) -> 
                let nest = nest |> Array.ofList |> Array.map (fun (decl, idx) -> decl.WithUniqueName(uniqueName d.Name idx))
                nest |> Array.sortInPlaceWith (fun a b -> compare a.Name b.Name)
                let nest = nest |> Array.distinctBy (fun x -> x.Range, x.BodyRange, x.Name, x.Kind) 
                
                { Declaration = d.WithUniqueName(uniqueName d.Name idx); Nested = nest } )                  
        items |> Array.sortInPlaceWith (fun a b -> compare a.Declaration.Name b.Declaration.Name)
        new FSharpNavigationItems(items)

    let empty = new FSharpNavigationItems([| |])

[<System.Obsolete("This type has been renamed to FSharpNavigationTopLevelDeclaration")>]
type TopLevelDeclaration = FSharpNavigationTopLevelDeclaration

[<System.Obsolete("This type has been renamed to FSharpNavigationDeclarationItem")>]
type DeclarationItem = FSharpNavigationDeclarationItem

[<System.Obsolete("This type has been renamed to FSharpNavigationItems")>]
type NavigationItems = FSharpNavigationItems

[<System.Obsolete("This type has been renamed to FSharpNavigationDeclarationItemKind")>]
type DeclarationItemKind = FSharpNavigationDeclarationItemKind
