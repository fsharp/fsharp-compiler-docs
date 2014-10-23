// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

//----------------------------------------------------------------------------
// API to the compiler as an incremental service for parsing,
// type checking and intellisense-like environment-reporting.
//----------------------------------------------------------------------------

namespace Microsoft.FSharp.Compiler.SourceCodeServices

open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.Range
open System.Collections.Generic
open Microsoft.FSharp.Compiler.Env 
open Microsoft.FSharp.Compiler.Infos
open Microsoft.FSharp.Compiler.Nameres
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops

/// Describe a comment as either a block of text or a file+signature reference into an intellidoc file.
//
// Note: instances of this type do not hold any references to any compiler resources.
[<RequireQualifiedAccess>]
type FSharpXmlDoc =
    /// No documentation is available
    | None
    /// The text for documentation 
    | Text of string
    /// Indicates that the text for the documentation can be found in a .xml documentation file, using the given signature key
    | XmlDocFileSignature of (*File:*) string * (*Signature:*)string

/// A single tool tip display element
//
// Note: instances of this type do not hold any references to any compiler resources.
[<RequireQualifiedAccess>]
type FSharpToolTipElement = 
    | None
    /// A single type, method, etc with comment.
    | Single of (* text *) string * FSharpXmlDoc
    /// For example, a method overload group.
    | Group of ((* text *) string * FSharpXmlDoc) list
    /// An error occurred formatting this element
    | CompositionError of string

/// Information for building a tool tip box.
//
// Note: instances of this type do not hold any references to any compiler resources.
type FSharpToolTipText = 
    /// A list of data tip elements to display.
    | FSharpToolTipText of FSharpToolTipElement list  
    
[<Sealed>]
/// Represents a declaration in F# source code, with information attached ready for display by an editor.
/// Returned by GetDeclarations.
//
// Note: this type holds a weak reference to compiler resources. 
type FSharpDeclaration =
    /// Get the display name for the declaration.
    member Name : string
    /// Get the description text for the declaration. Commputing this property may require using compiler
    /// resources and may trigger execution of a type provider method to retrieve documentation.
    ///
    /// May return "Loading..." if timeout occurs
    member DescriptionText : FSharpToolTipText
    /// Get the description text, asynchronously.  Never returns "Loading...".
    member DescriptionTextAsync : Async<FSharpToolTipText>
    /// Get the glyph integer for the declaration as used by Visual Studio.
    member Glyph : int
    
[<Sealed>]
/// Represents a set of declarations in F# source code, with information attached ready for display by an editor.
/// Returned by GetDeclarations.
//
// Note: this type holds a weak reference to compiler resources. 
type FSharpDeclarationSet =
    member Items : FSharpDeclaration[]

    // Implementation details used by other code in the compiler    
    static member internal Create : infoReader:InfoReader * m:range * denv:DisplayEnv * items:Item list * reactor:IReactorOperations * checkAlive:(unit -> bool) -> FSharpDeclarationSet
    static member internal Error : message:string -> FSharpDeclarationSet
    static member Empty : FSharpDeclarationSet


// implementation details used by other code in the compiler    
module internal ItemDescriptionsImpl = 
    val isFunction : TcGlobals -> TType -> bool
    val ParamNameAndTypesOfUnaryCustomOperation : TcGlobals -> MethInfo -> ParamNameAndType list

    val GetXmlDocSigOfEntityRef : InfoReader -> range -> EntityRef -> (string option * string) option
    val GetXmlDocSigOfValRef : TcGlobals -> TyconRef -> ValRef -> (string option * string) option
    val GetXmlDocSigOfRecdFieldInfo : RecdFieldInfo -> (string option * string) option
    val GetXmlDocSigOfUnionCaseInfo : UnionCaseInfo -> (string option * string) option
    val GetXmlDocSigOfMethInfo : InfoReader -> range -> MethInfo -> (string option * string) option
    val GetXmlDocSigOfActivePatternCase : TcGlobals -> ValRef -> (string option * string) option
    val GetXmlDocSigOfProp : InfoReader -> range -> PropInfo -> (string option * string) option
    val GetXmlDocSigOfEvent : InfoReader -> range -> EventInfo -> (string option * string) option
    val FormatDescriptionOfItem : bool -> InfoReader -> range -> DisplayEnv -> Item -> FSharpToolTipElement
    val FormatReturnTypeOfItem  : InfoReader -> range -> DisplayEnv -> Item -> string
    val RemoveDuplicateItems : TcGlobals -> Item list -> Item list
    val RemoveExplicitlySuppressed : TcGlobals -> Item list -> Item list
    val GetF1Keyword : Item -> string option
    val rangeOfItem : TcGlobals -> bool option -> Item -> range option
    val fileNameOfItem : TcGlobals -> string option -> range -> Item -> string
    val FullNameOfItem : TcGlobals -> Item -> string
    val ccuOfItem : TcGlobals -> Item -> CcuThunk option



[<System.Obsolete("This type has been renamed to 'FSharpDeclaration'")>]
/// Renamed to FSharpDeclaration
type Declaration = FSharpDeclaration


[<System.Obsolete("This type has been renamed to 'FSharpDeclarationGroup'")>]
/// Renamed to FSharpDeclarationGroup
type DeclarationGroup = FSharpDeclarationGroup

[<System.Obsolete("This type has been renamed to 'FSharpXmlDoc'")>]
/// Renamed to FSharpXmlDoc
type XmlComment = FSharpXmlDoc

[<System.Obsolete("This type has been renamed to 'FSharpToolTipElement'")>]
/// Renamed to FSharpToolTipElement
type ToolTipElement = FSharpToolTipElement

[<System.Obsolete("This type has been renamed to 'FSharpToolTipText'")>]
/// Renamed to FSharpToolTipText
type ToolTipText = FSharpToolTipText

[<System.Obsolete("This type has been renamed to 'FSharpToolTipText'")>]
/// Renamed to FSharpToolTipText
type DataTipText = FSharpToolTipText

[<AutoOpen>]
module Obsoletes = 
    [<System.Obsolete("The cases of this union type have been renamed to 'FSharpXmlDoc.None', 'FSharpXmlDoc.Text' or 'FSharpXmlDoc.XmlDocFileSignature'", true)>]
    type Dummy = 
    | XmlCommentNone 
    | XmlCommentText of string 
    | XmlCommentSignature of string * string 

    [<System.Obsolete("The cases of this union type have been renamed to 'FSharpToolTipElement.None', 'FSharpToolTipElement.Single', 'FSharpToolTipElement.Group' or 'FSharpToolTipElement.CompositionError'",true)>]
    type Dummy2 = 
    | ToolTipElementNone 
    | ToolTipElement of  string * FSharpXmlDoc 
    | ToolTipElementGroup of (string * FSharpXmlDoc) list 
    | ToolTipElementCompositionError of string  


    [<System.Obsolete("The single case of this union type has been renamed to 'FSharpToolTipText'",true)>]
    type Dummy3 = 
        | ToolTipText of FSharpToolTipElement list  
