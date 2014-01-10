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
type XmlComment =
    | XmlCommentNone
    | XmlCommentText of string
    | XmlCommentSignature of (*File:*) string * (*Signature:*)string

/// A single tool tip display element
//
// Note: instances of this type do not hold any references to any compiler resources.
type ToolTipElement = 
    | ToolTipElementNone
    /// A single type, method, etc with comment.
    | ToolTipElement of (* text *) string * XmlComment
    /// For example, a method overload group.
    | ToolTipElementGroup of ((* text *) string * XmlComment) list
    /// An error occurred formatting this element
    | ToolTipElementCompositionError of string

/// Information for building a tool tip box.
//
// Note: instances of this type do not hold any references to any compiler resources.
type ToolTipText = 
    /// A list of data tip elements to display.
    | ToolTipText of ToolTipElement list  
    
[<System.Obsolete("This type has been renamed to 'ToolTipText'")>]
type DataTipText = ToolTipText

[<Sealed>]
// Note: this type holds a weak reference to compiler resources. 
type Declaration =
    /// Get the display name for the declaration.
    member Name : string
    /// Get the description text for the declaration. Commputing this property may require using compiler
    /// resources and may trigger execution of a type provider method to retrieve documentation.
    member DescriptionText : ToolTipText
    /// Get the glyph integer for the declaration as used by Visual Studio.
    member Glyph : int
    
[<Sealed>]
/// Represents a set of declarations returned by GetDeclarations.
//
// Note: this type holds a weak reference to compiler resources. 
type DeclarationSet =
    member Items : Declaration[]

    // Implementation details used by other code in the compiler    
    static member internal Create : infoReader:InfoReader * m:range * denv:DisplayEnv * items:Item list * syncop:((unit->unit)->unit) * checkAlive:(unit -> bool) -> DeclarationSet
    static member internal Error : message:string -> DeclarationSet
    static member Empty : DeclarationSet


module internal TestHooks =
    val FormatOverloadsToListScope                   : (ToolTipElement->ToolTipElement) -> System.IDisposable
    
    
// implementation details used by other code in the compiler    
module internal ItemDescriptionsImpl = 
    val isFunction : TcGlobals -> TType -> bool
    val ParamNameAndTypesOfUnaryCustomOperation : TcGlobals -> MethInfo -> ParamNameAndType list
    val FormatDescriptionOfItem : bool -> InfoReader -> range -> DisplayEnv -> Item -> ToolTipElement
    val FormatReturnTypeOfItem  : InfoReader -> range -> DisplayEnv -> Item -> string
    val RemoveDuplicateItems : TcGlobals -> Item list -> Item list
    val RemoveExplicitlySuppressed : TcGlobals -> Item list -> Item list
    val GetF1Keyword : Item -> string option
    val rangeOfItem : TcGlobals -> bool -> Item -> range option
    val fileNameOfItem : TcGlobals -> string option -> range -> Item -> string



