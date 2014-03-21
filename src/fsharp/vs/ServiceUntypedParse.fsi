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
open Microsoft.FSharp.Compiler.ErrorLogger
open System.Collections.Generic

[<Sealed>]
type ParseFileResults = 
    member ParseTree : Ast.ParsedInput option
    /// Notable parse info for ParameterInfo at a given location
    member FindNoteworthyParamInfoLocations : pos:pos -> NoteworthyParamInfoLocations option
    /// Name of the file for which this information were created
    member FileName                       : string
    /// Get declared items and the selected item at the specified location
    member GetNavigationItems             : unit -> NavigationItems
    /// Return the inner-most range associated with a possible breakpoint location
    member ValidateBreakpointLocation : pos:pos -> range option
    /// When these files change then the build is invalid
    member DependencyFiles : string list

    /// Get the errors and warnings for the parse
    member Errors : ErrorInfo[]

    /// Indicates if any errors occured during the parse
    member ParseHadErrors : bool

    internal new : errors : ErrorInfo[] * input : Ast.ParsedInput option * parseHadErrors : bool * dependencyFiles : string list -> ParseFileResults

/// Information about F# source file names
module internal SourceFile =
   /// Whether or not this file is compilable
   val IsCompilable : string -> bool
   /// Whether or not this file should be a single-file project
   val MustBeSingleFileProject : string -> bool

type internal CompletionPath = string list * string option // plid * residue

type internal InheritanceContext = 
    | Class
    | Interface
    | Unknown

type internal RecordContext =
    | CopyOnUpdate of range * CompletionPath // range
    | Constructor of string // typename
    | New of CompletionPath

type internal CompletionContext = 
    // completion context cannot be determined due to errors
    | Invalid
    // completing something after the inherit keyword
    | Inherit of InheritanceContext * CompletionPath
    // completing records field
    | RecordField of RecordContext
    | RangeOperator


// implementation details used by other code in the compiler    
module (*internal*) UntypedParseImpl =
    open Microsoft.FSharp.Compiler.Ast
    val TryFindExpressionASTLeftOfDotLeftOfCursor : pos * ParsedInput option -> (pos * bool) option
    val GetRangeOfExprLeftOfDot : pos  * ParsedInput option -> range option
    val TryFindExpressionIslandInPosition : pos * ParsedInput option -> string option
    val TryGetCompletionContext : pos * ParseFileResults option -> CompletionContext option

// implementation details used by other code in the compiler    
module internal SourceFileImpl =
    val IsInterfaceFile : string -> bool 
    val AdditionalDefinesForUseInEditor : string -> string list
