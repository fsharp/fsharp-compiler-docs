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
// SimpleSourceCodeServices API to the compiler is a simplified service for parsing,
// type checking, intellisense and compilation.
//----------------------------------------------------------------------------

namespace Microsoft.FSharp.Compiler.SimpleSourceCodeServices

open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

// TODO: make xmlCommentRetriever a parameter of the service
/// Represents a declaration returned by GetDeclarations
[<Class>]
type Declaration = 
    /// Get the name of a declaration
    member Name: string
    /// Compute the description for a declaration
    member GetDescription: unit -> string

/// Represents the results of type checking
[<Class>]
type CheckFileResults = 
    /// Return the errors resulting from the type-checking
    member Errors: ErrorInfo []

    /// Get the declarations at the given code location.
    member GetDeclarations: line:Line0 * col:int * qualifyingNames:string list * partialName:string * ?xmlCommentRetriever:(string * string -> string) -> Async<Declaration []>

    /// Get the Visual Studio F1-help keyword for the item at the given position
    member GetF1Keyword: line:Line0 * col:int * names:string list -> string option

    /// Get the data tip text at the given position
    member GetToolTipText: line:Line0 * col:int * names:string list * ?xmlCommentRetriever:(string * string -> string) -> string

    /// Get the location of the declaration at the given position
    member GetDeclarationLocation: line:Line0 * col:int * names:string list * isDecl:bool
         -> FindDeclResult
    /// Get the full type checking results 
    member FullResults: Microsoft.FSharp.Compiler.SourceCodeServices.CheckFileResults

    [<System.Obsolete("This method has been renamed to GetToolTipText")>] 
    member GetDataTipText: line:Line0 * col:int * names:string list * ?xmlCommentRetriever:(string * string -> string) -> string


/// Provides simple services for checking and compiling F# scripts
type SimpleSourceCodeServices = 

    /// Create a singleton global isntance for checking and compiling F# scripts
    new: unit -> SimpleSourceCodeServices

    /// Tokenize a single line, returning token information and a tokenization state represented by an integer
    member TokenizeLine: line:string * state:int64
         -> TokenInformation [] * int64

    /// Tokenize an entire file, line by line
    member TokenizeFile: source:string -> TokenInformation [] []

    /// Return information about matching braces in a single file.
    member MatchBraces: filename:string * source:string * ?otherFlags:string [] -> (Range01 * Range01) []

    /// For errors, quick info, goto-definition, declaration list intellisense, method overload intellisense
    member ParseAndCheckScript: filename:string * source:string * ?otherFlags:string [] -> CheckFileResults

    /// Compile using the given flags.  Source files names are resolved via the FileSystem API. The output file must be given by a -o flag. 
    member Compile: argv:string [] -> ErrorInfo [] * int

    /// Compiles to a dynamic assembly usinng the given flags.  Any source files names 
    /// are resolved via the FileSystem API. An output file name must be given by a -o flag, but this will not
    /// be written - instead a dynamic assembly will be created and loaded.
    ///
    /// If the 'execute' parameter is given the entry points for the code are executed and 
    /// the given TextWriters are used for the stdout and stderr streams respectively. In this 
    /// case, a global setting is modified during the execution.
    member CompileToDynamicAssembly: otherFlags:string [] * execute:(TextWriter * TextWriter) option
         -> ErrorInfo [] * int * System.Reflection.Assembly option
            
    [<System.Obsolete("This method has been renamed to ParseAndCheckScript")>] 
    member TypeCheckScript: filename:string * source:string * otherFlags:string [] -> CheckFileResults
