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
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

/// Represents a declaration returned by GetDeclarations. Simpler than the one in 'SourceCodeServices' because
/// it formats the XML content for you (apart from ones read from XML doc files using xmlCommentRetriever)
[<Class>]
[<System.Obsolete("This type has been deprecated. Use the SourceCodeServices API directly. See http://fsharp.github.io/FSharp.Compiler.Service/editor.html")>] 
type SimpleDeclaration = 
    /// Get the name of a declaration
    member Name: string
    /// Compute the description for a declaration
    member GetDescription: unit -> string

/// Represents the results of type checking. A mild simplification of SourceCodeService's CheckFileResults.
/// Normally it is better to use the full CheckFileResults directly, available from 'FullResults'.
[<Class>]
[<System.Obsolete("This type has been deprecated. Use the SourceCodeServices API directly. See http://fsharp.github.io/FSharp.Compiler.Service/editor.html")>] 
type SimpleCheckFileResults = 
    /// Return the errors resulting from the type-checking
    [<System.Obsolete("This type has been deprecated. Use the SourceCodeServices API directly. See http://fsharp.github.io/FSharp.Compiler.Service/editor.html")>] 
    member Errors: FSharpErrorInfo []

    /// Get the declarations at the given code location.
    [<System.Obsolete("This type has been deprecated. Use the SourceCodeServices API directly. See http://fsharp.github.io/FSharp.Compiler.Service/editor.html")>] 
    member GetDeclarationListInfo: line:int * col:int * qualifyingNames:string list * partialName:string * ?xmlCommentRetriever:(string * string -> string) -> Async<SimpleDeclaration []>

    /// Get the Visual Studio F1-help keyword for the item at the given position
    [<System.Obsolete("This type has been deprecated. Use the SourceCodeServices API directly. See http://fsharp.github.io/FSharp.Compiler.Service/editor.html")>] 
    member GetF1KeywordAlternate: line:int * col:int * names:string list -> Async<string option>

    /// Get the data tip text at the given position
    [<System.Obsolete("This type has been deprecated. Use the SourceCodeServices API directly. See http://fsharp.github.io/FSharp.Compiler.Service/editor.html")>] 
    member GetToolTipTextAlternate: line:int * col:int * names:string list * ?xmlCommentRetriever:(string * string -> string) -> Async<string>

    /// Get the location of the declaration at the given position
    [<System.Obsolete("This type has been deprecated. Use the SourceCodeServices API directly. See http://fsharp.github.io/FSharp.Compiler.Service/editor.html")>] 
    member GetDeclarationLocationAlternate: line:int * col:int * names:string list * isDecl:bool -> Async<FindDeclResult>

    /// Get the full type checking results 
    [<System.Obsolete("This type has been deprecated. Use the SourceCodeServices API directly. See http://fsharp.github.io/FSharp.Compiler.Service/editor.html")>] 
    member FullResults: Microsoft.FSharp.Compiler.SourceCodeServices.CheckFileResults

    [<System.Obsolete("This method has been renamed to GetToolTipText")>] 
    member GetDataTipText: line:Line0 * col:int * names:string list * ?xmlCommentRetriever:(string * string -> string) -> string

    [<System.Obsolete("This member has been replaced by GetDeclarationListInfo, which accepts 1-based line numbers rather than a 0-based line numbers. See https://github.com/fsharp/FSharp.Compiler.Service/issues/64")>]
    member GetDeclarations: line:Line0 * col:int * qualifyingNames:string list * partialName:string * ?xmlCommentRetriever:(string * string -> string) -> Async<SimpleDeclaration []>

    [<System.Obsolete("This member has been replaced by GetF1KeywordAlternate, which accepts 1-based line numbers rather than a 0-based line numbers. See https://github.com/fsharp/FSharp.Compiler.Service/issues/64")>]
    member GetF1Keyword: line:Line0 * col:int * names:string list -> string option

    [<System.Obsolete("This member has been replaced by GetToolTipTextAlternate, which accepts 1-based line numbers rather than a 0-based line numbers. See https://github.com/fsharp/FSharp.Compiler.Service/issues/64")>]
    member GetToolTipText: line:Line0 * col:int * names:string list * ?xmlCommentRetriever:(string * string -> string) -> string

    [<System.Obsolete("This member has been replaced by GetDeclarationLocationAlternate, which accepts 1-based line numbers rather than a 0-based line numbers. See https://github.com/fsharp/FSharp.Compiler.Service/issues/64")>]
    member GetDeclarationLocation: line:Line0 * col:int * names:string list * isDecl:bool -> FindDeclResult


/// Provides simpler version of services for checking and compiling F# scripts
type SimpleSourceCodeServices = 

    /// Create a singleton global isntance for checking and compiling F# scripts
    new: ?msbuildEnabled: bool * ?msbuildVersion : string-> SimpleSourceCodeServices

    /// Tokenize a single line, returning token information and a tokenization state represented by an integer
    member TokenizeLine: line:string * state:int64 -> FSharpTokenInfo [] * int64

    /// Tokenize an entire file, line by line
    member TokenizeFile: source:string -> FSharpTokenInfo [] []

    /// Return information about matching braces in a single file.
    [<System.Obsolete("This method has been deprecated. Use the SourceCodeServices API directly. See http://fsharp.github.io/FSharp.Compiler.Service/editor.html")>] 
    member MatchBracesAlternate: filename:string * source:string * ?otherFlags:string [] -> Async<(range * range) []>

    [<System.Obsolete("This member has been replaced by MatchBracesAlternate, which produces 1-based line numbers rather than a 0-based line numbers. See https://github.com/fsharp/FSharp.Compiler.Service/issues/64")>]
    member MatchBraces: filename:string * source:string * ?otherFlags:string [] -> (Range01 * Range01) []

    /// For errors, quick info, goto-definition, declaration list intellisense, method overload intellisense
    [<System.Obsolete("This method has been deprecated. Use the SourceCodeServices API directly. See http://fsharp.github.io/FSharp.Compiler.Service/symbols.html")>] 
    member ParseAndCheckScript: filename:string * source:string * ?otherFlags:string [] -> Async<SimpleCheckFileResults>

    /// For analysis of a project
    [<System.Obsolete("This method has been deprecated. Use the SourceCodeServices API directly. see http://fsharp.github.io/FSharp.Compiler.Service/project.html")>] 
    member ParseAndCheckProject: projectFileName:string * argv:string [] -> Async<FSharpCheckProjectResults>

    /// Compile using the given flags.  Source files names are resolved via the FileSystem API. 
    /// The output file must be given by a -o flag. 
    /// The first argument is ignored and can just be "fsc.exe".
    member Compile: argv:string [] -> FSharpErrorInfo [] * int
    
    /// TypeCheck and compile provided AST
    member Compile: ast:ParsedInput list * assemblyName:string * outFile:string * dependencies:string list * ?pdbFile:string * ?executable:bool * ?noframework:bool -> FSharpErrorInfo [] * int

    /// Compiles to a dynamic assembly usinng the given flags.  
    ///
    /// The first argument is ignored and can just be "fsc.exe".
    ///
    /// Any source files names are resolved via the FileSystem API. An output file name must be given by a -o flag, but this will not
    /// be written - instead a dynamic assembly will be created and loaded.
    ///
    /// If the 'execute' parameter is given the entry points for the code are executed and 
    /// the given TextWriters are used for the stdout and stderr streams respectively. In this 
    /// case, a global setting is modified during the execution.
    member CompileToDynamicAssembly: otherFlags:string [] * execute:(TextWriter * TextWriter) option -> FSharpErrorInfo [] * int * System.Reflection.Assembly option

    /// TypeCheck and compile provided AST
    member CompileToDynamicAssembly: ast:ParsedInput list * assemblyName:string * dependencies:string list * execute:(TextWriter * TextWriter) option * ?debug:bool * ?noframework:bool -> FSharpErrorInfo [] * int * System.Reflection.Assembly option
            
    [<System.Obsolete("This method has been renamed to ParseAndCheckScript")>] 
    member TypeCheckScript: filename:string * source:string * otherFlags:string [] -> Async<SimpleCheckFileResults>
