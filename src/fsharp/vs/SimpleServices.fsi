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


/// Provides simpler version of services for checking and compiling F# scripts
type SimpleSourceCodeServices = 

    /// Create a singleton global isntance for checking and compiling F# scripts
    new: ?msbuildEnabled: bool -> SimpleSourceCodeServices

    /// Tokenize a single line, returning token information and a tokenization state represented by an integer
    member TokenizeLine: line:string * state:int64 -> FSharpTokenInfo [] * int64

    /// Tokenize an entire file, line by line
    member TokenizeFile: source:string -> FSharpTokenInfo [] []

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
            
