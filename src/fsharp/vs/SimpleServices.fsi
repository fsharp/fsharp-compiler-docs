namespace Microsoft.FSharp.Compiler.SimpleSourceCodeServices

    open System.IO
    open Microsoft.FSharp.Compiler
    open Microsoft.FSharp.Compiler.SourceCodeServices

    /// Represents a declaration returned by GetDeclarations
    [<Class>]
    type Declaration = 
        /// Get the name of a declaration
        member Name : string
        /// Compute the description for a declaration
        member GetDescription : unit -> string

    /// Represents the results of type checking
    [<Class>]
    type TypeCheckResults  =

        /// Return the errors resulting from the type-checking
        member Errors : ErrorInfo[]

        /// Get the declarations at the given code location.
        member GetDeclarations : line: int * col: int * names: Names * residue: string * ?xmlCommentRetriever : (string * string -> string) -> Async<Declaration[]>

        /// Get the Visual Studio F1-help keyword for the item at the given position
        member GetF1Keyword : line: int * col: int * names: Names -> string option

        /// Get the data tip text at the given position
        member GetDataTipText: line: int * col: int * names: Names * ?xmlCommentRetriever : (string * string -> string) -> string

        /// Get the location of the declaration at the given position
        member GetDeclarationLocation: line: int * col: int * names: Names * isDecl: bool -> FindDeclResult

        /// Get the full type checking results 
        member FullResults : Microsoft.FSharp.Compiler.SourceCodeServices.TypeCheckResults

    /// Provides simple services for checking and compiling F# scripts
    type SimpleSourceCodeServices = 
        /// Create a singleton global isntance for checking and compiling F# scripts
        new : unit -> SimpleSourceCodeServices

        /// Tokenize a single line, returning token information and a tokenization state represented by an integer
        member TokenizeLine: line: string * state: int64 -> TokenInformation[] * int64 

        /// Tokenize an entire file, line by line
        member TokenizeFile: source: string -> TokenInformation[][] 

        /// Return information about matching braces in a single file.
        member MatchBraces: filename: string * source: string -> (Range * Range) [] 

        /// For errors, quick info, goto-definition, declaration list intellisense, method overload intellisense
        member TypeCheckScript: filename:string * source:string * otherFlags:string[] -> TypeCheckResults

        /// Compile using the given flags.  Source files names are resolved via the FileSystem API. The output file must be given by a -o flag. 
        member Compile: argv: string[] -> ErrorInfo[] * int

        /// Compiles to a dynamic assembly usinng the given flags.  Any source files names 
        /// are resolved via the FileSystem API. An output file name must be given by a -o flag, but this will not
        /// be written - instead a dynamic assembly will be created and loaded.
        ///
        /// If the 'execute' parameter is given the entry points for the code are executed and 
        /// the given TextWriters are used for the stdout and stderr streams respectively. In this 
        /// case, a global setting is modified during the execution.
        member CompileToDynamicAssembly: otherFlags: string[] * execute: (TextWriter * TextWriter) option -> ErrorInfo[] * int * System.Reflection.Assembly option
            
