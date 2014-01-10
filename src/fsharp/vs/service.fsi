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
// SourceCodeServices API to the compiler as an incremental service for parsing,
// type checking and intellisense-like environment-reporting.
//----------------------------------------------------------------------------

namespace Microsoft.FSharp.Compiler.SourceCodeServices
open System
open System.Collections.Generic

open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.Range

/// Represents one parameter for one overload in a method overload set
[<Sealed>]
type MethodOverloadsParam = 
    member Name: string
    member CanonicalTypeTextForSorting: string
    member Display: string
    member Description: string

/// Represents one overload in a method overload set
[<Sealed>]
type MethodOverloadsItem = 
    member Description : ToolTipText
    member Type: string
    member Parameters: MethodOverloadsParam[]
    /// Indicates that this not really a method, but actually a static arguments list, like TP<42,"foo"> ?
    member IsStaticArguments: bool

/// Represents a method overload set
[<Sealed>]
type MethodOverloads = 
    member Name: string
    member Methods: MethodOverloadsItem[] 

/// Represents the reason why the GetDeclarationLocation operation failed.
[<RequireQualifiedAccess>]
type FindDeclFailureReason = 
    // generic reason: no particular information about error
    | Unknown
    // source code file is not available
    | NoSourceCode
    // trying to find declaration of ProvidedType without TypeProviderDefinitionLocationAttribute
    | ProvidedType of string
    // trying to find declaration of ProvidedMember without TypeProviderDefinitionLocationAttribute
    | ProvidedMember of string

/// Represents the result of the GetDeclarationLocation operation.
[<NoEquality; NoComparison; RequireQualifiedAccess>]
type FindDeclResult = 
    /// declaration not found + reason
    | DeclNotFound of FindDeclFailureReason
    /// found declaration; return (position-in-file, name-of-file)
    | DeclFound      of Position * string
     
/// Represents the checking context implied by the ProjectOptions (i.e. the command line arguments, #r etc.)
[<Sealed>]
type TypeCheckContext =
    /// Get the assemblies referenced
    member GetReferencedAssemblies : unit -> FSharpAssembly list


[<Sealed>]
/// A handle to the results of TypeCheckFileInProject.  
/// A live object of this type keeps the background corresponding background builder (and type providers) alive (through reference-counting)
type TypeCheckFileResults =
    /// The errors returned by parsing a source file
    member Errors : ErrorInfo[]

    /// Get a view of the contents of the assembly up to and including the file just checked
    member PartialAssemblySignature : FSharpAssemblySignature

    /// Get the context implied by the ProjectOptions (i.e. the command line arguments, #r etc.)
    member TypeCheckContext : TypeCheckContext

    member HasFullTypeCheckInfo: bool

    /// Intellisense autocompletions
    ///
    ///   untypedParseInfoOpt: If this is present, it is used to filter declarations based on location in the
    ///                        parse tree, specifically at 'open' declarations, 'inherit' of class or interface
    ///                        'record field' locations and r.h.s. of 'range' operator a..b
    ///   
    ///   line: the line number where the completion is happening
    ///
    ///   colAtEndOfNamesAndResidue: the column number (1-based) at the end of the 'names' text 
    ///
    ///   names : A pair giving a cracked long identifier to the left of the position and a "residue" of a partial long identifier to the right
    ///
    ///   lineStr : the text of the line where the completion is happening. This is used to make a couple
    ///             of adhoc corrections to completion accuracy, checking for ".." and adding one to 
    ///             colAtEndOfNamesAndResidue when lineStr.[colAtEndOfNamesAndResidue] is '.'
    ///
    ///   hasTextChangedSinceLastTypecheck: 
    ///         If text has been used from a captured name resolution from the typecheck, then 
    ///         callback to the client to check if the text has changed. If it has, then give up
    ///         and assume that we're going to repeat the operation later on.

    member GetDeclarations                : untypedParseInfoOpt:UntypedParseInfo option * line: Line0 * colAtEndOfPartialName: int * lineText:string * qualifyingNames: string list * partialName: string * hasTextChangedSinceLastTypecheck: (obj * Range01 -> bool) -> Async<DeclarationSet>

    /// Resolve the names at the given location to give a formatted tool tip 
    ///
    ///   tokenTag: Used to discriminate between 'identifiers', 'strings' and others.
    ///             For strings, an attempt is made to give a tooltip relevant to a #r "..."
    ///              
    ///             TODO: this should be replaced by a look into the untyped AST, which gives us this information.

    member GetToolTipText                 : line:Line0 * colAtEndOfNames:int * lineText:string * names:string list * tokenTag:int -> ToolTipText

    [<Obsolete("This method has been renamed to GetToolTipText")>]
    member GetDataTipText                 : line:Line0 * colAtEndOfNames:int * lineText:string * names:string list * tokenTag:int -> ToolTipText

    /// Resolve the names at the given location to give F1 keyword
    ///
    member GetF1Keyword                   : line:Line0 * colAtEndOfNames:int * lineText:string * names:string list -> string option
    /// Resolve the names at the given location to a set of methods
    ///
    member GetMethods                     : line:Line0 * colAtEndOfNames:int * lineText:string * names:string list option -> MethodOverloads

    /// Resolve the names at the given location to the declaration location of the corresponding construct
    ///
    ///   tokenTag: Used to discriminate between 'identifiers' and others.
    ///             If it is not an identifier, no result is given.
    ///   preferSignature: If false, then make an attempt to go to the implementation (rather than the signature if present)
    member GetDeclarationLocation         : line:Line0 * colAtEndOfNames:int * lineText:string * names:string list * tokenTag:int * preferSignature:bool -> FindDeclResult

    /// Get any extra colorization info that is available after the typecheck
    member GetExtraColorizations : unit -> (Range01 * TokenColorKind)[]

/// wraps the set of unresolved references providing implementations of Equals\GetHashCode
/// of this objects of this type can be used as parts of types with generated Equals\GetHashCode
/// i.e. records or DUs
type UnresolvedReferencesSet = class end

/// A set of information describing a project or script build configuration.
type ProjectOptions = 
    { 
      // Note that this may not reduce to just the project directory, because there may be two projects in the same directory.
      ProjectFileName: string
      ProjectFileNames: string[]
      ProjectOptions: string[]
      /// When true, the typechecking environment is known a priori to be incomplete. 
      /// This can happen, for example, when a .fs file is opened outside of a project.
      /// It may be appropriate, then, to not show error messages related to type checking
      /// since they will just be noise.
      IsIncompleteTypeCheckEnvironment : bool
      /// When true, use the reference resolution rules for scripts rather than the rules for compiler.
      UseScriptResolutionRules : bool
      /// Timestamp of project/script load
      LoadTime : DateTime
      UnresolvedReferences : UnresolvedReferencesSet option
    }
         
          
/// Information about the compilation environment    
module CompilerEnvironment =
    /// These are the names of assemblies that should be referenced for .fs or .fsi files that
    /// are not asscociated with a project.
    val DefaultReferencesForOrphanSources : string list
    /// Return the compilation defines that should be used when editing the given file.
    val GetCompilationDefinesForEditing : filename : string * compilerFlags : string list -> string list
    /// Return true if this is a subcategory of error or warning message that the language service can emit
    val IsCheckerSupportedSubcategory : string -> bool

/// Information about the debugging environment
module internal DebuggerEnvironment =
    /// Return the language ID, which is the expression evaluator id that the
    /// debugger will use.
    val GetLanguageID : unit -> Guid
    
/// Callback which can be used by the host to indicate to the checker that a requested result has become obsolete,
/// e.g. because of typing by the user in the editor window. This can be used to marginally increase accuracy
/// of intellisense results in some situations.
[<NoComparison;NoEquality>]
type IsResultObsolete = 
    | IsResultObsolete of (unit->bool)

/// The result of calling TypeCheckResult including the possibility of abort and background compiler not caught up.
[<NoComparison; RequireQualifiedAccess>]
type TypeCheckFileAnswer =
    | NoAntecedant
    | Aborted // because result was obsolete
    | TypeCheckSucceeded of TypeCheckFileResults    

[<Sealed; AutoSerializable(false)>]      
type InteractiveChecker =
    /// Create an instance of an InteractiveChecker.  Currently resources are not reclaimed.
    static member Create : unit -> InteractiveChecker

    /// Parse a source code file, returning information about brace matching in the file
    /// Return an enumeration of the matching parethetical tokens in the file
    member MatchBraces : filename : string * source: string * options: ProjectOptions -> (Range01 * Range01)[]

    /// Parse a source code file, returning a handle that can be used for obtaining navigation bar information
    /// To get the full information, call 'TypeCheckFileInProject' method on the result
    member ParseFileInProject : filename: string * source: string * options: ProjectOptions -> UntypedParseInfo        

    /// Typecheck a source code file, returning a handle to the results of the parse including
    /// the reconstructed types in the file.
    ///
    /// Return NoAntecedent if the background builder is not yet done prepring the type check context for the 
    /// file (e.g. loading references and parsing/checking files in the project that this file depends upon). 
    /// In this case, the caller can either retry, or wait for FileTypeCheckStateIsDirty to be raised for this file.
    member TypeCheckFileInProjectIfReady : parsed: UntypedParseInfo * filename: string * fileversion: int * source: string * options: ProjectOptions * isResultObsolete: IsResultObsolete * textSnapshotInfo: obj -> TypeCheckFileAnswer

    // /// Complete typecheck for a complete set of options.
    // member TypeCheckProject : options: ProjectOptions -> Async<TypeCheckProjectResults>

    /// For a given script file, get the ProjectOptions implied by the #load closure. Optional 'otherFlags'
    member GetProjectOptionsFromScriptRoot : filename : string * source : string * loadedTimeStamp : DateTime * otherFlags : string[] -> ProjectOptions
        
#if NO_QUICK_SEARCH_HELPERS // only used in QuickSearch prototype
#else
    /// For QuickSearch index - not used by VS2008/VS2010/VS11
    member GetSlotsCount : options : ProjectOptions -> int
    /// For QuickSearch index - not used by VS2008/VS2010/VS11
    member UntypedParseForSlot : slot:int * options : ProjectOptions -> UntypedParseInfo
#endif // QUICK_SEARCH

    /// Try to get recent type check results for a file. This may arbitrarily refuse to return any
    /// results if the InteractiveChecker would like a chance to recheck the file, in which case
    /// ParseFileInProject and TypeCheckFileInProject should be called. If the source of the file
    /// has changed the results returned by this function may be out of date, though may
    /// still be usable for generating intellsense menus and information.
    member TryGetRecentTypeCheckResultsForFile : filename: string * options:ProjectOptions -> (UntypedParseInfo * TypeCheckFileResults * (*version*)int) option

    /// This function is called when the entire environment is known to have changed for reasons not encoded in the ProjectOptions of any project/compilation.
    /// For example, the type provider approvals file may have changed.
    member InvalidateAll : unit -> unit    
        
    /// This function is called when the configuration is known to have changed for reasons not encoded in the ProjectOptions.
    /// For example, dependent references may have been deleted or created.
    member InvalidateConfiguration: options: ProjectOptions -> unit    

    /// Begin background parsing the given project.
    member StartBackgroundCompile: options: ProjectOptions -> unit

    /// Stop the background compile.
    member StopBackgroundCompile : unit -> unit

    /// Block until the background compile finishes.
    member WaitForBackgroundCompile : unit -> unit
    
    /// Report a statistic for testability
    static member GlobalForegroundParseCountStatistic : int

    /// Report a statistic for testability
    static member GlobalForegroundTypeCheckCountStatistic : int

    /// Flush all caches and garbage collect
    member ClearLanguageServiceRootCachesAndCollectAndFinalizeAllTransients : unit -> unit

    /// This function is called when a project has been cleaned/rebuilt, and thus any live type providers should be refreshed.
    member NotifyProjectCleaned: options: ProjectOptions -> unit    
    
    /// Notify the host that the logical type checking context for a file has now been updated internally
    /// and that the file has become eligible to be re-typechecked for errors.
    [<CLIEvent>]
    member FileTypeCheckStateIsDirty : IEvent<string>

    // For internal use only 
    member internal ReactorOps : IReactorOperations

    [<Obsolete("This member now takes an additional 'otherFlags' argument")>]
    member GetCheckOptionsFromScriptRoot : filename : string * source : string * loadedTimeStamp : DateTime -> ProjectOptions

    [<Obsolete("This member has been renamed to GetProjectOptionsFromScriptRoot")>]
    member GetCheckOptionsFromScriptRoot : filename : string * source : string * loadedTimeStamp : DateTime * otherFlags: string[] -> ProjectOptions

    [<Obsolete("This member has been renamed to ParseFileInProject")>]
    member UntypedParse : filename: string * source: string * options: ProjectOptions -> UntypedParseInfo        

    [<Obsolete("This member has been renamed to TypeCheckFileInProjectIfReady")>]
    member TypeCheckSource : parsed: UntypedParseInfo * filename: string * fileversion: int * source: string * options: ProjectOptions * isResultObsolete: IsResultObsolete * textSnapshotInfo: obj -> TypeCheckFileAnswer
    
// For internal use only 
and internal IReactorOperations = 
    abstract RunAsyncOp : (unit -> 'T) -> Async<'T>
    abstract AsyncOp: Reactor.Operation -> unit
    abstract SyncOp: Reactor.Operation -> unit
    abstract RunSyncOp: (unit -> 'T) -> 'T

// An object to typecheck source in a given typechecking environment.
// Used internally to provide intellisense over F# Interactive.
type internal ParseAndCheckHelper =
    internal new : ops: IReactorOperations * tcConfig: Build.TcConfig * tcGlobals: Env.TcGlobals * tcImports: Build.TcImports * tcState: Build.TcState * loadClosure: Build.LoadClosure option ->  ParseAndCheckHelper 
    member internal ParseAndCheckInteraction : source:string -> UntypedParseInfo * TypeCheckFileResults

/// A set of helpers related to naming of identifiers
module PrettyNaming =
    val IsIdentifierPartCharacter     : char -> bool
    val IsLongIdentifierPartCharacter : char -> bool
    val GetLongNameFromString         : string -> string list
    // Temporary workaround for no localized resources in FSharp.LanguageService.dll
    val FormatAndOtherOverloadsString : int -> string


[<Obsolete("This type has been renamed to MethodOverloadsParam")>]
type Param = MethodOverloadsParam

[<Obsolete("This type has been renamed to MethodOverloadsItem")>]
type Method = MethodOverloadsItem

[<Obsolete("This type has been renamed to ProjectOptions")>]
type CheckOptions = ProjectOptions

[<Obsolete("This type has been renamed to TypeCheckFileAnswer")>]
type TypeCheckAnswer = TypeCheckFileAnswer

[<Obsolete("NotifyFileTypeCheckStateIsDirty has been replaced by the FileTypeCheckStateIsDirty event on the InteractiveChecker type")>]
type NotifyFileTypeCheckStateIsDirty = NotifyFileTypeCheckStateIsDirty of (string -> unit)
        
