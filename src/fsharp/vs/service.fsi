// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

//----------------------------------------------------------------------------
// SourceCodeServices API to the compiler as an incremental service for parsing,
// type checking and intellisense-like environment-reporting.
//----------------------------------------------------------------------------

namespace Microsoft.FSharp.Compiler.SourceCodeServices
open System
open System.Collections.Generic

open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.TcGlobals
open Microsoft.FSharp.Compiler.NameResolution
open Microsoft.FSharp.Compiler.CompileOps

/// Represents one parameter for one method (or other item) in a group. 
[<Sealed>]
type FSharpMethodGroupItemParameter = 

    /// The name of the parameter.
    member ParameterName: string

    /// A key that can be used for sorting the parameters, used to help sort overloads.
    member CanonicalTypeTextForSorting: string

    /// The text to display for the parameter including its name, its type and visual indicators of other
    /// information such as whether it is optional.
    member Display: string

    /// The descriptive help text to display for the parameter.
    member Description: string

    [<Obsolete("This member has been renamed to 'ParameterName'")>]
    member Name: string

/// Represents one method (or other item) in a method group. The item may represent either a method or 
/// a single, non-overloaded item such as union case or a named function value.
[<Sealed>]
type FSharpMethodGroupItem = 

    /// The formatted description text for the method (or other item)
    member Description : ToolTipText

    /// The formatted type text for the method (or other item)
    member TypeText: string

    /// The parameters of the method in the overload set
    member Parameters: FSharpMethodGroupItemParameter[]

    /// Indicates that this not really a method, but actually a static arguments list, like TP<42,"foo">
    member IsStaticArguments: bool

    [<Obsolete("This member has been renamed to 'TypeText'")>]
    member Type: string

/// Represents a group of methods (or other items) returned by GetMethods.  
[<Sealed>]
type FSharpMethodGroup = 
    /// The shared name of the methods (or other items) in the group
    member MethodName: string

    /// The methods (or other items) in the group
    member Methods: FSharpMethodGroupItem[] 

    [<Obsolete("This member has been renamed to 'MethodName'")>]
    member Name: string

/// Represents the reason why the GetDeclarationLocation operation failed.
[<RequireQualifiedAccess>]
type FSharpFindDeclFailureReason = 

    /// Generic reason: no particular information about error
    | Unknown

    /// Source code file is not available
    | NoSourceCode

    /// Trying to find declaration of ProvidedType without TypeProviderDefinitionLocationAttribute
    | ProvidedType of string

    /// Trying to find declaration of ProvidedMember without TypeProviderDefinitionLocationAttribute
    | ProvidedMember of string

/// Represents the result of the GetDeclarationLocation operation.
[<RequireQualifiedAccess>]
type FSharpFindDeclResult = 
    /// Indicates a declaration location was not found, with an additional reason
    | DeclNotFound of FSharpFindDeclFailureReason
    /// Indicates a declaration location was found
    | DeclFound      of range
     
/// Represents the checking context implied by the ProjectOptions 
[<Sealed>]
type FSharpProjectContext =
    /// Get the resolution and full contents of the assemblies referenced by the project options
    member GetReferencedAssemblies : unit -> FSharpAssembly list

    /// Get the accessibility rights for this project context w.r.t. InternalsVisibleTo attributes granting access to other assemblies
    member AccessibilityRights : FSharpAccessibilityRights

/// Represents the use of an F# symbol from F# source code
[<Sealed>]
type FSharpSymbolUse = 
    // For internal use only
    internal new : g:TcGlobals * denv: Tastops.DisplayEnv * symbol:FSharpSymbol * itemOcc:ItemOccurence * range: range -> FSharpSymbolUse

    /// The symbol referenced
    member Symbol : FSharpSymbol 

    /// The display context active at the point where the symbol is used. Can be passed to FSharpType.Format
    /// and other methods to format items in a way that is suitable for a specific source code location.
    member DisplayContext : FSharpDisplayContext

    /// Indicates if the reference is a definition for the symbol, either in a signature or implementation
    member IsFromDefinition : bool

    /// Indicates if the reference is in a pattern
    member IsFromPattern : bool

    /// Indicates if the reference is in a syntactic type
    member IsFromType : bool

    /// Indicates if the reference is in an attribute
    member IsFromAttribute : bool

    /// Indicates if the reference is via the member being implemented in a class or object expression
    member IsFromDispatchSlotImplementation : bool

    /// Indicates if the reference is either a builder or a custom operation in a compuation expression
    member IsFromComputationExpression : bool

    /// The file name the reference occurs in 
    member FileName: string 

    /// The range of text representing the reference to the symbol
    member RangeAlternate: range
    [<Obsolete("This member has been replaced by RangeAlternate, which produces 1-based line numbers rather than a 0-based line numbers. See https://github.com/fsharp/FSharp.Compiler.Service/issues/64")>]
    member Range: Range01
    [<Obsolete("Renamed to IsFromDefinition")>]
    member IsDefinition : bool

/// A handle to the results of CheckFileInProject.
[<Sealed>]
type FSharpCheckFileResults =
    /// The errors returned by parsing a source file.
    member Errors : FSharpErrorInfo[]

    /// Get a view of the contents of the assembly up to and including the file just checked
    member PartialAssemblySignature : FSharpAssemblySignature

    /// Get the resolution of the ProjectOptions 
    member ProjectContext : FSharpProjectContext

    /// Indicates whether type checking successfully occured with some results returned. If false, indicates that 
    /// an unrecoverable error in earlier checking/parsing/resolution steps.
    member HasFullTypeCheckInfo: bool

    /// <summary>Get the items for a declaration list</summary>
    ///
    /// <param name="ParsedFileResultsOpt">
    ///    If this is present, it is used to filter declarations based on location in the
    ///    parse tree, specifically at 'open' declarations, 'inherit' of class or interface
    ///    'record field' locations and r.h.s. of 'range' operator a..b
    /// </param>
    /// <param name="line">The line number where the completion is happening</param>
    /// <param name="colAtEndOfNamesAndResidue">The column number (1-based) at the end of the 'names' text </param>
    /// <param name="qualifyingNames">The long identifier to the left of the '.'</param>
    /// <param name="partialName">The residue of a partial long identifier to the right of the '.'</param>
    /// <param name="lineStr">The residue of a partial long identifier to the right of the '.'</param>
    /// <param name="lineText">
    ///    The text of the line where the completion is happening. This is only used to make a couple
    ///    of adhoc corrections to completion accuracy (e.g. checking for "..")
    /// </param>
    /// <param name="hasTextChangedSinceLastTypecheck">
    ///    If text has been used from a captured name resolution from the typecheck, then 
    ///    callback to the client to check if the text has changed. If it has, then give up
    ///    and assume that we're going to repeat the operation later on.
    /// </param>

    member GetDeclarationListInfo : ParsedFileResultsOpt:FSharpParseFileResults option * line: int * colAtEndOfPartialName: int * lineText:string * qualifyingNames: string list * partialName: string * ?hasTextChangedSinceLastTypecheck: (obj * range -> bool) -> Async<FSharpDeclarationListInfo>

    /// <summary>Get the items for a declaration list in FSharpSymbol format</summary>
    ///
    /// <param name="ParsedFileResultsOpt">
    ///    If this is present, it is used to filter declarations based on location in the
    ///    parse tree, specifically at 'open' declarations, 'inherit' of class or interface
    ///    'record field' locations and r.h.s. of 'range' operator a..b
    /// </param>
    /// <param name="line">The line number where the completion is happening</param>
    /// <param name="colAtEndOfNamesAndResidue">The column number (1-based) at the end of the 'names' text </param>
    /// <param name="qualifyingNames">The long identifier to the left of the '.'</param>
    /// <param name="partialName">The residue of a partial long identifier to the right of the '.'</param>
    /// <param name="lineStr">The residue of a partial long identifier to the right of the '.'</param>
    /// <param name="lineText">
    ///    The text of the line where the completion is happening. This is only used to make a couple
    ///    of adhoc corrections to completion accuracy (e.g. checking for "..")
    /// </param>
    /// <param name="hasTextChangedSinceLastTypecheck">
    ///    If text has been used from a captured name resolution from the typecheck, then 
    ///    callback to the client to check if the text has changed. If it has, then give up
    ///    and assume that we're going to repeat the operation later on.
    /// </param>
    member GetDeclarationListSymbols : ParsedFileResultsOpt:FSharpParseFileResults option * line: int * colAtEndOfPartialName: int * lineText:string * qualifyingNames: string list * partialName: string * ?hasTextChangedSinceLastTypecheck: (obj * range -> bool) -> Async<FSharpSymbolUse list list>


    /// <summary>Compute a formatted tooltip for the given location</summary>
    ///
    /// <param name="line">The line number where the information is being requested.</param>
    /// <param name="colAtEndOfNames">The column number at the end of the identifiers where the information is being requested.</param>
    /// <param name="lineText">The text of the line where the information is being requested.</param>
    /// <param name="names">The identifiers at the location where the information is being requested.</param>
    /// <param name="tokenTag">Used to discriminate between 'identifiers', 'strings' and others. For strings, an attempt is made to give a tooltip for a #r "..." location. Use a value from FSharpTokenInfo.Tag, or FSharpTokenTag.Identifier, unless you have other information available.</param>
    member GetToolTipTextAlternate : line:int * colAtEndOfNames:int * lineText:string * names:string list * tokenTag:int -> Async<ToolTipText>

    /// <summary>Compute the Visual Studio F1-help key identifier for the given location, based on name resolution results</summary>
    ///
    /// <param name="line">The line number where the information is being requested.</param>
    /// <param name="colAtEndOfNames">The column number at the end of the identifiers where the information is being requested.</param>
    /// <param name="lineText">The text of the line where the information is being requested.</param>
    /// <param name="names">The identifiers at the location where the information is being requested.</param>
    member GetF1KeywordAlternate                   : line:int * colAtEndOfNames:int * lineText:string * names:string list -> Async<string option>


    /// <summary>Compute a set of method overloads to show in a dialog relevant to the given code location.</summary>
    ///
    /// <param name="line">The line number where the information is being requested.</param>
    /// <param name="colAtEndOfNames">The column number at the end of the identifiers where the information is being requested.</param>
    /// <param name="lineText">The text of the line where the information is being requested.</param>
    /// <param name="names">The identifiers at the location where the information is being requested.</param>
    member GetMethodsAlternate              : line:int * colAtEndOfNames:int * lineText:string * names:string list option -> Async<FSharpMethodGroup>

    /// <summary>Compute a set of method overloads to show in a dialog relevant to the given code location.  The resulting method overloads are returned as symbols.</summary>
    /// <param name="line">The line number where the information is being requested.</param>
    /// <param name="colAtEndOfNames">The column number at the end of the identifiers where the information is being requested.</param>
    /// <param name="lineText">The text of the line where the information is being requested.</param>
    /// <param name="names">The identifiers at the location where the information is being requested.</param>
    member GetMethodsAsSymbols : line:int * colAtEndOfNames:int * lineText:string * names:string list -> Async<FSharpSymbolUse list option>

    /// <summary>Resolve the names at the given location to the declaration location of the corresponding construct.</summary>
    ///
    /// <param name="line">The line number where the information is being requested.</param>
    /// <param name="colAtEndOfNames">The column number at the end of the identifiers where the information is being requested.</param>
    /// <param name="lineText">The text of the line where the information is being requested.</param>
    /// <param name="names">The identifiers at the location where the information is being requested.</param>
    /// <param name="preferFlag">If not given, then get the location of the symbol. If false, then prefer the location of the corresponding symbol in the implementation of the file (rather than the signature if present). If true, prefer the location of the corresponding symbol in the signature of the file (rather than the implementation).</param>
    member GetDeclarationLocationAlternate         : line:int * colAtEndOfNames:int * lineText:string * names:string list * ?preferFlag:bool -> Async<FSharpFindDeclResult>


    /// <summary>Resolve the names at the given location to a use of symbol.</summary>
    ///
    /// <param name="line">The line number where the information is being requested.</param>
    /// <param name="colAtEndOfNames">The column number at the end of the identifiers where the information is being requested.</param>
    /// <param name="lineText">The text of the line where the information is being requested.</param>
    /// <param name="names">The identifiers at the location where the information is being requested.</param>
    member GetSymbolUseAtLocation  : line:int * colAtEndOfNames:int * lineText:string * names:string list -> Async<FSharpSymbolUse option>

    /// <summary>Get any extra colorization info that is available after the typecheck</summary>
    member GetExtraColorizationsAlternate : unit -> (range * FSharpTokenColorKind)[]

    /// <summary>Get the locations of format specifiers</summary>
    member GetFormatSpecifierLocations : unit -> range[]

    /// Get all textual usages of all symbols throughout the file
    member GetAllUsesOfAllSymbolsInFile : unit -> Async<FSharpSymbolUse[]>

    /// Get the textual usages that resolved to the given symbol throughout the file
    member GetUsesOfSymbolInFile : symbol:FSharpSymbol -> Async<FSharpSymbolUse[]>


    [<System.Obsolete("Please change to use GetSymbolUseAtLocation(...).Symbol")>]
    member GetSymbolAtLocationAlternate  : line:int * colAtEndOfNames:int * lineText:string * names:string list -> Async<FSharpSymbol option>

    [<Obsolete("This member has been replaced by GetSymbolAtLocationAlternate, which accepts a 1-based line number rather than a 0-based line number. See https://github.com/fsharp/FSharp.Compiler.Service/issues/64")>]
    member GetSymbolAtLocation  : line:Line0 * colAtEndOfNames:int * lineText:string * names:string list -> FSharpSymbol option

    [<Obsolete("This member has been replaced by GetExtraColorizationsAlternate, which produces 1-based line numbers rather than a 0-based line numbers. See https://github.com/fsharp/FSharp.Compiler.Service/issues/64")>]
    member GetExtraColorizations : unit -> (Range01 * FSharpTokenColorKind)[]

    [<Obsolete("This member has been replaced by GetDeclarationListInfo, which accepts a 1-based line number rather than a 0-based line number. See https://github.com/fsharp/FSharp.Compiler.Service/issues/64")>]
    member GetDeclarations : ParsedFileResultsOpt:FSharpParseFileResults option * line: Line0 * colAtEndOfPartialName: int * lineText:string * qualifyingNames: string list * partialName: string * ?hasTextChangedSinceLastTypecheck: (obj * Range01 -> bool) -> Async<FSharpDeclarationListInfo>

    [<Obsolete("This member has been replaced by GetToolTipTextAlternate, which accepts a 1-based line number rather than a 0-based line number. See https://github.com/fsharp/FSharp.Compiler.Service/issues/64")>]
    member GetToolTipText : line:Line0 * colAtEndOfNames:int * lineText:string * names:string list * tokenTag:int -> ToolTipText

    [<Obsolete("This method has been renamed to GetToolTipText")>]
    member GetDataTipText : line:Line0 * colAtEndOfNames:int * lineText:string * names:string list * tokenTag:int -> ToolTipText

    [<Obsolete("This member has been replaced by GetF1KeywordAlternate, which accepts a 1-based line number rather than a 0-based line number. See https://github.com/fsharp/FSharp.Compiler.Service/issues/64")>]
    member GetF1Keyword                   : line:Line0 * colAtEndOfNames:int * lineText:string * names:string list -> string option

    [<Obsolete("This member has been replaced by GetMethodsAlternate, which accepts a 1-based line number rather than a 0-based line number. See https://github.com/fsharp/FSharp.Compiler.Service/issues/64")>]
    member GetMethods                     : line:Line0 * colAtEndOfNames:int * lineText:string * names:string list option -> FSharpMethodGroup
    [<Obsolete("This member has been replaced by GetDeclarationLocationAlternate, which accepts a 1-based line number rather than a 0-based line number. See https://github.com/fsharp/FSharp.Compiler.Service/issues/64")>]
    member GetDeclarationLocation         : line:Line0 * colAtEndOfNames:int * lineText:string * names:string list * preferSignature:bool -> FSharpFindDeclResult

    [<Obsolete("This overload is obsolete. The tokenTag parameter is no longer required for this member")>]
    member GetDeclarationLocation         : line:Line0 * colAtEndOfNames:int * lineText:string * names:string list * tokenTag:int * preferSignature:bool -> FSharpFindDeclResult

    [<Obsolete("This method has been renamed to 'GetDeclarationListInfo'")>]
    member GetDeclarationsAlternate : ParsedFileResultsOpt:FSharpParseFileResults option * line: int * colAtEndOfPartialName: int * lineText:string * qualifyingNames: string list * partialName: string * ?hasTextChangedSinceLastTypecheck: (obj * range -> bool) -> Async<FSharpDeclarationListInfo>

    [<Obsolete("This method has been renamed to 'GetDeclarationListSymbols'")>]
    member GetDeclarationSymbols : ParsedFileResultsOpt:FSharpParseFileResults option * line: int * colAtEndOfPartialName: int * lineText:string * qualifyingNames: string list * partialName: string * ?hasTextChangedSinceLastTypecheck: (obj * range -> bool) -> Async<FSharpSymbolUse list list>

/// A handle to the results of CheckFileInProject.
[<Sealed>]
type FSharpCheckProjectResults =
    /// The errors returned by processing the project
    member Errors : FSharpErrorInfo[]

    /// Get a view of the overall signature of the assembly. Only valid to use if HasCriticalErrors is false.
    member AssemblySignature : FSharpAssemblySignature

    /// Get a view of the overall contents of the assembly. Only valid to use if HasCriticalErrors is false.
    member AssemblyContents : FSharpAssemblyContents

    /// Get the resolution of the ProjectOptions 
    member ProjectContext : FSharpProjectContext

    /// Get the textual usages that resolved to the given symbol throughout the project
    member GetUsesOfSymbol : symbol:FSharpSymbol -> Async<FSharpSymbolUse[]>

    /// Get all textual usages of all symbols throughout the project
    member GetAllUsesOfAllSymbols : unit -> Async<FSharpSymbolUse[]>

    /// Indicates if critical errors existed in the project options
    member HasCriticalErrors : bool 


/// <summary>Unused in this API</summary>
type UnresolvedReferencesSet 

/// <summary>A set of information describing a project or script build configuration.</summary>
type FSharpProjectOptions = 
    { 
      // Note that this may not reduce to just the project directory, because there may be two projects in the same directory.
      ProjectFileName: string
      /// The files in the project
      ProjectFileNames: string[]
      /// Additional command line argument options for the project. These can include additional files and references.
      OtherOptions: string[]
      /// The command line arguments for the other projects referenced by this project, indexed by the
      /// exact text used in the "-r:" reference in FSharpProjectOptions.
      ReferencedProjects: (string * FSharpProjectOptions)[]
      /// When true, the typechecking environment is known a priori to be incomplete, for
      /// example when a .fs file is opened outside of a project. In this case, the number of error 
      /// messages reported is reduced.
      IsIncompleteTypeCheckEnvironment : bool
      /// When true, use the reference resolution rules for scripts rather than the rules for compiler.
      UseScriptResolutionRules : bool
      /// Timestamp of project/script load, used to differentiate between different instances of a project load.
      /// This ensures that a complete reload of the project or script type checking
      /// context occurs on project or script unload/reload.
      LoadTime : DateTime
      /// Unused in this API and should be 'None'
      UnresolvedReferences : UnresolvedReferencesSet option
    }
    [<Obsolete("This member has been renamed to 'OtherOptions'")>]
    member ProjectOptions: string[] 
         
          
/// Information about the compilation environment    
module internal CompilerEnvironment =
    /// These are the names of assemblies that should be referenced for .fs, .ml, .fsi, .mli files that
    /// are not associated with a project.
    val DefaultReferencesForOrphanSources : string list
    /// Return the compilation defines that should be used when editing the given file.
    val GetCompilationDefinesForEditing : filename : string * compilerFlags : string list -> string list
    /// Return true if this is a subcategory of error or warning message that the language service can emit
    val IsCheckerSupportedSubcategory : string -> bool

/// Information about the debugging environment
module internal DebuggerEnvironment =
    /// Return the language ID, which is the expression evaluator id that the
    /// debugger will use.
    val GetLanguageID : unit -> System.Guid
    
/// This file has become eligible to be re-typechecked.
/// This notifies the language service that it needs to set the dirty flag on files whose typecheck antecedents have changed.
type internal NotifyFileTypeCheckStateIsDirty = NotifyFileTypeCheckStateIsDirty of (string -> unit)
        
/// Identical to _VSFILECHANGEFLAGS in vsshell.idl
type internal DependencyChangeCode =
    | NoChange = 0x0
    | FileChanged = 0x00000001
    | TimeChanged = 0x00000002
    | Deleted = 0x00000008
    | Added = 0x00000010   
    
/// Callback that indicates whether a requested result has become obsolete.    
[<NoComparison;NoEquality>]
type internal IsResultObsolete = 
    | IsResultObsolete of (unit->bool)

/// The result of calling TypeCheckResult including the possibility of abort and background compiler not caught up.
[<RequireQualifiedAccess>]
type FSharpCheckFileAnswer =
    | Aborted // because isResultObsolete caused an abandonment of the operation
    | Succeeded of FSharpCheckFileResults    

#if SILVERLIGHT
#else
#if FX_ATLEAST_45
[<Sealed; AutoSerializable(false)>]      
/// Represents the information gathered by parsing and processing a .fsproj project file format.
type FSharpProjectFileInfo =
    /// Parse and process a .fsproj file 
    static member Parse : fsprojFileName: string * ?properties: (string * string) list * ?enableLogging: bool -> FSharpProjectFileInfo
    /// The command-line arguments for compiling this project
    member Options : string list
    /// The FrameworkVersion for the project
    member FrameworkVersion : string option
    /// The paths to the project files referenced by this project
    member ProjectReferences : string list
    /// The resolved references for the project
    member References : string list
    /// The list of files marked 'Compile' for the project
    member CompileFiles : string list
    /// The list of resource files for the project
    member ResourceFiles : string list
    /// The list of files marked 'Content' in the project
    member ContentFiles : string list
    /// The list of files marked 'None' in the project
    member OtherFiles : string list
    /// The name of the primary output file for the project
    member OutputFile : string option
    /// The directory in which the project file resides
    member Directory : string
    /// The name of the output assembly for the project
    member AssemblyName : string option
    /// The name of the output path for the project
    member OutputPath : string option
    /// The full path to the project file
    member FullPath : string 
    /// Logging output from the build if requested
    member LogOutput : string
#endif
#endif

    /// Parse a source code file, returning information about brace matching in the file
    /// Return an enumeration of the matching parenthetical tokens in the file
    member MatchBraces : filename : string * source: string * options: CheckOptions -> (Range * Range)[]

    /// <summary>
    /// <para>Parse and typecheck all files in a project.</para>
    /// <para>All files are read from the FileSystem API</para>
    /// </summary>
    ///
    /// <param name="options">The options for the project or script.</param>
    member ParseAndCheckProject : options: FSharpProjectOptions -> Async<FSharpCheckProjectResults>

    /// <summary>
    /// <para>For a given script file, get the FSharpProjectOptions implied by the #load closure.</para>
    /// <para>All files are read from the FileSystem API, except the file being checked.</para>
    /// </summary>
    ///
    /// Return None if the background builder is not yet done preparing the type check results for the antecedent to the 
    /// file.
    member TypeCheckSource : parsed: UntypedParseInfo * filename: string * fileversion: int * source: string * options: CheckOptions * isResultObsolete: IsResultObsolete * textSnapshotInfo: obj -> TypeCheckAnswer
    
    /// For a given script file, get the CheckOptions implied by the #load closure
    member GetCheckOptionsFromScriptRoot : filename : string * source : string * loadedTimestamp : System.DateTime -> CheckOptions
        
#if NO_QUICK_SEARCH_HELPERS // only used in QuickSearch prototype
#else
    /// For QuickSearch index - not used by VS2008/VS2010/VS11
    member GetSlotsCount : options : CheckOptions -> int
    /// For QuickSearch index - not used by VS2008/VS2010/VS11
    member UntypedParseForSlot : slot:int * options : CheckOptions -> UntypedParseInfo
#endif // QUICK_SEARCH

    /// Try to get recent type check results for a file. This may arbitrarily refuse to return any
    /// results if the InteractiveChecker would like a chance to recheck the file, in which case
    /// UntypedParse and TypeCheckSource should be called. If the source of the file
    /// has changed the results returned by this function may be out of date, though may
    /// still be usable for generating intellisense menus and information.
    member TryGetRecentTypeCheckResultsForFile : filename: string * options:CheckOptions -> (UntypedParseInfo * TypeCheckResults * (*version*)int) option

    /// This function is called when the entire environment is known to have changed for reasons not encoded in the ProjectOptions of any project/compilation.
    /// For example, the type provider approvals file may have changed.
    member InvalidateAll : unit -> unit    
        
    /// This function is called when the configuration is known to have changed for reasons not encoded in the ProjectOptions.
    /// For example, dependent references may have been deleted or created.
    member InvalidateConfiguration: options: FSharpProjectOptions -> unit    

    /// Begin background parsing the given project.
    member StartBackgroundCompile: options: FSharpProjectOptions -> unit

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

    /// Current queue length of the service, for debug purposes. 
    /// In addition, a single async operation or a step of a background build 
    /// may be in progress - such an operation is not counted in the queue length.
    member CurrentQueueLength : int

    /// This function is called when a project has been cleaned/rebuilt, and thus any live type providers should be refreshed.
    member NotifyProjectCleaned: options: FSharpProjectOptions -> unit    
    
    /// Notify the host that the logical type checking context for a file has now been updated internally
    /// and that the file has become eligible to be re-typechecked for errors.
    ///
    /// The event will be raised on a background thread.
    member BeforeBackgroundFileCheck : IEvent<string>

    /// Raised after a parse of a file in the background analysis.
    ///
    /// The event will be raised on a background thread.
    member FileParsed : IEvent<string>

    /// Raised after a check of a file in the background analysis.
    ///
    /// The event will be raised on a background thread.
    member FileChecked : IEvent<string>
    
    [<Obsolete("Renamed to BeforeBackgroundFileCheck")>]
    member FileTypeCheckStateIsDirty : IEvent<string>

    /// Notify the host that a project has been fully checked in the background (using file contents provided by the file system API)
    ///
    /// The event may be raised on a background thread.
    member ProjectChecked : IEvent<string>

    // For internal use only 
    member internal ReactorOps : IReactorOperations

    [<Obsolete("This member now takes an additional 'otherFlags' argument")>]
    member GetCheckOptionsFromScriptRoot : filename : string * source : string * loadedTimeStamp : DateTime -> FSharpProjectOptions

    [<Obsolete("This member has been renamed to GetProjectOptionsFromScript")>]
    member GetCheckOptionsFromScriptRoot : filename : string * source : string * loadedTimeStamp : DateTime * otherFlags: string[] -> FSharpProjectOptions

    [<Obsolete("This member has been renamed to ParseFileInProject")>]
    member UntypedParse : filename: string * source: string * options: FSharpProjectOptions -> FSharpParseFileResults        

    [<Obsolete("This member has been renamed to CheckFileInProjectIfReady")>]
    member TypeCheckSource : parsed: FSharpParseFileResults * filename: string * fileversion: int * source: string * options: FSharpProjectOptions * isResultObsolete: IsResultObsolete * textSnapshotInfo: obj -> FSharpCheckFileAnswer option
    
    // One shared global singleton for use by multiple add-ins
    static member Instance : FSharpChecker


// An object to typecheck source in a given typechecking environment.
// Used internally to provide intellisense over F# Interactive.
type internal FsiInteractiveChecker =
    internal new : ops: IReactorOperations * tcConfig: TcConfig * tcGlobals: TcGlobals * tcImports: TcImports * tcState: TcState * loadClosure: LoadClosure option ->  FsiInteractiveChecker 
    member internal ParseAndCheckInteraction : source:string -> FSharpParseFileResults * FSharpCheckFileResults * FSharpCheckProjectResults

/// Information about the compilation environment
type [<Class>] CompilerEnvironment =
    /// The default location of FSharp.Core.dll and fsc.exe based on the version of fsc.exe that is running
    static member BinFolderOfDefaultFSharpCompiler : string option -> string option

/// Information about the compilation environment 
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]   
module CompilerEnvironment =
    /// These are the names of assemblies that should be referenced for .fs or .fsi files that
    /// are not asscociated with a project.
    val DefaultReferencesForOrphanSources : string list
    /// Return the compilation defines that should be used when editing the given file.
    val GetCompilationDefinesForEditing : filename : string * compilerFlags : string list -> string list
    /// Return true if this is a subcategory of error or warning message that the language service can emit
    val IsCheckerSupportedSubcategory : string -> bool

/// Information about the debugging environment
module DebuggerEnvironment =
    /// Return the language ID, which is the expression evaluator id that the
    /// debugger will use.
    val GetLanguageID : unit -> Guid
    

/// A set of helpers related to naming of identifiers
module PrettyNaming =
    val IsIdentifierPartCharacter     : char -> bool
    val IsLongIdentifierPartCharacter : char -> bool
    val GetLongNameFromString         : string -> string list

    val FormatAndOtherOverloadsString : int -> string

    /// A utility to help determine if an identifier needs to be quoted 
    val QuoteIdentifierIfNeeded : string -> string

    /// All the keywords in the F# langauge 
    val KeywordNames : string list

[<Obsolete("This type has been renamed to FSharpMethodGroupItemParameter")>]
/// Renamed to FSharpMethodGroupItemParameter
type Param = FSharpMethodGroupItemParameter

[<Obsolete("This type has been renamed to FSharpMethodGroupItem")>]
/// Renamed to FSharpMethodGroupItem
type Method = FSharpMethodGroupItem

[<Obsolete("This type has been renamed to FSharpMethodGroupItem")>]
/// Renamed to FSharpMethodGroupItem
type MethodGroupItem = FSharpMethodGroupItem

[<Obsolete("This type has been renamed to FSharpMethodGroup")>]
/// Renamed to FSharpMethodGroup
type MethodGroup = FSharpMethodGroup

[<Obsolete("This type has been renamed to FSharpProjectOptions")>]
/// Renamed to FSharpProjectOptions
type CheckOptions = FSharpProjectOptions

[<Obsolete("This type has been renamed to FSharpProjectOptions")>]
/// Renamed to FSharpProjectOptions
type ProjectOptions = FSharpProjectOptions

[<Obsolete("This type has been renamed to FSharpCheckFileAnswer")>]
/// Renamed to FSharpCheckFileAnswer
type TypeCheckAnswer = FSharpCheckFileAnswer

[<Obsolete("This type has been renamed to FSharpCheckFileAnswer")>]
/// Renamed to FSharpCheckFileAnswer
type CheckFileAnswer = FSharpCheckFileAnswer

[<Obsolete("This type has been renamed to FSharpCheckFileResults")>]
/// Renamed to FSharpCheckFileResults
type TypeCheckResults = FSharpCheckFileResults

[<Obsolete("This type has been renamed to FSharpParseFileResults")>]
/// Renamed to FSharpParseFileResults
type UntypedParseInfo = FSharpParseFileResults

[<Obsolete("NotifyFileTypeCheckStateIsDirty has been replaced by the FileTypeCheckStateIsDirty event on the FSharpChecker type")>]
/// Obsolete and replaced
type NotifyFileTypeCheckStateIsDirty = NotifyFileTypeCheckStateIsDirty of (string -> unit)
        
[<Obsolete("This type has been renamed to FSharpProjectContext")>]
/// Renamed to FSharpProjectContext
type ProjectContext = FSharpProjectContext

[<Obsolete("This type has been renamed to FSharpCheckFileResults")>]
/// Renamed to FSharpCheckFileResults
type CheckFileResults = FSharpCheckFileResults

[<Obsolete("This type has been renamed to FSharpFindDeclFailureReason")>]
/// Renamed to FSharpFindDeclFailureReason
type FindDeclFailureReason = FSharpFindDeclFailureReason

[<Obsolete("This type has been renamed to FSharpFindDeclResult")>]
/// Renamed to FSharpFindDeclResult
type FindDeclResult = FSharpFindDeclResult

[<Obsolete("This type has been renamed to FSharpCheckProjectResults")>]
/// Renamed to FSharpCheckProjectResults
type CheckProjectResults = FSharpCheckProjectResults

[<Obsolete("This type has been renamed to FSharpChecker")>]
/// Renamed to FSharpChecker
type InteractiveChecker = FSharpChecker
