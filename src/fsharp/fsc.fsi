module internal Microsoft.FSharp.Compiler.Driver 

open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Build

//----------------------------------------------------------------------------
// Public helpers - entry point for fsc.exe

#if NO_COMPILER_BACKEND
#else
/// fsc.exe calls this
val mainCompile : argv : string[] * bannerAlreadyPrinted : bool * exiter : Exiter * loggerProvider: (TcConfigBuilder * Exiter -> ErrorLogger) option -> unit

//----------------------------------------------------------------------------
// Internal helpers used to implement the SimpleCodeServices API

type internal SigningInfo = SigningInfo of (* delaysign:*) bool * (*signer:*)  string option * (*container:*) string option

/// A global variable representing a parameter used to configure the compilation service.
val mutable internal tcImportsCapture: (TcImports -> unit) option

/// A global variable representing a parameter used to configure the compilation service.
val mutable internal dynamicAssemblyCreator: (TcConfig * ILGlobals * ErrorLogger * string * string option * ILModuleDef * SigningInfo -> unit) option    

type ILResource with 
    /// Read the bytes from a resource local to an assembly
    member internal Bytes : byte[]


//----------------------------------------------------------------------------
/// Internal helpers used to implement the Visual Studio hosted CodeServices API.
///
/// The F# project system calls this to pop up type provider security dialog if needed.
val internal runFromCommandLineToImportingAssemblies : (string -> unit) * string[] * string * string * Exiter -> unit


#endif
