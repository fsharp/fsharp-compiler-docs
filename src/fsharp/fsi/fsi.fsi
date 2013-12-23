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


module Microsoft.FSharp.Compiler.Interactive.Shell

open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

type public FsiEvaluationSessionHostConfig = 
    /// Called by the evaluation session to ask the host for parameters to format text for output
    abstract FormatProvider: System.IFormatProvider  
    /// Called by the evaluation session to ask the host for parameters to format text for output
    abstract FloatingPointFormat: string 
    /// Called by the evaluation session to ask the host for parameters to format text for output
    abstract AddedPrinters : Choice<(System.Type * (obj -> string)), (System.Type * (obj -> obj))>  list
    /// Called by the evaluation session to ask the host for parameters to format text for output
    abstract ShowDeclarationValues: bool  
    /// Called by the evaluation session to ask the host for parameters to format text for output
    abstract ShowIEnumerable: bool  
    /// Called by the evaluation session to ask the host for parameters to format text for output
    abstract ShowProperties : bool  
    /// Called by the evaluation session to ask the host for parameters to format text for output
    abstract PrintSize : int  
    /// Called by the evaluation session to ask the host for parameters to format text for output
    abstract PrintDepth : int  
    /// Called by the evaluation session to ask the host for parameters to format text for output
    abstract PrintWidth : int
    /// Called by the evaluation session to ask the host for parameters to format text for output
    abstract PrintLength : int

    /// The evaluation session calls this to report the preferred view of the command line arguments after 
    /// stripping things like "/use:file.fsx", "-r:Foo.dll" etc.
    abstract ReportUserCommandLineArgs : string [] with set


    /// The evaluation session calls this to ask the host for the special console reader. 
    /// Returning 'Some' indicates a console is to be used, so some special rules apply.
    ///
    /// A "console" gets used if 
    ///     --readline- is specified (the default on Windows + .NET); and 
    ///     not --fsi-server (which should always be combined with --readline-); and 
    ///     ConsoleReadLine() returns a Some
    ///
    /// "Peekahead" occurs if --peekahead- is not specified (i.e. it is the default):
    ///     - If a console is being used then 
    ///         - a prompt is printed early 
    ///         - a background thread is created 
    ///         - the ConsoleReadLine() callback is used to read the first line
    ///     - Otherwise call inReader.Peek()
    ///
    /// Further lines are read as follows:
    ///     - If a console is being used then use ConsoleReadLine()
    ///     - Otherwise use inReader.ReadLine()

    abstract ConsoleReadLine : (unit -> string) option 

    /// The evaluation session calls this at an appropriate point in the startup phase if the --fsi-server parameter was given
    abstract StartServer : fsiServerName:string -> unit
    
    /// Called by the evaluation session to ask the host to enter a dispatch loop like Application.Run().
    /// Only called if --gui option is used (which is the default).
    /// Gets called towards the end of startup and every time a ThreadAbort escaped to the backup driver loop.
    /// Return true if a 'restart' is required, which is a bit meaningless.
    abstract EventLoopRun : unit -> bool

    /// Request that the given operation be run synchronously on the event loop.
    abstract EventLoopInvoke : codeToRun: (unit -> 'T) -> 'T

    /// Schedule a restart for the event loop.
    abstract EventLoopScheduleRestart : unit -> unit


[<Class>]
type FsiValue = 
  member ReflectionValue : obj
  member ReflectionType : System.Type

/// The primary type, representing a full F# Interactive session, reading from the given
/// text input, writing to the given text output and error writers.
type FsiEvaluationSession = 
    new : fsiConfig: FsiEvaluationSessionHostConfig * argv:string[] * inReader:TextReader * outWriter:TextWriter * errorWriter: TextWriter -> FsiEvaluationSession

    /// A host calls this to request an interrupt on the evaluation thread.
    member Interrupt : unit -> unit

    /// A host calls this to get the completions for a long identifier, e.g. in the console
    member GetCompletions : longIdent: string -> seq<string>

    /// Execute the code as if it had been entered as one or more interactions, with an
    /// implicit termination at the end of the input. Stop on first error, discarding the rest
    /// of the input. Errors are sent to the output writer, a 'true' return value indicates there
    /// were no errors overall. Execution is performed on the 'Run()' thread.
    member EvalInteraction : code: string -> unit

    /// Execute the code as if it had been entered as one or more interactions, with an
    /// implicit termination at the end of the input. Stop on first error, discarding the rest
    /// of the input. Errors are sent to the output writer, a 'true' return value indicates there
    /// were no errors overall. Parsing is serialized via an agent, and execution is performed on the
    /// 'Run()' thread.
    member EvalExpression : code: string -> FsiValue option

    [<Experimental("Experimental")>]
    /// Typecheck the given script fragment in the type checking context implied by the current state
    /// of F# Interactive. The results can be used to access intellisense, perform resolutions,
    /// check brace matching and other information.
    ///
    /// TODO: this operation is not yet thread safe, i.e. should not be run concurrently with other operations.
    /// The same applies to subsequent operations performed on the returned object.
    ///
    /// TODO: this operation should really return an object that supports all the operations of the InteractiveChecker
    /// object, i.e. brace matching, untyped parse trees etc.
    member TypeCheckScriptFragment : code: string -> UntypedParseInfo * TypeCheckResults

    /// A host calls this to determine if the --gui parameter is active
    member IsGui : bool

    /// A host calls this to get the active language ID if provided by fsi-server-lcid
    member LCID : int option

    /// A host calls this to report an unhandled exception in a standard way, e.g. an exception on the GUI thread gets printed to stderr
    member ReportUnhandledException : exn: exn -> unit

    /// Load the dummy interaction, load the initial files, and,
    /// if interacting, start the background thread to read the standard input.
    ///
    /// Performs these steps:
    ///    - Load the dummy interaction, if any
    ///    - Set up exception handling, if any
    ///    - Load the initial files, if any
    ///    - Start the background thread to read the standard input, if any
    ///    - Sit in the GUI event loop indefinitely, if needed

    member Run : unit -> unit


/// Defines a read-only input stream used to feed content to the hosted F# Interactive dynamic compiler.
[<AllowNullLiteral>]
type CompilerInputStream = 
    inherit Stream
    new : unit -> CompilerInputStream
    /// Feeds content into the stream.
    member Add: str:string -> unit

/// Defines a write-only stream used to capture output of the hosted F# Interactive dynamic compiler.
[<AllowNullLiteral>]
type CompilerOutputStream  =
    inherit Stream
    new : unit -> CompilerOutputStream

    member Read : unit -> string
