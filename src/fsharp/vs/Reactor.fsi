// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.Compiler.SourceCodeServices

// For internal use only 
type internal IReactorOperations = 

    /// Put the operation in thq queue, and return an async handle to its result. 
    abstract EnqueueAndAwaitOpAsync : (unit -> 'T) -> Async<'T>

    /// Enqueue an operation and return immediately. 
    abstract EnqueueOp: (unit -> unit) -> unit

/// Reactor is intended for long-running but interruptible operations, interleaved
/// with one-off asynchronous operations. 
///
/// It is used to guard the global compiler state while maintaining  responsiveness on 
/// the UI thread.
module internal Reactor = 
    
    /// Reactor operations
    [<Sealed>]
    type Reactor =

        /// Start background building using the given build function, which is called repeatedly
        /// until it returns 'false'
        member StartBackgroundOp : build:(unit -> bool) -> unit

        /// Halt the current implicit background operation
        member StopBackgroundOp : unit -> unit

        /// Block until the current implicit background build is complete.
        member WaitForBackgroundOpCompletion : unit -> unit

        /// Enqueue an operation and return immediately. 
        member EnqueueOp : op:(unit -> unit) -> unit

        /// For debug purposes
        member CurrentQueueLength : int
    
    // TODO: For all AsyncOps: if the operation gets cancelled, the background thread and Reactor don't abandon their work,
    // even when it is ultimately an Eventually<_> compuation which could easily be abandoned, or an IncrementalBuild.Eval
    // operation which can be halted part way through.

        /// Put the operation in thq queue, and return an async handle to its result. 
        member EnqueueAndAwaitOpAsync : (unit -> 'T) -> Async<'T>

    /// Get the reactor for FSharp.Compiler.dll
    val Reactor : unit -> Reactor
  
