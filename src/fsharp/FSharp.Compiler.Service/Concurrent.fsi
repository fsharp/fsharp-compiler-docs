module internal Internal.Utilities.Concurrent

    open System

    /// thread safe memoization combinator
    val memoize : ('T -> 'S) -> ('T -> 'S)

    type Atom<'T when 'T : not struct> =
      class
        new : value:'T -> Atom<'T>
        member Set : v:'T -> unit
        member Swap : f:('T -> 'T) -> unit
        member Transact : f:('T -> 'T * 'S) -> 'S
        member Value : 'T
      end

    type Latch =
      class
        new : unit -> Latch
        member Trigger : unit -> bool
        member Value : bool
      end

    type UniqueIntGenerator =
      class
        new : unit -> UniqueIntGenerator
        member Next : unit -> int
      end

    type UniqueInt64Generator =
      class
        new : unit -> UniqueInt64Generator
        member Next : unit -> int64
      end

    /// Thread contextual and portable dependency injection mechanism
    type CompilerThreadContext =
        class
            /// number of threads that currently operate on this context
            member ConsumingThreads : int
            /// Unique identifier for this context instance
            member ContextId : Guid

            /// returns context for current thread, if present.
            static member LocalContext : CompilerThreadContext option
            /// Initiates a new context
            static member Create : unit -> CompilerThreadContext
            /// Installs given context to the current thread
            /// returns disposer for uninstalling
            static member InstallContextToCurrentThread : ctx:CompilerThreadContext -> IDisposable
            /// Installs factory to a global resource
            static member InstallResourceFactory : f:(unit -> 'T) -> Resource<'T>
        end

    and Resource<'T> =
        class
            /// returns thread local value of given resource.
            member ThreadLocalValue : 'T
        end