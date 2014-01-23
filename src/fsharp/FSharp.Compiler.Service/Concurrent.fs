module internal Internal.Utilities.Concurrent

    open System
    open System.Threading
    open System.Collections.Concurrent
    open System.Collections.Generic

    /// thread safe memoization combinator
    let memoize (f : 'T -> 'S) =
        let dict = new ConcurrentDictionary<'T,'S>()
        fun (t : 'T) ->
            let ok, s = dict.TryGetValue t
            if ok then s
            else
                dict.AddOrUpdate(t, f, (fun _ s -> s))

    /// container for optimistic concurrency
    type Atom<'T when 'T : not struct>(value : 'T) =
        let refCell = ref value
    
        let rec swap f = 
            let currentValue = !refCell
            let result = Interlocked.CompareExchange<'T>(refCell, f currentValue, currentValue)
            if obj.ReferenceEquals(result, currentValue) then ()
            else System.Threading.Thread.SpinWait 20; swap f

        let transact f =
            let output = ref Unchecked.defaultof<'S>
            let f' x = let t,s = f x in output := s ; t
            swap f' ; !output
        
        member __.Value with get() : 'T = !refCell
        member __.Swap (f : 'T -> 'T) : unit = swap f
        member __.Set (v : 'T) : unit = swap (fun _ -> v)
        member __.Transact (f : 'T -> 'T * 'S) : 'S = transact f

    /// thread-safe latch
    type Latch () =
        let mutable switch = 0
        member __.Value = switch <> 0
        member __.Trigger () = Interlocked.CompareExchange(&switch, 1, 0) = 0

    /// thread safe unique integer generator
    type UniqueIntGenerator() =
        let mutable count = 0
        member __.Next() = Interlocked.Increment(&count)

    /// thread safe unique integer generator
    type UniqueInt64Generator() =
        let mutable count = 0L
        member __.Next() = Interlocked.Increment(&count)

    /// Thread-cotnextual portable dependency injection mechanism
    type CompilerThreadContext private () =

        static let mutable factoryCount = 0
        static let resourceFactories = new ConcurrentDictionary<int, unit -> obj> () :> IDictionary<_,_>
        static let threadLocalContext = new ThreadLocal<CompilerThreadContext option ref>(fun () -> ref None)

        let contextId = Guid.NewGuid()
        let mutable consumingThreads = 0
        let resourceContainer = new ConcurrentDictionary<int, obj> ()

        /// unique identifier for context
        member __.ContextId = contextId
        member __.ConsumingThreads = consumingThreads

        static member LocalContext = threadLocalContext.Value.Value

        member private __.GetResource<'T> (id : int) =
            let ok, value = resourceContainer.TryGetValue id
            if ok then value :?> 'T
            else
                // resource not installed in context, perform installation now
                let ok, factory = resourceFactories.TryGetValue id
                if ok then
                    resourceContainer.AddOrUpdate(id, (fun _ -> factory ()), (fun _ value -> value)) :?> 'T
                else
                    failwithf "CompilerThreadContext: no factory for resource of type '%O' has been installed." typeof<'T>

        member private ctx.InstallContextToCurrentThread () =
            let threadState = threadLocalContext.Value
            match threadState.Value with
            | Some ctx' when obj.ReferenceEquals(ctx, ctx') -> { new IDisposable with member __.Dispose() = () }
            | Some _ -> invalidOp "CompilerThreadContext: a context is already installed on this thread."
            | None ->
                let _ = Interlocked.Increment &consumingThreads
                threadState := Some ctx

                // return IDisposable uninstaller
                let l = new Latch()
                { 
                    new IDisposable with
                        member __.Dispose () = 
                            if l.Trigger() then 
                                let _ = Interlocked.Decrement &consumingThreads
                                threadState := None
                }

        static member internal GetResource<'T> id =
            match threadLocalContext.Value.Value with
            | None -> failwith "CompilerThreadContext: no context is installed on current thread."
            | Some ctx -> ctx.GetResource<'T> id

        static member InstallContextToCurrentThread (ctx : CompilerThreadContext) =
            ctx.InstallContextToCurrentThread ()

        static member Create () = new CompilerThreadContext ()

        static member InstallResourceFactory(f : unit -> 'T) =
            let id = Interlocked.Increment &factoryCount
            resourceFactories.Add(id, fun () -> f () :> obj)
            new Resource<'T>(id)

    and Resource<'T> internal (resourceId : int) =
        member __.ThreadLocalValue with get () = CompilerThreadContext.GetResource<'T> resourceId