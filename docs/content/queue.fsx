(*** hide ***)
#I "../../bin/v4.5/"
(**
Compiler Services: Notes on the FSharpChecker operations queue
=================================================

This is a design note on the FSharpChecker component and its operations queue.  See also the notes on the [FSharpChecker caches](caches.html)

FSharpChecker maintains an operations queue. Items from the FSharpChecker operations queue are currently processed 
sequentially and in order. 

In addition to operations in the queue, there is also a low-priority, interleaved background operation
to bring the checking of the checking of "the current background project" up-to-date.  When the queue is empty
this operation is conducted in small incremental fragments of parse/check work (cooperatively time-sliced to be approximately <50ms, 
see `maxTimeShareMilliseconds` in IncrementalBuild.fs). For a working definition of "current background project" 
you currently  have to see the calls to  `StartBackgroundCompile` 
in [service.fs](https://github.com/fsharp/FSharp.Compiler.Service/blob/master/src/fsharp/vs/service.fs),
where an API call which in turn calls StartBackgroundCompile indicates that the current background project has been specified.

Many calls to the FSharpChecker API enqueue an operation in the FSharpChecker compiler queue. These correspond to the 
calls to EnqueueAndAwaitOpAsync in [service.fs](https://github.com/fsharp/FSharp.Compiler.Service/blob/master/src/fsharp/vs/service.fs).

* For example, calling `ParseAndCheckProject` enqueues a `ParseAndCheckProjectImpl` operation. The length of the 
  operation will depend on how much work is required to bring the project analysis up-to-date.

* Likewise, calling any of `GetUsesOfSymbol`, `GetAllUsesOfAllSymbols`, `ParseFileInProject`, 
  `GetBackgroundParseResultsForFileInProject`, `MatchBraces`, `CheckFileInProjectIfReady`, `ParseAndCheckFileInProject`, `GetBackgroundCheckResultsForFileInProject`, 
  `ParseAndCheckProject`, `GetProjectOptionsFromScript`, `InvalidateConfiguration`, `InvaidateAll` and operations 
  on FSharpCheckResults will cause an operation to be enqueued. The length of the operation will 
  vary - many will be very fast - but they won't be processed until other operations already in the queue are complete.

Some operations do not enqueue anything on the FSharpChecker operations queue - notably any accesses to the Symbol APIs.
These are multi-threaded access to the TAST data produced by other FSharpChecker operations.

In advanced interactive scenarios you may need to consider contributing design changes and extensions to how the FSharpChecker queue works, 
in order to better serve your needs. Some tools throw a lot of interactive work at the FSharpChecker operations queue. 
If you are writing such a component, we encourage you to get more involved in making carefully considered 
changes, cleanup and documentation to how the queue works, to ensure it is suitable for your needs. 

For example, from the FCS point of view, it is plausible to consider changing of extending how the 
FSharpChecker operations queue works - the design of the queue and its processing is not sacred. 
In theory you could submit extensions and documentation that allow prioritization of operations, or interleaving of 
operations, or cancellation of operations, or more explicitness about the "current background project", and so on. 

Finally, for those writing interactive editors which use FCS, in the current state of affairs you 
should be cautious about having any auto-operation  (e.g. operations like "Find Unused Declarations" 
which run automatically and are unassociated with a user action, but not operations like "Find All References" 
which a user explicitly triggers) request a check of  the entire project. That will cause very long operations 
and contention on the FSharpChecker operations queue. Any such very-long-running request should normally be associated 
with a user action, and preferably it should be cancellable. 

Alternatively, it is reasonable to trigger these 
kinds of auto-operations when the ProjectChecked event is triggered on FSharpChecker. This event indicates the 
completion of interleaved checking of the "current background project" (specified above). Again, if 
the spec of "current background project" is not suitable or sufficiently controlled for your needs, please 
help to document it and adjust it.

Summary
-------

In this design note, you learned that the FSharpChecker component keeps an operations queue. When using FSharpChecker
in highly interactive situations, you should carefully consider the characteristics of the operations you are 
enqueueing.
*)
