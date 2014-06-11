// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.Compiler.SourceCodeServices
open System
open Microsoft.FSharp.Control
open Microsoft.FSharp.Compiler.Lib

// For internal use only 
type internal IReactorOperations = 
    abstract EnqueueAndAwaitOpAsync : (unit -> 'T) -> Async<'T>
    abstract EnqueueOp: (unit -> unit) -> unit

module internal Reactor =

    type ResultOrException<'TResult> =
        | Result of 'TResult
        | Exception of System.Exception

    [<NoEquality; NoComparison>]
    type ReactorCommands = 
        /// Kick off a build.
        | StartBackgroundOp of (unit -> bool)
        /// Do a bit of work on the given build.
        | Step                                                        
        /// Do some work not synchronized in the mailbox.
        | Op of (unit -> unit) 
        /// Stop building after finishing the current unit of work.
        | StopBackgroundOp of AsyncReplyChannel<ResultOrException<unit>>              
        /// Finish building.
        | FinishBackgroundOp of AsyncReplyChannel<ResultOrException<unit>>            
        override rc.ToString() = 
            match rc with
            | StartBackgroundOp _->"StartBackgroundOp" 
            | Step->"Step"
            | Op _->"Op" 
            | StopBackgroundOp _->"StopBackgroundOp"
            | FinishBackgroundOp _->"FinishBackgroundOp"
        
    [<NoEquality; NoComparison>]
    type ReactorState = 
        | Idling
        | ActivelyBuilding of (unit -> bool)
        | FinishingBuild of (unit -> bool) * AsyncReplyChannel<ResultOrException<unit>>
        /// An exception was seen in a prior state. The exception is preserved so it can be thrown back to the calling thread.
        | BackgroundError of Exception                                         
        override rs.ToString() = 
            match rs with 
            | Idling->"Idling" 
            | ActivelyBuilding _->"ActivelyBuilding"
            | FinishingBuild _->"FinishingBuild" 
            | BackgroundError _->"BackgroundError"

     [<AutoSerializable(false);Sealed>]
     /// There is one global Reactor for the entire language service, no matter how many projects or files
     /// are open. 
     type Reactor() = 
        // We need to store the culture for the VS thread that is executing now,
        // so that when the reactor picks up a thread from the threadpool we can set the culture
#if SILVERLIGHT
        let culture = System.Threading.Thread.CurrentThread.CurrentCulture
#else
        let culture = new System.Globalization.CultureInfo(System.Threading.Thread.CurrentThread.CurrentUICulture.LCID)
#endif

        // Post an exception back to FinishingBuild channel.
        let UnexpectedFinishingBuild commandName (channel: AsyncReplyChannel<_>) = 
            channel.Reply(Exception (new Exception(sprintf "[Bug]Did not expect %s during FinishingBuild." commandName)))        
                
        // Kick off a build.
        let HandleStartBackgroundOp (inbox: MailboxProcessor<_>)  build state = 
            inbox.Post Step
            match state with 
            | ActivelyBuilding _oldBuild -> ActivelyBuilding build  // replace the background build
            | Idling -> ActivelyBuilding build  // start the background build
            | FinishingBuild _  -> state // ignore the request for a new background build
            | BackgroundError _ -> state // ignore the request for a new background build until error is reported
                
        // Stop the build.
        let HandleStopBackgroundOp (channel:AsyncReplyChannel<_>) state = 
            match state with 
            | ActivelyBuilding _oldBuild -> channel.Reply(Result ())
            | Idling -> channel.Reply(Result ())
            | FinishingBuild(_, channel) -> UnexpectedFinishingBuild "StopBackgroundOp" channel
            | BackgroundError e-> channel.Reply(Exception e)

            Idling
                
        // Interleave the given operation with other work
        let HandleOp op state = 
            try 
                op()
                state                            
            with e ->
                System.Diagnostics.Debug.Assert(false, sprintf "Bug in target of HandleOp: %A: %s\nThe most recent error reported to an error scope: %+A\n" (e.GetType()) e.Message e.StackTrace)
                state
                
        // Do a step in the build.
        let HandleStep (inbox: MailboxProcessor<_>)  state = 
            match state with
            | FinishingBuild(build,_) 
            | ActivelyBuilding(build) -> 

                // Gather any required reply channel.
                let replyChannel = 
                    match state with 
                    | Idling | ActivelyBuilding _ | BackgroundError _ -> None
                    | FinishingBuild(_,channel) -> Some channel
                    
                try
                    if build() then
                        // More work
                        inbox.Post Step
                        state
                    else
                        // Work is done. Reply if there is a channel for it.
                        match replyChannel with
                        | Some(replyChannel)-> replyChannel.Reply(Result ())    
                        | None->()

                        // Switch to idle state.
                        Idling
                with e->
                    System.Diagnostics.Debug.Assert(false, sprintf "[Bug]Failure in HandleStep: %s" (e.ToString()))
                    match replyChannel with
                    | Some(replyChannel)->
                        replyChannel.Reply(Exception e)
                        Idling
                    | None->BackgroundError e                 

            | Idling -> Idling

            | BackgroundError _ -> state
                        
            
        let HandleFinishBackgroundO (inbox: MailboxProcessor<_>)  (channel:AsyncReplyChannel<_>) state = 
            match state with
            | ActivelyBuilding(build) ->
                inbox.Post Step
                FinishingBuild(build,channel)

            | FinishingBuild(_, channelOld) ->
                // Don't expect to get here. If this is required then we need to keep all channels and post back to each
                // when the build finishes. For now, throw an exception back.                    
                UnexpectedFinishingBuild "FinishBackgroundOping" channel
                UnexpectedFinishingBuild "FinishBackgroundOping" channelOld
                Idling

            | Idling ->
                channel.Reply(Result ())
                Idling

            | BackgroundError e ->
                // We have a waiting channel to post our exception to.
                channel.Reply(Exception e)
                Idling
                    
        /// Mailbox dispatch function.                
        let builder = 
          MailboxProcessor<_>.Start <| fun inbox ->        
                                             
            // Async workflow which receives messages and dispatches to worker functions.
            let rec loop (state: ReactorState) = 
                async { let! msg = inbox.Receive()
                        System.Threading.Thread.CurrentThread.CurrentUICulture <- culture

                        let newState = 
                            match msg with
                            | StartBackgroundOp build -> HandleStartBackgroundOp inbox build state 
                            | Step -> HandleStep inbox state
                            | Op op -> HandleOp op state
                            | StopBackgroundOp channel -> HandleStopBackgroundOp channel state
                            | FinishBackgroundOp channel -> HandleFinishBackgroundO inbox channel state

                        return! loop newState
                      }
            loop Idling
            

        // [Foreground Mailbox Accessors] -----------------------------------------------------------                
        member r.StartBackgroundOp(build) = builder.Post(StartBackgroundOp build)

        member r.StopBackgroundOp() = 
            match builder.PostAndReply(fun replyChannel->StopBackgroundOp(replyChannel)) with
            | Result result->result
            | Exception excn->
                raise excn

        member r.EnqueueOp(op) =
            builder.Post(Op(op)) 

        member r.CurrentQueueLength =
            builder.CurrentQueueLength

        // This is for testing only
        member r.WaitForBackgroundOpCompletion() =
            match builder.PostAndReply(fun replyChannel->FinishBackgroundOp(replyChannel)) with
            | Result result->result
            | Exception excn->raise excn

        member r.EnqueueAndAwaitOpAsync f = 
            let resultCell = AsyncUtil.AsyncResultCell<_>()
            r.EnqueueOp(
                fun () ->
                    let result =
                        try
                            f () |> AsyncUtil.AsyncOk
                        with
                        |   e -> e |> AsyncUtil.AsyncException
                    resultCell.RegisterResult(result)
            )
            resultCell.AsyncResult

    let theReactor = Reactor()
    let Reactor() = theReactor

