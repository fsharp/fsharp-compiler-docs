// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.Compiler.SourceCodeServices
open System
open Microsoft.FSharp.Control
open Microsoft.FSharp.Compiler.Lib

// For internal use only 
type internal IReactorOperations = 
    abstract RunAsyncOp : (unit -> 'T) -> Async<'T>
    abstract StartAsyncOp: (unit -> unit) -> unit

module internal Reactor =

    type BuildStepper = (unit -> (*keep building*)bool)
    type Operation = unit -> unit

    type ResultOrException<'TResult> =
        | Result of 'TResult
        | Exception of System.Exception

    [<NoEquality; NoComparison>]
    type ReactorCommands = 
        /// Kick off a build.
        | StartBuild of BuildStepper
        /// Do a bit of work on the given build.
        | Step                                                        
        /// Do some work not synchronized in the mailbox.
        | AsyncOp of Operation 
        /// Stop building after finishing the current unit of work.
        | StopBuild of AsyncReplyChannel<ResultOrException<unit>>              
        /// Finish building.
        | FinishBuild of AsyncReplyChannel<ResultOrException<unit>>            
        override rc.ToString() = 
            match rc with
            | StartBuild _->"StartBuild" 
            | Step->"Step"
            | AsyncOp _->"AsyncOp" 
            | StopBuild _->"StopBuild"
            | FinishBuild _->"FinishBuild"
        
    [<NoEquality; NoComparison>]
    type ReactorState = 
        | Idling
        | ActivelyBuilding of BuildStepper
        | FinishingBuild of BuildStepper * AsyncReplyChannel<ResultOrException<unit>>
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
        let culture = new System.Globalization.CultureInfo(System.Threading.Thread.CurrentThread.CurrentUICulture.LCID)

        /// Mailbox dispatch function.                
        let Dispatch (inbox: MailboxProcessor<_>) =
        
            // Post an exception back to FinishingBuild channel.
            let UnexpectedFinishingBuild commandName (channel: AsyncReplyChannel<_>) = 
                channel.Reply(Exception (new Exception(sprintf "[Bug]Did not expect %s during FinishingBuild." commandName)))        
                
            // Kick off a build.
            let HandleStartBuild build state = 
                inbox.Post Step
                match state with 
                | ActivelyBuilding(_) 
                | Idling -> ActivelyBuilding build 
                | FinishingBuild(_) -> state
                | BackgroundError(_)-> state
                
            // Stop the build.
            let HandleStopBuild (channel:AsyncReplyChannel<_>) state = 
                match state with 
                  | ActivelyBuilding(_) 
                  | Idling -> channel.Reply(Result ())
                  | FinishingBuild(_, channel) -> UnexpectedFinishingBuild "StopBuild" channel
                  | BackgroundError e-> channel.Reply(Exception e)
                Idling
                
            // Do the given operation
            let HandleAsyncOp op state = 
                try 
                    op()
                    state                            
                with 
                | e->
                    System.Diagnostics.Debug.Assert(false, sprintf "Bug in target of HandleAsyncOp: %A: %s\nThe most recent error reported to an error scope: %+A\n" (e.GetType()) e.Message e.StackTrace)
                    state
                
            // Do a step in the build.
            let HandleStep state = 
                match state with
                | FinishingBuild(build,_) 
                | ActivelyBuilding(build) -> 

                    // Gather any required reply channel.
                    let replyChannel = 
                        match state with 
                        | Idling | ActivelyBuilding(_) | BackgroundError(_)->None
                        | FinishingBuild(_,channel)->Some(channel)
                    
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
                        
            
            let HandleFinishBuilding (channel:AsyncReplyChannel<_>) = function
                | ActivelyBuilding(build)->
                    inbox.Post Step
                    FinishingBuild(build,channel)
                | FinishingBuild(_, channelOld)->
                    // Don't expect to get here. If this is required then we need to keep all channels and post back to each
                    // when the build finishes. For now, throw an exception back.                    
                    UnexpectedFinishingBuild "FinishBuilding" channel
                    UnexpectedFinishingBuild "FinishBuilding" channelOld
                    Idling
                | Idling->
                    channel.Reply(Result ())
                    Idling
                | BackgroundError e->
                    // We have a waiting channel to post our exception to.
                    channel.Reply(Exception e)
                    Idling
                    
                                             
            // Async workflow which receives messages and dispatches to worker functions.
            let rec loop (state: ReactorState) = 
                async { let! msg = inbox.Receive()
                        System.Threading.Thread.CurrentThread.CurrentUICulture <- culture

                        let newState = 
                            match msg with
                            | StartBuild build -> HandleStartBuild build state
                            | Step -> HandleStep state
                            | AsyncOp op -> HandleAsyncOp op state
                            | StopBuild channel -> HandleStopBuild channel state
                            | FinishBuild channel -> HandleFinishBuilding channel state

                        return! loop newState
                      }
            loop Idling
            
        let builder = MailboxProcessor<_>.Start(Dispatch)

        // [Foreground Mailbox Accessors] -----------------------------------------------------------                
        member r.StartBuilding(build) = builder.Post(StartBuild build)

        member r.StopBuilding() = 
            match builder.PostAndReply(fun replyChannel->StopBuild(replyChannel)) with
            | Result result->result
            | Exception excn->
                raise excn

        member r.StartAsyncOp(op) =
            builder.Post(AsyncOp(op)) 

        // This is for testing only
        member r.WaitForBackgroundCompile() =
            match builder.PostAndReply(fun replyChannel->FinishBuild(replyChannel)) with
            | Result result->result
            | Exception excn->raise excn

        member r.RunAsyncOp f = 
            let resultCell = AsyncUtil.AsyncResultCell<_>()
            r.StartAsyncOp(
                fun () ->
                    let result =
                        try
                            f () |> AsyncUtil.AsyncOk
                        with
                        |   e -> e |> AsyncUtil.AsyncException
                    resultCell.RegisterResult(result)
            )
            resultCell.AsyncResult

    let mutable theReactor = Reactor()
    let Reactor() = theReactor

