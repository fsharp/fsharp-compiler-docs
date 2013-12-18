(*** hide ***)
#I "../../bin/"
(**
Interactive Service: Embedding F# Interactive
=============================================

Referencing all the stuff
*)

#r "FSharp.Compiler.Service.dll"
#r "FSharp.Interactive.Service.dll"
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Interactive.Shell

(**
Creeating
----------------------------------------------------------------------------
*)
open System
open System.IO


let sbOut = new Text.StringBuilder()
let sbErr = new Text.StringBuilder()
let argv = [| "C:\\fsi.exe" |]

let inStream = new StringReader("")
let outStream = new StringWriter(sbOut)
let errStream = new StringWriter(sbErr)
let fsi = FsiEvaluationSession(Array.append argv [|"--noninteractive"|], inStream, outStream, errStream)

(**
Doing something
*)
let evalExpression text =
  match fsi.EvalExpression(text) with
  | Some value -> printfn "%A" value.ReflectionValue
  | None -> printfn "Got no result!"

let evalInteraction text = 
  fsi.EvalInteraction(text)



evalExpression "42+1"
evalInteraction "printfn \"bye\""

(**
Exception handling
----------------------------------------------------------------------------
*)

try 
  evalExpression "42 + 1.0"
with e ->
  match e.InnerException with
  | null -> printfn "Error evaluating expression (%s)" e.Message
  | WrappedError(err, _) -> printfn "Error evaluating expression (Wrapped: %s)" err.Message
  | _ -> printfn "Error evaluating expression (%s)" e.Message
      

(**
Fin!
*)