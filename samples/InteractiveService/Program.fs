open System
open System.IO

open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Interactive.Shell

let sbOut = new Text.StringBuilder()
let sbErr = new Text.StringBuilder()
let argv = System.Environment.GetCommandLineArgs()

do
  let inStream = new StringReader("")
  let outStream = new StringWriter(sbOut)
  let errStream = new StringWriter(sbErr)
  let fsi = FsiEvaluationSession(Array.append argv [|"--noninteractive"|], inStream, outStream, errStream)

  while true do
    try
      
      let text = Console.ReadLine()
      if text.StartsWith("=") then 
        match fsi.EvalExpression(text.Substring(1)) with
        | Some value -> printfn "%A" value.ReflectionValue
        | None -> printfn "Got no result!"
      else
        fsi.EvalInteraction(text)
        printfn "Ok"

    with e ->
      match e.InnerException with
      | null -> printfn "Error evaluating expression (%s)" e.Message
      | WrappedError(err, _) -> printfn "Error evaluating expression (%s)" err.Message
      | _ -> printfn "Error evaluating expression (%s)" e.Message
      
