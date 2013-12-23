open System
open System.IO

open Microsoft.FSharp.Compiler.Interactive.Shell

let sbOut = new Text.StringBuilder()
let sbErr = new Text.StringBuilder()
let argv = System.Environment.GetCommandLineArgs()

do
  let inStream = new StringReader("")
  let outStream = new StringWriter(sbOut)
  let errStream = new StringWriter(sbErr)

  let fsiConfig = 
      { // Connect the configuration through to the 'fsi' object from FSharp.Compiler.Interactive.Settings
          new FsiEvaluationSessionHostConfig with 
          member __.FormatProvider = fsi.FormatProvider
          member __.FloatingPointFormat = fsi.FloatingPointFormat
          member __.AddedPrinters = 
              // Unfortunately the printers added by the users are not directly available in FSharp.Compiler.Interactive.Settings.
              // They should be, but we want to avoid modifying FSharp.Compiler.Interactive.Settings to avoid republishing that DLL.
              // So we access these via reflection
              typeof<Microsoft.FSharp.Compiler.Interactive.InteractiveSession>.InvokeMember("AddedPrinters",(System.Reflection.BindingFlags.GetProperty ||| System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance),null,box fsi, [| |]) |> unbox
          member __.ShowDeclarationValues = fsi.ShowDeclarationValues
          member __.ShowIEnumerable = fsi.ShowIEnumerable
          member __.ShowProperties = fsi.ShowProperties
          member __.PrintSize = fsi.PrintSize  
          member __.PrintDepth = fsi.PrintDepth
          member __.PrintWidth = fsi.PrintWidth
          member __.PrintLength = fsi.PrintLength
          member __.ReportUserCommandLineArgs with set args = fsi.CommandLineArgs <- args
          member __.StartServer(fsiServerName) =  failwith "--fsi-server not implemented in this version of fsi.exe"
          member __.EventLoopRun() = fsi.EventLoop.Run()
          member __.EventLoopInvoke(f) = fsi.EventLoop.Invoke(f)
          member __.EventLoopScheduleRestart() = fsi.EventLoop.ScheduleRestart()
          member __.ConsoleReadLine = None }


  let fsi = FsiEvaluationSession(fsiConfig, Array.append argv [|"--noninteractive"|], inStream, outStream, errStream)

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
      | err -> printfn "Error evaluating expression (%s)" err.Message
     // | _ -> printfn "Error evaluating expression (%s)" e.Message
      
