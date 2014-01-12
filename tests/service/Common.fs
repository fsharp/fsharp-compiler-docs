module FSharp.Compiler.Service.Tests.Common

open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices

// Create one global interactive checker instance 
let checker = InteractiveChecker.Create()

let parseAndTypeCheckFileInProject (file, input) = 
    let checkOptions = checker.GetProjectOptionsFromScriptRoot(file, input)
    let untypedRes = checker.ParseFileInProject(file, input, checkOptions)
    let typedRes = checker.CheckFileInProject(untypedRes, file, 0, input, checkOptions) |> Async.RunSynchronously
    match typedRes with
    | CheckFileAnswer.Succeeded(res) -> untypedRes, res
    | res -> failwithf "Parsing did not finish... (%A)" res

type TempFile(ext, contents) = 
    let tmpFile =  Path.ChangeExtension(System.IO.Path.GetTempFileName() , ext)
    do File.WriteAllText(tmpFile, contents)
    interface System.IDisposable with 
        member x.Dispose() = try File.Delete tmpFile with _ -> ()
    member x.Name = tmpFile

let getBackgroundParseResultsForScriptText (input) = 
    use file =  new TempFile("fsx", input)
    let checkOptions = checker.GetProjectOptionsFromScriptRoot(file.Name, input)
    checker.GetBackgroundParseResultsForFileInProject(file.Name, checkOptions)  |> Async.RunSynchronously


let getBackgroundCheckResultsForScriptText (input) = 
    use file =  new TempFile("fsx", input)
    let checkOptions = checker.GetProjectOptionsFromScriptRoot(file.Name, input)
    checker.GetBackgroundCheckResultsForFileInProject(file.Name, checkOptions) |> Async.RunSynchronously


