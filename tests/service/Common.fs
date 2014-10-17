module FSharp.Compiler.Service.Tests.Common

open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices

// Create one global interactive checker instance 
let checker = FSharpChecker.Create()

let parseAndTypeCheckFileInProject (file, input) = 
    let checkOptions = checker.GetProjectOptionsFromScript(file, input) |> Async.RunSynchronously
    let parseResult, typedRes = checker.ParseAndCheckFileInProject(file, 0, input, checkOptions) |> Async.RunSynchronously
    match typedRes with
    | FSharpCheckFileAnswer.Succeeded(res) -> parseResult, res
    | res -> failwithf "Parsing did not finish... (%A)" res

type TempFile(ext, contents) = 
    let tmpFile =  Path.ChangeExtension(System.IO.Path.GetTempFileName() , ext)
    do File.WriteAllText(tmpFile, contents)
    interface System.IDisposable with 
        member x.Dispose() = try File.Delete tmpFile with _ -> ()
    member x.Name = tmpFile

#nowarn "57"

let getBackgroundParseResultsForScriptText (input) = 
    use file =  new TempFile("fsx", input)
    let checkOptions = checker.GetProjectOptionsFromScript(file.Name, input) |> Async.RunSynchronously
    checker.GetBackgroundParseResultsForFileInProject(file.Name, checkOptions)  |> Async.RunSynchronously


let getBackgroundCheckResultsForScriptText (input) = 
    use file =  new TempFile("fsx", input)
    let checkOptions = checker.GetProjectOptionsFromScript(file.Name, input) |> Async.RunSynchronously
    checker.GetBackgroundCheckResultsForFileInProject(file.Name, checkOptions) |> Async.RunSynchronously


let sysLib nm = 
    if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then // file references only valid on Windows 
        @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\" + nm + ".dll"
    else
        let sysDir = System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory()
        let (++) a b = System.IO.Path.Combine(a,b)
        sysDir ++ nm + ".dll" 

let fsCore4300() = 
    if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then // file references only valid on Windows 
        @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll"  
    else 
        sysLib "FSharp.Core"


let mkProjectCommandLineArgs (dllName, fileNames) = 
    [|  yield "--simpleresolution" 
        yield "--noframework" 
        yield "--debug:full" 
        yield "--define:DEBUG" 
        yield "--optimize-" 
        yield "--out:" + dllName
        yield "--doc:test.xml" 
        yield "--warn:3" 
        yield "--fullpaths" 
        yield "--flaterrors" 
        yield "--target:library" 
        for x in fileNames do 
            yield x
        let references = 
            [ yield sysLib "mscorlib"
              yield sysLib "System"
              yield sysLib "System.Core"
              yield fsCore4300() ]
        for r in references do
                yield "-r:" + r |]

