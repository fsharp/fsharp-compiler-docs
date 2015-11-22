namespace FSharp.Compiler.Service

open System.Diagnostics
open System.Text
open System.IO
open System
open System.Runtime.Serialization.Formatters.Binary

open FSharp.Compiler.Service.ProjectCracker

type ProjectCrackerReader =

    static member GetProjectOptionsFromProjectFileLogged(projectFileName : string, ?properties : (string * string) list, ?loadedTimeStamp, ?enableLogging) =
        let loadedTimeStamp = defaultArg loadedTimeStamp DateTime.MaxValue // Not 'now', we don't want to force reloading
        let properties = defaultArg properties []
        let enableLogging = defaultArg enableLogging false

        let rec convert (opts: FSharp.Compiler.Service.ProjectCracker.ProjectOptions) : FSharpProjectOptions =
            let referencedProjects = Array.map (fun (a, b) -> a, convert b) opts.ReferencedProjectOptions
            { ProjectFileName = opts.ProjectFile
              ProjectFileNames = [| |]
              OtherOptions = opts.Options
              ReferencedProjects = referencedProjects
              IsIncompleteTypeCheckEnvironment = false
              UseScriptResolutionRules = false
              LoadTime = loadedTimeStamp
              UnresolvedReferences = None }

        let arguments = new StringBuilder()
        arguments.Append(projectFileName) |> ignore
        arguments.Append(' ').Append(enableLogging.ToString()) |> ignore
        for k, v in properties do
            arguments.Append(' ').Append(k).Append(' ').Append(v) |> ignore

        let p = new System.Diagnostics.Process()
        p.StartInfo.FileName <- Path.Combine(Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly().Location),
                                             "FSharp.Compiler.Service.ProjectCracker.exe")
        p.StartInfo.Arguments <- arguments.ToString()
        p.StartInfo.UseShellExecute <- false
        p.StartInfo.CreateNoWindow <- true
        p.StartInfo.RedirectStandardOutput <- true
        ignore <| p.Start()
    
        let ser = new System.Runtime.Serialization.Json.DataContractJsonSerializer(typeof<FSharp.Compiler.Service.ProjectCracker.ProjectOptions>)
        let opts = ser.ReadObject(p.StandardOutput.BaseStream) :?> FSharp.Compiler.Service.ProjectCracker.ProjectOptions
        p.WaitForExit()
        
        convert opts, opts.LogOutput

    let GetProjectOptionsFromProjectFile(projectFileName : string, ?properties : (string * string) list, ?loadedTimeStamp) =
        fst (ic.GetProjectOptionsFromProjectFileLogged(projectFileName, ?properties=properties, ?loadedTimeStamp=loadedTimeStamp))
