namespace FSharp.Compiler.Service

open System.Diagnostics
open System.Text
open System.IO
open System
open System.Runtime

open Microsoft.FSharp.Compiler.SourceCodeServices

type ProjectCracker =

    static member GetProjectOptionsFromProjectFileLogged(projectFileName : string, ?properties : (string * string) list, ?loadedTimeStamp, ?enableLogging) =
        let loadedTimeStamp = defaultArg loadedTimeStamp DateTime.MaxValue // Not 'now', we don't want to force reloading
        let properties = defaultArg properties []
        let enableLogging = defaultArg enableLogging false
        let logMap = ref Map.empty

        let rec convert (opts: FSharp.Compiler.Service.ProjectCracker.Exe.ProjectOptions) : FSharpProjectOptions =
            let referencedProjects = Array.map (fun (a, b) -> a, convert b) opts.ReferencedProjectOptions
            logMap := Map.add opts.ProjectFile opts.LogOutput !logMap
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
                                             "FSharp.Compiler.Service.ProjectCracker.Exe.exe")
        p.StartInfo.Arguments <- arguments.ToString()
        p.StartInfo.UseShellExecute <- false
        p.StartInfo.CreateNoWindow <- true
        p.StartInfo.RedirectStandardOutput <- true
        ignore <| p.Start()
    
        let fmt = new Serialization.Formatters.Binary.BinaryFormatter()
        let opts = fmt.Deserialize(p.StandardOutput.BaseStream) :?> FSharp.Compiler.Service.ProjectCracker.Exe.ProjectOptions
        p.WaitForExit()
        
        convert opts, !logMap

    static member GetProjectOptionsFromProjectFile(projectFileName : string, ?properties : (string * string) list, ?loadedTimeStamp) =
        fst (ProjectCracker.GetProjectOptionsFromProjectFileLogged(projectFileName, ?properties=properties, ?loadedTimeStamp=loadedTimeStamp))
