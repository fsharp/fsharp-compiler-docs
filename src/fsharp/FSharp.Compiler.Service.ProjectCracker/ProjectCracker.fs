namespace Microsoft.FSharp.Compiler.SourceCodeServices

#if !NETSTANDARD1_6
open System.Runtime.Serialization.Json
open System.Runtime
open System.Diagnostics
#endif
open System.Text
open System.IO
open System

type ProjectCracker =

    static member GetProjectOptionsFromProjectFileLogged(projectFileName : string, ?properties : (string * string) list, ?loadedTimeStamp, ?enableLogging) =
        let loadedTimeStamp = defaultArg loadedTimeStamp DateTime.MaxValue // Not 'now', we don't want to force reloading
        let properties = defaultArg properties []
        let enableLogging = defaultArg enableLogging true
        let logMap = ref Map.empty

        let rec convert (opts: Microsoft.FSharp.Compiler.SourceCodeServices.ProjectCrackerTool.ProjectOptions) : FSharpProjectOptions =
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

#if NETSTANDARD1_6
        let arguments = [|
            yield projectFileName
            yield enableLogging.ToString()
            for k, v in properties do
                yield k
                yield v
        |]
        
        let ret, opts = ProjectCrackerTool.crackOpen arguments
        ignore ret
#else
        let arguments = new StringBuilder()
        arguments.Append('"').Append(projectFileName).Append('"') |> ignore
        arguments.Append(' ').Append(enableLogging.ToString()) |> ignore
        for k, v in properties do
            arguments.Append(' ').Append(k).Append(' ').Append(v) |> ignore
        let codebase = Path.GetDirectoryName(Uri(typeof<ProjectCracker>.Assembly.CodeBase).LocalPath)
        
        let p = new System.Diagnostics.Process()
        p.StartInfo.FileName <- Path.Combine(codebase,"FSharp.Compiler.Service.ProjectCrackerTool.exe")
        p.StartInfo.Arguments <- arguments.ToString()
        p.StartInfo.UseShellExecute <- false
        p.StartInfo.CreateNoWindow <- true
        p.StartInfo.RedirectStandardOutput <- true
        ignore <| p.Start()
    
        let ser = new System.Runtime.Serialization.Json.DataContractJsonSerializer(typeof<Microsoft.FSharp.Compiler.SourceCodeServices.ProjectCrackerTool.ProjectOptions>)
        let opts = ser.ReadObject(p.StandardOutput.BaseStream) :?> Microsoft.FSharp.Compiler.SourceCodeServices.ProjectCrackerTool.ProjectOptions
#endif
        
        convert opts, !logMap

    static member GetProjectOptionsFromProjectFile(projectFileName : string, ?properties : (string * string) list, ?loadedTimeStamp) =
        fst (ProjectCracker.GetProjectOptionsFromProjectFileLogged(
                projectFileName,
                ?properties=properties,
                ?loadedTimeStamp=loadedTimeStamp,
                enableLogging=false))
