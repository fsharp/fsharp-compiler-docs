namespace Microsoft.FSharp.Compiler.SourceCodeServices

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

        let arguments = [|
            yield projectFileName
            yield enableLogging.ToString()
            for k, v in properties do
                yield k
                yield v
        |]
        
        let ret, opts = ProjectCrackerTool.crackOpen arguments
        ignore ret
        convert opts, !logMap

    static member GetProjectOptionsFromProjectFile(projectFileName : string, ?properties : (string * string) list, ?loadedTimeStamp) =
        fst (ProjectCracker.GetProjectOptionsFromProjectFileLogged(
                projectFileName,
                ?properties=properties,
                ?loadedTimeStamp=loadedTimeStamp,
                enableLogging=false))
