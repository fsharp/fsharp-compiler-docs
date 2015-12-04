namespace Microsoft.FSharp.Compiler.SourceCodeServices.ProjectCrackerTool

type ProjectOptions =
  {
    ProjectFile: string
    Options: string[]
    ReferencedProjectOptions: (string * ProjectOptions)[]
    LogOutput: string
  }
