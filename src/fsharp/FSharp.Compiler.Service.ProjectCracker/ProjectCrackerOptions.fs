namespace FSharp.Compiler.Service.ProjectCracker

type ProjectOptions =
  {
    ProjectFile: string
    Options: string[]
    ReferencedProjectOptions: (string * ProjectOptions)[]
    LogOutput: string
  }
