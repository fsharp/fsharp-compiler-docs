namespace FSharp.Compiler.Service.ProjectCracker.Exe

type ProjectOptions =
  {
    ProjectFile: string
    Options: string[]
    ReferencedProjectOptions: (string * ProjectOptions)[]
    LogOutput: string
  }
