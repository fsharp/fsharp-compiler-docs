#r "../_lib/Fornax.Core.dll"
#r "../../packages/docs/FSharp.Formatting/lib/netstandard2.0/FSharp.MetadataFormat.dll"

open System
open System.IO
open FSharp.MetadataFormat

type ApiPageInfo<'a> = {
    ParentName: string
    ParentUrlName: string
    NamespaceName: string
    NamespaceUrlName: string
    Info: 'a
}

type AssemblyEntities = {
  Label: string
  Modules: ApiPageInfo<Module> list
  Types: ApiPageInfo<Type> list
  GeneratorOutput: GeneratorOutput
}

let rec collectModules pn pu nn nu (m: Module) =
    [
        yield { ParentName = pn; ParentUrlName = pu; NamespaceName = nn; NamespaceUrlName = nu; Info =  m}
        yield! m.NestedModules |> List.collect (collectModules m.Name m.UrlName nn nu )
    ]


let loader (projectRoot: string) (siteContent: SiteContents) =
    let label = "FSharp.Compiler.Service"
    let dll = Path.Combine(projectRoot, "..", "..", "artifacts", "bin", "fcs", "Release", "netstandard2.0", "FSharp.Compiler.Service.dll")
    let libs = Path.Combine(projectRoot, "..", "..", "artifacts", "bin", "fcs", "Release", "netstandard2.0")

    if (not (File.Exists dll)) then failwithf "Unable to find FCS dll at %s" dll

    printfn "generating api ref for %s at %s" label dll

    let output = MetadataFormat.Generate(dll, markDownComments = true, publicOnly = true, libDirs = [libs])

    let allModules =
      output.AssemblyGroup.Namespaces
      |> List.collect (fun n ->
          List.collect (collectModules n.Name n.Name n.Name n.Name) n.Modules
      )

    let allTypes =
      [
          yield!
              output.AssemblyGroup.Namespaces
              |> List.collect (fun n ->
                  n.Types |> List.map (fun t -> {ParentName = n.Name; ParentUrlName = n.Name; NamespaceName = n.Name; NamespaceUrlName = n.Name; Info = t} )
              )
          yield!
              allModules
              |> List.collect (fun n ->
                  n.Info.NestedTypes |> List.map (fun t -> {ParentName = n.Info.Name; ParentUrlName = n.Info.UrlName; NamespaceName = n.NamespaceName; NamespaceUrlName = n.NamespaceUrlName; Info = t}) )
      ]
    let entities = {
    Label = label
    Modules = allModules
    Types = allTypes
    GeneratorOutput = output
    }
    siteContent.Add entities
    printfn "generated api ref for %s at %s" label dll

    siteContent