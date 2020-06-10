#r "../_lib/Fornax.Core.dll"

type UrlRoot = | Root of string
with 
  member x.subRoute (route: string) =
    let (Root root) = x
    root.TrimEnd('/') + "/" + route.TrimStart('/')
  member x.subRoutef pattern = 
    Printf.kprintf x.subRoute pattern

type Theme = 
| Blue
| Green
| Red

type SiteInfo = {
    title: string
    description: string
    theme_variant: Theme
    root_url: UrlRoot
}

let config = {
    title = "FSharp Compiler Service"
    description = "F# compiler services for creating IDE tools, language extensions and for F# embedding"
    theme_variant = Theme.Blue
    root_url =
      #if WATCH
        Root "//localhost:8080"
      #else
        Root "//fsharp.github.io/FSharp.Compiler.Service"
      #endif
}

let loader (projectRoot: string) (siteContent: SiteContents) =
    siteContent.Add(config)
    siteContent
