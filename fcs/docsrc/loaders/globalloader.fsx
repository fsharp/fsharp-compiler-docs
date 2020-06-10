#r "../_lib/Fornax.Core.dll"

type SiteInfo = {
    title: string
    description: string
    theme_variant: string option
    root_url: string
}

let config = {
    title = "FSharp Compiler Service"
    description = "F# compiler services for creating IDE tools, language extensions and for F# embedding"
    theme_variant = Some "blue"
    root_url =
      #if WATCH
        "http://localhost:8080/"
      #else
        "https://fsharp.github.io/FSharp.Compiler.Service/"
      #endif
}

let loader (projectRoot: string) (siteContent: SiteContents) =
    siteContent.Add(config)

    siteContent
