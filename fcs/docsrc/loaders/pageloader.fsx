#r "../_lib/Fornax.Core.dll"


type Shortcut = {
    title: string
    link: string
    icon: string
}

let loader (projectRoot: string) (siteContent: SiteContents) =
    siteContent.Add({title = "Home"; link = "/"; icon = "fas fa-home"})
    siteContent.Add({title = "F# Software Foundation"; link = "https://fsharp.org"; icon = "fas fa-globe"})
    siteContent.Add({title = "GitHub repo"; link = "https://github.com/fsharp/FSharp.Compiler.Service"; icon = "fab fa-github"})
    siteContent.Add({title = "License"; link = "https://github.com/fsharp/FSharp.Compiler.Service/blob/master/LICENSE"; icon = "far fa-file"})
    siteContent.Add({title = "Release Notes"; link = "https://github.com/fsharp/FSharp.Compiler.Service/blob/master/RELEASE_NOTES.md"; icon = "far fa-file-alt"})
    siteContent