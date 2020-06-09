#r "../_lib/Fornax.Core.dll"


type Shortcut = {
    title: string
    link: string
    icon: string
}

let loader (projectRoot: string) (siteContet: SiteContents) =
    siteContet.Add({title = "Home"; link = "/"; icon = "fas fa-home"})
    siteContet.Add({title = "F# Software Foundation"; link = "https://fsharp.org"; icon = "fas fa-globe"})
    siteContet.Add({title = "GitHub repo"; link = "https://github.com/fsharp/FSharp.Compiler.Service"; icon = "fab fa-github"})
    siteContet.Add({title = "License"; link = "https://github.com/fsharp/FSharp.Compiler.Service/blob/master/LICENSE"; icon = "far fa-file"})
    siteContet.Add({title = "Release Notes"; link = "https://github.com/fsharp/FSharp.Compiler.Service/blob/master/RELEASE_NOTES.md"; icon = "far fa-file-alt"})
    siteContet