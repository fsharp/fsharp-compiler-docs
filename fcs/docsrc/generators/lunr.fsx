#r "../_lib/Fornax.Core.dll"
#r "../../packages/docs/Newtonsoft.Json/lib/netstandard2.0/Newtonsoft.Json.dll"
#r "../../packages/docs/FSharp.Formatting/lib/netstandard2.0/FSharp.MetadataFormat.dll"

#if !FORNAX
#load "../loaders/contentloader.fsx"
#load "../loaders/apirefloader.fsx"
#load "../loaders/globalloader.fsx"
#endif

open Apirefloader
open FSharp.MetadataFormat


type Entry = {
    uri: string
    title: string
    content: string
}

let generate (ctx : SiteContents) (projectRoot: string) (page: string) =
    printfn "generating search index"
    try
        let siteInfo = ctx.TryGetValue<Globalloader.SiteInfo>().Value
        let rootUrl = siteInfo.root_url
        let pages = ctx.TryGetValues<Contentloader.Post> () |> Option.defaultValue Seq.empty
        let entries =
          pages
          |> Seq.map (fun n ->
              {uri = rootUrl.subRoute (n.link.Replace("content/", "")); title = n.title; content = n.text}
          )

        let all = ctx.TryGetValue<AssemblyEntities>()
        let refs =
          match all with
          | None -> []
          | Some assembyEntities ->
              let generatorOutput = assembyEntities.GeneratorOutput
              let allModules = assembyEntities.Modules
              let allTypes = assembyEntities.Types

              let gen =
                  let ctn =
                      sprintf "%s \n %s" generatorOutput.AssemblyGroup.Name (generatorOutput.AssemblyGroup.Namespaces |> Seq.map (fun n -> n.Name) |> String.concat " ")
                  {uri = (rootUrl.subRoute "/reference/index.html"); title = sprintf "%s - API Reference" assembyEntities.Label; content = ctn }

              let mdlsGen =
                  allModules
                  |> Seq.map (fun m ->
                      let m = m.Info
                      let cnt =
                          sprintf "%s \n %s \n %s \n %s \n %s \n %s"
                              m.Name
                              m.Comment.FullText
                              (m.NestedModules |> List.map (fun m -> m.Name + " " + m.Comment.FullText ) |> String.concat " ")
                              (m.NestedTypes |> List.map (fun m -> m.Name + " " + m.Comment.FullText ) |> String.concat " ")
                              (m.ValuesAndFuncs |> List.map (fun m -> m.Name + " " + m.Comment.FullText ) |> String.concat " ")
                              (m.TypeExtensions |> List.map (fun m -> m.Name + " " + m.Comment.FullText ) |> String.concat " ")


                      {uri = rootUrl.subRoutef "/reference/%s.html" m.UrlName ; title = m.Name; content = cnt }
                  )

              let tsGen =
                  allTypes
                  |> Seq.map (fun m ->
                      let m = m.Info
                      let cnt =
                          sprintf "%s \n %s \n %s"
                              m.Name
                              m.Comment.FullText
                              (m.AllMembers |> List.map (fun m -> m.Name + " " + m.Comment.FullText ) |> String.concat " ")


                      {uri = rootUrl.subRoutef "/reference/%s.html" m.UrlName ; title = m.Name; content = cnt }
                  )
              [yield! entries; gen; yield! mdlsGen; yield! tsGen]
              
        printfn "generated search index"
        [|yield! entries; yield! refs|]
        |> Newtonsoft.Json.JsonConvert.SerializeObject
    with
    | e -> 
        failwithf "error while generating index for %s\n%A" page e
    

