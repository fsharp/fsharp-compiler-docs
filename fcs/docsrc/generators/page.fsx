#r "../_lib/Fornax.Core.dll"

#load "./partials/layout.fsx"

open Html


let generate' (ctx : SiteContents) (page: string) =
    let posts =
        ctx.TryGetValues<Contentloader.Post> ()
        |> Option.defaultValue Seq.empty
        |> Seq.map (fun post -> sprintf "content/%s" post.file, post)
        |> Map.ofSeq

    match posts |> Map.tryFind page with
    | Some post ->
        Layout.layout ctx [ !! post.content ] post.title
    | None ->
        let allPostPaths = posts |> Map.toList |> List.map (fst >> fun s -> "* " + s) |> List.sort |> String.concat "\n"
        failwithf "Couldn't find page '%s' in available posts. Known posts are:\n%s" page allPostPaths

let generate (ctx : SiteContents) (projectRoot: string) (page: string) =
    generate' ctx page
    |> Layout.render ctx
