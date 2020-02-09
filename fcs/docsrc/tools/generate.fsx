// --------------------------------------------------------------------------------------
// Builds the documentation from `.fsx` and `.md` files in the 'docsrc/content' directory
// (the generated documentation is stored in the 'docs' directory)
// --------------------------------------------------------------------------------------

#r "paket: groupref generate //"
#load "./.fake/generate.fsx/intellisense.fsx"


// Binaries that have XML documentation (in a corresponding generated XML file)
let referenceBinaries = [ "FSharp.Compiler.Service.dll" ]
// Web site location for the generated documentation
let website = "https://fsharp.github.io/FSharp.Compiler.Service"

// Specify more information about your project
let info =
  [ "project-name", "F# Compiler Services"
    "project-author", "Microsoft Corporation, Dave Thomas, Anh-Dung Phan, Tomas Petricek"
    "project-summary", "F# compiler services for creating IDE tools, language extensions and for F# embedding"
    "project-github", "https://github.com/fsharp/FSharp.Compiler.Service"
    "project-nuget", "https://www.nuget.org/packages/FSharp.Compiler.Service" ]

// --------------------------------------------------------------------------------------
// For typical project, no changes are needed below
// --------------------------------------------------------------------------------------

open Fake
open System.IO
open Fake.IO.FileSystemOperators
open Fake.IO
open Fake.Core
open FSharp.Literate
open FSharp.Formatting.Razor

let root = "."

// Paths with template/source/output locations
let bin         = __SOURCE_DIRECTORY__ @@ "../../../release/fcs/netcoreapp3.0"
let content     = __SOURCE_DIRECTORY__ @@ "../content"
let output      = __SOURCE_DIRECTORY__ @@ "../../../docs"
let files       = __SOURCE_DIRECTORY__ @@ "../files"
let templates   = __SOURCE_DIRECTORY__ @@ "templates"
let formatting  = @"C:\Users\nojaf\.nuget\packages\fsharp.formatting\4.0.0-alpha03" // "__SOURCE_DIRECTORY__ @@ "../../packages/FSharp.Formatting/"
let docTemplate = formatting @@ "templates/docpage.cshtml"

// Where to look for *.csproj templates (in this order)
let layoutRoots =
  [ templates;
    formatting @@ "templates"
    formatting @@ "templates/reference" ]

// Copy static files and CSS + JS from F# Formatting
let copyFiles () =
  Shell.copyRecursive files output true 
  |> Trace.tracefn "Copying file: %A"
  Directory.ensure (output @@ "content")
  Shell.copyRecursive (formatting @@ "styles") (output @@ "content") true
  |> Trace.tracefn "Copying styles and scripts: %A"

let clr = Path.GetDirectoryName(typeof<System.Object>.Assembly.Location) //System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory()
printfn "CLR: %s" clr
let fsfmt = @"C:\Users\nojaf\.nuget\packages\fsharp.formatting\4.0.0-alpha03\lib\netstandard2.0" // __SOURCE_DIRECTORY__ @@ ".." @@ ".." @@ @"packages" @@ "FSharp.Formatting" @@ "lib" @@ "net40"

// Build API reference from XML comments
let buildReference () =
  Shell.cleanDir (output @@ "reference")
  for lib in referenceBinaries do
    RazorMetadataFormat.Generate
      ( bin @@ lib, output @@ "reference", layoutRoots,
        parameters = ("root", root)::info,
        sourceRepo = "https://github.com/fsharp/FSharp.Compiler.Service/tree/master/src",
        sourceFolder = @"..\..\..\src",
        assemblyReferences =
             [clr @@ "System.Runtime.dll"
              clr @@ "System.dll"
              clr @@ "System.Core.dll"
              clr @@ "Microsoft.CSharp.dll"
              clr @@ "System.Linq.dll"
              // clr @@ "System.dll"
              clr @@ "System.Reflection.Metadata.dll"
              clr @@ "System.Numerics.dll"
              clr @@ "System.Collections.Immutable.dll"
              clr @@ "System.IO.dll"
              clr @@ "mscorlib.dll"
              fsfmt @@ "FSharp.MetadataFormat.dll"
              fsfmt @@ "RazorEngine.NetCore.dll"
              @"C:\Users\nojaf\.nuget\packages\fsharp.core\4.7.0\lib\netstandard2.0\FSharp.Core.dll"
              // bin @@ "FSharp.Core.dll"
              bin @@ "FSharp.Compiler.Service.dll"
//              clr @@ "System.Collections.dll"
//              clr @@ "System.Core.dll"
//              clr @@ "System.Data.dll"
//              clr @@ "System.dll"
//              clr @@ "System.Drawing.dll"
//              clr @@ "System.IO.dll"
//              clr @@ "System.Linq.dll"
//              clr @@ "System.Linq.Expressions.dll"
//              clr @@ "System.Net.Requests.dll"
//              clr @@ "System.Numerics.dll"
//              clr @@ "System.Reflection.dll"
//              clr @@ "System.Runtime.dll"
//              clr @@ "System.Runtime.Numerics.dll"
//              clr @@ "System.Threading.dll"
//              clr @@ "System.Threading.Tasks.dll"
//              clr @@ "System.Web.dll"
//              clr @@ "System.Xml.dll"
              @"C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App\2.1.13\Microsoft.AspNetCore.dll"
              @"C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App\2.1.13\Microsoft.AspNetCore.Razor.dll"
              @"C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App\2.1.13\Microsoft.AspNetCore.Razor.Language.dll"
              @"C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App\2.1.13\Microsoft.AspNetCore.Razor.Runtime.dll"
             ] )

// Build documentation from `fsx` and `md` files in `docsrc/content`
let buildDocumentation () =
  for dir in [content] do
    let sub = if dir.Length > content.Length then dir.Substring(content.Length + 1) else "."
    RazorLiterate.ProcessDirectory
      ( dir, docTemplate, output @@ sub, replacements = ("root", root)::info,
        layoutRoots = layoutRoots, generateAnchors = true, processRecursive=false )

// Generate
// copyFiles()
buildDocumentation()
buildReference()

