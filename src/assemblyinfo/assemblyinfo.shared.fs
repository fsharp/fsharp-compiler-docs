namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.0.17")>]
[<assembly: AssemblyFileVersionAttribute("0.0.17")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.17"
