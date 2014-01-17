namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.0.11")>]
[<assembly: AssemblyFileVersionAttribute("0.0.11")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.11"
