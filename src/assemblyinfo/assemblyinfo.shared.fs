namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.0.42")>]
[<assembly: AssemblyFileVersionAttribute("0.0.42")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.42"
