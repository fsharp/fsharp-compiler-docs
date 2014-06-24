namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.0.55")>]
[<assembly: AssemblyFileVersionAttribute("0.0.55")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.55"
