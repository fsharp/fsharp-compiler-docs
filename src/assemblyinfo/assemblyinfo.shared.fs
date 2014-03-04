namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.0.22")>]
[<assembly: AssemblyFileVersionAttribute("0.0.22")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.22"
