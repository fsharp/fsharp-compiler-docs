open System.Diagnostics
open System.Text
open System.IO
open System
open System.Runtime.Serialization.Formatters.Binary

open FSharp.Compiler.Service.ProjectCracker

let x (argv: string[]) =
    let p = new System.Diagnostics.Process()
    p.StartInfo.FileName <- Path.Combine(Path.GetDirectoryName(Environment.GetCommandLineArgs().[0]), "FSharp.Compiler.Service.ProjectCracker.exe")
    p.StartInfo.Arguments <- argv.[0] // TODO: Also pass properties
    p.StartInfo.UseShellExecute <- false
    p.StartInfo.RedirectStandardOutput <- true
    //p.StartInfo.RedirectStandardError  <- true

    let sb = new StringBuilder()
    //let sb2 = new StringBuilder()
    //p.OutputDataReceived.Add(fun ev -> ignore <| sb.Append(ev.Data))
    //p.ErrorDataReceived.Add(fun ev -> ignore <| sb2.Append(ev.Data))
    ignore <| p.Start()
    //p.BeginOutputReadLine()
    //p.BeginErrorReadLine()
    p.WaitForExit()

    let fmt = new BinaryFormatter()
    
    let opts = fmt.Deserialize(p.StandardOutput.BaseStream)// :?> ProjectOptions
    0

[<EntryPoint>]
let main argv =
    let p = new System.Diagnostics.Process()
    p.StartInfo.FileName <- Path.Combine(Path.GetDirectoryName(Environment.GetCommandLineArgs().[0]), "FSharp.Compiler.Service.ProjectCracker.exe")
    p.StartInfo.Arguments <- argv.[0] // TODO: Also pass properties
    p.StartInfo.UseShellExecute <- false
    p.StartInfo.RedirectStandardOutput <- true
    ignore <| p.Start()
    p.WaitForExit()
    let fmt = new BinaryFormatter()
    //use fs = new FileStream("DataFile.dat", FileMode.Create)
    //fmt.Serialize(fs, po)
    //fs.Close()

    //use fsin = new FileStream("DataFile.dat", FileMode.Open)
    let opts = fmt.Deserialize(p.StandardOutput.BaseStream)// :?> ProjectOptions
    //fsin.Close()
    printfn "%A" opts
    0 // return an integer exit code
