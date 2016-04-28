open System
open System.IO
open System.Reflection
open NUnitLite
open NUnit.Common

type private TypeInThisAssembly = class end

[<EntryPoint>]
let main argv = 
    printfn "Testing...Testing...1...2...3..."
    let writer = new ExtendedTextWrapper(Console.Out)
    let runner = new AutoRun(typeof<TypeInThisAssembly>.GetTypeInfo().Assembly)
    runner.Execute(argv, writer, Console.In)
