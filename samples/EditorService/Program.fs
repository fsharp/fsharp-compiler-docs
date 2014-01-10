// Open the namespace with InteractiveChecker type
open System
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

// Create an interactive checker instance (ignore notifications)
let checker = InteractiveChecker.Create()

let parseWithTypeInfo (file, input) = 
  let checkOptions = checker.GetProjectOptionsFromScriptRoot(file, input, DateTime.Now, [| |])
  let untypedRes = checker.ParseFileInProject(file, input, checkOptions)
  // This might need some time - wait until all DLLs are loaded etc.
  let rec waitForTypeCheck(n) = async {
    let typedRes = checker.TypeCheckFileInProjectIfReady(untypedRes, file, 0, input, checkOptions, IsResultObsolete(fun _ -> false), null)
    match typedRes with
    | TypeCheckFileAnswer.TypeCheckSucceeded(res) -> return untypedRes, res
    | res when n > 100 -> return failwithf "Parsing did not finish... (%A)" res
    | _ -> 
        do! Async.Sleep(100)
        return! waitForTypeCheck(n + 1) }

  waitForTypeCheck 0 |> Async.RunSynchronously

// ----------------------------------------------------------------------------
// Example
// ----------------------------------------------------------------------------

let input = 
  """
  let foo() = 
    let msg = "Hello world"
    if true then 
      printfn "%s" msg.
  """
let inputLines = input.Split('\n')
let file = "/home/user/Test.fsx"

let identToken = Parser.tagOfToken(Parser.token.IDENT("")) 
let untyped, parsed = parseWithTypeInfo(file, input)

// Get tool tip at the specified location
let tip = parsed.GetToolTipText(1, 7, inputLines.[1], ["foo"], identToken)
printfn "%A" tip

// Get declarations (autocomplete) for a location
let decls = 
  parsed.GetDeclarations(Some untyped, 4, 23, inputLines.[4], [], "msg", fun _ -> false)
  |> Async.RunSynchronously

for item in decls.Items do
  printfn " - %s" item.Name
