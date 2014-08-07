
#if INTERACTIVE
#r "../../bin/v45/FSharp.Compiler.Service.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#load "FsUnit.fs"
#load "Common.fs"
#else
module FSharp.Compiler.Service.Tests.InteractiveChecker
#endif

open NUnit.Framework
open FsUnit
open System
open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
open FSharp.Compiler.Service.Tests.Common

let longIdentToString (longIdent: Ast.LongIdent) =
    String.Join(".", longIdent |> List.map (fun ident -> ident.ToString()))
let longIdentWithDotsToString (Ast.LongIdentWithDots (longIdent, _)) = longIdentToString longIdent

let posToTuple (pos: Range.pos) = (pos.Line, pos.Column)
let rangeToTuple (range: Range.range) = (posToTuple range.Start, posToTuple range.End)

let identsAndRanges (input: Ast.ParsedInput) =
    let identAndRange ident (range: Range.range) =
      (ident, rangeToTuple range)
    let identAndRange' (componentInfo: Ast.SynComponentInfo) =
        let ((Ast.SynComponentInfo.ComponentInfo(attrs, typarDecls, typarConstraints, longIdent, _, _, _, range))) = componentInfo
        // TODO : attrs, typarDecls and typarConstraints
        [identAndRange (longIdentToString longIdent) range]
    let identsAndRanges''' (typeDefn: Ast.SynTypeDefn) =
        let (Ast.SynTypeDefn.TypeDefn(componentInfo, repr, members, _)) = typeDefn
        // TODO : repr and members
        identAndRange' componentInfo
    let rec identsAndRanges'' (moduleDecl: Ast.SynModuleDecl) =
        match moduleDecl with
        | Ast.SynModuleDecl.Types(typeDefns, _) -> (typeDefns |> List.collect identsAndRanges''')
        | Ast.SynModuleDecl.ModuleAbbrev(ident, _, range) -> [ identAndRange (ident.ToString()) range ]
        | Ast.SynModuleDecl.NestedModule(componentInfo, decls, _, _) -> (identAndRange' componentInfo) @ (decls |> List.collect identsAndRanges'')
        | Ast.SynModuleDecl.Let(_, _, _) -> failwith "Not implemented yet"
        | Ast.SynModuleDecl.DoExpr(_, _, range) -> failwith "Not implemented yet"
        | Ast.SynModuleDecl.Exception(_, range) -> failwith "Not implemented yet"
        | Ast.SynModuleDecl.Open(longIdentWithDots, range) -> [ identAndRange (longIdentWithDotsToString longIdentWithDots) range ]
        | Ast.SynModuleDecl.Attributes(attrs, range) -> failwith "Not implemented yet"
        | Ast.SynModuleDecl.HashDirective(_, range) -> failwith "Not implemented yet"
        | Ast.SynModuleDecl.NamespaceFragment(moduleOrNamespace) -> identsAndRanges' moduleOrNamespace
    and identsAndRanges' (Ast.SynModuleOrNamespace(longIdent, _, moduleDecls, _, _, _, range)) =
        (identAndRange (longIdentToString longIdent) range) :: (moduleDecls |> List.collect identsAndRanges'')

    match input with
    | Ast.ImplFile(Ast.ParsedImplFileInput(_, _, _, _, _, modulesOrNamespaces, _)) ->
         modulesOrNamespaces |> List.collect identsAndRanges'
    | Ast.SigFile _ -> []

let parseAndExtractRanges code =
    let file = "/home/user/Test.fsx"
    let checker = InteractiveChecker.Create()
    let result =
        async {
            let! projectOptions = checker.GetProjectOptionsFromScript(file, code)
            let! input = checker.ParseFileInProject(file, code, projectOptions)
            return input.ParseTree
        }
        |> Async.RunSynchronously
    match result with
    | Some tree -> tree |> identsAndRanges
    | None -> failwith "fail to parse..."

let input =
    """
    namespace N

    type Sample () = class end
    """

[<Test>]
let ``Test ranges - namespace`` () =
    parseAndExtractRanges input |> should equal [("N", ((4, 4), (5, 4))); ("Sample", ((4, 9), (4, 15)))]

let input2 =
    """
    module M

    type Sample () = class end
    """
    
[<Test>]
let ``Test ranges - module`` () =
    parseAndExtractRanges input2 |> should equal [("M", ((4, 4), (5, 4))); ("Sample", ((4, 9), (4, 15)))]

let input3 =
    """
    namespace global

    type Sample () = class end
    """

[<Test>]
let ``Test ranges - global namespace`` () =
    parseAndExtractRanges input3 |> should equal [("", ((4, 4), (5, 4))); ("Sample", ((4, 9), (4, 15)))]
