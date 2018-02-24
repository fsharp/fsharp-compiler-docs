﻿// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Threading
open System.Threading.Tasks
open System.Runtime.CompilerServices

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Completion
open Microsoft.CodeAnalysis.Options
open Microsoft.CodeAnalysis.Text

open Microsoft.VisualStudio.FSharp.LanguageService
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

type internal FSharpCompletionProvider
    (
        workspace: Workspace,
        serviceProvider: SVsServiceProvider,
        checkerProvider: FSharpCheckerProvider,
        projectInfoManager: FSharpProjectOptionsManager,
        assemblyContentProvider: AssemblyContentProvider
    ) =

    inherit CompletionProvider()

    static let userOpName = "CompletionProvider"
    static let declarationItemsCache = ConditionalWeakTable<string, FSharpDeclarationListItem>()
    static let [<Literal>] NameInCodePropName = "NameInCode"
    static let [<Literal>] FullNamePropName = "FullName"
    static let [<Literal>] IsExtensionMemberPropName = "IsExtensionMember"
    static let [<Literal>] NamespaceToOpenPropName = "NamespaceToOpen"
    static let [<Literal>] IsKeywordPropName = "IsKeyword"

    static let keywordCompletionItems =
        Lexhelp.Keywords.keywordsWithDescription
        |> List.filter (fun (keyword, _) -> not (PrettyNaming.IsOperatorName keyword))
        |> List.sortBy (fun (keyword, _) -> keyword)
        |> List.mapi (fun n (keyword, description) ->
             CommonCompletionItem.Create(keyword, CompletionItemRules.Default, Nullable Glyph.Keyword, sortText = sprintf "%06d" (1000000 + n))
                .AddProperty("description", description)
                .AddProperty(IsKeywordPropName, ""))
    
    let checker = checkerProvider.Checker

    let xmlMemberIndexService = serviceProvider.GetService(typeof<IVsXMLMemberIndexService>) :?> IVsXMLMemberIndexService
    let documentationBuilder = XmlDocumentation.CreateDocumentationBuilder(xmlMemberIndexService, serviceProvider.DTE)
        
    static let noCommitOnSpaceRules = 
        // These are important.  They make sure we don't _commit_ autocompletion when people don't expect them to.  Some examples:
        //
        // * type Foo() =
        //       member val a = 12 with get, <<---- Don't commit autocomplete!
        //
        // * type MyRecord = { name: <<---- Don't commit autocomplete!
        //
        // * type My< <<---- Don't commit autocomplete!
        //
        // * let myClassInstance = MyClass(Date= <<---- Don't commit autocomplete!
        //
        // * let xs = [1..10] <<---- Don't commit autocomplete! (same for arrays)
        let noCommitChars = [|' '; '='; ','; '.'; '<'; '>'; '('; ')'; '!'; ':'; '['; ']'; '|'|].ToImmutableArray()

        CompletionItemRules.Default.WithCommitCharacterRule(CharacterSetModificationRule.Create(CharacterSetModificationKind.Remove, noCommitChars))
    
    static let getRules() = if Settings.IntelliSense.ShowAfterCharIsTyped then noCommitOnSpaceRules else CompletionItemRules.Default

    static let mruItems = Dictionary<(* Item.FullName *) string, (* hints *) int>()
    
    static member ShouldTriggerCompletionAux(sourceText: SourceText, caretPosition: int, trigger: CompletionTriggerKind, getInfo: (unit -> DocumentId * string * string list)) =
        // Skip if we are at the start of a document
        if caretPosition = 0 then false
        // Skip if it was triggered by an operation other than insertion
        elif not (trigger = CompletionTriggerKind.Insertion) then false
        // Skip if we are not on a completion trigger
        else
            let triggerPosition = caretPosition - 1
            let triggerChar = sourceText.[triggerPosition]

            // do not trigger completion if it's not single dot, i.e. range expression
            if not Settings.IntelliSense.ShowAfterCharIsTyped && triggerPosition > 0 && sourceText.[triggerPosition - 1] = '.' then
                false
            else
                let documentId, filePath, defines = getInfo()
                CompletionUtils.shouldProvideCompletion(documentId, filePath, defines, sourceText, triggerPosition) &&
                (triggerChar = '.' || (Settings.IntelliSense.ShowAfterCharIsTyped && CompletionUtils.isStartingNewWord(sourceText, triggerPosition)))
                

    static member ProvideCompletionsAsyncAux(checker: FSharpChecker, sourceText: SourceText, caretPosition: int, options: FSharpProjectOptions, filePath: string, 
                                             textVersionHash: int, getAllSymbols: FSharpCheckFileResults -> AssemblySymbol list) = 
        asyncMaybe {
            let! parseResults, _, checkFileResults = checker.ParseAndCheckDocument(filePath, textVersionHash, sourceText.ToString(), options, allowStaleResults = true, userOpName = userOpName)

            let textLines = sourceText.Lines
            let caretLinePos = textLines.GetLinePosition(caretPosition)
            let caretLine = textLines.GetLineFromPosition(caretPosition)
            let fcsCaretLineNumber = Line.fromZ caretLinePos.Line  // Roslyn line numbers are zero-based, FSharp.Compiler.Service line numbers are 1-based
            let caretLineColumn = caretLinePos.Character
            let partialName = QuickParse.GetPartialLongNameEx(caretLine.ToString(), caretLineColumn - 1) 
            
            let getAllSymbols() = 
                getAllSymbols checkFileResults
                |> List.filter (fun entity -> entity.FullName.Contains "." && not (PrettyNaming.IsOperatorName entity.Symbol.DisplayName))

            let! declarations = checkFileResults.GetDeclarationListInfo(Some(parseResults), fcsCaretLineNumber, caretLine.ToString(), 
                                                                        partialName, getAllSymbols, userOpName=userOpName) |> liftAsync
            let results = List<Completion.CompletionItem>()
            
            let getKindPriority = function
                | CompletionItemKind.Property -> 0
                | CompletionItemKind.Field -> 1
                | CompletionItemKind.Method (isExtension = false) -> 2
                | CompletionItemKind.Event -> 3
                | CompletionItemKind.Argument -> 4
                | CompletionItemKind.Other -> 5
                | CompletionItemKind.Method (isExtension = true) -> 6

            let sortedDeclItems =
                declarations.Items
                |> Array.sortWith (fun x y ->
                    let mutable n = (not x.IsResolved).CompareTo(not y.IsResolved)
                    if n <> 0 then n else
                        n <- (getKindPriority x.Kind).CompareTo(getKindPriority y.Kind) 
                        if n <> 0 then n else
                            n <- (not x.IsOwnMember).CompareTo(not y.IsOwnMember)
                            if n <> 0 then n else
                                n <- StringComparer.OrdinalIgnoreCase.Compare(x.Name, y.Name)
                                if n <> 0 then n else
                                    x.MinorPriority.CompareTo(y.MinorPriority))

            let maxHints = if mruItems.Values.Count = 0 then 0 else Seq.max mruItems.Values

            sortedDeclItems |> Array.iteri (fun number declItem ->
                let glyph = Tokenizer.FSharpGlyphToRoslynGlyph (declItem.Glyph, declItem.Accessibility)
                let name =
                    match declItem.NamespaceToOpen with
                    | Some namespaceToOpen -> sprintf "%s (open %s)" declItem.Name namespaceToOpen
                    | _ -> declItem.Name
                    
                let filterText =
                    match declItem.NamespaceToOpen, declItem.Name.Split '.' with
                    // There is no namespace to open and the item name does not contain dots, so we don't need to pass special FilterText to Roslyn.
                    | None, [|_|] -> null
                    // Either we have a namespace to open ("DateTime (open System)") or item name contains dots ("Array.map"), or both.
                    // We are passing last part of long ident as FilterText.
                    | _, idents -> Array.last idents

                let completionItem = 
                    CommonCompletionItem.Create(name, glyph = Nullable glyph, rules = getRules(), filterText = filterText)
                                        .AddProperty(FullNamePropName, declItem.FullName)
                        
                let completionItem =
                    match declItem.Kind with
                    | CompletionItemKind.Method (isExtension = true) ->
                          completionItem.AddProperty(IsExtensionMemberPropName, "")
                    | _ -> completionItem
                
                let completionItem =
                    if name <> declItem.NameInCode then
                        completionItem.AddProperty(NameInCodePropName, declItem.NameInCode)
                    else completionItem

                let completionItem =
                    match declItem.NamespaceToOpen with
                    | Some ns -> completionItem.AddProperty(NamespaceToOpenPropName, ns)
                    | None -> completionItem

                let priority = 
                    match mruItems.TryGetValue declItem.FullName with
                    | true, hints -> maxHints - hints
                    | _ -> number + maxHints + 1

                let sortText = sprintf "%06d" priority

                //#if DEBUG
                //Logging.Logging.logInfof "***** %s => %s" name sortText
                //#endif

                let completionItem = completionItem.WithSortText(sortText)

                declarationItemsCache.Remove(completionItem.DisplayText) |> ignore // clear out stale entries if they exist
                declarationItemsCache.Add(completionItem.DisplayText, declItem)
                results.Add(completionItem))

            if results.Count > 0 && not declarations.IsForType && not declarations.IsError && List.isEmpty partialName.QualifyingIdents then
                let lineStr = textLines.[caretLinePos.Line].ToString()
                
                let completionContext =
                    parseResults.ParseTree 
                    |> Option.bind (fun parseTree ->
                         UntypedParseImpl.TryGetCompletionContext(Pos.fromZ caretLinePos.Line caretLinePos.Character, parseTree, lineStr))
                
                match completionContext with
                | None -> results.AddRange(keywordCompletionItems)
                | _ -> ()
            
            return results
        }

    override this.ShouldTriggerCompletion(sourceText: SourceText, caretPosition: int, trigger: CompletionTrigger, _: OptionSet) =
        let getInfo() = 
            let documentId = workspace.GetDocumentIdInCurrentContext(sourceText.Container)
            let document = workspace.CurrentSolution.GetDocument(documentId)
            let defines = projectInfoManager.GetCompilationDefinesForEditingDocument(document)
            (documentId, document.FilePath, defines)

        FSharpCompletionProvider.ShouldTriggerCompletionAux(sourceText, caretPosition, trigger.Kind, getInfo)
    
    override this.ProvideCompletionsAsync(context: Completion.CompletionContext) =
        asyncMaybe {
            let document = context.Document
            let! sourceText = context.Document.GetTextAsync(context.CancellationToken)
            let defines = projectInfoManager.GetCompilationDefinesForEditingDocument(document)
            do! Option.guard (CompletionUtils.shouldProvideCompletion(document.Id, document.FilePath, defines, sourceText, context.Position))
            let! _parsingOptions, projectOptions = projectInfoManager.TryGetOptionsForEditingDocumentOrProject(document)
            let! textVersion = context.Document.GetTextVersionAsync(context.CancellationToken)
            let getAllSymbols(fileCheckResults: FSharpCheckFileResults) =
                if Settings.IntelliSense.ShowAllSymbols
                then assemblyContentProvider.GetAllEntitiesInProjectAndReferencedAssemblies(fileCheckResults)
                else []
            let! results = 
                FSharpCompletionProvider.ProvideCompletionsAsyncAux(checker, sourceText, context.Position, projectOptions, 
                                                                    document.FilePath, textVersion.GetHashCode(), getAllSymbols)
            context.AddItems(results)
        } |> Async.Ignore |> RoslynHelpers.StartAsyncUnitAsTask context.CancellationToken
        
    override this.GetDescriptionAsync(_: Document, completionItem: Completion.CompletionItem, cancellationToken: CancellationToken): Task<CompletionDescription> =
        async {
            let exists, declarationItem = declarationItemsCache.TryGetValue(completionItem.DisplayText)
            if exists then
                let! description = declarationItem.StructuredDescriptionTextAsync
                let documentation = List()
                let collector = RoslynHelpers.CollectTaggedText documentation
                // mix main description and xmldoc by using one collector
                XmlDocumentation.BuildDataTipText(documentationBuilder, collector, collector, collector, collector, collector, description) 
                return CompletionDescription.Create(documentation.ToImmutableArray())
            else
                return CompletionDescription.Empty
        } |> RoslynHelpers.StartAsyncAsTask cancellationToken

    override this.GetChangeAsync(document, item, _, cancellationToken) : Task<CompletionChange> =
        async {
            let fullName =
                match item.Properties.TryGetValue FullNamePropName with
                | true, x -> Some x
                | _ -> None

            // do not add extension members, keywords and not yet resolved symbols to the MRU list
            if not (item.Properties.ContainsKey NamespaceToOpenPropName) && not (item.Properties.ContainsKey IsExtensionMemberPropName) &&
               not (item.Properties.ContainsKey IsKeywordPropName) then
                match fullName with
                | Some fullName ->
                    match mruItems.TryGetValue fullName with
                    | true, hints -> mruItems.[fullName] <- hints + 1
                    | _ -> mruItems.[fullName] <- 1
                | _ -> ()
            
            let nameInCode =
                match item.Properties.TryGetValue NameInCodePropName with
                | true, x -> x
                | _ -> item.DisplayText

            return!
                asyncMaybe {
                    let! ns = 
                        match item.Properties.TryGetValue NamespaceToOpenPropName with
                        | true, ns -> Some ns
                        | _ -> None
                    let! sourceText = document.GetTextAsync(cancellationToken)
                    let textWithItemCommitted = sourceText.WithChanges(TextChange(item.Span, nameInCode))
                    let line = sourceText.Lines.GetLineFromPosition(item.Span.Start)
                    let! parsingOptions, _options = projectInfoManager.TryGetOptionsForEditingDocumentOrProject(document)
                    let! parsedInput = checker.ParseDocument(document, parsingOptions, sourceText, userOpName)
                    let fullNameIdents = fullName |> Option.map (fun x -> x.Split '.') |> Option.defaultValue [||]
                    
                    let insertionPoint = 
                        if Settings.CodeFixes.AlwaysPlaceOpensAtTopLevel then OpenStatementInsertionPoint.TopLevel
                        else OpenStatementInsertionPoint.Nearest

                    let! ctx = ParsedInput.tryFindNearestPointToInsertOpenDeclaration line.LineNumber parsedInput fullNameIdents insertionPoint
                    let finalSourceText, changedSpanStartPos = OpenDeclarationHelper.insertOpenDeclaration textWithItemCommitted ctx ns
                    let fullChangingSpan = TextSpan.FromBounds(changedSpanStartPos, item.Span.End)
                    let changedSpan = TextSpan.FromBounds(changedSpanStartPos, item.Span.End + (finalSourceText.Length - sourceText.Length))
                    let changedText = finalSourceText.ToString(changedSpan)
                    return CompletionChange.Create(TextChange(fullChangingSpan, changedText)).WithNewPosition(Nullable (changedSpan.End))
                }
                |> Async.map (Option.defaultValue (CompletionChange.Create(TextChange(item.Span, nameInCode))))

        } |> RoslynHelpers.StartAsyncAsTask cancellationToken