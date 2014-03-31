(*** hide ***)
#I "../../../bin/"
(**
インタラクティブサービス: F# Interactiveの組み込み
==================================================

このチュートリアルでは、独自のアプリケーションに
F# Interactiveを組み込む方法について紹介します。
F# Interactiveは対話式のスクリプティング環境で、
F#コードを高度に最適化されたILコードへとコンパイルしつつ、
それを即座に実行することができます。
F# Interactiveサービスを使用すると、独自のアプリケーションに
F#の評価機能を追加できます。

> **注意:** F# Interactiveは様々な方法で組み込むことができます。
  最も簡単な方法は `fsi.exe` プロセスとの間で標準入出力経由でやりとりする方法です。
  このチュートリアルではF# Interactiveの機能を.NET APIで
  直接呼び出す方法について紹介します。
  ただし入力用のコントロールを備えていない場合、別プロセスでF# Interactiveを
  起動するのはよい方法だといえます。
  理由の1つとしては `StackOverflowException` を処理する方法がないため、
  出来の悪いスクリプトによってはホストプロセスが停止させられてしまう
  場合があるからです。

しかしそれでもF# InteractiveサービスにはF# Interactiveを実行ファイルに埋め込んで
実行出来る(そしてアプリケーションの各機能とやりとり出来る)、あるいは
機能限定されたF#コード(たとえば独自のDSLによって生成されたコード)だけを
実行させることが出来るという便利さがあります。

F# Interactiveの開始
--------------------

まずF# Interactiveサービスを含むライブラリへの参照を追加します:
*)

#r "FSharp.Compiler.Service.dll"
open Microsoft.FSharp.Compiler.Interactive.Shell

(**
F# Interactiveとやりとりするには、入出力を表すストリームを作成する必要があります。
これらのストリームを使用することで、
いくつかのF#コードに対する評価結果を後から出力することができます:
*)
open System
open System.IO

// 入出力のストリームを初期化
let sbOut = new Text.StringBuilder()
let sbErr = new Text.StringBuilder()
let inStream = new StringReader("")
let outStream = new StringWriter(sbOut)
let errStream = new StringWriter(sbErr)

// コマンドライン引数を組み立てて、FSIセッションを開始する
let argv = [| "C:\\fsi.exe" |]
let allArgs = Array.append argv [|"--noninteractive"|]

let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
let fsiSession = FsiEvaluationSession(fsiConfig, allArgs, inStream, outStream, errStream)  

(**
コードの評価および実行
----------------------

F# Interactiveサービスにはサービスとやりとりするためのメソッドが2つ用意されています。
1つは `EvalExpression` で、式を評価してその結果を返します。
結果には戻り値が( `obj` として)含まれる他、値に対して静的に推測された型も含まれます:
*)
/// 式を評価して結果を返す
let evalExpression text =
  match fsiSession.EvalExpression(text) with
  | Some value -> printfn "%A" value.ReflectionValue
  | None -> printfn "結果が得られませんでした！"
(**
一方、 `EvalInteraction` メソッドは結果を返しません。
このメソッドは画面出力機能であったり、F#の式としては不正なものの、
F# Interactiveコンソールには入力できるようなものなど、
副作用を伴う命令を評価する場合に使用できます。
たとえば `#time "on"` (あるいはその他のディレクティブ)や `open System` 、
その他のトップレベルステートメントなどが該当します。
*)
/// 命令を評価して、結果は無視する
let evalInteraction text = 
  fsiSession.EvalInteraction(text)
(**
これら2つのメソッドは文字列を引数にとり、
それをF#コードとして評価(あるいは実行)します。
指定するコードの終端に `;;` を入力する必要はありません。
実行したいコードだけを入力します:
*)
evalExpression "42+1"
evalInteraction "printfn \"bye\""

(**
`EvalScript` メソッドを使用すると、完全な .fsx スクリプトを評価することができます。
*)
/// スクリプトを評価して結果を無視する
let evalScript scriptPath =
    fsiSession.EvalScript(scriptPath)

evalScript "sample.fsx"

(**
評価コンテキスト内での型チェック
--------------------------------

F# Interactiveの一連のスクリプティングセッション中で
コードの型チェックを実行したいような状況を考えてみましょう。
たとえばまず宣言を評価します:
*)

evalInteraction "let xxx = 1 + 1"

(**

次に部分的に完全な `xxx + xx` というコードの型チェックを実行したいとします:
*)

let parseResults, checkResults, checkProjectResults = fsiSession.ParseAndCheckInteraction("xxx + xx")

(** 
`parseResults` と `checkResults` はそれぞれ [エディタ](editor.html)
のページで説明している `ParseFileResults` と `CheckFileResults` 型です。
たとえば以下のようなコードでエラーを確認出来ます:
*)
checkResults.Errors.Length // 1

(** 
コードはF# Interactiveセッション内において、その時点までに実行された
有効な宣言からなる論理的な型コンテキストと結びつく形でチェックされます。

また、宣言リスト情報やツールチップテキスト、シンボルの解決といった処理を
要求することもできます:

*)
open Microsoft.FSharp.Compiler

let identToken = Parser.tagOfToken(Parser.token.IDENT("")) 
checkResults.GetToolTipTextAlternate(1, 2, "xxx + xx", ["xxx"], identToken) // a tooltip

checkResults.GetSymbolAtLocationAlternate(1, 2, "xxx + xx", ["xxx"]) // symbol xxx
  
(**
例外処理
--------

コンパイルエラーをもっと洗練された形で処理して、
使い勝手のよいエラーメッセージを出力させたい場合には
以下のようにするとよいでしょう:
*)

try 
  evalExpression "42 + 1.0"
with e ->
  match e.InnerException with
  | null -> 
      printfn "式 (%s) の評価時にエラーが発生しました" e.Message
  //| WrappedError(err, _) -> 
  //    printfn "(ラップされた)式 (%s) の評価時にエラーが発生しました" err.Message
  | _ -> 
      printfn "式 (%s) の評価時にエラーが発生しました" e.Message
