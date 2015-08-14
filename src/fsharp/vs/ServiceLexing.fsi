// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

//----------------------------------------------------------------------------
// API to the compiler as an incremental service for lexing.
//----------------------------------------------------------------------------

namespace Microsoft.FSharp.Compiler.SourceCodeServices

open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.Range
open System.Collections.Generic

/// Represents encode, internal information for the state of the laxing engine at the end of a line
type FSharpTokenizerLexState = int64

/// Represents stable information for the state of the laxing engine at the end of a line
type FSharpTokenizerColorState =
    | Token = 1
    | IfDefSkip = 3
    | String = 4
    | Comment = 5
    | StringInComment = 6
    | VerbatimStringInComment = 7
    | CamlOnly = 8
    | VerbatimString = 9
    | SingleLineComment = 10
    | EndLineThenSkip = 11
    | EndLineThenToken = 12
    | TripleQuoteString = 13
    | TripleQuoteStringInComment = 14
    | InitialState = 0 
    

/// Gives an indicattion of the color class to assign to the token an IDE
type FSharpTokenColorKind =
    | Default = 0
    | Text = 0
    | Keyword = 1
    | Comment = 2
    | Identifier = 3
    | String = 4
    | UpperIdentifier = 5
    | InactiveCode = 7
    | PreprocessorKeyword = 8
    | Number = 9
    | Operator = 10
#if COLORIZE_TYPES
    | TypeName = 11
#endif
    
/// Gives an indication of what should happen when the token is typed in an IDE
type FSharpTokenTriggerClass =
    | None         = 0x00000000
    | MemberSelect = 0x00000001
    | MatchBraces  = 0x00000002
    | ChoiceSelect = 0x00000004
    | MethodTip    = 0x000000F0
    | ParamStart   = 0x00000010
    | ParamNext    = 0x00000020
    | ParamEnd     = 0x00000040    
    
/// Gives an indication of the class to assign to the characters of the token an IDE
type FSharpTokenCharKind = 
    | Default     = 0x00000000
    | Text        = 0x00000000
    | Keyword     = 0x00000001
    | Identifier  = 0x00000002
    | String      = 0x00000003
    | Literal     = 0x00000004
    | Operator    = 0x00000005
    | Delimiter   = 0x00000006
    | WhiteSpace  = 0x00000008
    | LineComment = 0x00000009
    | Comment     = 0x0000000A    

/// Some of the values in the field FSharpTokenInfo.Tag
module FSharpTokenTag = 
    /// Indicates the token is an identifier
    val Identifier: int
    /// Indicates the token is a string
    val String : int
    
/// Information about a particular token from the tokenizer
type FSharpTokenInfo = 
    { /// Left column of the token.
      LeftColumn:int
      /// Right column of the token.
      RightColumn:int
      ColorClass:FSharpTokenColorKind
      /// Gives an indication of the class to assign to the token an IDE
      CharClass:FSharpTokenCharKind
      /// Actions taken when the token is typed
      FSharpTokenTriggerClass:FSharpTokenTriggerClass
      /// The tag is an integer identifier for the token
      Tag:int
      /// Provides additional information about the token
      TokenName:string;
      /// The full length consumed by this match, including delayed tokens (which can be ignored in naive lexers)
      FullMatchedLength: int }

/// Object to tokenize a line of F# source code, starting with the given lexState.  The lexState should be 0 for
/// the first line of text. Returns an array of ranges of the text and two enumerations categorizing the
/// tokens and characters covered by that range, i.e. FSharpTokenColorKind and FSharpTokenCharKind.  The enumerations
/// are somewhat adhoc but useful enough to give good colorization options to the user in an IDE.
///
/// A new lexState is also returned.  An IDE-plugin should in general cache the lexState 
/// values for each line of the edited code.
[<Sealed>] 
type (*internal*) FSharpLineTokenizer =
    /// Scan one token from the line
    member ScanToken : lexState:FSharpTokenizerLexState -> FSharpTokenInfo option * FSharpTokenizerLexState
    static member ColorStateOfLexState : FSharpTokenizerLexState -> FSharpTokenizerColorState
    static member LexStateOfColorState : FSharpTokenizerColorState -> FSharpTokenizerLexState
    

/// Tokenizer for a source file. Holds some expensive-to-compute resources at the scope of the file.
[<Sealed>]
type (*internal*) FSharpSourceTokenizer =
    new : conditionalDefines:string list * fileName:string -> FSharpSourceTokenizer
    member CreateLineTokenizer : lineText:string -> FSharpLineTokenizer
    member CreateBufferTokenizer : bufferFiller:(char[] * int * int -> int) -> FSharpLineTokenizer
    

module internal TestExpose =     
    val TokenInfo                                    : Parser.token -> (FSharpTokenColorKind * FSharpTokenCharKind * FSharpTokenTriggerClass) 


[<System.Obsolete("This type has been renamed to FSharpSourceTokenizer")>]
/// Renamed to FSharpSourceTokenizer
type SourceTokenizer = FSharpSourceTokenizer

[<System.Obsolete("This type has been renamed to FSharpLineTokenizer")>]
/// Renamed to FSharpLineTokenizer
type LineTokenizer = FSharpLineTokenizer

[<System.Obsolete("This type has been renamed to FSharpTokenInfo")>]
/// Renamed to FSharpTokenInfo
type TokenInformation = FSharpTokenInfo

[<System.Obsolete("This type has been renamed to FSharpTokenTriggerClass")>]
/// Renamed to FSharpTokenTriggerClass
type TriggerClass = FSharpTokenTriggerClass

[<System.Obsolete("This type has been renamed to FSharpTokenCharKind")>]
/// Renamed to FSharpTokenCharKind
type TokenCharKind = FSharpTokenCharKind

[<System.Obsolete("This type has been renamed to FSharpTokenColorKind")>]
/// Renamed to FSharpTokenColorKind
type TokenColorKind = FSharpTokenColorKind

[<System.Obsolete("This type has been renamed to FSharpTokenizerColorState")>]
/// Renamed to FSharpTokenizerColorState
type ColorState = FSharpTokenizerColorState

[<System.Obsolete("This type has been renamed to FSharpTokenizerLexState")>]
/// Renamed to FSharpTokenizerLexState
type LexState = FSharpTokenizerLexState
