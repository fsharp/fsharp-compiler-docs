#### 0.0.2-alpha - 
* Integrate hosted FSI configuration, SimpleSourceCodeServices, cleanup to SourceCodeServices API

#### 0.0.3-alpha - 
* Integrate FSharp.PowerPack.Metadata as the FSharp* symbol API
* Renamed Param --> MethodGroupItemParameter and hid record from view, made into an object
* Renamed Method --> MethodGroupItem and hid record from view, made into an object
* Renamed Methods --> MethodGroup and hid record from view, made into an object
* Renamed MethodGroup.Name --> MethodGroup.MethodName
* Renamed DataTip --> ToolTip consistently across all text
* Renamed CheckOptions --> ProjectOptions
* Renamed TypeCheckAnswer --> CheckFileAnswer
* Renamed UntypedParseInfo --> ParseFileResults
* Removed GetCheckOptionsFromScriptRoot member overload in favour of optional argument
* Renamed GetCheckOptionsFromScriptRoot --> GetProjectOptionsFromScript
* Renamed UntypedParse --> ParseFileInProject
* Renamed TypeCheckSource --> CheckFileInProjectIfReady
* Added numerous methods to API including CheckFileInProject
* Added experimental GetBackgroundCheckResultsForFileInProject, GetBackgroundParseResultsForFileInProject
* Added PartialAssemblySignature to TypeCheckResults/CheckFileResults
* Added CurrentPartialAssemblySignature to FsiEvaluationSession 
* Added ParseAndCheckInteraction to FsiEvaluationSession to support intellisense implementation against a script fragment 
* Added initial testing in tests/service
* Added ParseAndCheckProject to SourceCodeServices API. This will eventually return "whole project" information such as symbol tables.
* Added GetDefaultConfiguration to simplify process of configuring FsiEvaluationSession
* Added PartialAssemblySignatureUpdated event to FsiEvaluationSession
* Added travis build

#### 0.0.4-alpha - 
* Added documentation of file system API
* Reporte errors correctly from ParseAndCheckProject


#### 0.0.5-alpha - 
* Added GetUsesOfSymbol(), FSharpSymbol type, GetSymbolAtLocation(...)

#### 0.0.6-alpha - 
* Fix version number

#### 0.0.7-alpha - 
* Fix docs
* Make symbols more robust to missing assemblies
* Be robust to failures on IncrementalBuilder creation
* Allow use of MSBuild resolution by IncrementalBuilder

#### 0.0.8-alpha - 
* Fix fsc corrupting assemblies when generating pdb files

#### 0.0.9-alpha - 
* Fix fsc corrupting assemblies when generating pdb files (really)
* Give better error messages for missing assemblies
* Report more information about symbols returned by GetSymbolAtLocation (through subtypes)
* Fix typos in docs
* Return full project results from ParseAndCheckInteraction 
* Be more robust to missing assembly references by default.

#### 0.0.10-alpha - 
* Fix bug where 'multiple references to FSharp.Core' was given as error for scripts

#### 0.0.11-alpha - 
* Add 'IsUnresolved' 

#### 0.0.12-alpha - 
* Make the parts of the lexer/parser used by 'XmlDoc' tools in F# VS Power tools public

#### 0.0.13-alpha - 
* Fix #39 - Constructor parameters are mistaken for record fields in classes

#### 0.0.14 - 
* Update version number as nuget package may not have published properly

#### 0.0.15 - 
* Update version number as nuget package may not have published properly

#### 0.0.16 - 
* Make FSharpEntity.BaseType return an option
* FsiSesion got a new "EvalScript" method which allows to evaluate .fsx files

#### 0.0.17 - 
* Improvements to symbol accuracy w.r.t. type abbreviations 

#### 0.0.18 - 
* Add GetAllUsesOfAllSymbols and GetAllUsesOfAllSymbolsInFile

#### 0.0.19 - 
* Change return type of GetAllUsesOfSymbol, GetAllUsesOfAllSymbols and GetAllUsesOfAllSymbolsInFile to FSharpSymbolUse
* Add symbol uses when an abstract member is implemented.

#### 0.0.20 - 
* Update version number as nuget package may not have published properly

#### 0.0.21 - 
* Add GetUsesOfSymbolInFile
* Better symbol resolution results for type parameter symbols

#### 0.0.22 - 
* Provide symbol location for type parameters

#### 0.0.23 - 
* Move to one-based line numbering everywhere
* Provide better symbol information for active patterns

#### 0.0.24 - 
* Update version number as nuget package may not have published properly

#### 0.0.25 - 
* Add optional source argument to TryGetRecentTypeCheckResultsForFile to specify that source must match exactly

#### 0.0.26 - 
* Fix off-by-one in reporting of range for active pattern name

#### 0.0.27 - 
* Fix exception tag symbol reporting

#### 0.0.28 - 
* Fix symbols for named union fields
* Add FSharpActivePatternCase to refine FSharpSymbol 

#### 0.0.29 - 
* Fix symbols for named union fields in patterns

#### 0.0.30 - 
* Add initial prototype version of multi-project support, through optional ProjectReferences in ProjectOptions. Leave this empty
  to use DLL/file-based references to results from other projects.

#### 0.0.31 - 
* Fix performance problem with CheckFileInProject

#### 0.0.32 - 
* Make ParseFileInProject asynchronous
* Add ParseAndCheckFileInProject
* Use cached results in ParseAndCheckFileInProject if available

#### 0.0.33 - 
* Add FullName and Assembly properties for symbols
* Fix #76
* Add Japanese documentation

#### 0.0.34 - 
* Add StaticParameters property to entities, plus FSharpStaticParameter symbol
* Fix #65

#### 0.0.35 - 
* Fix #38 - FSharp.Compiler.Services should tolerate an FSharp.Core without siginfo/optdata in the search path


#### 0.0.36 - 
* Fix #71 - Expose static parameters and xml docs of type providers
* Fix #63 - SourceCodeServices: #r ignores include paths passed as command-line flags

#### 0.0.37 - 
* Obsolete HasDefaultValue - see https://github.com/fsharp/FSharp.Compiler.Service/issues/77

#### 0.0.38 - 
* Fixed #94 and #89 by addition of new properties to the FSharpSymbolUse type
* Fixed #93 by addition of IsOpaque to FSharpEntity type
* Fixed #92 - Issue with nested classes
* Fixed #87 - Allow analysis of members from external assemblies

#### 0.0.39 - 
* Fixed #79 - Usage points for symbols in union patterns

#### 0.0.40 - 
* Fixed #86 - Expose Microsoft.FSharp.Compiler.Interactive.Shell.Settings.fsi
* Fixed #99 - Add IsNamespace property to FSharpEntity

#### 0.0.41 - 
* Fixed #104 - Make all operations that may utilize the FCS reactor async
* Add FSharpDisplayContext and FSharpType.Format
* Replace GetSymbolAtLocationAlternate by GetSymbolUseAtLocation
