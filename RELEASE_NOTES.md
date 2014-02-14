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

#### 0.0.18 - 
* Change return type of GetAllUsesOfSymbol, GetAllUsesOfAllSymbols and GetAllUsesOfAllSymbolsInFile to FSharpSymbolUse
* Add symbol uses when an abstract member is implemented.

