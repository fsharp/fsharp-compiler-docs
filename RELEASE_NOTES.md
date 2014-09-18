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

#### 0.0.42 - 
* Fix #105 - Register enum symbols in patterns
* Fix #107 - Return correct results for inheritance chain of .NET types
* Fix #101 - Add DeclaringEntity property

#### 0.0.43 - 
* Fix #109 - Duplicates in GetUsesOfSymbolInFile 

#### 0.0.44 - 
* Integrate latest changes from visualfsharp.codeplex.com via github.com/fsharp/fsharp 
* Fix problem with task that generates description text of declaration
* Add AllInterfaceTypes to FSharpEntity and FSharpType
* Add BaseType to FSharpType to propagate instantiation
* Add Instantiate to FSharpType 

#### 0.0.45 - 
* Add optional project cache size parameter to InteractiveChecker
* Switch to openBinariesInMemory for SimpleSourceCodeServices
* Cleanup SimpleSourceCodeServices to avoid code duplication

#### 0.0.46 - 
* Fix multi-project analysis when referenced projects have changed (#141)
* Fix process exit on bad arguments to FsiEvaluationSession (#126)
* Deprecate FsiEvaluationSession constructor and add FsiEvaluationSession.Create static method to allow for future API that can return errors
* Return additional 'property' and 'event' methods for F#-defined types to regularize symbols (#108, #143)
* Add IsPropertySetterMethod and IsPropertyGetterMethod which only return true for getter/setter methods, not properties. Deprecate IsSetterMethod and IsGetterMethod in favour of these.
* Add IsEventAddMethod and IsEventRemoveMethod which return true for add/remove methods with an associated event
* Change IsProperty and IsEvent to only return true for the symbols for properties and events, rather than the methods assocaited with these
* Fix value of Assembly for some symbols (e.g. property symbols)

#### 0.0.47 - 
* Adjust fix for #143 for F# types with abstract+default events

#### 0.0.48 - 
* Allow own fsi object without referencing FSharp.Compiler.Interactive.Settings.dll (#127)

#### 0.0.49 - 
* Fix #138 - Fix symbol equality for provided type members 
* Fix #150 - Return IsGetterMethod = true for declarations of F# properties (no separate 'property' symbol is yet returned, see #79)
* Fix #132 - Add IsStaticInstantiation on FSharpEntity to allow clients to detect fake symbols arising from application of static parameters
* Fix #154 - Add IsArrayType on FSharpEntity to allow clients to detect the symbols for array types
* Fix #96 - Return resolutions of 'Module' and 'Type' in "Module.field" and "Type.field"

#### 0.0.50 - 
* Fix #79 - FindUsesOfSymbol returns None at definition of properties with explicit getters and setters 

#### 0.0.51 - 
* Add IsAccessible to FSharpSymbol, and ProjectContext.AccessibilityRights to give the context of an access

#### 0.0.52 - 
* Fix caches keeping hold of stale entries

#### 0.0.53 - 
* Add queue length to InteractiveChecker

#### 0.0.54 - 
* Fix for #159 - Unsubscribe from TP Invalidate events when disposing builders 

#### 0.0.55 - 
* Integrate changes for F# 3.1.x, Fix #166

#### 0.0.56 - 
* Fix for #160 - Nuget package contains .NET 4.0 and 4.5

#### 0.0.57 - 
* Second fix for #160 - Nuget package now contains .NET 4.0 and 4.5

#### 0.0.58 - 
* Fix for #156 - The FSharp.Core should be retrieved from the hosting environment

#### 0.0.59 - 
* Fix for #184 - Fix EvalScript by using verbatim string for #Load  
* Fix for #183 - The line no. reporting is still using 0-based indexes in errors. This is confusing.

#### 0.0.60 - 
* #207 - Add IsLiteral/LiteralValue to FSharpField 
* #205 - Add IsOptionalArg and related properties to FSharpParameter 
* #210 - Check default/override members via 'IsOverrideOrExplicitMember' 
* #209 - Add TryFullName to FSharpEntity 

#### 0.0.61 - 
* #216 - Return associated getters/setters from F# properties  
* #214 - Added missing XmlDocSig for FSharpMemberFunctionOrValue's Events, Methods and Properties  
* #213 - Retrieve information for all active pattern cases  
* #188 - Fix leak in file handles when using multiple instances of FsiEvaluationSession, and add optionally collectible assemblies

#### 0.0.62 - 
* Integrate to latest http://github.com/fsharp/fsharp (#80f9221f811217bd890b3a670d717ebc510aeeaf)

#### 0.0.63 - 
* #221 - Normalize return types of .NET events

