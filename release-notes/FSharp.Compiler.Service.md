---
category: Release Notes
categoryindex: 600
index: 4
title: FSharp.Compiler.Service
---

# FSharp.Compiler.Service

<h2 data-fsdocs-heading="1"><a name="43.12.201" class="anchor" href="#43.12.201">43.12.201 - 2026-03-12</a></h2><a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.12.201" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.12.201-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.12.202" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.12.202-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.12.203" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.12.203-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.12.204" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.12.204-blue"></a><h3 data-fsdocs-heading="2"><a name="43.12.201-Fixed" class="anchor" href="#43.12.201-Fixed">Fixed</a></h3>
<ul>
<li>Fixed SRTP resolution regression causing FS0030 value restriction errors with FSharpPlus curryN-style patterns in .NET 9 SDK. (<a href="https://github.com/dotnet/fsharp/pull/19218">PR #19218</a>)</li>
<li>Fix FS3261 nullness warning when implementing INotifyPropertyChanged or ICommand CLIEvent properties. (<a href="https://github.com/dotnet/fsharp/issues/18361">Issue #18361</a>, <a href="https://github.com/dotnet/fsharp/issues/18349">Issue #18349</a>, <a href="https://github.com/dotnet/fsharp/pull/19221">PR #19221</a>)</li>
<li>Type relations cache: optimize key generation (<a href="https://github.com/dotnet/fsharp/issues/18767">Issue #19116</a>) (<a href="https://github.com/dotnet/fsharp/pull/19120">PR #19120</a>)</li>
<li>Fixed QuickParse to correctly handle optional parameter syntax with <code>?</code> prefix, resolving syntax highlighting issues. (<a href="https://developercommunity.visualstudio.com/t/F-Highlighting-fails-on-optional-parame/11008753">Issue #11008753</a>) (<a href="https://github.com/dotnet/fsharp/pull/19162">PR #19162</a>)</li>
<li>Fix <code>--preferreduilang</code> switch leaking into <code>fsi.CommandLineArgs</code> when positioned after script file (<a href="https://github.com/dotnet/fsharp/pull/19151">PR #19151</a>)</li>
<li>Optimize empty string pattern matching to use null-safe .Length check instead of string equality comparison for better performance. (<a href="https://github.com/dotnet/fsharp/pull/19189">PR #19189</a>)</li>
<li>Fixed runtime crash when using interfaces with unimplemented static abstract members as constrained type arguments. (<a href="https://github.com/dotnet/fsharp/issues/19184">Issue #19184</a>, <a href="https://github.com/dotnet/fsharp/pull/19185">PR #19185</a>)</li>
<li>Fix delegates with <code>[&lt;OptionalArgument&gt;]</code> and caller info attributes failing to compile. (<a href="https://github.com/dotnet/fsharp/issues/18868">Issue #18868</a>, <a href="https://github.com/dotnet/fsharp/pull/19069">PR #19069</a>)</li>
<li>Type checker: mark generated event tree nodes as synthetic (<a href="https://github.com/dotnet/fsharp/pull/19213">PR #19213</a>)</li>
<li>Nullness: Fix nullness refinement in match expressions to correctly narrow type to non-null after matching null case. (<a href="https://github.com/dotnet/fsharp/issues/18488">Issue #18488</a>, <a href="https://github.com/dotnet/fsharp/pull/18852">PR #18852</a>)</li>
<li>Scripts: Fix resolving the dotnet host path when an SDK directory is specified. (<a href="https://github.com/dotnet/fsharp/pull/18960">PR #18960</a>)</li>
<li>Fix excessive StackGuard thread jumping (<a href="https://github.com/dotnet/fsharp/pull/18971">PR #18971</a>)</li>
<li>Adjust conservative method-overload duplicate detection rules for nativeptr types (<a href="https://github.com/dotnet/fsharp/pull/18911">PR #18911</a>)</li>
<li>Checking: Fix checking nested fields for records and anonymous (<a href="https://github.com/dotnet/fsharp/pull/18964">PR #18964</a>)</li>
<li>Fix name is bound multiple times is not reported in 'as' pattern (<a href="https://github.com/dotnet/fsharp/pull/18984">PR #18984</a>)</li>
<li>Syntax Tree: fix return type info for let! / and! / use! (<a href="https://github.com/dotnet/fsharp/pull/19004">PR #19004</a>)</li>
<li>Fix: warn FS0049 on upper union case label. (<a href="https://github.com/dotnet/fsharp/pull/19003">PR #19003</a>)</li>
<li>Type relations cache: handle potentially &quot;infinite&quot; types (<a href="https://github.com/dotnet/fsharp/pull/19010">PR #19010</a>)</li>
<li>Disallow recursive structs with lifted type parameters (<a href="https://github.com/dotnet/fsharp/issues/18993">Issue #18993</a>, <a href="https://github.com/dotnet/fsharp/pull/19031">PR #19031</a>)</li>
<li>Fix units-of-measure changes not invalidating incremental builds. (<a href="https://github.com/dotnet/fsharp/issues/19049">Issue #19049</a>, <a href="https://github.com/dotnet/fsharp/pull/19050">PR #19050</a>)</li>
<li>Fix race in graph checking of type extensions. (<a href="https://github.com/dotnet/fsharp/pull/19062">PR #19062</a>)</li>
<li>Type relations cache: handle unsolved type variables (<a href="https://github.com/dotnet/fsharp/issues/19037">Issue #19037</a>) (<a href="https://github.com/dotnet/fsharp/pull/19040">PR #19040</a>)</li>
<li>Fix insertion context for modules with multiline attributes. (<a href="https://github.com/dotnet/fsharp/issues/18671">Issue #18671</a>, <a href="https://github.com/dotnet/fsharp/pull/19066">PR #19066</a>)</li>
<li>Fix <code>--typecheck-only</code> for scripts stopping after processing <code>#load</code>-ed script (<a href="https://github.com/dotnet/fsharp/pull/19048">PR #19048</a>)</li>
<li>Fix object expressions in struct types generating invalid IL with byref fields causing TypeLoadException at runtime. (<a href="https://github.com/dotnet/fsharp/issues/19068">Issue #19068</a>, <a href="https://github.com/dotnet/fsharp/pull/19070">PR #19070</a>)</li>
<li>Fix duplicate .cctor issue for discriminated unions with generic statics (<a href="https://github.com/dotnet/fsharp/issues/18767">Issue #18767</a>, <a href="https://github.com/dotnet/fsharp/pull/18801">PR #18801</a>)</li>
<li>Fix early/unconditional execution of PackageFSharpDesignTimeTools target. (<a href="https://github.com/dotnet/fsharp/issues/18924">Issue #18924</a>, <a href="https://github.com/dotnet/fsharp/issues/12320">Issue #12320</a>, <a href="https://github.com/dotnet/fsharp/pull/18929">PR #18929</a>)</li>
</ul>
<h3 data-fsdocs-heading="3"><a name="43.12.201-Added" class="anchor" href="#43.12.201-Added">Added</a></h3>
<ul>
<li>Detect and error on static extension members extending types with the same simple name but different namespaces in the same module. (<a href="https://github.com/dotnet/fsharp/pull/18821">PR #18821</a>)</li>
<li>FSharpDiagnostic: add default severity (<a href="https://github.com/dotnet/fsharp/pull/19152">#19152</a>)</li>
<li>Add warning FS3879 for XML documentation comments not positioned as first non-whitespace on line. (<a href="https://github.com/dotnet/fsharp/pull/18891">PR #18891</a>)</li>
<li>FsiEvaluationSession.ParseAndCheckInteraction: add keepAssemblyContents optional parameter (<a href="https://github.com/dotnet/fsharp/pull/19155">#19155</a>)</li>
<li>Add FSharpCodeCompletionOptions (<a href="https://github.com/dotnet/fsharp/pull/19030">PR #19030</a>)</li>
<li>Type checker: recover on checking binding parameter constraints (<a href="https://github.com/dotnet/fsharp/pull/19046">#19046</a>)</li>
<li>Debugger: provide breakpoint ranges for short lambdas (<a href="https://github.com/dotnet/fsharp/pull/19067">#19067</a>)</li>
<li>Add support for triple quoted ASCII byte string (<a href="https://github.com/dotnet/fsharp/pull/19182">#19182</a>)</li>
</ul>
<h3 data-fsdocs-heading="4"><a name="43.12.201-Changed" class="anchor" href="#43.12.201-Changed">Changed</a></h3>
<ul>
<li>Parallel compilation features: ref resolution, graph based checking, ILXGen and optimization enabled by default (<a href="https://github.com/dotnet/fsharp/pull/18998">PR #18998</a>)</li>
<li>Make graph based type checking and parallel optimizations deterministic (<a href="https://github.com/dotnet/fsharp/pull/19028">PR #19028</a>)</li>
<li>Centralize compiler's target framework moniker logic into a single source of truth. (<a href="https://github.com/dotnet/fsharp/pull/19251">PR #19251</a>)</li>
</ul>
<h3 data-fsdocs-heading="5"><a name="43.12.201-Breaking Changes" class="anchor" href="#43.12.201-Breaking Changes">Breaking Changes</a></h3>
<ul>
<li><code>SynExpr.LetOrUse</code> holds <code>SynLetOrUse</code>. (<a href="https://github.com/dotnet/fsharp/pull/19090">PR #19090</a>)</li>
<li><code>SynExprLetOrUseTrivia</code> is now <code>SynLetOrUseTrivia</code>. (<a href="https://github.com/dotnet/fsharp/pull/19090">PR #19090</a>)</li>
<li><code>SynMemberDefn.LetBindings</code> has trivia. (<a href="https://github.com/dotnet/fsharp/pull/19090">PR #19090</a>)</li>
<li><code>SynModuleDecl.Let</code> has trivia. (<a href="https://github.com/dotnet/fsharp/pull/19090">PR #19090</a>)</li>
<li>Removed support for <code>.ml</code> and <code>.mli</code> source files. (<a href="https://github.com/dotnet/fsharp/pull/19143">PR #19143</a>)</li>
<li>Removed <code>#light</code> and <code>#indent</code> directives (they are now a no-op; combined with <code>off</code> they give an error). (<a href="https://github.com/dotnet/fsharp/pull/19143">PR #19143</a>)</li>
<li>Removed <code>--light</code>, <code>--indentation-syntax</code>, <code>--no-indendation-syntax</code>, <code>--ml-keywords</code> and <code>--mlcompatibility</code> compiler/fsi flags. (<a href="https://github.com/dotnet/fsharp/pull/19143">PR #19143</a>)</li>
<li>Removed parsing support for long-deprecated ML (non-light) constructs. (<a href="https://github.com/dotnet/fsharp/pull/19143">PR #19143</a>)</li>
</ul>

<h2 data-fsdocs-heading="6"><a name="43.12.100" class="anchor" href="#43.12.100">43.12.100 - 2026-03-10</a></h2><a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.12.100" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.12.100-blue"></a><h3 data-fsdocs-heading="7"><a name="43.12.100-Fixed" class="anchor" href="#43.12.100-Fixed">Fixed</a></h3>
<ul>
<li><p>Fix internal error FS0192 &quot;Iterate2D&quot; when a <code>[&lt;ReflectedDefinition(true)&gt;]</code> parameter auto-quotes an argument that captures a not-yet-generalized use of an inferred generically-recursive function. The auto-quoted (<code>Expr.WithValue</code>) copy now keeps a fresh link to the recursive-value use so it receives the same inferred type arguments as the executable expression at the letrec point. (<a href="https://github.com/dotnet/fsharp/issues/20379">Issue #20379</a>)</p>
</li>
<li><p>Fix <code>NativePtr.stackalloc</code> nested in a larger expression (e.g. a call argument or the right of an assignment) producing an assembly that throws <code>InvalidProgramException</code> at load. (<a href="https://github.com/dotnet/fsharp/issues/8083">Issue #8083</a>, <a href="https://github.com/dotnet/fsharp/pull/20302">PR #20302</a>)</p>
</li>
<li><p>Fix internal error &quot;Unexpected generalized type variables when compiling an active pattern&quot; when an active pattern is used in a <code>let</code> binding whose right-hand side is a generic value, e.g. <code>let (T) = id</code>. Such a binding is now checked like the equivalent <code>match</code> and is not generalized. (<a href="https://github.com/dotnet/fsharp/issues/16856">Issue #16856</a>, <a href="https://github.com/dotnet/fsharp/pull/20383">PR #20383</a>)</p>
</li>
<li><p>Fix Release-only (<code>--optimize+</code>) <code>System.InvalidProgramException</code> from <code>Seq.collect</code> / <code>yield!</code> over a value-type (struct) collection implementing <code>seq&lt;'T&gt;</code> (e.g. <code>ImmutableArray&lt;_&gt;</code>) when materialised with <code>List.ofSeq</code> / <code>Seq.toList</code> / <code>Seq.toArray</code> or a list/array comprehension. The collector lowering now boxes a struct sub-collection to <code>seq&lt;'T&gt;</code> before calling <code>AddMany</code>/<code>AddManyAndClose</code> (matching the coercion the type checker already inserts for <code>yield!</code>), and uses <code>unit</code> as the try/finally result type instead of the body type (removing a spurious <code>ldnull</code> store). (<a href="https://github.com/dotnet/fsharp/issues/20203">Issue #20203</a>)</p>
</li>
<li><p>Fix recursive inline SRTP resolution being truncated by one currying level (e.g. FSharpPlus <code>memoizeN</code>), a regression from the function-domain unification order change in <a href="https://github.com/dotnet/fsharp/pull/15181">PR #15181</a>; the contravariant domain now keeps the inference variable that still carries the pending member constraint. (<a href="https://github.com/dotnet/fsharp/pull/20247">PR #20247</a>)</p>
</li>
<li><p>Fix exponential (2^N) compile time in pattern matching with shared guards and partial active patterns. (<a href="https://github.com/dotnet/fsharp/issues/18425">Issue #18425</a>, <a href="https://github.com/dotnet/fsharp/pull/20244">PR #20244</a>)</p>
</li>
<li><p>Fix incorrect Debug lowering of inline builders that compose low-level resumable state machines. (<a href="https://github.com/dotnet/fsharp/issues/20466">Issue #20466</a>, <a href="https://github.com/dotnet/fsharp/pull/20469">PR #20469</a>)</p>
</li>
<li><p>Fix incorrect <code>StructLayout(Size = 1)</code> emission for data-less struct unions where the compiler-generated tag field makes the actual runtime size larger. (<a href="https://github.com/dotnet/fsharp/pull/19759">PR #19759</a>)</p>
</li>
<li><p>Fix FS0750 &quot;This construct may only be used within computation expressions&quot; incorrectly raised for <code>let!</code>/<code>use!</code>/<code>do!</code> appearing in the right-hand side of a plain <code>let</code> binding inside a computation expression. The right-hand side is now desugared as a nested computation of the same builder whose result is bound with <code>let!</code>, keeping its bindings correctly scoped. (<a href="https://github.com/dotnet/fsharp/issues/19457">Issue #19457</a>, <a href="https://github.com/dotnet/fsharp/pull/19868">PR #19868</a>)</p>
</li>
<li><p>Stop leaking a <code>System.Diagnostics.Metrics.MeterListener</code> per <code>Cache</code> in DEBUG builds. Each cache created a <code>CacheMetrics.CacheMetricsListener</code> (which starts a <code>MeterListener</code> registered in the process-global metrics registry) and never disposed it, so listeners accumulated for the lifetime of the process. Because every cache hit/miss/add published to all registered listeners, the per-operation cost grew linearly with the number of leaked listeners, so repeated checks (and Debug FCS test runs) slowed down over time. The per-cache <code>CacheMetricsListener</code> and the per-instance <code>cacheId</code> tag are removed; <code>DebugDisplay</code> and tests now read the existing name-aggregated stats populated by the single <code>ListenToAll</code> listener, so no per-cache listener is created and no per-operation cost is added. (<a href="https://github.com/dotnet/fsharp/pull/19995">PR #19995</a>)</p>
</li>
<li><p>Fix state machine lowering dropping the side-effectful receiver of an unused unit-typed member access (e.g. inside <code>task { (effectful()).UnitProp }</code>). (<a href="https://github.com/dotnet/fsharp/issues/13099">Issue #13099</a>, <a href="https://github.com/dotnet/fsharp/pull/19885">PR #19885</a>)</p>
</li>
<li><p><code>--deterministic</code> Release builds now produce byte-identical <code>FSharp.Compiler.Service.dll</code> under <code>--parallelcompilation+</code> and <code>--parallelcompilation-</code>, so it is restored to the determinism gate (now also checked sequential-vs-parallel). Code generation runs the same deferred per-file drain in both modes, with type/member/field emit-order keys and generated names derived from the file being emitted rather than thread-scheduling order. (<a href="https://github.com/dotnet/fsharp/issues/19928">Issue #19928</a>, <a href="https://github.com/dotnet/fsharp/pull/19929">PR #19929</a>)</p>
</li>
<li><p>Fix <code>[&lt;CompiledName&gt;]</code> silently producing duplicate IL entries (FS0192/FS2014) when applied to a multi-value let-binding (e.g. <code>let a, b = 1, 2</code>); now emits FS0755 at type-check time. (<a href="https://github.com/dotnet/fsharp/issues/6131">Issue #6131</a>, <a href="https://github.com/dotnet/fsharp/pull/19924">PR #19924</a>)</p>
</li>
<li><p>Fix FS3236 when taking the address of an untyped generalized <code>let</code> binding (e.g. <code>let ffff = ValueNone</code>) passed to an <code>inref</code> parameter. (<a href="https://github.com/dotnet/fsharp/issues/19608">Issue #19608</a>, <a href="https://github.com/dotnet/fsharp/pull/19948">PR #19948</a>)</p>
</li>
<li><p>Fix IntelliSense not suggesting later named arguments after the first one is autocompleted, for methods (including overloaded methods like <code>Task.Factory.StartNew</code>). (<a href="https://github.com/dotnet/fsharp/issues/19906">Issue #19906</a>, <a href="https://github.com/dotnet/fsharp/pull/19940">PR #19940</a>)</p>
</li>
<li><p>Restore packaging of an F# design-time type provider that is activated via a <code>ProjectReference</code> carrying <code>IsFSharpDesignTimeProvider=&quot;true&quot;</code>. The provider assembly is again included under <code>fsharp41</code> when packing (including <code>pack --no-build</code>); <code>PackageFSharpDesignTimeTools</code> now resolves the provider via <code>GetTargetPath</code>, which works in <code>dotnet pack</code>'s <code>BuildProjectReferences=false</code> content build without forcing an early <code>ResolveReferences</code>. (<a href="https://github.com/dotnet/fsharp/issues/18924">Issue #18924</a>, <a href="https://github.com/dotnet/fsharp/pull/19979">PR #19979</a>)</p>
</li>
<li><p>Provided types used from multiple files no longer produce spurious FS0001 type mismatches under parallel compilation; provided-type entities are now interned so every file linking a given provided type shares one entity. (<a href="https://github.com/dotnet/fsharp/pull/19969">PR #19969</a>)</p>
</li>
<li><p>TypeProviders-SDK providers now load under an unoptimized compiler; the <code>systemRuntimeContainsType</code> closure field the SDK reflects on (<code>tcImports</code>) is captured stably regardless of optimization settings. (<a href="https://github.com/dotnet/fsharp/pull/19969">PR #19969</a>)</p>
</li>
<li><p>Provided namespaces are now interned and appended atomically alongside provided types, completing the #19969 fix: under parallel compilation two files linking the same provided namespace no longer build disjoint subtrees that strand the provided types under them (spurious FS0001/FS0039). (<a href="https://github.com/dotnet/fsharp/issues/20020">Issue #20020</a>, <a href="https://github.com/dotnet/fsharp/pull/20021">PR #20021</a>)</p>
</li>
<li><p>Fixed: Inheriting from an undefined type now reports <code>FS0039</code> exactly once instead of three times. Phase 1F and Phase 2A of inherit-clause type-checking now skip re-resolving a syntactic clause whose Phase 1D resolution already failed with <code>UndefinedName</code>, eliminating both the duplicate diagnostic and the redundant work. (<a href="https://github.com/dotnet/fsharp/issues/16432">Issue #16432</a>, <a href="https://github.com/dotnet/fsharp/pull/19862">PR #19862</a>)</p>
</li>
<li><p>Fix several F# editor semantic-classification errors: F# delegate declarations no longer highlight the <code>delegate of …</code> syntax as a method, computation-expression builders inside list/array comprehensions are classified as <code>ComputationExpression</code>, the closing <code>]</code> of an open-ended slice (e.g. <code>xs[0..]</code>) is no longer classified as <code>Function</code>/<code>Method</code>, and <code>open type T</code> is no longer reported as unused when its imported members (static members, static fields, or DU union cases) are used. (<a href="https://github.com/dotnet/fsharp/issues/19905">Issue #19905</a>, <a href="https://github.com/dotnet/fsharp/pull/19960">PR #19960</a>)</p>
</li>
<li><p>Diagnostic FS0027 now emits a parameter-specific message (suggesting a <code>let mutable x = x</code> shadow or <code>byref&lt;_&gt;</code>) instead of the illegal <code>let mutable x = expression</code> shadow when the assignment target is a function or method parameter. (<a href="https://github.com/dotnet/fsharp/issues/15803">Issue #15803</a>, <a href="https://github.com/dotnet/fsharp/pull/19866">PR #19866</a>)</p>
</li>
<li><p>Recursive <code>inline</code> functions and members now emit a single clear error (FS3890) instead of a misleading FS1113/FS1114 optimizer cascade. (<a href="https://github.com/dotnet/fsharp/issues/17991">Issue #17991</a>, <a href="https://github.com/dotnet/fsharp/pull/19803">PR #19803</a>)</p>
</li>
<li><p>Report <code>FS0037 Duplicate definition of type or module</code> at type-check time when two sibling modules in a <code>module rec</code> / <code>namespace rec</code> group share a name, instead of letting the duplicate slip through to IL emit where it surfaced as the cryptic <code>FS2014: duplicate entry . in type index table</code>. (<a href="https://github.com/dotnet/fsharp/issues/6694">Issue #6694</a>, <a href="https://github.com/dotnet/fsharp/pull/19913">PR #19913</a>)</p>
</li>
<li><p>Unused <code>Unchecked.defaultof&lt;'T&gt;</code> bindings are now eliminated under optimization at their use sites. This removes redundant <code>initobj</code>/<code>ldnull;pop</code> sequences left behind after inlining SRTP helpers that use <code>Unchecked.defaultof</code> as dummy arguments to drive static-member-constraint resolution. Such bindings are preserved inside <code>inline</code> bodies (whose optimized form is pickled as cross-assembly optimization info and re-inlined by consumers) and whenever the erased type still references unsolved type variables, so no <code>FS0073</code> regression is introduced. (<a href="https://github.com/dotnet/fsharp/issues/18128">Issue #18128</a>, <a href="https://github.com/dotnet/fsharp/pull/19758">PR #19758</a>)</p>
</li>
<li><p>Semantic classification no longer marks recursive object self-references (<code>as this</code>, <code>let rec</code> self-refs) as mutable. (<a href="https://github.com/dotnet/fsharp/issues/5229">Issue #5229</a>)</p>
</li>
<li><p>Fix <code>MethodAccessException</code> under <code>--realsig+</code> when a closure (inner <code>let rec</code>, <code>task</code>/<code>async</code> state machine, or quotation splice) inside a member defined in an intrinsic type augmentation (<code>type C with member ...</code>) accesses a <code>private</code> member of <code>C</code>. The synthesized closure is now nested inside the declaring type instead of beside it in the module class. (<a href="https://github.com/dotnet/fsharp/issues/19933">Issue #19933</a>, <a href="https://github.com/dotnet/fsharp/pull/19955">PR #19955</a>)</p>
</li>
<li><p>Preserve source range for type errors on empty-bodied computation expressions (e.g. <code>foo {}</code>) in pipelines, function arguments, and type-annotated contexts, instead of reporting <code>unknown(1,1)</code>. (<a href="https://github.com/dotnet/fsharp/issues/19550">Issue #19550</a>, <a href="https://github.com/dotnet/fsharp/pull/19849">PR #19849</a>)</p>
</li>
<li><p>Fix multiline nested type arguments failing to parse when the closing <code>&gt;</code> aligns with the opening type name's column. (<a href="https://github.com/dotnet/fsharp/issues/15171">Issue #15171</a>)</p>
</li>
<li><p>Tooltip &quot;Full name&quot; now shows demangled companion module names (e.g. <code>MyType.func</code> instead of <code>MyTypeModule.func</code>). (<a href="https://github.com/dotnet/fsharp/issues/17335">Issue #17335</a>, <a href="https://github.com/dotnet/fsharp/pull/19867">PR #19867</a>)</p>
</li>
<li><p>Fix spurious FS0410 accessibility error when tuple-deconstructing bindings use private types in the same module scope. (<a href="https://github.com/dotnet/fsharp/issues/4161">Issue #4161</a>, <a href="https://github.com/dotnet/fsharp/pull/19947">PR #19947</a>)</p>
</li>
<li><p>Fix internal error (FS0193) when calling an indexed property setter with a named argument that matches an indexer parameter. (<a href="https://github.com/dotnet/fsharp/issues/16034">Issue #16034</a>, <a href="https://github.com/dotnet/fsharp/pull/19851">PR #19851</a>)</p>
</li>
<li><p>Fix missing FS1182 (&quot;unused binding&quot;) warning for unused <code>let</code> function bindings inside class types. (<a href="https://github.com/dotnet/fsharp/issues/13849">Issue #13849</a>, <a href="https://github.com/dotnet/fsharp/pull/19805">PR #19805</a>)</p>
</li>
<li><p>Fix internal compiler error FS1110 in <code>task { let! }</code> (and other computation expressions) when a generic IL extension method whose <code>this</code>-parameter is a method-level type variable is in scope (e.g. <code>open ReactiveUI</code>). Regression from PR #19536. (<a href="https://github.com/dotnet/fsharp/issues/19936">Issue #19936</a>)</p>
</li>
<li><p>Fix inner mutually-recursive <code>let rec ... and ...</code> functions under <code>--realsig+</code> not being lifted to top-level static methods (TLR), causing <code>FSharpFunc</code> closure allocations and loss of <code>tail.</code> opcodes — the large struct-mutual-recursion perf regression reported in <a href="https://github.com/dotnet/fsharp/issues/17607">Issue #17607</a>. (<a href="https://github.com/dotnet/fsharp/pull/19882">PR #19882</a>)</p>
</li>
<li><p>Fix <code>TypeLoadException</code> (&quot;Specialize tried to implicitly override a method with weaker type parameter constraints&quot;) and the related CLR crash with constrained inline calls by stripping constraints from closure-class typars in <code>EraseClosures.convIlxClosureDef</code>. (<a href="https://github.com/dotnet/fsharp/issues/14492">Issue #14492</a>, <a href="https://github.com/dotnet/fsharp/issues/19075">Issue #19075</a>, <a href="https://github.com/dotnet/fsharp/pull/19882">PR #19882</a>)</p>
</li>
<li><p>Fix FS0073 during code generation when closure generic arguments contain an unsolved type variable. (<a href="https://github.com/dotnet/fsharp/pull/20229">PR #20229</a>)</p>
</li>
<li><p>Fix <code>FieldAccessException</code> at runtime when the optimizer relocates a read of a <code>protected</code> (family) base-class field into a method outside the field's family (e.g. a trivial member inlined into module/startup code under <code>--optimize+</code>). Protected (family) IL field access is no longer hoisted out of its declaring family by inlining or method-splitting. (<a href="https://github.com/dotnet/fsharp/issues/19963">Issue #19963</a>, <a href="https://github.com/dotnet/fsharp/pull/19964">PR #19964</a>)</p>
</li>
<li><p>Suppress hover/symbol resolution for wildcard <code>_</code> patterns inside <code>member _.…</code> bodies that incorrectly showed <code>val _: T</code> tooltip. (<a href="https://github.com/dotnet/fsharp/pull/19760">PR #19760</a>)</p>
</li>
<li><p>Report <code>seq { }</code> implicit-yield diagnostics and format specifier locations once instead of twice: the classification pass that decides whether the body is a statement or a yielded element no longer double-reports to the sink or diagnostics. (<a href="https://github.com/dotnet/fsharp/issues/16419">Issue #16419</a>, <a href="https://github.com/dotnet/fsharp/pull/19791">PR #19791</a>, <a href="https://github.com/dotnet/fsharp/pull/19895">PR #19895</a>)</p>
</li>
<li><p>Stabilize codegen order under <code>--parallelcompilation+</code> so <code>--deterministic</code> Release builds produce byte-identical IL across rebuilds: optimizer Val iteration, IlxGen type/method/field/event emit order, anonymous-record extra-binding drain, and <code>FileIndex</code> assignment now follow source position rather than thread-scheduling order. (<a href="https://github.com/dotnet/fsharp/issues/19732">Issue #19732</a>, <a href="https://github.com/dotnet/fsharp/pull/19810">PR #19810</a>)</p>
</li>
<li><p>Reject non-function bindings for single-case and partial active pattern names with FS1209, matching the existing multi-case behavior. (<a href="https://github.com/dotnet/fsharp/pull/19763">PR #19763</a>)</p>
</li>
<li><p>Fix FS0421 &quot;The address of the variable cannot be used at this point&quot; incorrectly raised for the discard pattern <code>let _ = &amp;expr</code> when <code>let x = &amp;expr</code> compiles. (<a href="https://github.com/dotnet/fsharp/issues/18841">Issue #18841</a>, <a href="https://github.com/dotnet/fsharp/pull/19811">PR #19811</a>)</p>
</li>
<li><p>Fix double <code>Dispose</code> call when a <code>use</code> binding aliases its value via an <code>as</code> pattern or rebinds an existing <code>use</code>-bound value. (<a href="https://github.com/dotnet/fsharp/issues/12300">Issue #12300</a>, <a href="https://github.com/dotnet/fsharp/pull/19858">PR #19858</a>)</p>
</li>
<li><p>Honor <code>--nowarn</code> and <code>--warnaserror</code> for warnings emitted during command-line option parsing (<a href="https://github.com/dotnet/fsharp/issues/19576">Issue #19576</a>, <a href="https://github.com/dotnet/fsharp/pull/19776">PR #19776</a>)</p>
</li>
<li><p>Bitwise operators (<code>|||</code>, <code>&amp;&amp;&amp;</code>, <code>^^^</code>) applied to enums with a non-integral underlying type (e.g. <code>char</code>) are now rejected at compile time with FS0001 instead of compiling and failing at runtime with <code>NotSupportedException</code>. Gated behind the <code>ErrorOnBitwiseOpsOnNonIntegralEnums</code> language feature (F# 11.0). (<a href="https://github.com/dotnet/fsharp/issues/11785">Issue #11785</a>, <a href="https://github.com/dotnet/fsharp/pull/20322">PR #20322</a>)</p>
</li>
<li><p>Fix <code>[&lt;return: X&gt;]</code> prefix attributes being silently dropped on class members, and fix false-positive <code>AllowMultiple=false</code> errors when <code>[&lt;X&gt;]</code> and <code>[&lt;return: X&gt;]</code> are applied to the same binding. (<a href="https://github.com/dotnet/fsharp/issues/17904">Issue #17904</a>, <a href="https://github.com/dotnet/fsharp/issues/19020">Issue #19020</a>, <a href="https://github.com/dotnet/fsharp/pull/19738">PR #19738</a>)</p>
</li>
<li><p>Fix <code>=</code> adjacent to an interpolated string (e.g. <code>C(Name=$&quot;value&quot;)</code>) being lexed as the invalid operator <code>=$</code> instead of an assignment followed by an interpolated string. (<a href="https://github.com/dotnet/fsharp/issues/16696">Issue #16696</a>)</p>
</li>
<li><p>Extend the <code>=</code> adjacent to an interpolated string fix to the verbatim (<code>=$@&quot;…&quot;</code>, <code>=@$&quot;…&quot;</code>) and extended multi-dollar (<code>=$$&quot;&quot;&quot;…&quot;&quot;&quot;</code>) interpolated-string forms. (<a href="https://github.com/dotnet/fsharp/issues/16696">Issue #16696</a>, <a href="https://github.com/dotnet/fsharp/pull/19984">PR #19984</a>)</p>
</li>
<li><p>Preserve type abbreviations (<code>string</code>, user-defined aliases) in the refined type of bindings introduced after a <code>| null</code> pattern in a <code>match</code> expression. (<a href="https://github.com/dotnet/fsharp/issues/19646">Issue #19646</a>, <a href="https://github.com/dotnet/fsharp/pull/19745">PR #19745</a>)</p>
</li>
<li><p>Fix attributes on return type of unparenthesized tuple methods being silently dropped from IL. (<a href="https://github.com/dotnet/fsharp/issues/462">Issue #462</a>, <a href="https://github.com/dotnet/fsharp/pull/19714">PR #19714</a>)</p>
</li>
<li><p>Fix cross-assembly calls to inline SRTP functions from libraries compiled with F# 4.7 and earlier. (<a href="https://github.com/dotnet/fsharp/issues/20253">Issue #20253</a>, <a href="https://github.com/dotnet/fsharp/pull/20260">PR #20260</a>)</p>
</li>
<li><p>Fix enum values losing their type when used in a custom attribute argument of type <code>obj</code> (they were stored as the underlying integer instead of the enum). (<a href="https://github.com/dotnet/fsharp/issues/995">Issue #995</a>, <a href="https://github.com/dotnet/fsharp/pull/19975">PR #19975</a>)</p>
</li>
<li><p>Fix false-positive nullness warning (FS3261) when pattern matching narrows nullness inside seq/list/array comprehensions. (<a href="https://github.com/dotnet/fsharp/issues/19644">Issue #19644</a>, <a href="https://github.com/dotnet/fsharp/pull/19743">PR #19743</a>)</p>
</li>
<li><p>Fix internal error FS0073 &quot;Undefined or unsolved type variable&quot; in IlxGen when nested inline SRTP functions with multiple overloads leave unsolved typars in the non-witness codegen path. (<a href="https://github.com/dotnet/fsharp/issues/19709">Issue #19709</a>, <a href="https://github.com/dotnet/fsharp/pull/19710">PR #19710</a>)</p>
</li>
<li><p>Fix internal error FS0073 &quot;Undefined or unsolved type variable&quot; in IlxGen <code>GenGenericArgs</code> when a phantom typar reaches a closure's generic actuals in the emitted fallback body of a nested inline SRTP member. (<a href="https://github.com/dotnet/fsharp/pull/19602">PR #19602</a>)</p>
</li>
<li><p>Fix NRE when calling virtual Object methods on value types through inline SRTP functions. (<a href="https://github.com/dotnet/fsharp/issues/8098">Issue #8098</a>, <a href="https://github.com/dotnet/fsharp/pull/19511">PR #19511</a>)</p>
</li>
<li><p>Narrow overload-resolution error ranges to the method name only instead of covering the entire expression. (<a href="https://github.com/dotnet/fsharp/issues/14284">Issue #14284</a>, <a href="https://github.com/dotnet/fsharp/pull/19505">PR #19505</a>)</p>
</li>
<li><p>Fix attributes not resolved from opened namespaces in <code>namespace rec</code> / <code>module rec</code> scopes. (<a href="https://github.com/dotnet/fsharp/issues/7931">Issue #7931</a>, <a href="https://github.com/dotnet/fsharp/pull/19502">PR #19502</a>)</p>
</li>
<li><p>Fix DU case names matching IWSAM member names no longer cause duplicate property entries. (Issue <a href="https://github.com/dotnet/fsharp/issues/14321">#14321</a>, <a href="https://github.com/dotnet/fsharp/pull/19341">PR #19341</a>)</p>
</li>
<li><p>Fix DefaultAugmentation(false) duplicate entry in method table. (Issue <a href="https://github.com/dotnet/fsharp/issues/16565">#16565</a>, <a href="https://github.com/dotnet/fsharp/pull/19341">PR #19341</a>)</p>
</li>
<li><p>Fix abstract event accessors now have SpecialName flag. (Issue <a href="https://github.com/dotnet/fsharp/issues/5834">#5834</a>, <a href="https://github.com/dotnet/fsharp/pull/19341">PR #19341</a>)</p>
</li>
<li><p>Fix warning 20 (&quot;expression is implicitly ignored&quot;) pointing at the wrong range when the last expression in a sequential block (e.g. inside <code>for</code>, <code>while</code> loops) is non-unit. The squiggle now correctly highlights only the offending expression. (<a href="https://github.com/dotnet/fsharp/issues/5418">Issue #5418</a>, <a href="https://github.com/dotnet/fsharp/pull/19504">PR #19504</a>)</p>
</li>
<li><p>Extend the warning 20 range fix to also walk through <code>let</code>/<code>use</code> bindings, so the squiggle lands on the offending expression when it is the body of a <code>let</code>/<code>use</code>. (<a href="https://github.com/dotnet/fsharp/issues/5418">Issue #5418</a>, <a href="https://github.com/dotnet/fsharp/pull/19896">PR #19896</a>)</p>
</li>
<li><p>Fix missing &quot;No implementation was given&quot; error when F# class inherits from a C# class with <code>abstract override</code> members without providing an implementation. (<a href="https://github.com/dotnet/fsharp/issues/7776">Issue #7776</a>, <a href="https://github.com/dotnet/fsharp/pull/19503">PR #19503</a>)</p>
</li>
<li><p>Fix CLIEvent properties to be correctly recognized as events: <code>IsEvent</code> returns <code>true</code> and <code>XmlDocSig</code> uses <code>E:</code> prefix instead of <code>P:</code>. (<a href="https://github.com/dotnet/fsharp/issues/10273">Issue #10273</a>, <a href="https://github.com/dotnet/fsharp/pull/18584">PR #18584</a>)</p>
</li>
<li><p>Fix extra sequence point at the end of match expressions. (<a href="https://github.com/dotnet/fsharp/issues/12052">Issue #12052</a>, <a href="https://github.com/dotnet/fsharp/pull/19278">PR #19278</a>)</p>
</li>
<li><p>Fix wrong sequence point range for <code>return</code>/<code>yield</code>/<code>return!</code>/<code>yield!</code> inside computation expressions. (<a href="https://github.com/dotnet/fsharp/issues/19248">Issue #19248</a>, <a href="https://github.com/dotnet/fsharp/pull/19278">PR #19278</a>)</p>
</li>
<li><p>Fix extra out-of-order sequence point for <code>use</code> in <code>task</code> computation expressions. (<a href="https://github.com/dotnet/fsharp/issues/19255">Issue #19255</a>, <a href="https://github.com/dotnet/fsharp/pull/19278">PR #19278</a>)</p>
</li>
<li><p>Fix debug points failing to bind in body of <code>[ for x in xs -&gt; body ]</code> comprehensions. (<a href="https://github.com/dotnet/fsharp/issues/13504">Issue #13504</a>, <a href="https://github.com/dotnet/fsharp/pull/19278">PR #19278</a>)</p>
</li>
<li><p>Fix outref parameter compiled as byref. (Issue <a href="https://github.com/dotnet/fsharp/issues/13468">#13468</a>, <a href="https://github.com/dotnet/fsharp/pull/19340">PR #19340</a>)</p>
</li>
<li><p>Fix static abstract interface members with byref params. (Issue <a href="https://github.com/dotnet/fsharp/issues/18135">#18135</a>, <a href="https://github.com/dotnet/fsharp/pull/19340">PR #19340</a>)</p>
</li>
<li><p>Fix codegen to produce IL passing ILVerify: specialized stelem/ldelem for primitives, callvirt→call on value types, castclass at interface join points, filter→catch inside finally handlers, witness field alignment in state machine structs. (<a href="https://github.com/dotnet/fsharp/pull/19372">PR #19372</a>)</p>
</li>
<li><p>Fix object expressions in struct types no longer generate invalid IL with byref fields. (Issue <a href="https://github.com/dotnet/fsharp/issues/19068">#19068</a>, <a href="https://github.com/dotnet/fsharp/pull/19339">PR #19339</a>)</p>
</li>
<li><p>Avoid duplicate parameter names in closure constructors. (Issue <a href="https://github.com/dotnet/fsharp/issues/17692">#17692</a>, <a href="https://github.com/dotnet/fsharp/pull/19339">PR #19339</a>)</p>
</li>
<li><p>Improve let-rec codegen: reorder bindings to allocate lambda closures before non-lambda values that reference them. (<a href="https://github.com/dotnet/fsharp/pull/19339">PR #19339</a>)</p>
</li>
<li><p>Fix <code>YieldFromFinal</code>/<code>ReturnFromFinal</code> being incorrectly called in non-tail positions (<code>for</code>, <code>use</code>, <code>use!</code>, <code>try/with</code> handler). (<a href="https://github.com/dotnet/fsharp/issues/19402">Issue #19402</a>, <a href="https://github.com/dotnet/fsharp/pull/19403">PR #19403</a>)</p>
</li>
<li><p>Fixed how the source ranges of warn directives are reported (as trivia) in the parser output (by not reporting leading spaces). (<a href="https://github.com/dotnet/fsharp/issues/19405">Issue #19405</a>, <a href="(https://github.com/dotnet/fsharp/pull/19408)">PR #19408</a>)</p>
</li>
<li><p>Fix UoM value type <code>ToString()</code> returning garbage values when <code>--checknulls+</code> is enabled, caused by double address-taking in codegen. (<a href="https://github.com/dotnet/fsharp/issues/19435">Issue #19435</a>, <a href="https://github.com/dotnet/fsharp/pull/19440">PR #19440</a>)</p>
</li>
<li><p>Fix accessibility and type-matching for extension method lookups. (<a href="https://github.com/dotnet/fsharp/issues/19349">Issue #19349</a>, <a href="https://github.com/dotnet/fsharp/pull/19536">PR #19536</a>)</p>
</li>
<li><p>Fix completion inconsistently showing some obsolete members (fields and events) while hiding others (methods and properties). All obsolete members are now consistently hidden by default. (<a href="https://github.com/dotnet/fsharp/issues/13512">Issue #13512</a>, <a href="https://github.com/dotnet/fsharp/pull/19506">PR #19506</a>)</p>
</li>
<li><p>Fix O(n) <code>TypeStructure.GetHashCode</code> performance regression causing sustained high CPU in IDE mode with generative type providers. (<a href="https://github.com/dotnet/fsharp/issues/18925">Issue #18925</a>, <a href="https://github.com/dotnet/fsharp/pull/19369">PR #19369</a>)</p>
</li>
<li><p>Fix TypeLoadException when creating delegate with voidptr parameter. (Issue <a href="https://github.com/dotnet/fsharp/issues/11132">#11132</a>, <a href="https://github.com/dotnet/fsharp/pull/19338">PR #19338</a>)</p>
</li>
<li><p>Suppress tail calls when localloc (NativePtr.stackalloc) is used. (Issue <a href="https://github.com/dotnet/fsharp/issues/13447">#13447</a>, <a href="https://github.com/dotnet/fsharp/pull/19338">PR #19338</a>)</p>
</li>
<li><p>Fix stack overflow when graph-checking long statement sequences on platforms with large stack frames (e.g. Mono ppc64le), by flattening <code>SynExpr.Sequential</code> chains in <code>FileContentMapping</code>. (<a href="https://github.com/dotnet/fsharp/issues/19988">Issue #19988</a>, <a href="https://github.com/dotnet/fsharp/pull/20028">PR #20028</a>)</p>
</li>
<li><p>Fix TypeLoadException in Release builds with inline constraints. (Issue <a href="https://github.com/dotnet/fsharp/issues/14492">#14492</a>, <a href="https://github.com/dotnet/fsharp/pull/19338">PR #19338</a>)</p>
</li>
<li><p>Fix nativeptr in interfaces leads to TypeLoadException. (Issue <a href="https://github.com/dotnet/fsharp/issues/14508">#14508</a>, <a href="https://github.com/dotnet/fsharp/pull/19338">PR #19338</a>)</p>
</li>
<li><p>Fix box instruction for literal upcasts. (Issue <a href="https://github.com/dotnet/fsharp/issues/18319">#18319</a>, <a href="https://github.com/dotnet/fsharp/pull/19338">PR #19338</a>)</p>
</li>
<li><p>Fix Decimal Literal causes InvalidProgramException in Debug builds. (Issue <a href="https://github.com/dotnet/fsharp/issues/18956">#18956</a>, <a href="https://github.com/dotnet/fsharp/pull/19338">PR #19338</a>)</p>
</li>
<li><p>Fix <code>AttributeUsage.AllowMultiple</code> not being inherited for attributes subclassed in C#. (<a href="https://github.com/dotnet/fsharp/issues/17107">Issue #17107</a>, <a href="https://github.com/dotnet/fsharp/pull/19315">PR #19315</a>)</p>
</li>
<li><p>Fix signature generation: recursive module <code>do</code> binding leaking compiler-generated val. (<a href="https://github.com/dotnet/fsharp/issues/13832">Issue #13832</a>, <a href="https://github.com/dotnet/fsharp/pull/19586">PR #19586</a>)</p>
</li>
<li><p>Fix signature generation: literal values in attribute arguments now preserve literal identifier name. (<a href="https://github.com/dotnet/fsharp/issues/13810">Issue #13810</a>, <a href="https://github.com/dotnet/fsharp/pull/19586">PR #19586</a>)</p>
</li>
<li><p>Fix signature generation: struct types with non-comparable/non-equatable fields now include <code>[&lt;NoComparison&gt;]</code>/<code>[&lt;NoEquality&gt;]</code>. (<a href="https://github.com/dotnet/fsharp/issues/15339">Issue #15339</a>, <a href="https://github.com/dotnet/fsharp/pull/19586">PR #19586</a>)</p>
</li>
<li><p>Fix <code>MemoryMappedFileViewStream</code> copying from non-array-backed <code>ReadOnlyMemory</code> values by falling back to the safe <code>Stream.Write</code> path instead of the unsupported <code>PositionPointer</code> fast path. (<a href="https://github.com/dotnet/fsharp/issues/20263">Issue #20263</a>, <a href="https://github.com/dotnet/fsharp/pull/20271">PR #20271</a>)</p>
</li>
<li><p>Fix signature generation: backtick escaping for identifiers containing backticks. (<a href="https://github.com/dotnet/fsharp/issues/15389">Issue #15389</a>, <a href="https://github.com/dotnet/fsharp/pull/19586">PR #19586</a>)</p>
</li>
<li><p>Fix signature generation: <code>private</code> keyword placement for prefix-style type abbreviations. (<a href="https://github.com/dotnet/fsharp/issues/15560">Issue #15560</a>, <a href="https://github.com/dotnet/fsharp/pull/19586">PR #19586</a>)</p>
</li>
<li><p>Fix signature generation: missing <code>[&lt;Class&gt;]</code> attribute for types without visible constructors. (<a href="https://github.com/dotnet/fsharp/issues/16531">Issue #16531</a>, <a href="https://github.com/dotnet/fsharp/pull/19586">PR #19586</a>)</p>
</li>
<li><p>Fix F# exception serialization now preserves fields (gated behind <code>--langversion:11</code>). (Issue <a href="https://github.com/dotnet/fsharp/issues/878">#878</a>, <a href="https://github.com/dotnet/fsharp/pull/19342">PR #19342</a>, <a href="https://github.com/dotnet/fsharp/pull/19746">PR #19746</a>)</p>
</li>
<li><p>Fix computation expressions desugaring final-position <code>do!</code> as <code>return!</code>/<code>yield!</code>; it now uses <code>Bind</code> while explicit <code>return!</code>/<code>yield!</code> retain final methods. (<a href="https://github.com/dotnet/fsharp/issues/20448">Issue #20448</a>, <a href="https://github.com/dotnet/fsharp/pull/20449">PR #20449</a>)</p>
</li>
<li><p>Fix methods being tagged as <code>Member</code> instead of <code>Method</code> in tooltips. (<a href="https://github.com/dotnet/fsharp/issues/10540">Issue #10540</a>, <a href="https://github.com/dotnet/fsharp/pull/19507">PR #19507</a>)</p>
</li>
<li><p>Fix Debug-mode compilation when mixing resumable and standard computation expressions. (<a href="https://github.com/dotnet/fsharp/issues/19625">Issue #19625</a>, <a href="https://github.com/dotnet/fsharp/pull/19630">PR #19630</a>)</p>
</li>
<li><p>IlxGen: fix missing CompilationMapping attribute for generic values (<a href="https://github.com/dotnet/fsharp/pull/19643">PR #19643</a>)</p>
</li>
<li><p>Fix internal error <code>FS0192: encodeCustomAttrElemType</code> when using arrays of user-defined types as custom attribute arguments. Empty arrays (e.g. <code>[&lt;DefaultValue([||] : A[])&gt;]</code>) now compile successfully; non-empty arrays of unencodable types report a proper diagnostic (FS3887) instead of an internal error. (<a href="https://github.com/dotnet/fsharp/issues/12796">Issue #12796</a>, <a href="https://github.com/dotnet/fsharp/pull/19472">PR #19472</a>)</p>
</li>
<li><p>Cleanup in IL base-call checking: skip <code>resolveILMethodRefWithRescope</code> when the method is not declared on the immediate IL type, instead of relying on the <code>try/with</code> around a <code>failwith</code>. <code>FS1201</code> still applies when the abstract member is declared on the immediate IL base. (<a href="https://github.com/dotnet/fsharp/issues/20264">Issue #20264</a>, <a href="https://github.com/dotnet/fsharp/pull/20272">PR #20272</a>)</p>
</li>
<li><p>Fix internal compiler error in <code>use</code> bindings when a C#-style <code>Dispose</code> extension method is in scope alongside <code>IDisposable.Dispose</code>. (<a href="https://github.com/dotnet/fsharp/issues/19552">Issue #19552</a>, <a href="https://github.com/dotnet/fsharp/pull/19568">PR #19568</a>)</p>
</li>
<li><p>Fix signature generation: single-case struct DU gets spurious bar causing FS0300. (<a href="https://github.com/dotnet/fsharp/issues/19597">Issue #19597</a>, <a href="https://github.com/dotnet/fsharp/pull/19609">PR #19609</a>)</p>
</li>
<li><p>Fix signature generation: backticked active pattern case names lose escaping. (<a href="https://github.com/dotnet/fsharp/issues/19592">Issue #19592</a>, <a href="https://github.com/dotnet/fsharp/pull/19609">PR #19609</a>)</p>
</li>
<li><p>Fix signature generation: <code>namespace global</code> header dropped from generated signature. (<a href="https://github.com/dotnet/fsharp/issues/19593">Issue #19593</a>, <a href="https://github.com/dotnet/fsharp/pull/19609">PR #19609</a>)</p>
</li>
<li><p>Fix signature generation: SRTP constraints use postfix syntax that fails conformance, now uses explicit type param declarations. (<a href="https://github.com/dotnet/fsharp/issues/19594">Issue #19594</a>, <a href="https://github.com/dotnet/fsharp/pull/19609">PR #19609</a>)</p>
</li>
<li><p>Fix signature generation: type params with special characters missing backtick escaping. (<a href="https://github.com/dotnet/fsharp/issues/19595">Issue #19595</a>, <a href="https://github.com/dotnet/fsharp/pull/19609">PR #19609</a>)</p>
</li>
<li><p>Fix internal error when using custom attribute with <code>[&lt;Optional&gt;]</code> value type parameter and no <code>[&lt;DefaultParameterValue&gt;]</code>. (<a href="https://github.com/dotnet/fsharp/issues/8353">Issue #8353</a>, <a href="https://github.com/dotnet/fsharp/pull/19484">PR #19484</a>)</p>
</li>
<li><p>Fix overload resolution of static member extension if one or more intrinsics candidates exist (<a href="https://github.com/dotnet/fsharp/issues/19664">Issue #19664</a>, <a href="https://github.com/dotnet/fsharp/pull/19698">PR #19698</a>)</p>
</li>
<li><p>Fix parallel compilation of scripts (<a href="https://github.com/dotnet/fsharp/pull/19649">PR #19649</a>)</p>
</li>
<li><p>Fix parser recovery, name resolution, and code completion for unfinished enum patterns (<a href="https://github.com/dotnet/fsharp/pull/19708">PR #19708</a>)</p>
</li>
<li><p>Parser: fix unexpected diagnostics in debug builds, improve error messages (<a href="https://github.com/dotnet/fsharp/pull/19730">PR #19730</a>)</p>
</li>
<li><p>Fix parser error for anonymous record type aliases with postfix type operators (e.g. <code>{| Id: Guid |} []</code>) when closing bracket is column-aligned with opening bracket. (<a href="https://github.com/dotnet/fsharp/issues/17407">Issue #17407</a>, <a href="https://github.com/dotnet/fsharp/pull/19762">PR #19762</a>)</p>
</li>
<li><p>Fix internal error when resolving SRTP <code>get_Item</code> witness for <code>string</code> indexers (<code>unknown builtin witness 'get_ItemDynamic'</code>). (<a href="https://github.com/dotnet/fsharp/issues/18093">Issue #18093</a>, <a href="https://github.com/dotnet/fsharp/pull/19757">PR #19757</a>)</p>
</li>
<li><p>Fix false FS1113 error on inline instance members when a class-scope self identifier (<code>as self</code>) is present. (<a href="https://github.com/dotnet/fsharp/issues/17899">Issue #17899</a>, <a href="https://github.com/dotnet/fsharp/pull/19761">PR #19761</a>)</p>
</li>
<li><p>Allow <code>| null</code> nullable annotation on a <code>[&lt;MeasureAnnotatedAbbreviation&gt;]</code> over a reference type (e.g. the FSharp.UMX <code>type string&lt;[&lt;Measure&gt;] 'm&gt; = string</code> pattern). (<a href="https://github.com/dotnet/fsharp/issues/19657">Issue #19657</a>)</p>
</li>
<li><p>Fix <code>[&lt;Struct&gt;] ?param</code> optional parameters could not be passed using the explicit <code>?param = expr</code> caller-side syntax with a <code>ValueOption</code> value. (<a href="https://github.com/dotnet/fsharp/issues/19711">Issue #19711</a>, <a href="https://github.com/dotnet/fsharp/pull/19742">PR #19742</a>)</p>
</li>
<li><p>Fix signature conformance: overloaded member with unit parameter <code>M(())</code> now matches sig <code>member M: unit -&gt; unit</code>. (<a href="https://github.com/dotnet/fsharp/issues/19596">Issue #19596</a>, <a href="https://github.com/dotnet/fsharp/pull/19615">PR #19615</a>)</p>
</li>
<li><p>Fix <code>--quiet</code> not suppressing NuGet restore output on stdout in F# Interactive (<a href="https://github.com/dotnet/fsharp/issues/18086">Issue #18086</a>)</p>
</li>
<li><p>Reference assembly MVIDs are now deterministic across compiler invocations. Previously, <code>--refout</code> / <code>&lt;ProduceReferenceAssembly&gt;true&lt;/ProduceReferenceAssembly&gt;</code> produced a different MVID every build because the implied signature hash used .NET's randomized <code>String.GetHashCode()</code>. (<a href="https://github.com/dotnet/fsharp/issues/19751">Issue #19751</a>, <a href="https://github.com/dotnet/fsharp/pull/19801">PR #19801</a>)</p>
</li>
<li><p>Parser: recover on unfinished if and binary expressions (<a href="https://github.com/dotnet/fsharp/pull/19724">PR #19724</a>)</p>
</li>
<li><p>Parser: recover on missing 'when' conditions (<a href="https://github.com/dotnet/fsharp/pull/#20071">PR ##20071</a>)</p>
</li>
<li><p>Fix <code>SynExpr.shouldBeParenthesizedInContext</code> to report parentheses as required around <code>SynExpr.Sequential</code> expressions used as record or anonymous-record field values, so the IDE &quot;remove unnecessary parentheses&quot; analyzer no longer breaks code like <code>{| A = ((); B = 3) |}</code>. (<a href="https://github.com/dotnet/fsharp/issues/17826">Issue #17826</a>, <a href="https://github.com/dotnet/fsharp/pull/19850">PR #19850</a>)</p>
</li>
<li><p>Fix semantic classification of <code>IDisposable</code> and other interface types in type-occurrence positions being incorrectly classified as <code>DisposableType</code> instead of <code>Interface</code>. (<a href="https://github.com/dotnet/fsharp/issues/16268">Issue #16268</a>, <a href="https://github.com/dotnet/fsharp/pull/19809">PR #19809</a>)</p>
</li>
<li><p>Fix missing semantic classification on second and later type qualifiers in nested copy-and-update expressions like <code>{ p with Person.Info.X = 1; Person.Info.Y = 2 }</code>. (<a href="https://github.com/dotnet/fsharp/issues/17428">Issue #17428</a>, <a href="https://github.com/dotnet/fsharp/pull/19878">PR #19878</a>)</p>
</li>
<li><p>Parser: recover on unfinished if and binary expressions (<a href="https://github.com/dotnet/fsharp/pull/19724">PR #19724</a>)</p>
</li>
<li><p>Fix QuickInfo / <code>GetAllUsesOfAllSymbolsInFile</code> for overloaded CE <code>[&lt;CustomOperation&gt;]</code> keywords to report the actually-resolved overload instead of the first-registered one. (<a href="https://github.com/dotnet/fsharp/issues/11612">Issue #11612</a>, <a href="https://github.com/dotnet/fsharp/issues/15206">Issue #15206</a>, <a href="https://github.com/dotnet/fsharp/pull/19865">PR #19865</a>)</p>
</li>
<li><p>Fix <code>open</code> declaration insertion in <code>.fsx</code>/<code>.fsscript</code> scripts being placed before <code>#r</code>/<code>#load</code> directives, producing invalid script files. (<a href="https://github.com/dotnet/fsharp/issues/16271">Issue #16271</a>, <a href="https://github.com/dotnet/fsharp/pull/19879">PR #19879</a>)</p>
</li>
<li><p>Warn FS3888 when a compiler-semantic attribute on a value/member or type/module is present in the <code>.fs</code> but missing from the <code>.fsi</code>. Such attributes were previously ignored at the consumer side. Under the <code>ErrorOnMissingSignatureAttribute</code> preview language feature, FS3888 is an error. (<a href="https://github.com/dotnet/fsharp/issues/19560">Issue #19560</a>, <a href="https://github.com/dotnet/fsharp/pull/19880">PR #19880</a>)</p>
</li>
<li><p>Emit debug points at a stack-empty position (<a href="https://github.com/dotnet/fsharp/pull/19877">PR #19877</a>)</p>
</li>
<li><p>Fix spurious XmlDoc warnings (unknown parameter / no documentation for parameter) under <code>--warnon:3390</code> when a get/set property documents the full parameter set across both accessors. (<a href="https://github.com/dotnet/fsharp/issues/13684">Issue #13684</a>, <a href="https://github.com/dotnet/fsharp/pull/19884">PR #19884</a>)</p>
</li>
<li><p>Replace internal compiler error FS0193 with a clear FS3891 diagnostic when a type abbreviation aliases a generic attribute type (e.g. <code>type B = A&lt;int&gt;</code> then <code>[&lt;B&gt;] ...</code>). Generic attributes remain unsupported in F#. (<a href="https://github.com/dotnet/fsharp/issues/7877">Issue #7877</a>, <a href="https://github.com/dotnet/fsharp/pull/19915">PR #19915</a>)</p>
</li>
<li><p>Fix Go to Metadata rendering of IL literal (<code>const</code>) fields - they now appear with <code>[&lt;Literal&gt;]</code> and their constant value, e.g. <code>System.Char.MaxValue</code> no longer shows as a plain <code>static val</code>. (<a href="https://github.com/dotnet/fsharp/issues/11526">Issue #11526</a>, <a href="https://github.com/dotnet/fsharp/pull/19922">PR #19922</a>)</p>
</li>
<li><p>FSI multi-assembly emit (<code>--multiemit+</code>) now attaches <code>System.Diagnostics.DebuggableAttribute(DisableOptimizations|Default)</code> to each submission's manifest when local optimizations are disabled (<code>--optimize-</code>), matching the single-emit and regular-compiler behavior so debuggers see submissions as unoptimized. (<a href="https://github.com/dotnet/fsharp/issues/14572">Issue #14572</a>, <a href="https://github.com/dotnet/fsharp/pull/19921">PR #19921</a>)</p>
</li>
<li><p>Stop F# Interactive from mutating script arguments that follow <code>--</code>. Abbreviated flags like <code>-d</code>, <code>-r</code>, <code>-I</code> after the <code>--</code> separator are no longer colon-joined with their next token in <code>fsi.CommandLineArgs</code>. (<a href="https://github.com/dotnet/fsharp/issues/10819">Issue #10819</a>, <a href="https://github.com/dotnet/fsharp/pull/19926">PR #19926</a>)</p>
</li>
<li><p>Fix Go-to-Definition for provided constructors that lack <code>TypeProviderDefinitionLocationAttribute</code>. Navigation now falls back to the declaring type instead of silently failing. (<a href="https://github.com/dotnet/fsharp/issues/5538">Issue #5538</a>, <a href="https://github.com/dotnet/fsharp/pull/19917">PR #19917</a>)</p>
</li>
<li><p>Render the declaring type for C#-style extension methods in overload-resolution error messages so the suggestion no longer falsely attributes the extension to the receiver type. (<a href="https://github.com/dotnet/fsharp/issues/9838">Issue #9838</a>, <a href="https://github.com/dotnet/fsharp/pull/19925">PR #19925</a>)</p>
</li>
<li><p>Fix FSI pretty printing to distinguish anonymous records (<code>{| ... |}</code>) from nominal records (<code>{ ... }</code>). (<a href="https://github.com/dotnet/fsharp/issues/6116">Issue #6116</a>, <a href="https://github.com/dotnet/fsharp/pull/19919">PR #19919</a>)</p>
</li>
<li><p>Fix dot-completion after indexed expressions (<code>a.[0].Data.</code>, <code>a[0].Data.</code>, <code>[1;2].Length.</code>) returning unrelated global completions instead of expression-typings members. (<a href="https://github.com/dotnet/fsharp/issues/4966">Issue #4966</a>, <a href="https://github.com/dotnet/fsharp/pull/19934">PR #19934</a>)</p>
</li>
<li><p>Quotations of <code>match s with &quot;&quot; -&gt; _</code> no longer leak the <code>s &lt;&gt; null &amp;&amp; s.Length = 0</code> lowering; the empty-string optimization moved from pattern-match compilation to the optimizer so quoted expressions keep <code>op_Equality(s, &quot;&quot;)</code>. (<a href="https://github.com/dotnet/fsharp/issues/19873">Issue #19873</a>)</p>
</li>
<li><p>Parser: recover on unfinished abstract members (<a href="https://github.com/dotnet/fsharp/pull/20070">PR #20070</a>)</p>
</li>
<li><p>Fix #5795: Allow attributes defined in a <code>module rec</code> / <code>namespace rec</code> scope to be used on union cases, record fields, and generic type parameters of types in the same recursive scope. (<a href="https://github.com/dotnet/fsharp/issues/5795">Issue #5795</a>, <a href="https://github.com/dotnet/fsharp/pull/19744">PR #19744</a>)</p>
</li>
<li><p>Import: Don't walk non-F# assemblies when labelling trait constraint sources (PR <a href="https://github.com/dotnet/fsharp/pull/20090">#20090</a>)</p>
</li>
<li><p>Avoid per-instance lock object in InterruptibleLazy and DelayInitArrayMap (PR <a href="https://github.com/dotnet/fsharp/pull/20088">#20088</a>)</p>
</li>
<li><p>IL: fix leaking binary view (<a href="https://github.com/dotnet/fsharp/pull/20250">PR #20250</a>)</p>
</li>
</ul>
<h3 data-fsdocs-heading="8"><a name="43.12.100-Added" class="anchor" href="#43.12.100-Added">Added</a></h3>
<ul>
<li>Added a &quot;most concrete&quot; tiebreaker for overload resolution (<code>--langversion:preview</code>). (<a href="https://github.com/fsharp/fslang-design/pull/834">RFC FS-1340</a>, <a href="https://github.com/dotnet/fsharp/pull/19277">PR #19277</a>)</li>
<li>Added support for <code>OverloadResolutionPriorityAttribute</code> in overload resolution (<code>--langversion:preview</code>). (<a href="https://github.com/fsharp/fslang-design/pull/828">RFC FS-1338</a>, <a href="https://github.com/dotnet/fsharp/pull/19277">PR #19277</a>)</li>
<li>Added internal synthesized-name replay infrastructure for compiler-generated names, preserving normal compilation output while enabling future hot reload name stability work.</li>
<li>Added <code>FSharpMemberOrFunctionOrValue.IsPropertyAccessor</code> convenience property that returns true for compiler-generated property accessors (<code>get_X</code> / <code>set_X</code>). (<a href="https://github.com/dotnet/fsharp/issues/18157">Issue #18157</a>, <a href="https://github.com/dotnet/fsharp/pull/19883">PR #19883</a>)</li>
<li>Added warning FS3884 when a function or delegate value is used as an interpolated string argument. (<a href="https://github.com/dotnet/fsharp/pull/19289">PR #19289</a>)</li>
<li>Symbols: add ObsoleteDiagnosticInfo (<a href="https://github.com/dotnet/fsharp/pull/19359">PR #19359</a>)</li>
<li>FCS: add FSharpCheckFileResults.HasErrors (<a href="https://github.com/dotnet/fsharp/pull/19892">PR #19892</a>)</li>
<li>Add <code>#version;;</code> directive to F# Interactive to display version and environment information. (<a href="https://github.com/dotnet/fsharp/issues/13307">Issue #13307</a>, <a href="https://github.com/dotnet/fsharp/pull/19332">PR #19332</a>)</li>
<li>Add diagnostic FS3889 when a namespace and a type have the same fully-qualified name in the same assembly, replacing the misleading FS0247 &quot;namespace and a module&quot; error. (<a href="https://github.com/dotnet/fsharp/issues/17827">Issue #17827</a>, <a href="https://github.com/dotnet/fsharp/pull/19802">PR #19802</a>)</li>
<li>Debug: rework for expressions stepping (<a href="https://github.com/dotnet/fsharp/pull/19894">PR #19894</a>)</li>
<li>Debug: rework conditional erasure, fix stepping over literals (<a href="https://github.com/dotnet/fsharp/pull/19897">PR #19897</a>)</li>
<li>Record spreads (<a href="https://github.com/fsharp/fslang-design/pull/805">RFC FS-1151</a>, <a href="https://github.com/dotnet/fsharp/pull/18927">PR #18927</a>, <a href="https://github.com/dotnet/fsharp/pull/20206">PR #20206</a>)</li>
<li>Debug: fix if and match condition sequence points (<a href="https://github.com/dotnet/fsharp/pull/19932">PR #19932</a>)</li>
<li>Surface the synthesized all-fields constructor of F# record types to F# code under the <code>RecordConstructorSyntax</code> preview feature, via a new <code>MethInfo.RecdCtor</code> case. (<a href="https://github.com/dotnet/fsharp/pull/19974">PR #19974</a>)</li>
<li>Under <code>--reflectionfree</code>, discriminated unions, records and anonymous records now get a <a href="../../reflectionfree-printing.md">generated <code>ToString</code></a> (rendering each field like <code>Option</code> does) instead of falling back to the namespace-qualified type name. (<a href="https://github.com/dotnet/fsharp/pull/19976">PR #19976</a>)</li>
<li>Support common types of <code>NotNullIfNotNullAttribute</code> usage. If a method parameter is marked with <code>NotNullIfNotNullAttribute</code>, the compiler will now honor this attribute and mark the return type as non-null. (<a href="https://github.com/dotnet/fsharp/pull/19977">PR #19977</a>)</li>
<li>Checker: recover on checking language version (<a href="https://github.com/dotnet/fsharp/pull/19970">PR ##19970</a>)</li>
<li>Implied argument names for function-to-delegate coercions now fall back to the delegate's <code>Invoke</code> parameter names when the function has no recoverable names (e.g. a partial application like <code>System.Func&lt;int, int&gt;((+) 1)</code>), instead of synthetic <code>delegateArg0</code>, <code>delegateArg1</code>, … names. (<a href="https://github.com/dotnet/fsharp/pull/20001">PR #20001</a>)</li>
<li>Add internal <code>ResetCompilerGeneratedNameState</code> to <code>CompilerGlobalState</code> name generators so warm-checker re-compilation can produce fresh-process-identical generated names. (<a href="https://github.com/dotnet/fsharp/pull/20017">PR #20017</a>)</li>
<li>Add internal ECMA-335 Edit-and-Continue metadata delta writer to AbstractIL. (<a href="https://github.com/dotnet/fsharp/pull/20019">PR #20019</a>)</li>
<li>Add Roslyn-format EnC CustomDebugInformation codec and portable PDB method CDI emission support to AbstractIL. (<a href="https://github.com/dotnet/fsharp/pull/20018">PR #20018</a>)</li>
<li>Extension members (including operators) now participate in SRTP (statically-resolved type parameter) constraint resolution, so member constraints on <code>inline</code> code can be satisfied by extension members — including operators defined on types you don't own via type extensions. Intrinsic members keep priority over extension members, and extension members solve SRTP constraints without satisfying nominal static-abstract interface (IWSAM) constraints. Also adds the <code>[&lt;AllowOverloadOnReturnType&gt;]</code> attribute for overloads that differ only by return type, and tuple type extensions (<code>type ('T1 * 'T2) with</code> / <code>type struct ('T1 * 'T2) with</code>). Requires <code>--langversion:preview</code>. (<a href="https://github.com/fsharp/fslang-design/blob/main/RFCs/FS-1043-extension-members-for-operators-and-srtp-constraints.md">RFC FS-1043</a>, <a href="https://github.com/fsharp/fslang-suggestions/issues/230">fslang-suggestions#230</a>, <a href="https://github.com/dotnet/fsharp/pull/19602">PR #19602</a>)</li>
<li>Add internal hot reload baseline reading for recorded EnC state and synthesized-name snapshot PDB data. (<a href="https://github.com/dotnet/fsharp/pull/20026">PR #20026</a>)</li>
<li>Support for the <code>&lt;include&gt;</code> XML documentation tag: at compile time, documentation is copied from an external XML file selected by an XPath query and emitted into the generated documentation file. <code>&lt;inheritdoc&gt;</code> remains unsupported. (<a href="https://github.com/dotnet/fsharp/issues/19175">Issue #19175</a>, <a href="https://github.com/dotnet/fsharp/pull/19186">PR #19186</a>)</li>
<li>Expand <code>&lt;inheritdoc/&gt;</code> at tooling time. In IDE tooltips, completion, and signature help, documentation is inherited from base classes, interfaces, overridden members, and constructors (matched by parameter signature). The FCS Symbols API (<code>FSharpSymbol.XmlDoc</code>) additionally resolves explicit <code>cref</code> targets, but does not expand constructor inheritance. The compiler emits the tag verbatim into generated XML documentation files, matching C#; <code>&lt;include&gt;</code> is not implemented. (<a href="https://github.com/dotnet/fsharp/issues/19175">Issue #19175</a>, <a href="https://github.com/dotnet/fsharp/pull/19188">PR #19188</a>)</li>
<li>Add symbol and type highlighting to F# diagnostics (<a href="https://github.com/dotnet/fsharp/pull/20097">PR #20097</a>)</li>
<li>IL: hold custom attributes in fields rather than a union case (<a href="https://github.com/dotnet/fsharp/pull/20287">PR #20287</a>)</li>
<li>IL: reuse the cached ILTypeRef in ILTypeInfo.FromType (<a href="https://github.com/dotnet/fsharp/pull/20255">PR #20255</a>)</li>
<li>Name resolution: group C#-style extension members per <code>open</code> and extended type (<a href="https://github.com/dotnet/fsharp/pull/20298">PR #20298</a>)</li>
</ul>
<h3 data-fsdocs-heading="9"><a name="43.12.100-Improved" class="anchor" href="#43.12.100-Improved">Improved</a></h3>
<ul>
<li>IL: cache C# extension methods per CCU (<a href="https://github.com/dotnet/fsharp/pull/20256">PR #20256</a>)</li>
<li>Nullness warning FS3261 on dotted method or property access (e.g. <code>x.Member</code>) now underlines the receiver expression and includes the member name and (when known) the binding name in the message. (<a href="https://github.com/dotnet/fsharp/issues/19658">Issue #19658</a>, <a href="https://github.com/dotnet/fsharp/pull/19814">PR #19814</a>)</li>
<li>Import: share assembly CCUs between projects (<a href="https://github.com/dotnet/fsharp/pull/20296">PR #20296</a>)</li>
<li>IL: share the pickled references (<a href="https://github.com/dotnet/fsharp/pull/20301">PR #20301</a>)</li>
<li>Direct delegate construction (<a href="https://github.com/dotnet/fsharp/pull/19993">PR ##19993</a>)</li>
<li>IL: add <code>ILPreNamespace</code>, make <code>ILPreTypeDef</code> creation lazy (<a href="https://github.com/dotnet/fsharp/pull/20092">PR #20092</a>)</li>
<li>IL: share ILCallingConv instances (<a href="https://github.com/dotnet/fsharp/pull/20254">PR #20254</a>)</li>
<li>IL: cache the ILTypeRef of a type def (<a href="https://github.com/dotnet/fsharp/pull/20259">PR #20259</a>)</li>
<li>IL: use empty tables for members when possible (<a href="https://github.com/dotnet/fsharp/pull/20249">PR #20249</a>)</li>
<li>Make Entity's adhoc members list lazy (<a href="https://github.com/dotnet/fsharp/pull/20286/changes">PR #20286</a>)</li>
<li>Constraint solver: <code>TryD</code> is now <code>inline</code> with <code>[&lt;InlineIfLambda&gt;]</code> on its always-run continuation, so the argument closures are no longer allocated at the (very hot) constraint-solver call sites; <code>IgnoreFailedMemberConstraintResolution</code> is <code>inline</code> so its forwarded continuation stays a literal. (<a href="https://github.com/dotnet/fsharp/pull/20367">PR #20367</a>)</li>
<li><code>DelayedILModuleReader</code> no longer boxes its cached <code>ILModuleReader</code> on every read: the field is typed <code>ILModuleReader | null</code> and matched directly. (<a href="https://github.com/dotnet/fsharp/pull/20413">PR #20413</a>)</li>
<li>Optimizer: passing a partial application of a non-inline module-level function to an <code>[&lt;InlineIfLambda&gt;]</code> parameter (e.g. <code>xs |&gt; Option.map (f a b)</code>) no longer allocates a per-call <code>FSharpFunc</code> closure when a captured argument is non-trivial (a field read, a call). Under optimization the argument is eta-expanded to a lambda with its captured evaluations floated above the binding, so the parameter's uses beta-reduce and the closure is eliminated. Captured arguments are still evaluated exactly once, in their original left-to-right order, and the binding keeps its sequence point. Partial applications of inline/SRTP functions and curried members can still allocate closures. (<a href="https://github.com/dotnet/fsharp/pull/20487">PR #20487</a>)</li>
</ul>
<h3 data-fsdocs-heading="10"><a name="43.12.100-Changed" class="anchor" href="#43.12.100-Changed">Changed</a></h3>
<ul>
<li><p>The <code>--warnaserror</code> option now ignores unrecognized diagnostic identifiers in warning lists while still applying recognized F# warning codes. (<a href="https://github.com/dotnet/fsharp/pull/20246">PR #20246</a>)</p>
</li>
<li><p>Reduce allocations when inlining a generic function by fusing the optimizer's copy and type-instantiation passes into a single tree traversal instead of cloning the body twice. (<a href="https://github.com/dotnet/fsharp/pull/20363">PR #20363</a>)</p>
</li>
<li><p>Improvements in error and warning messages: new error FS3885 when <code>let!</code>/<code>use!</code> is the final expression in a computation expression; new warning FS3886 when a list literal contains a single tuple element (likely missing <code>;</code> separator); improved wording for FS0003, FS0025, FS0039, FS0072, FS0247, FS0597, FS0670, FS3082, and SRTP operator-not-in-scope hints. (<a href="https://github.com/dotnet/fsharp/pull/19398">PR #19398</a>)</p>
</li>
<li><p>Exception field serialization (<code>GetObjectData</code> and field-restoring constructor) is now gated behind <code>langversion:11</code> (<code>LanguageFeature.ExceptionFieldSerializationSupport</code>). With langversion ≤10, exception codegen is unchanged from pre-#19342 behavior. (<a href="https://github.com/dotnet/fsharp/pull/19746">PR #19746</a>)</p>
</li>
<li><p>field serialization (<code>GetObjectData</code> and field-restoring constructor) is now gated behind <code>langversion:11</code> (<code>LanguageFeature.ExceptionFieldSerializationSupport</code>). With langversion ≤10, exception codegen is unchanged from pre-#19342 behavior. (<a href="https://github.com/dotnet/fsharp/pull/19746">PR #19746</a>)</p>
</li>
<li><p><code>Async.RunImmediate</code> renamed and replaced with impl of <code>FSharp.Core</code>'s <code>Async.RunSynchronouslyImmediate</code>, wherein <code>Exception</code>s are unwrapped (i.e., no egregious <code>AggregateException</code> wrapping). (<a href="https://github.com/fsharp/fslang-suggestions/issues/1042">Issue #1042</a>, <a href="https://github.com/dotnet/fsharp/pull/19804">PR #19804</a>, <a href="https://github.com/dotnet/fsharp/pull/20245">PR #20245</a>)</p>
</li>
<li><p>Lower string-typed interpolated strings to <code>System.String.Concat</code> rather than the reflection-based <code>printf</code> engine, making them trim- and NativeAOT-compatible. This generalizes and ungates the previous all-string <code>String.Concat</code> optimization, so it now applies to every string-typed interpolation. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1108">Language suggestion #1108</a>, <a href="https://github.com/dotnet/fsharp/pull/19971">PR #19971</a>)</p>
</li>
<li><p>Stabilized several <code>preview</code> language features into F# 11.0 (<code>--langversion:11.0</code>, enabled by default with a .NET 11 SDK): <code>MethodOverloadsCache</code>, <code>ErrorOnMissingSignatureAttribute</code>, <code>DirectDelegateConstruction</code>, <code>AccessProtectedBaseFieldFromClosure</code>, and <code>RecordSpreads</code>. <code>FromEndSlicing</code> intentionally remains in <code>preview</code>. (<a href="https://github.com/dotnet/fsharp/pull/20199">PR #20199</a>)</p>
</li>
<li><p>Interpolated string holes (e.g. <code>$&quot;{x}&quot;</code>) are now formatted with invariant culture (via the <code>string</code> operator) instead of the current thread culture. (<a href="https://github.com/dotnet/fsharp/pull/19971">PR #19971</a>)</p>
</li>
<li><p>Lines starting with <code>#:</code> are now ignored (<a href="https://github.com/fsharp/fslang-suggestions/issues/1440">Language suggestion 1440</a>, <a href="https://github.com/fsharp/fslang-design/pull/830">RFC FS-1337</a>, <a href="https://github.com/dotnet/fsharp/pull/20212">PR #20212</a>)</p>
</li>
<li><p><code>[&lt;return: X&gt;]</code> attributes written in front of a binding are again reported by <code>SynBinding.attributes</code> in the untyped syntax tree, with their original grouping and <code>[&lt; &gt;]</code> ranges. The rotation into <code>SynValInfo.retInfo</code> added by <a href="https://github.com/dotnet/fsharp/pull/19738">PR #19738</a> now happens while normalizing a binding for checking instead of in the parser, so both fixes from that PR are unchanged while tools reading the parse tree (formatters, analyzers, source generators) again see what was written. (<a href="https://github.com/dotnet/fsharp/pull/20356">PR #20356</a>)</p>
</li>
<li><p>Calculate Entity.PublicPath instead of storing (<a href="https://github.com/dotnet/fsharp/pull/20285">PR #20285</a>)</p>
</li>
<li><p>Remove the always-on <code>NullableOptionalInterop</code> language feature flag and collapse its guarded code to the enabled path; <code>--disableLanguageFeature:NullableOptionalInterop</code> is no longer a recognized feature name. (<a href="https://github.com/dotnet/fsharp/issues/20154">Issue #20154</a>, <a href="https://github.com/dotnet/fsharp/pull/20378">PR #20378</a>)</p>
</li>
<li><p>Remove trailing whitespace from source files. No functional change: whitespace inside string literals and inactive <code>#if</code> regions is preserved. (<a href="https://github.com/dotnet/fsharp/pull/20355">PR #20355</a>)</p>
</li>
</ul>
<h3 data-fsdocs-heading="11"><a name="43.12.100-Breaking Changes" class="anchor" href="#43.12.100-Breaking Changes">Breaking Changes</a></h3>
<ul>
<li><p>Add <code>ExtendedLayoutAttribute</code> support for future .NET runtime interop. <code>ILTypeDefLayout</code> has a new <code>Extended</code> case. (<a href="https://github.com/dotnet/fsharp/issues/19190">Issue #19190</a>, <a href="https://github.com/dotnet/fsharp/pull/19194">PR #19194</a>)</p>
</li>
<li><p><code>FSharp.Compiler.Syntax.SynInterpolatedStringPart.FillExpr</code> now carries a <code>SynInterpolationFormatting</code> value (separating .NET alignment/format from printf specifiers) instead of an <code>Ident option</code>. (<a href="https://github.com/dotnet/fsharp/pull/19971">PR #19971</a>)</p>
</li>
<li><p><code>FSharp.Compiler.Syntax.SynComponentInfo</code> now holds the type name as <code>synType: SynType option</code> instead of the previous <code>longId: LongIdent</code> field, so tuple-type extensions such as <code>type ('T1 * 'T2) with</code> can be represented. A <code>member LongIdent</code> compatibility property returns the long identifier for named types and an empty list for tuple or erroneous type names. AST consumers that pattern-matched on the <code>longId</code> field must switch to the <code>synType</code> field or the <code>LongIdent</code> member. (<a href="https://github.com/dotnet/fsharp/pull/19602">PR #19602</a>)</p>
</li>
<li><p>Optimizer: don't inline named functions in debug builds (<a href="https://github.com/dotnet/fsharp/pull/19548">PR #19548</a></p>
</li>
<li><p>LexFilter: drop non-strict mode (<a href="https://github.com/dotnet/fsharp/pull/20106">PR #20106</a>)</p>
</li>
</ul>

<h2 data-fsdocs-heading="12"><a name="43.11.300" class="anchor" href="#43.11.300">43.11.300 - 2026-05-12</a></h2><a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.11.300" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.11.300-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.11.301" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.11.301-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.11.302" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.11.302-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.11.303" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.11.303-blue"></a><h3 data-fsdocs-heading="13"><a name="43.11.300-Fixed" class="anchor" href="#43.11.300-Fixed">Fixed</a></h3>
<ul>
<li>Fix strong name signature size to align with Roslyn for public signing (<a href="https://github.com/dotnet/fsharp/issues/17451">Issue #17451</a>, <a href="https://github.com/dotnet/fsharp/pull/19242">PR #19242</a>)</li>
<li>Nullness: Fix UoM ToString returning <code>string | null</code> for value types. (<a href="https://github.com/dotnet/fsharp/issues/17539">Issue #17539</a>, <a href="https://github.com/dotnet/fsharp/pull/19262">PR #19262</a>)</li>
<li>Nullness: Fix pipe operator nullness warning location to point at nullable argument. (<a href="https://github.com/dotnet/fsharp/issues/18013">Issue #18013</a>, <a href="https://github.com/dotnet/fsharp/pull/19262">PR #19262</a>)</li>
<li>Nullness: Fix false positive warning when passing non-null AllowNullLiteral constructor result. (<a href="https://github.com/dotnet/fsharp/issues/18021">Issue #18021</a>, <a href="https://github.com/dotnet/fsharp/pull/19262">PR #19262</a>)</li>
<li>Nullness: Allow <code>not null</code> constraint on type extensions. (<a href="https://github.com/dotnet/fsharp/issues/18334">Issue #18334</a>, <a href="https://github.com/dotnet/fsharp/pull/19262">PR #19262</a>)</li>
<li>Nullness: Simplify tuple null elimination to prevent over-inference of non-null. (<a href="https://github.com/dotnet/fsharp/issues/19042">Issue #19042</a>, <a href="https://github.com/dotnet/fsharp/pull/19262">PR #19262</a>)</li>
<li>Fixed Find All References not correctly finding active pattern cases in signature files. (<a href="https://github.com/dotnet/fsharp/issues/19173">Issue #19173</a>, <a href="https://github.com/dotnet/fsharp/issues/14969">Issue #14969</a>, <a href="https://github.com/dotnet/fsharp/pull/19252">PR #19252</a>)</li>
<li>Fixed Rename not correctly handling operators containing <code>.</code> (e.g., <code>-.-</code>). (<a href="https://github.com/dotnet/fsharp/issues/17221">Issue #17221</a>, <a href="https://github.com/dotnet/fsharp/issues/14057">Issue #14057</a>, <a href="https://github.com/dotnet/fsharp/pull/19252">PR #19252</a>)</li>
<li>Fixed Find All References not correctly applying <code>#line</code> directive remapping. (<a href="https://github.com/dotnet/fsharp/issues/9928">Issue #9928</a>, <a href="https://github.com/dotnet/fsharp/pull/19252">PR #19252</a>)</li>
<li>Fixed <code>SynPat.Or</code> pattern variables (non-left-most) incorrectly classified as bindings instead of uses. (<a href="https://github.com/dotnet/fsharp/issues/5546">Issue #5546</a>, <a href="https://github.com/dotnet/fsharp/pull/19252">PR #19252</a>)</li>
<li>Fixed Find All References not finding discriminated union types defined inside modules. (<a href="https://github.com/dotnet/fsharp/issues/5545">Issue #5545</a>, <a href="https://github.com/dotnet/fsharp/pull/19252">PR #19252</a>)</li>
<li>Fixed synthetic event handler values appearing in Find All References results. (<a href="https://github.com/dotnet/fsharp/issues/4136">Issue #4136</a>, <a href="https://github.com/dotnet/fsharp/pull/19252">PR #19252</a>)</li>
<li>Fixed Find All References not finding all usages of C# extension methods. (<a href="https://github.com/dotnet/fsharp/issues/16993">Issue #16993</a>, <a href="https://github.com/dotnet/fsharp/pull/19252">PR #19252</a>)</li>
<li>Fixed Find All References on discriminated union cases not including case tester properties (e.g., <code>.IsCase</code>). (<a href="https://github.com/dotnet/fsharp/issues/16621">Issue #16621</a>, <a href="https://github.com/dotnet/fsharp/pull/19252">PR #19252</a>)</li>
<li>Fixed Find All References on record types not including copy-and-update expressions. (<a href="https://github.com/dotnet/fsharp/issues/15290">Issue #15290</a>, <a href="https://github.com/dotnet/fsharp/pull/19252">PR #19252</a>)</li>
<li>Fixed Find All References on constructor definitions not finding all constructor usages. (<a href="https://github.com/dotnet/fsharp/issues/14902">Issue #14902</a>, <a href="https://github.com/dotnet/fsharp/pull/19252">PR #19252</a>)</li>
<li>Fixed Find All References producing corrupted duplicate constructor symbol references with shifted ranges, and removed duplicate attribute symbols from type definition error-reporting. (<a href="https://github.com/dotnet/fsharp/issues/19336">Issue #19336</a>, <a href="https://github.com/dotnet/fsharp/pull/19358">PR #19358</a>)</li>
<li>Fixed semantic classification regression where copy-and-update record fields were colored as type names, and union case tester dot was colored as union case. (<a href="https://github.com/dotnet/fsharp/pull/19311">PR #19311</a>)</li>
<li>Fix false FS1182 (unused variable) warning for query expression variables used in where, let, join, and select clauses. (<a href="https://github.com/dotnet/fsharp/issues/422">Issue #422</a>)</li>
<li>Fix FS0229 B-stream misalignment when reading metadata from assemblies compiled with LangVersion &lt; 9.0, introduced by <a href="https://github.com/dotnet/fsharp/pull/17706">#17706</a>. (<a href="https://github.com/dotnet/fsharp/pull/19260">PR #19260</a>)</li>
<li>Fix FS3356 false positive for instance extension members with same name on different types, introduced by <a href="https://github.com/dotnet/fsharp/pull/18821">#18821</a>. (<a href="https://github.com/dotnet/fsharp/pull/19260">PR #19260</a>)</li>
<li>Fix graph-based type checking incorrectly resolving dependencies when the same module name is defined across multiple files in the same namespace. (<a href="https://github.com/dotnet/fsharp/pull/19280">PR #19280</a>)</li>
<li>F# Scripts: Fix default reference paths resolving when an SDK directory is specified. (<a href="https://github.com/dotnet/fsharp/pull/19270">PR #19270</a>)</li>
<li>Improve static compilation of state machines. (<a href="https://github.com/dotnet/fsharp/pull/19297">PR #19297</a>)</li>
<li>Fix a bug where <code>let!</code> and <code>use!</code> were incorrectly allowed outside computation expressions. <a href="https://github.com/dotnet/fsharp/pull/19347">PR #19347</a></li>
<li>Fix TypeLoadException when creating delegate with voidptr parameter. (Issue <a href="https://github.com/dotnet/fsharp/issues/11132">#11132</a>, <a href="https://github.com/dotnet/fsharp/pull/19338">PR #19338</a>)</li>
<li>Suppress tail calls when localloc (NativePtr.stackalloc) is used. (Issue <a href="https://github.com/dotnet/fsharp/issues/13447">#13447</a>, <a href="https://github.com/dotnet/fsharp/pull/19338">PR #19338</a>)</li>
<li>Fix TypeLoadException in Release builds with inline constraints. (Issue <a href="https://github.com/dotnet/fsharp/issues/14492">#14492</a>, <a href="https://github.com/dotnet/fsharp/pull/19338">PR #19338</a>)</li>
<li>Fix nativeptr in interfaces leads to TypeLoadException. (Issue <a href="https://github.com/dotnet/fsharp/issues/14508">#14508</a>, <a href="https://github.com/dotnet/fsharp/pull/19338">PR #19338</a>)</li>
<li>Fix box instruction for literal upcasts. (Issue <a href="https://github.com/dotnet/fsharp/issues/18319">#18319</a>, <a href="https://github.com/dotnet/fsharp/pull/19338">PR #19338</a>)</li>
<li>Fix Decimal Literal causes InvalidProgramException in Debug builds. (Issue <a href="https://github.com/dotnet/fsharp/issues/18956">#18956</a>, <a href="https://github.com/dotnet/fsharp/pull/19338">PR #19338</a>)</li>
</ul>
<h3 data-fsdocs-heading="14"><a name="43.11.300-Added" class="anchor" href="#43.11.300-Added">Added</a></h3>
<ul>
<li><p>FSharpType: add ImportILType (<a href="https://github.com/dotnet/fsharp/pull/19300">PR #19300</a>)</p>
</li>
<li><p>Type checker: recover on argument/overload checking (<a href="https://github.com/dotnet/fsharp/pull/19314">PR #19314</a>)</p>
</li>
<li><p>FSharpType: add more predefined type checks (<a href="https://github.com/dotnet/fsharp/pull/19325">PR #19325</a>)</p>
</li>
<li><p>Introduced a separate <code>NotifyRelatedSymbolUse</code> sink and <code>[&lt;Flags&gt;] RelatedSymbolUseKind</code> enum for related symbol lookups (union case testers, copy-and-update record types). <code>GetUsesOfSymbolInFile</code> and <code>GetSemanticClassification</code> accept an optional <code>relatedSymbolKinds</code> parameter to opt in. (<a href="https://github.com/dotnet/fsharp/pull/19361">PR #19361</a>)</p>
</li>
<li><p>Support <code>#exit;;</code> as alias to <code>#quit;;</code> in F# Interactive. (<a href="https://github.com/dotnet/fsharp/pull/19329">PR #19329</a>)</p>
</li>
<li><p>FCS: capture additional types during analysis (<a href="https://github.com/dotnet/fsharp/pull/19305">PR #19305</a>)</p>
</li>
</ul>
<h3 data-fsdocs-heading="15"><a name="43.11.300-Changed" class="anchor" href="#43.11.300-Changed">Changed</a></h3>
<ul>
<li>Centralized product TFM (Target Framework Moniker) into MSBuild props file <code>eng/TargetFrameworks.props</code>. Changing the target framework now only requires editing one file, and it integrates with MSBuild's <code>--getProperty</code> for scripts.</li>
<li>Overload resolution results are now cached, providing compile time improvements for code with repeated method calls. (<a href="https://github.com/dotnet/fsharp/issues/18807">Issue #18807</a>)</li>
<li>Symbols: safer qualified name getting (<a href="https://github.com/dotnet/fsharp/pull/19298">PR #19298</a>)</li>
</ul>
<h3 data-fsdocs-heading="16"><a name="43.11.300-Breaking Changes" class="anchor" href="#43.11.300-Breaking Changes">Breaking Changes</a></h3>

<h2 data-fsdocs-heading="17"><a name="43.10.100" class="anchor" href="#43.10.100">43.10.100 - 2025-11-11</a></h2><a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.10.100" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.10.100-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.10.101" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.10.101-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.10.102" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.10.102-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.10.103" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.10.103-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.10.104" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.10.104-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.10.105" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.10.105-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.10.106" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.10.106-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.10.107" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.10.107-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.10.108" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.10.108-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.10.109" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.10.109-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.10.110" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.10.110-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.10.111" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.10.111-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.10.112" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.10.112-blue"></a><h3 data-fsdocs-heading="18"><a name="43.10.100-Added" class="anchor" href="#43.10.100-Added">Added</a></h3>
<ul>
<li>Symbols: add Mfv.ApparentEnclosingType (<a href="https://github.com/dotnet/fsharp/pull/17494">PR #17494</a>)</li>
<li>Add opt-in warning attribute not valid for union case with fields <a href="https://github.com/dotnet/fsharp/pull/18532">PR #18532</a>)</li>
<li>Add support for <code>when 'T : Enum</code> library-only static optimization constraint. (<a href="https://github.com/dotnet/fsharp/pull/18546">PR #18546</a>)</li>
<li>Add support for tail calls in computation expressions (<a href="https://github.com/dotnet/fsharp/pull/18804">PR #18804</a>)</li>
<li>Add <code>--typecheck-only</code> flag support for F# Interactive (FSI) scripts to type-check without execution. (<a href="https://github.com/dotnet/fsharp/issues/18686">Issue #18686</a>, <a href="https://github.com/dotnet/fsharp/pull/18687">PR #18687</a>)</li>
<li>Diagnostics: add extended data for 'No constructors' error (<a href="https://github.com/dotnet/fsharp/pull/18863">PR #18863</a>)</li>
<li>FSharpType.Format: support top-level prefix generic types style. (<a href="https://github.com/dotnet/fsharp/pull/18897">PR #18897</a>)</li>
<li>FCS: allow getting captured types (<a href="https://github.com/dotnet/fsharp/pull/18878">PR #18878</a>)</li>
</ul>
<h3 data-fsdocs-heading="19"><a name="43.10.100-Fixed" class="anchor" href="#43.10.100-Fixed">Fixed</a></h3>
<ul>
<li>Fix F# compiler to prevent tail call emission when pinned locals are present (<a href="https://github.com/dotnet/fsharp/pull/18893">PR #18893</a>)</li>
<li>Fix SignatureHash to include constant values in hash computation (<a href="https://github.com/dotnet/fsharp/issues/18758">Issue #18758</a>, <a href="https://github.com/dotnet/fsharp/pull/18771">PR #18771</a>)</li>
<li>Fix parsing errors using anonymous records and units of measures (<a href="https://github.com/dotnet/fsharp/pull/18543">PR #18543</a>)</li>
<li>Fix parsing errors using anonymous records and code quotations (<a href="https://github.com/dotnet/fsharp/pull/18603">PR #18603</a>)</li>
<li>Better error message for attribute targets. (<a href="https://github.com/dotnet/fsharp/pull/18641">PR #18641</a>)</li>
<li>Fixed: Allow <code>return</code>, <code>return!</code>, <code>yield</code>, <code>yield!</code> type annotations without parentheses (<a href="https://github.com/dotnet/fsharp/pull/18533">PR #18533</a>)</li>
<li>Allow <code>let!</code>, <code>use!</code>, <code>and!</code> type annotations without requiring parentheses ((<a href="https://github.com/dotnet/fsharp/pull/18508">PR #18508</a> and <a href="https://github.com/dotnet/fsharp/pull/18682">PR #18682</a>))</li>
<li>Fix find all references for F# exceptions (<a href="https://github.com/dotnet/fsharp/pull/18565">PR #18565</a>)</li>
<li>Shorthand lambda: fix completion for chained calls and analysis for unfinished expression (<a href="https://github.com/dotnet/fsharp/pull/18560">PR #18560</a>)</li>
<li>Completion: fix previous namespace considered opened <a href="https://github.com/dotnet/fsharp/pull/18609">PR #18609</a></li>
<li>Fix active pattern typechecking regression. (<a href="https://github.com/dotnet/fsharp/issues/18638">Issue #18638</a>, <a href="https://github.com/dotnet/fsharp/pull/18642">PR #18642</a>)</li>
<li>Fix nullness warnings when casting non-nullable values to <code>IEquatable&lt;T&gt;</code> to match C# behavior. (<a href="https://github.com/dotnet/fsharp/issues/18759">Issue #18759</a>, <a href="https://github.com/dotnet/fsharp/pull/18770">PR #18770</a>)</li>
<li>Error on invalid declarations in type definitions.(<a href="https://github.com/dotnet/fsharp/issues/10066">Issue #10066</a>, <a href="https://github.com/dotnet/fsharp/pull/18813">PR #18813</a>)</li>
<li>Fix IsByRefLikeAttribute types being incorrectly suppressed in completion lists. Types like <code>Span&lt;T&gt;</code> and <code>ReadOnlySpan&lt;T&gt;</code> now appear correctly in IntelliSense. (<a href="https://github.com/dotnet/fsharp/pull/18784">PR #18784</a>)</li>
<li>Fix SRTP nullness constraint resolution for types imported from older assemblies. AmbivalentToNull types now use legacy F# nullness rules instead of always satisfying <code>'T : null</code> constraints. (<a href="https://github.com/dotnet/fsharp/issues/18390">Issue #18390</a>, <a href="https://github.com/dotnet/fsharp/issues/18344">Issue #18344</a>, <a href="https://github.com/dotnet/fsharp/pull/18785">PR #18785</a>)</li>
<li>Fix Show XML doc for enum fields in external metadata (<a href="https://github.com/dotnet/fsharp/issues/17939#issuecomment-3137410105">Issue #17939</a>, <a href="https://github.com/dotnet/fsharp/pull/18800">PR #18800</a>)</li>
<li>Fix nullable types formatting in <code>FSharpType.Format</code> and tooltips to include parentheses. (<a href="https://github.com/dotnet/fsharp/pull/18842">PR #18842</a>)</li>
<li>TypeMismatchDiagnosticExtendedData: fix expected and actual types calculation. (<a href="https://github.com/dotnet/fsharp/pull/18851">PR #18851</a>)</li>
<li>Format top-level generic types using a prefix style in inherit/interface declarations and flexible type annotations. (<a href="https://github.com/dotnet/fsharp/pull/18897">PR #18897</a>)</li>
<li>Parser: fix range for computed binding expressions (<a href="https://github.com/dotnet/fsharp/pull/18903">PR #18903</a>)</li>
<li>Tests: set test source for range debug printing (<a href="https://github.com/dotnet/fsharp/pull/18879">PR #18879</a>)</li>
<li>Checker: fix declaring type for abbreviated types extensions (<a href="https://github.com/dotnet/fsharp/pull/18909">PR #18909</a>)</li>
<li>Caches: type subsumption cache key perf regression (<a href="https://github.com/dotnet/fsharp/issues/18925">Issue #18925</a> <a href="https://github.com/dotnet/fsharp/pull/18926">PR #18926</a>)</li>
<li>Ensure that line directives are applied to source identifiers (issue <a href="https://github.com/dotnet/fsharp/issues/18908">#18908</a>, PR <a href="https://github.com/dotnet/fsharp/pull/18918">#18918</a>)</li>
<li>Fix expected and actual types in ErrorFromAddingTypeEquation message and extended diagnostic data. (<a href="https://github.com/dotnet/fsharp/pull/18915">PR #18915</a>)</li>
<li>Editor: Fix Record fields completion in update record with partial field name. (<a href="https://github.com/dotnet/fsharp/pull/18946">PR #18946</a>)</li>
</ul>
<h3 data-fsdocs-heading="20"><a name="43.10.100-Changed" class="anchor" href="#43.10.100-Changed">Changed</a></h3>
<ul>
<li>Use <code>errorR</code> instead of <code>error</code> in <code>CheckDeclarations.fs</code> when possible. (<a href="https://github.com/dotnet/fsharp/pull/18645">PR #18645</a>)</li>
<li>Parser: Capture named fields block separators. (<a href="https://github.com/dotnet/fsharp/pull/18857">PR #18857</a>)</li>
<li>Type checker: use inner expr range in upcast constraints errors (<a href="https://github.com/dotnet/fsharp/pull/18850">PR #18850</a>)</li>
<li>Import <code>IEnumerable</code> as <code>seq</code>. (<a href="https://github.com/dotnet/fsharp/pull/18865">PR #18865</a>)</li>
</ul>
<h3 data-fsdocs-heading="21"><a name="43.10.100-Breaking Changes" class="anchor" href="#43.10.100-Breaking Changes">Breaking Changes</a></h3>
<ul>
<li>Scoped Nowarn: Add the #warnon compiler directive (<a href="https://github.com/fsharp/fslang-suggestions/issues/278">Language suggestion #278</a>, <a href="https://github.com/fsharp/fslang-design/pull/782">RFC FS-1146 PR</a>, <a href="https://github.com/dotnet/fsharp/pull/18049">PR #18049</a> and <a href="https://github.com/dotnet/fsharp/pull/18637">PR #18637</a>)</li>
<li>Simplify creation of <code>FSharpDiagnostics</code>. In a few cases, errors without ranges were assigned to the currently checked file, while in other cases they carried an empty range. The latter is now true in all cases. In a few cases, ranges at eof were corrected, while in others they were not. They are now always left uncorrected. This is a prerequisit for <a href="https://github.com/dotnet/fsharp/issues/18553">#18553</a>. (<a href="https://github.com/dotnet/fsharp/pull/18610">PR #18610</a>).</li>
<li><code>SynExprRecordField</code> now includes a <code>range</code> field (<a href="https://github.com/dotnet/fsharp/pull/18617">PR #18617</a>)</li>
<li>Mark <code>Range.Zero</code> as obsolete in favor of <code>Range.range0</code> (<a href="https://github.com/dotnet/fsharp/pull/18664">PR #18664</a>)</li>
<li>Use <code>Synbinding</code> to model <code>and!</code> (<a href="https://github.com/dotnet/fsharp/pull/18805">PR #18805</a>)</li>
<li>Redesign #line processing. The original positions (unaffected by #line directives) are now kept in the AST, and <code>__LINE__</code> and <code>__SOURCE_LINE__</code> show the original line numbers / file names. However, all diagnostics and debug information stays the same (shows the position transformed by the #line directives). (<a href="https://github.com/dotnet/fsharp/issues/18553">Issue #18553</a>, <a href="https://github.com/dotnet/fsharp/pull/18699">PR #18699</a>, <a href="https://github.com/dotnet/fsharp/pull/18828">PR 18828</a>, <a href="https://github.com/dotnet/fsharp/pull/18829">PR 18829</a>)</li>
<li>Unify <code>let</code>, <code>let!</code>, <code>use</code> and <code>use!</code> AST representation. (<a href="https://github.com/dotnet/fsharp/pull/18825">PR #18825</a>)[^1]</li>
</ul>
<h3 data-fsdocs-heading="22"><a name="43.10.100-Migration Guidance for AST Users" class="anchor" href="#43.10.100-Migration Guidance for AST Users">Migration Guidance for AST Users</a></h3>
<p><strong>Note:</strong> The unified AST introduces two new boolean fields:</p>
<ul>
<li><code>isFromSource</code>: Indicates if the binding comes from user-written code (<code>true</code>) or is compiler-generated (<code>false</code>)</li>
<li><code>isBang</code>: Distinguishes computation expression bindings (<code>let!</code>/<code>use!</code> = <code>true</code>) from regular bindings (<code>let</code>/<code>use</code> = <code>false</code>)</li>
</ul>
<h3 data-fsdocs-heading="23"><a name="43.10.100-1. Pattern Matching Updates" class="anchor" href="#43.10.100-1. Pattern Matching Updates">1. Pattern Matching Updates</a></h3>
<p><strong>Before:</strong></p>
<pre><code class="language-fsharp">match expr with
| SynExpr.LetOrUse(isRec, isUse, bindings, body, range, trivia) -&gt;
    // Handle regular let/use
| SynExpr.LetOrUseBang(spBind, isUse, isFromSource, pat, rhs, andBangs, body, range, trivia) -&gt;
    // Handle let!/use!
</code></pre>
<p><strong>After:</strong></p>
<pre><code class="language-fsharp">match expr with
| SynExpr.LetOrUse(isRec, isUse, isFromSource, isBang, bindings, body, range, trivia) -&gt;
    if isBang then
        // This is a let!/use! expression
        match bindings with
        | firstBinding :: andBangs -&gt;
            match firstBinding with
            | SynBinding(headPat = pat; expr = rhs) -&gt;
                // pat and rhs extracted from first binding
                // andBangs contains the and! bindings
        | [] -&gt; // error case
    else
        // This is a regular let/use expression
</code></pre>
<h3 data-fsdocs-heading="24"><a name="43.10.100-2. Construction Updates" class="anchor" href="#43.10.100-2. Construction Updates">2. Construction Updates</a></h3>
<p><strong>Before:</strong></p>
<pre><code class="language-fsharp">// Creating a let! expression
SynExpr.LetOrUseBang(
    bindDebugPoint,
    false,  // isUse
    true,   // isFromSource
    pat,
    rhsExpr,
    andBangs,
    bodyExpr,
    range,
    trivia
)
</code></pre>
<p><strong>After:</strong></p>
<pre><code class="language-fsharp">// Creating a let! expression
let firstBinding = SynBinding(
    accessibility = None,
    kind = SynBindingKind.Normal,
    isInline = false,
    isMutable = false,
    attributes = [],
    xmlDoc = PreXmlDoc.Empty,
    valData = SynInfo.emptySynValData,
    headPat = pat,           // Pattern moved here
    returnInfo = None,
    expr = rhsExpr,          // RHS moved here
    range = range,
    debugPoint = bindDebugPoint,  // Debug point moved here
    trivia = bindingTrivia
)
SynExpr.LetOrUse(
    false,  // isRecursive
    false,  // isUse
    true,   // isFromSource
    true,   // isBang (indicates let!)
    firstBinding :: andBangs,  // All bindings in single list
    bodyExpr,
    range,
    trivia
)
</code></pre>
<h3 data-fsdocs-heading="25"><a name="43.10.100-3. Common Migration Patterns" class="anchor" href="#43.10.100-3. Common Migration Patterns">3. Common Migration Patterns</a></h3>
<p><strong>Checking for computation expressions:</strong></p>
<pre><code class="language-fsharp">// Before
match expr with
| SynExpr.LetOrUseBang _ -&gt; true
| _ -&gt; false

// After
match expr with
| SynExpr.LetOrUse(isBang = true) -&gt; true
| _ -&gt; false
</code></pre>
<p><strong>Extracting pattern and expression from let!:</strong></p>
<pre><code class="language-fsharp">// Before
| SynExpr.LetOrUseBang(_, _, _, pat, rhs, _, _, _, _) -&gt;
    processBinding pat rhs

// After
| SynExpr.LetOrUse(isBang = true; bindings = binding :: _) -&gt;
    match binding with
    | SynBinding(headPat = pat; expr = rhs) -&gt;
        processBinding pat rhs
    | _ -&gt; // error
</code></pre>
<p><strong>Processing and! bindings:</strong></p>
<pre><code class="language-fsharp">// Before
| SynExpr.LetOrUseBang(_, _, _, firstPat, firstRhs, andBangs, _, _, _) -&gt;
    processFirst firstPat firstRhs
    for andBang in andBangs do
        processAndBang andBang

// After
| SynExpr.LetOrUse(isBang = true; bindings = bindings) -&gt;
    match bindings with
    | first :: rest -&gt;
        processBinding first
        for andBang in rest do
            processAndBang andBang
    | [] -&gt; // error
</code></pre>
<p>[^1]: See <a href="#migration-guidance-for-ast-users">Migration Guidance for AST Users</a> section for detailed information on how to update your code to work with the unified AST representation.</p>

<h2 data-fsdocs-heading="26"><a name="43.9.300" class="anchor" href="#43.9.300">43.9.300 - 2025-05-13</a></h2><a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.9.300" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.9.300-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.9.303" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.9.303-blue"></a><h3 data-fsdocs-heading="27"><a name="43.9.300-Fixed" class="anchor" href="#43.9.300-Fixed">Fixed</a></h3>
<ul>
<li>Fix missing TailCall warning in TOp.IntegerForLoop (<a href="https://github.com/dotnet/fsharp/pull/18399">PR #18399</a>)</li>
<li>Fix classification of <code>nameof</code> in <code>nameof&lt;'T&gt;</code>, <code>match … with nameof ident -&gt; …</code>. (<a href="https://github.com/dotnet/fsharp/issues/10026">Issue #10026</a>, <a href="https://github.com/dotnet/fsharp/pull/18300">PR #18300</a>)</li>
<li>Fix Realsig+ generates nested closures with incorrect Generic  (<a href="https://github.com/dotnet/fsharp/issues/17797">Issue #17797</a>, <a href="https://github.com/dotnet/fsharp/pull/17877">PR #17877</a>)</li>
<li>Fix optimizer internal error for records with static fields  (<a href="https://github.com/dotnet/fsharp/issues/18165">Issue #18165</a>, <a href="https://github.com/dotnet/fsharp/pull/18280">PR #18280</a>)</li>
<li>Fix nullness warning with flexible types  (<a href="https://github.com/dotnet/fsharp/issues/18056">Issue #18056</a>, <a href="https://github.com/dotnet/fsharp/pull/18266">PR #18266</a>)</li>
<li>Allow first branches of match and if expressions to return nullable results  (<a href="https://github.com/dotnet/fsharp/issues/18015">Issue #18015</a>, <a href="https://github.com/dotnet/fsharp/pull/18322">PR #18322</a>)</li>
<li>Fix internal error when missing measure attribute in an unsolved measure typar. (<a href="https://github.com/dotnet/fsharp/issues/7491">Issue #7491</a>, <a href="https://github.com/dotnet/fsharp/pull/18234">PR #18234</a>==</li>
<li>Set <code>Cancellable.token</code> from async computation (<a href="https://github.com/dotnet/fsharp/issues/18235">Issue #18235</a>, <a href="https://github.com/dotnet/fsharp/pull/18238">PR #18238</a>)</li>
<li>Fix missing nullness warning when static upcast dropped nullness (<a href="https://github.com/dotnet/fsharp/issues/18232">Issue #18232</a>, <a href="https://github.com/dotnet/fsharp/pull/18261">PR #18261</a>)</li>
<li>Cancellable: only cancel on OCE with own token (<a href="https://github.com/dotnet/fsharp/pull/18277">PR #18277</a>)</li>
<li>Cancellable: set token in more places (<a href="https://github.com/dotnet/fsharp/pull/18283">PR #18283</a>)</li>
<li>Cancellable: fix leaking cancellation token (<a href="https://github.com/dotnet/fsharp/pull/18295">PR #18295</a>)</li>
<li>Fix NRE when accessing nullable fields of types within their equals/hash/compare methods (<a href="https://github.com/dotnet/fsharp/pull/18296">PR #18296</a>)</li>
<li>Fix nullness warning for overrides of generic code with nullable type instance (<a href="https://github.com/dotnet/fsharp/issues/17988">Issue #17988</a>, <a href="https://github.com/dotnet/fsharp/pull/18337">PR #18337</a>)</li>
<li>Unsafe downcast from <code>obj</code> to generic <code>T</code> no longer requires <code>not null</code> constraint on <code>T</code>(<a href="https://github.com/dotnet/fsharp/issues/18275">Issue #18275</a>, <a href="https://github.com/dotnet/fsharp/pull/18343">PR #18343</a>)</li>
<li>Fix &quot;type inference problem too complicated&quot; for SRTP with T:null and T:struct dummy constraint(<a href="https://github.com/dotnet/fsharp/issues/18288">Issue #18288</a>, <a href="https://github.com/dotnet/fsharp/pull/18345">PR #18345</a>)</li>
<li>Fix for missing parse diagnostics in TransparentCompiler.ParseAndCheckProject (<a href="https://github.com/dotnet/fsharp/pull/18366">PR #18366</a>)</li>
<li>Miscellanous parentheses analyzer fixes. (<a href="https://github.com/dotnet/fsharp/pull/18350">PR #18350</a>, <a href="https://github.com/dotnet/fsharp/pull/18534">PR #18534</a>)</li>
<li>Fix duplicate parse error reporting for GetBackgroundCheckResultsForFileInProject (<a href="https://github.com/dotnet/fsharp/issues/18379">Issue #18379</a> <a href="https://github.com/dotnet/fsharp/pull/18380">PR #18380</a>)</li>
<li>Fix MethodDefNotFound when compiling code invoking delegate with option parameter (<a href="https://github.com/dotnet/fsharp/issues/5171">Issue #5171</a>, <a href="https://github.com/dotnet/fsharp/pull/18385">PR #18385</a>)</li>
<li>Fix #r nuget ...&quot; downloads unneeded packages (<a href="https://github.com/dotnet/fsharp/issues/18231">Issue #18231</a>, <a href="https://github.com/dotnet/fsharp/pull/18393">PR #18393</a>)</li>
<li>Fix checking bug in unpickling <a href="https://github.com/dotnet/fsharp/pull/18430">PR #18430</a></li>
<li>Reenable β-reduction and subsequent reoptimization of immediately-invoked F#-defined generic delegates. (<a href="https://github.com/dotnet/fsharp/pull/18401">PR #18401</a>)</li>
<li>Fixed <a href="https://github.com/dotnet/fsharp/issues/18433">#18433</a>, a rare case of an internal error in xml comment processing. (<a href="https://github.com/dotnet/fsharp/pull/18436">PR #18436</a>)</li>
<li>Fix confusing type inference error in task expression (<a href="https://github.com/dotnet/fsharp/issues/13789">Issue #13789</a>, <a href="https://github.com/dotnet/fsharp/pull/18450">PR #18450</a>)</li>
<li>Fix missing <code>null</code> highlighting in tooltips (<a href="https://github.com/dotnet/fsharp/pull/18457">PR #18457</a>)</li>
<li>Fix range of SynPat.Named doesn't include accessibility (<a href="https://github.com/dotnet/fsharp/pull/18526">PR #18526</a>)</li>
<li>Allow <code>_</code> in <code>use!</code> bindings values (lift FS1228 restriction) (<a href="https://github.com/dotnet/fsharp/pull/18487">PR #18487</a>)</li>
<li>Make <code>[&lt;CallerMemberName; Struct&gt;]</code> combination work(<a href="https://github.com/dotnet/fsharp/pull/18444/">PR #18444</a>)</li>
<li>Fix code completion considers types from own namespace non-imported (<a href="https://github.com/dotnet/fsharp/issues/18518">PR #18518</a>)</li>
<li>Code completion: fix getting qualifier expression in do statements in type decls (<a href="https://github.com/dotnet/fsharp/pull/18524">PR #18524</a>)</li>
<li>Fix parsing errors using anonymous records and units of measures (<a href="https://github.com/dotnet/fsharp/pull/18543">PR #18543</a>)</li>
<li>Fixed: <a href="https://github.com/dotnet/fsharp/issues/18441">#18441</a> FSI multi-emit unstable. (<a href="https://github.com/dotnet/fsharp/pull/18465">PR #18465</a>)</li>
<li>Fixed: Allow <code>return</code>, <code>return!</code>, <code>yield</code>, <code>yield!</code> type annotations without parentheses (<a href="https://github.com/dotnet/fsharp/pull/18533">PR #18533</a>)</li>
</ul>
<h3 data-fsdocs-heading="28"><a name="43.9.300-Added" class="anchor" href="#43.9.300-Added">Added</a></h3>
<ul>
<li>Added missing type constraints in FCS. (<a href="https://github.com/dotnet/fsharp/pull/18241">PR #18241</a>)</li>
<li>The 'use' keyword can be used on IDisposable|null without nullness warnings (<a href="https://github.com/dotnet/fsharp/pull/18262">PR #18262</a>)</li>
<li>Add support for C# <code>Experimental</code> attribute. (<a href="https://github.com/dotnet/fsharp/pull/18253">PR #18253</a>)</li>
<li>Nullness warnings are issued for signature&lt;&gt;implementation conformance (<a href="https://github.com/dotnet/fsharp/pull/18186">PR #18186</a>)</li>
<li>Symbols: Add FSharpAssembly.IsFSharp (<a href="https://github.com/dotnet/fsharp/pull/18290">PR #18290</a>)</li>
<li>Type checker: don't suppress errors while checking expressions (<a href="https://github.com/dotnet/fsharp/pull/18311">PR #18311</a>)</li>
<li>Type parameter constraint <code>null</code> in generic code will now automatically imply <code>not struct</code> (<a href="https://github.com/dotnet/fsharp/issues/18320">Issue #18320</a>, <a href="https://github.com/dotnet/fsharp/pull/18323">PR #18323</a>)</li>
<li>Add a switch to determine whether to generate a default implementation body for overridden method when completing. <a href="https://github.com/dotnet/fsharp/pull/18341">PR #18341</a></li>
<li>Use a more accurate range for CE Combine methods. <a href="https://github.com/dotnet/fsharp/pull/18394">PR #18394</a></li>
<li>Enable TypeSubsumptionCache for IDE use. <a href="https://github.com/dotnet/fsharp/pull/18499">PR #18499</a></li>
</ul>
<h3 data-fsdocs-heading="29"><a name="43.9.300-Changed" class="anchor" href="#43.9.300-Changed">Changed</a></h3>
<ul>
<li>FSharpCheckFileResults.ProjectContext.ProjectOptions will not be available when using the experimental Transparent Compiler feature. (<a href="https://github.com/dotnet/fsharp/pull/18205">PR #18205</a>)</li>
<li>Update <code>Obsolete</code> attribute checking to account for <code>DiagnosticId</code> and <code>UrlFormat</code> properties. (<a href="https://github.com/dotnet/fsharp/pull/18224">PR #18224</a>)</li>
<li>Remove <code>Cancellable.UsingToken</code> from tests (<a href="https://github.com/dotnet/fsharp/pull/18276">PR #18276</a>)</li>
<li>Added nullability annotations to <code>.Using</code> builder method for <code>async</code>, <code>task</code> and compiler-internal builders (<a href="https://github.com/dotnet/fsharp/pull/18292">PR #18292</a>)</li>
<li>Warn when <code>unit</code> is passed to an <code>obj</code>-typed argument  (<a href="https://github.com/dotnet/fsharp/pull/18330">PR #18330</a>)</li>
<li>Warning for &quot;useless null handling&quot; works with piped syntax constructs now (<a href="https://github.com/dotnet/fsharp/pull/18331">PR #18331</a>)</li>
<li>Make indent in generated overridden member code depend on the context, not fix to 4. (<a href="https://github.com/dotnet/fsharp/pull/18341">PR #18341</a>)</li>
<li>Adjust caller info attribute error message range (<a href="https://github.com/dotnet/fsharp/pull/18388">PR #18388</a>)</li>
<li>Make attribute targets mismatch a warning and not an error (<a href="https://github.com/dotnet/fsharp/pull/18492">PR #18492</a>)</li>
</ul>
<h3 data-fsdocs-heading="30"><a name="43.9.300-Breaking Changes" class="anchor" href="#43.9.300-Breaking Changes">Breaking Changes</a></h3>
<ul>
<li>Struct unions with overlapping fields now generate mappings needed for reading via reflection (<a href="https://github.com/dotnet/fsharp/issues/17797">Issue #18121</a>, <a href="https://github.com/dotnet/fsharp/pull/17877">PR #18274</a>)</li>
</ul>

<h2 data-fsdocs-heading="31"><a name="43.9.202" class="anchor" href="#43.9.202">43.9.202 - 2025-04-08</a></h2><a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.9.202" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.9.202-blue"></a><h3 data-fsdocs-heading="32"><a name="43.9.202-Fixed" class="anchor" href="#43.9.202-Fixed">Fixed</a></h3>
<ul>
<li>Fix missing TailCall warning in Sequential in use scope (<a href="https://github.com/dotnet/fsharp/pull/17927">PR #17927</a>)</li>
<li>Fix false negatives for passing null to &quot;obj&quot; arguments. Only &quot;obj | null&quot; can now subsume any type (<a href="https://github.com/dotnet/fsharp/pull/17757">PR #17757</a>)</li>
<li>Fix internal error when calling 'AddSingleton' and other overloads only differing in generic arity (<a href="https://github.com/dotnet/fsharp/pull/17804">PR #17804</a>)</li>
<li>Fix extension methods support for non-reference system assemblies (<a href="https://github.com/dotnet/fsharp/pull/17799">PR #17799</a>)</li>
<li>Ensure <code>frameworkTcImportsCache</code> mutations are threadsafe. (<a href="https://github.com/dotnet/fsharp/pull/17795">PR #17795</a>)</li>
<li>Disallow abstract member with access modifiers in sig file. (<a href="https://github.com/dotnet/fsharp/pull/17802">PR #17802</a>)</li>
<li>Fix concurrency issue in <code>ILPreTypeDefImpl</code>  (<a href="https://github.com/dotnet/fsharp/pull/17812">PR #17812</a>)</li>
<li>Fix nullness inference for member val and other OO scenarios  (<a href="https://github.com/dotnet/fsharp/pull/17845">PR #17845</a>)</li>
<li>Fix internal error when analyzing incomplete inherit member (<a href="https://github.com/dotnet/fsharp/pull/17905">PR #17905</a>)</li>
<li>Add warning when downcasting from nullable type to non-nullable (<a href="https://github.com/dotnet/fsharp/pull/17965">PR #17965</a>)</li>
<li>Fix missing nullness warning in case of method resolution multiple candidates (<a href="https://github.com/dotnet/fsharp/pull/17918">PR #17917</a>)</li>
<li>Fix failure to use bound values in <code>when</code> clauses of <code>try-with</code> in <code>seq</code> expressions (<a href="https://github.com/dotnet/fsharp/pull/17990">PR #17990</a>)</li>
<li>Fix locals allocating for the special <code>copyOfStruct</code> defensive copy (<a href="https://github.com/dotnet/fsharp/pull/18025">PR #18025</a>)</li>
<li>Fix lowering of computed array expressions when the expression consists of a simple mapping from a <code>uint64</code> or <code>unativeint</code> array. <a href="https://github.com/dotnet/fsharp/pull/18081">PR #18081</a></li>
<li>Add missing nullable-metadata for C# consumers of records,exceptions and DU subtypes generated from F# code. <a href="https://github.com/dotnet/fsharp/pull/18079">PR #18079</a></li>
<li>Reduce excess memory usage in TransparentCompiler. <a href="https://github.com/dotnet/fsharp/pull/17543">PR #17543</a></li>
<li>Fix a race condition in file book keeping in the compiler service (<a href="https://github.com/dotnet/fsharp/pull/18008">#18008</a>)</li>
<li>Fix trimming '%' characters when lowering interpolated string to a concat call <a href="https://github.com/dotnet/fsharp/pull/18123">PR #18123</a></li>
<li>Completion: fix qualified completion in sequence expressions <a href="https://github.com/dotnet/fsharp/pull/18111">PR #18111</a></li>
<li>Symbols: try to use ValReprInfoForDisplay in Mfv.CurriedParameterGroups (<a href="https://github.com/dotnet/fsharp/pull/18124">PR #18124</a>)</li>
<li>Shim/file system: fix leaks of the shim <a href="https://github.com/dotnet/fsharp/pull/18144">PR #18144</a></li>
</ul>
<h3 data-fsdocs-heading="33"><a name="43.9.202-Added" class="anchor" href="#43.9.202-Added">Added</a></h3>
<ul>
<li>Let <code>dotnet fsi --help</code> print a link to the documentation website. (<a href="https://github.com/dotnet/fsharp/pull/18006">PR #18006</a>)</li>
<li>Deprecate places where <code>seq</code> can be omitted. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1033">Language suggestion #1033</a>, <a href="https://github.com/dotnet/fsharp/pull/17772">PR #17772</a>)</li>
<li>Support literal attribute on decimals (<a href="https://github.com/dotnet/fsharp/pull/17769">PR #17769</a>)</li>
<li>Added type conversions cache, only enabled for compiler runs, guarded by language version preview (<a href="https://github.com/dotnet/fsharp/pull/17668">PR #17668</a>)</li>
<li>Added project property ParallelCompilation which turns on graph based type checking, parallel ILXGen and parallel optimization. By default on for users of langversion=preview (<a href="https://github.com/dotnet/fsharp/pull/17948">PR #17948</a>)</li>
<li>Adding warning when consuming generic method returning T|null for types not supporting nullness (structs,anons,tuples) (<a href="https://github.com/dotnet/fsharp/pull/18057">PR #18057</a>)</li>
<li>Sink: report SynPat.ArrayOrList type (<a href="https://github.com/dotnet/fsharp/pull/18127">PR #18127</a>)</li>
<li>Show the default value of compiler options (<a href="https://github.com/dotnet/fsharp/pull/18054">PR #18054</a>)</li>
<li>Support ValueOption + Struct attribute as optional parameter for methods (<a href="https://github.com/fsharp/fslang-suggestions/issues/1136">Language suggestion #1136</a>, <a href="https://github.com/dotnet/fsharp/pull/18098">PR #18098</a>)</li>
<li>Cancellable: add safer APIs to check the token (<a href="https://github.com/dotnet/fsharp/pull/18175">PR #18175</a>)</li>
</ul>
<h3 data-fsdocs-heading="34"><a name="43.9.202-Changed" class="anchor" href="#43.9.202-Changed">Changed</a></h3>
<ul>
<li>Make ILTypeDef interface impls calculation lazy. (<a href="https://github.com/dotnet/fsharp/pull/17392">PR #17392</a>)</li>
<li>Remove non-functional useSyntaxTreeCache option. (<a href="https://github.com/dotnet/fsharp/pull/17768">PR #17768</a>)</li>
<li>Better ranges for CE <code>let!</code> and <code>use!</code> error reporting. (<a href="https://github.com/dotnet/fsharp/pull/17712">PR #17712</a>)</li>
<li>Better ranges for CE <code>do!</code> error reporting. (<a href="https://github.com/dotnet/fsharp/pull/17779">PR #17779</a>)</li>
<li>Better ranges for CE <code>return, yield, return! and yield!</code> error reporting. (<a href="https://github.com/dotnet/fsharp/pull/17792">PR #17792</a>)</li>
<li>Better ranges for CE <code>match!</code>. (<a href="https://github.com/dotnet/fsharp/pull/17789">PR #17789</a>)</li>
<li>Better ranges for CE <code>use</code> error reporting. (<a href="https://github.com/dotnet/fsharp/pull/17811">PR #17811</a>)</li>
<li>Better ranges for <code>inherit</code> error reporting. (<a href="https://github.com/dotnet/fsharp/pull/17879">PR #17879</a>)</li>
<li>Better ranges for <code>inherit</code> <code>struct</code> error reporting. (<a href="https://github.com/dotnet/fsharp/pull/17886">PR #17886</a>)</li>
<li>Warn on uppercase identifiers in patterns. (<a href="https://github.com/dotnet/fsharp/pull/15816">PR #15816</a>)</li>
<li>Better ranges for <code>inherit</code> objects error reporting. (<a href="https://github.com/dotnet/fsharp/pull/17893">PR #17893</a>)</li>
<li>Better ranges for #nowarn error reporting; bring back #nowarn warnings for --langVersion:80; add warnings under feature flag (<a href="https://github.com/dotnet/fsharp/pull/17871">PR #17871</a>)</li>
<li>Better ranges for #nowarn error reporting; bring back #nowarn warnings for --langVersion:80; add warnings under feature flag (<a href="https://github.com/dotnet/fsharp/pull/17871">PR #17871</a>)</li>
<li>CheckAndThrow can be invoked only from within Cancellable context (<a href="https://github.com/dotnet/fsharp/pull/18037">PR #18037</a>)</li>
<li>Make ILTypeDef base type calculation lazy. (<a href="https://github.com/dotnet/fsharp/pull/18005">PR #18005</a>)</li>
<li>Revert EnforceAttributeTargets Feature. (<a href="https://github.com/dotnet/fsharp/pull/18005">PR #18005</a>)</li>
</ul>
<h3 data-fsdocs-heading="35"><a name="43.9.202-Breaking Changes" class="anchor" href="#43.9.202-Breaking Changes">Breaking Changes</a></h3>
<ul>
<li>Aliasing <code>StructAttribute</code> will now produce a warning (part of <a href="https://github.com/fsharp/fslang-suggestions/issues/18298">Language suggestion #18298</a>, <a href="https://github.com/dotnet/fsharp/pull/18355">PR #18355</a>)</li>
</ul>

<h2 data-fsdocs-heading="36"><a name="43.9.201" class="anchor" href="#43.9.201">43.9.201 - 2025-02-11</a></h2><a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.9.201" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.9.201-blue"></a><h3 data-fsdocs-heading="37"><a name="43.9.201-Fixed" class="anchor" href="#43.9.201-Fixed">Fixed</a></h3>
<ul>
<li>Fix Realsig+ generates nested closures with incorrect Generic  (<a href="https://github.com/dotnet/fsharp/issues/17797">Issue #17797</a>, <a href="https://github.com/dotnet/fsharp/pull/17877">PR #17877</a>)</li>
<li>Fix missing TailCall warning in Sequential in use scope (<a href="https://github.com/dotnet/fsharp/pull/17927">PR #17927</a>)</li>
<li>Fix false negatives for passing null to &quot;obj&quot; arguments. Only &quot;obj | null&quot; can now subsume any type (<a href="https://github.com/dotnet/fsharp/pull/17757">PR #17757</a>)</li>
<li>Fix internal error when calling 'AddSingleton' and other overloads only differing in generic arity (<a href="https://github.com/dotnet/fsharp/pull/17804">PR #17804</a>)</li>
<li>Fix extension methods support for non-reference system assemblies (<a href="https://github.com/dotnet/fsharp/pull/17799">PR #17799</a>)</li>
<li>Ensure <code>frameworkTcImportsCache</code> mutations are threadsafe. (<a href="https://github.com/dotnet/fsharp/pull/17795">PR #17795</a>)</li>
<li>Disallow abstract member with access modifiers in sig file. (<a href="https://github.com/dotnet/fsharp/pull/17802">PR #17802</a>)</li>
<li>Fix concurrency issue in <code>ILPreTypeDefImpl</code>  (<a href="https://github.com/dotnet/fsharp/pull/17812">PR #17812</a>)</li>
<li>Fix nullness inference for member val and other OO scenarios  (<a href="https://github.com/dotnet/fsharp/pull/17845">PR #17845</a>)</li>
<li>Add warning when downcasting from nullable type to non-nullable (<a href="https://github.com/dotnet/fsharp/pull/17965">PR #17965</a>)</li>
<li>Fix missing nullness warning in case of method resolution multiple candidates (<a href="https://github.com/dotnet/fsharp/pull/17918">PR #17917</a>)</li>
<li>Fix failure to use bound values in <code>when</code> clauses of <code>try-with</code> in <code>seq</code> expressions (<a href="https://github.com/dotnet/fsharp/pull/17990">PR #17990</a>)</li>
<li>Fix locals allocating for the special <code>copyOfStruct</code> defensive copy (<a href="https://github.com/dotnet/fsharp/pull/18025">PR #18025</a>)</li>
<li>Fix lowering of computed array expressions when the expression consists of a simple mapping from a <code>uint64</code> or <code>unativeint</code> array. <a href="https://github.com/dotnet/fsharp/pull/18081">PR #18081</a></li>
<li>Add missing nullable-metadata for C# consumers of records,exceptions and DU subtypes generated from F# code. <a href="https://github.com/dotnet/fsharp/pull/18079">PR #18079</a></li>
<li>Reduce excess memory usage in TransparentCompiler. <a href="https://github.com/dotnet/fsharp/pull/17543">PR #17543</a></li>
<li>Fix a race condition in file book keeping in the compiler service (<a href="https://github.com/dotnet/fsharp/pull/18008">#18008</a>)</li>
<li>Fix trimming '%' characters when lowering interpolated string to a concat call <a href="https://github.com/dotnet/fsharp/pull/18123">PR #18123</a></li>
<li>Completion: fix qualified completion in sequence expressions <a href="https://github.com/dotnet/fsharp/pull/18111">PR #18111</a></li>
<li>Symbols: try to use ValReprInfoForDisplay in Mfv.CurriedParameterGroups (<a href="https://github.com/dotnet/fsharp/pull/18124">PR #18124</a>)</li>
<li>Shim/file system: fix leaks of the shim <a href="https://github.com/dotnet/fsharp/pull/18144">PR #18144</a></li>
<li>fsi: fix auto-loading of script file inside NuGet package (<a href="https://github.com/dotnet/fsharp/pull/18177">PR #18177</a>)</li>
<li>Fix for <code>Obsolete</code> attribute warning/error not taken into account when used with a unit of measure <a href="https://github.com/dotnet/fsharp/pull/18182">PR #18182</a></li>
</ul>
<h3 data-fsdocs-heading="38"><a name="43.9.201-Added" class="anchor" href="#43.9.201-Added">Added</a></h3>
<ul>
<li>Let <code>dotnet fsi --help</code> print a link to the documentation website. (<a href="https://github.com/dotnet/fsharp/pull/18006">PR #18006</a>)</li>
<li>Deprecate places where <code>seq</code> can be omitted. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1033">Language suggestion #1033</a>, <a href="https://github.com/dotnet/fsharp/pull/17772">PR #17772</a>)</li>
<li>Support literal attribute on decimals (<a href="https://github.com/dotnet/fsharp/pull/17769">PR #17769</a>)</li>
<li>Added type conversions cache, only enabled for compiler runs, guarded by language version preview (<a href="https://github.com/dotnet/fsharp/pull/17668">PR #17668</a>)</li>
<li>Added project property ParallelCompilation which turns on graph based type checking, parallel ILXGen and parallel optimization. By default on for users of langversion=preview (<a href="https://github.com/dotnet/fsharp/pull/17948">PR #17948</a>)</li>
<li>Adding warning when consuming generic method returning T|null for types not supporting nullness (structs,anons,tuples) (<a href="https://github.com/dotnet/fsharp/pull/18057">PR #18057</a>)</li>
<li>Sink: report SynPat.ArrayOrList type (<a href="https://github.com/dotnet/fsharp/pull/18127">PR #18127</a>)</li>
<li>Show the default value of compiler options (<a href="https://github.com/dotnet/fsharp/pull/18054">PR #18054</a>)</li>
<li>Support ValueOption + Struct attribute as optional parameter for methods (<a href="https://github.com/fsharp/fslang-suggestions/issues/1136">Language suggestion #1136</a>, <a href="https://github.com/dotnet/fsharp/pull/18098">PR #18098</a>)</li>
<li>Cancellable: add safer APIs to check the token (<a href="https://github.com/dotnet/fsharp/pull/18175">PR #18175</a>)</li>
</ul>
<h3 data-fsdocs-heading="39"><a name="43.9.201-Changed" class="anchor" href="#43.9.201-Changed">Changed</a></h3>
<ul>
<li>Make ILTypeDef interface impls calculation lazy. (<a href="https://github.com/dotnet/fsharp/pull/17392">PR #17392</a>)</li>
<li>Remove non-functional useSyntaxTreeCache option. (<a href="https://github.com/dotnet/fsharp/pull/17768">PR #17768</a>)</li>
<li>Better ranges for CE <code>let!</code> and <code>use!</code> error reporting. (<a href="https://github.com/dotnet/fsharp/pull/17712">PR #17712</a>)</li>
<li>Better ranges for CE <code>do!</code> error reporting. (<a href="https://github.com/dotnet/fsharp/pull/17779">PR #17779</a>)</li>
<li>Better ranges for CE <code>return, yield, return! and yield!</code> error reporting. (<a href="https://github.com/dotnet/fsharp/pull/17792">PR #17792</a>)</li>
<li>Better ranges for CE <code>match!</code>. (<a href="https://github.com/dotnet/fsharp/pull/17789">PR #17789</a>)</li>
<li>Better ranges for CE <code>use</code> error reporting. (<a href="https://github.com/dotnet/fsharp/pull/17811">PR #17811</a>)</li>
<li>Better ranges for <code>inherit</code> error reporting. (<a href="https://github.com/dotnet/fsharp/pull/17879">PR #17879</a>)</li>
<li>Better ranges for <code>inherit</code> <code>struct</code> error reporting. (<a href="https://github.com/dotnet/fsharp/pull/17886">PR #17886</a>)</li>
<li>Warn on uppercase identifiers in patterns. (<a href="https://github.com/dotnet/fsharp/pull/15816">PR #15816</a>)</li>
<li>Better ranges for <code>inherit</code> objects error reporting. (<a href="https://github.com/dotnet/fsharp/pull/17893">PR #17893</a>)</li>
<li>Better ranges for #nowarn error reporting; bring back #nowarn warnings for --langVersion:80; add warnings under feature flag (<a href="https://github.com/dotnet/fsharp/pull/17871">PR #17871</a>)</li>
<li>Better ranges for #nowarn error reporting; bring back #nowarn warnings for --langVersion:80; add warnings under feature flag (<a href="https://github.com/dotnet/fsharp/pull/17871">PR #17871</a>)</li>
<li>CheckAndThrow can be invoked only from within Cancellable context (<a href="https://github.com/dotnet/fsharp/pull/18037">PR #18037</a>)</li>
<li>Make ILTypeDef base type calculation lazy. (<a href="https://github.com/dotnet/fsharp/pull/18005">PR #18005</a>)</li>
<li>Removed redundant hash directives around nullness syntax (<a href="https://github.com/dotnet/fsharp/issues/18061">Issue #18601</a>, <a href="https://github.com/dotnet/fsharp/pull/18203">PR #18203</a>, <a href="https://github.com/dotnet/fsharp/pull/18207">PR #18207</a>)</li>
</ul>
<h3 data-fsdocs-heading="40"><a name="43.9.201-Breaking Changes" class="anchor" href="#43.9.201-Breaking Changes">Breaking Changes</a></h3>
<ul>
<li>Aliasing <code>StructAttribute</code> will now produce a warning (part of <a href="https://github.com/fsharp/fslang-suggestions/issues/1136">Language suggestion #1136</a>, <a href="https://github.com/dotnet/fsharp/pull/18098">PR #18098</a>)</li>
<li>The <code>baseType</code> field in <code>SynMemberDefn.Inherit</code> has now type <code>SynType option</code>, to fix internal error when analyzing incomplete inherit member (<a href="https://github.com/dotnet/fsharp/pull/17905">PR #17905</a>)</li>
</ul>

<h2 data-fsdocs-heading="41"><a name="43.9.100" class="anchor" href="#43.9.100">43.9.100 - 2024-11-12</a></h2><a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.9.100" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.9.100-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.9.101" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.9.101-blue"></a><h3 data-fsdocs-heading="42"><a name="43.9.100-Fixed" class="anchor" href="#43.9.100-Fixed">Fixed</a></h3>
<ul>
<li>Fix wrong TailCall warning (<a href="https://github.com/dotnet/fsharp/issues/17604">Issue #17604</a>, <a href="https://github.com/dotnet/fsharp/pull/17637">PR #17637</a>)</li>
<li>Compiler hangs when compiling inline recursive invocation (<a href="https://github.com/dotnet/fsharp/issues/17376">Issue #17376</a>, <a href="https://github.com/dotnet/fsharp/pull/17394">PR #17394</a>)</li>
<li>Fix reporting IsFromComputationExpression only for CE builder type constructors and let bindings. (<a href="https://github.com/dotnet/fsharp/pull/17375">PR #17375</a>)</li>
<li>Optimize simple mappings in comprehensions when the body of the mapping has <code>let</code>-bindings and/or sequential expressions before a single yield. (<a href="https://github.com/dotnet/fsharp/pull/17419">PR #17419</a>)</li>
<li>C# protected property can be assigned in F# inherit constructor call. (<a href="https://github.com/dotnet/fsharp/issues/13299">Issue #13299</a>, <a href="https://github.com/dotnet/fsharp/pull/17391">PR #17391</a>)</li>
<li>MethodAccessException on equality comparison of a record with private fields. (<a href="https://github.com/dotnet/fsharp/issues/17447">Issue #17447</a>, <a href="https://github.com/dotnet/fsharp/pull/17467">PR #17391</a>)</li>
<li>Fix <code>function</code> implicit conversion. (<a href="https://github.com/dotnet/fsharp/issues/7401">Issue #7401</a>, <a href="https://github.com/dotnet/fsharp/pull/17487">PR #17487</a>)</li>
<li>Compiler fails to recognise namespace in FQN with enabled GraphBasedChecking. (<a href="https://github.com/dotnet/fsharp/issues/17508">Issue #17508</a>, <a href="https://github.com/dotnet/fsharp/pull/17510">PR #17510</a>)</li>
<li>Fix missing message for type error (FS0001). (<a href="https://github.com/dotnet/fsharp/issues/17373">Issue #17373</a>, <a href="https://github.com/dotnet/fsharp/pull/17516">PR #17516</a>)</li>
<li>Nullness export - make sure option&lt;&gt; and other UseNullAsTrueValue types are properly annotated as nullable for C# and reflection consumers <a href="https://github.com/dotnet/fsharp/pull/17528">PR #17528</a></li>
<li>MethodAccessException on equality comparison of a type private to module. (<a href="https://github.com/dotnet/fsharp/issues/17541">Issue #17541</a>, <a href="https://github.com/dotnet/fsharp/pull/17548">PR #17548</a>)</li>
<li>Fixed checking failure when <code>global</code> namespace is involved with enabled GraphBasedChecking (<a href="https://github.com/dotnet/fsharp/pull/17553">PR #17553</a>)</li>
<li>Add missing byte chars notations, enforce limits in decimal notation in byte char &amp; string (Issues <a href="https://github.com/dotnet/fsharp/issues/15867">#15867</a>, <a href="https://github.com/dotnet/fsharp/issues/15868">#15868</a>, <a href="https://github.com/dotnet/fsharp/issues/15869">#15869</a>, <a href="https://github.com/dotnet/fsharp/pull/15898">PR #15898</a>)</li>
<li>Parentheses analysis: keep extra parentheses around unit &amp; tuples in method definitions. (<a href="https://github.com/dotnet/fsharp/pull/17618">PR #17618</a>)</li>
<li>Fix IsUnionCaseTester throwing for non-methods/properties <a href="https://github.com/dotnet/fsharp/pull/17634">#17301</a></li>
<li>Fix xmlc doc tooltip display for nullable types <a href="https://github.com/dotnet/fsharp/pull/17741">#17741</a></li>
<li>Consider <code>open type</code> used when the type is an enum and any of the enum cases is used unqualified. (<a href="https://github.com/dotnet/fsharp/pull/17628">PR #17628</a>)</li>
<li>Guard for possible StackOverflowException when typechecking non recursive modules and namespaces (<a href="https://github.com/dotnet/fsharp/pull/17654">PR #17654</a>)</li>
<li>Nullable - fix for processing System.Nullable types with nesting (<a href="https://github.com/dotnet/fsharp/pull/17736">PR #17736</a>)</li>
<li>Fixes for the optimization of simple mappings in array and list comprehensions. (<a href="https://github.com/dotnet/fsharp/issues/17708">Issue #17708</a>, <a href="https://github.com/dotnet/fsharp/pull/17711">PR #17711</a>)</li>
</ul>
<h3 data-fsdocs-heading="43"><a name="43.9.100-Added" class="anchor" href="#43.9.100-Added">Added</a></h3>
<ul>
<li>Support for nullable reference types (<a href="https://github.com/dotnet/fsharp/pull/15181">PR #15181</a>)</li>
<li>Treat .ToString() on F# types as returning non-nullable string in --checknulls+ context (<a href="https://github.com/dotnet/fsharp/pull/17547">PR #17547</a>)</li>
<li>Parser: recover on missing union case fields (PR <a href="https://github.com/dotnet/fsharp/pull/17452">#17452</a>)</li>
<li>Parser: recover on missing union case field types (PR <a href="https://github.com/dotnet/fsharp/pull/17455">#17455</a>)</li>
<li>Sink: report function domain type (<a href="https://github.com/dotnet/fsharp/pull/17470">PR #17470</a>)</li>
<li>Allow access modifies to auto properties getters and setters (<a href="https://github.com/fsharp/fslang-suggestions/issues/430">Language suggestion #430</a>, <a href="https://github.com/dotnet/fsharp/pull/16687">PR 16687</a>, <a href="https://github.com/dotnet/fsharp/pull/16861">PR 16861</a>, <a href="https://github.com/dotnet/fsharp/pull/17522">PR 17522</a>)</li>
<li>Render C# nullable-analysis attributes in tooltips (<a href="https://github.com/dotnet/fsharp/pull/17485">PR #17485</a>)</li>
<li>Allow object expression without overrides. (<a href="https://github.com/fsharp/fslang-suggestions/issues/632">Language suggestion #632</a>, <a href="https://github.com/dotnet/fsharp/pull/17387">PR #17387</a>)</li>
<li>Enable FSharp 9.0 Language Version (<a href="https://github.com/dotnet/fsharp/issues/17497">Issue #17497</a>), <a href="https://github.com/dotnet/fsharp/pull/17500">PR</a>))</li>
<li>Enable LanguageFeature.EnforceAttributeTargets in F# 9.0. (<a href="https://github.com/dotnet/fsharp/issues/17558">Issue #17514</a>, <a href="https://github.com/dotnet/fsharp/pull/17558">PR #17516</a>)</li>
<li>Parser: better recovery for unfinished patterns (<a href="https://github.com/dotnet/fsharp/pull/17231">PR #17231</a>, <a href="https://github.com/dotnet/fsharp/pull/17232">PR #17232</a>))</li>
<li>Enable consuming generic arguments defined as <code>allows ref struct</code> in C# (<a href="https://github.com/dotnet/fsharp/issues/17597">Issue #17597</a>, display them in tooltips <a href="https://github.com/dotnet/fsharp/pull/17706">PR #17706</a>)</li>
<li>Trivia for SynTypeConstraint.WhereTyparNotSupportsNull. (<a href="https://github.com/dotnet/fsharp/issues/17721">Issue #17721</a>, <a href="https://github.com/dotnet/fsharp/pull/17745">PR #17745</a>)</li>
<li>Trivia for SynType.WithNull. (<a href="https://github.com/dotnet/fsharp/issues/17720">Issue #17720</a>, <a href="https://github.com/dotnet/fsharp/pull/17745">PR #17745</a>)</li>
</ul>
<h3 data-fsdocs-heading="44"><a name="43.9.100-Changed" class="anchor" href="#43.9.100-Changed">Changed</a></h3>
<ul>
<li>Change compiler default setting realsig+ when building assemblies (<a href="https://github.com/dotnet/fsharp/issues/17384">Issue #17384</a>, <a href="https://github.com/dotnet/fsharp/pull/17385">PR #17378</a>)</li>
<li>Change compiler default setting for compressedMetadata (<a href="https://github.com/dotnet/fsharp/issues/17379">Issue #17379</a>, <a href="https://github.com/dotnet/fsharp/pull/17383">PR #17383</a>)</li>
<li>Treat <code>{ new Foo() }</code> as <code>SynExpr.ObjExpr</code> (<a href="https://github.com/dotnet/fsharp/pull/17388">PR #17388</a>)</li>
<li>Optimize metadata reading for type members and custom attributes. (<a href="https://github.com/dotnet/fsharp/pull/17364">PR #17364</a>)</li>
<li>Enforce <code>AttributeTargets</code> on unions. (<a href="https://github.com/dotnet/fsharp/pull/17389">PR #17389</a>)</li>
<li>Applied nullable reference types to FSharp.Compiler.Service itself (<a href="https://github.com/dotnet/fsharp/pull/15310">PR #15310</a>)</li>
<li>Ensure that isinteractive multi-emit backing fields are not public. (<a href="https://github.com/dotnet/fsharp/issues/17438">Issue #17439</a>), (<a href="https://github.com/dotnet/fsharp/pull/17439">PR #17439</a>)</li>
<li>Better error reporting for unions with duplicated fields. (<a href="https://github.com/dotnet/fsharp/pull/17521">PR #17521</a>)</li>
<li>Better CE error reporting when using <code>use!</code> with <code>and!</code> (<a href="https://github.com/dotnet/fsharp/pull/17671">PR #17671</a>)</li>
<li>Better error reporting for let bindings. (<a href="https://github.com/dotnet/fsharp/pull/17601">PR #17601</a>)</li>
<li>Optimize ILTypeDef interface impls reading from metadata. (<a href="https://github.com/dotnet/fsharp/pull/17382">PR #17382</a>)</li>
<li>Make ILTypeDef interface impls calculation lazy. (<a href="https://github.com/dotnet/fsharp/pull/17392">PR #17392</a>)</li>
<li>Better error reporting for active patterns. (<a href="https://github.com/dotnet/fsharp/pull/17666">PR #17666</a>)</li>
<li>Multiple fsi sessions use separate temporary directories (<a href="https://github.com/dotnet/fsharp/pull/17760">PR #17760</a>)</li>
</ul>
<h3 data-fsdocs-heading="45"><a name="43.9.100-Breaking Changes" class="anchor" href="#43.9.100-Breaking Changes">Breaking Changes</a></h3>

<h2 data-fsdocs-heading="46"><a name="43.8.400" class="anchor" href="#43.8.400">43.8.400 - 2024-08-13</a></h2><a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.8.400" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.8.400-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.8.401" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.8.401-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.8.403" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.8.403-blue"></a><h3 data-fsdocs-heading="47"><a name="43.8.400-Fixed" class="anchor" href="#43.8.400-Fixed">Fixed</a></h3>
<ul>
<li>Enforce <code>AttributeTargets</code> on records. (<a href="https://github.com/dotnet/fsharp/pull/17207">PR #17207</a>)</li>
<li>Fix a false positive of the <code>[&lt;TailCall&gt;]</code> analysis in combination with async. (<a href="https://github.com/dotnet/fsharp/issues/17237">Issue #17237</a>, <a href="https://github.com/dotnet/fsharp/pull/17241">PR #17241</a>)</li>
<li>Extended #help directive in fsi to show documentation in the REPL. (<a href="https://github.com/dotnet/fsharp/pull/17140">PR #17140</a>)</li>
<li>Fix internal error when dotting into delegates with multiple type parameters. (<a href="https://github.com/dotnet/fsharp/pull/17227">PR #17227</a>)</li>
<li>Error for partial implementation of interface with static and non-static abstract members. (<a href="https://github.com/dotnet/fsharp/issues/17138">Issue #17138</a>, <a href="https://github.com/dotnet/fsharp/pull/17160">PR #17160</a>)</li>
<li>Optimize simple mappings with preludes in computed collections. (<a href="https://github.com/dotnet/fsharp/pull/17067">PR #17067</a>)</li>
<li>Improve error reporting for abstract members when used in classes. (<a href="https://github.com/dotnet/fsharp/pull/17063">PR #17063</a>)</li>
<li>Improve error reporting when property has same name as DU case. (<a href="https://github.com/dotnet/fsharp/issues/16646">Issue #16646</a>, <a href="https://github.com/dotnet/fsharp/pull/17088">PR #17088</a>)</li>
<li>Make typechecking of indexed setters with tuples on the right more consistent. (<a href="https://github.com/dotnet/fsharp/issues/16987">Issue #16987</a>, <a href="https://github.com/dotnet/fsharp/pull/17017">PR #17017</a>)</li>
<li>Static abstract method on classes no longer yields internal error. (<a href="https://github.com/dotnet/fsharp/issues/17044">Issue #17044</a>, <a href="https://github.com/dotnet/fsharp/pull/17055">PR #17055</a>)</li>
<li>Disallow calling abstract methods directly on interfaces. (<a href="https://github.com/dotnet/fsharp/issues/14012">Issue #14012</a>, <a href="https://github.com/dotnet/fsharp/issues/16299">Issue #16299</a>, <a href="https://github.com/dotnet/fsharp/pull/17021">PR #17021</a>)</li>
<li>Various parenthesization API fixes. (<a href="https://github.com/dotnet/fsharp/pull/16977">PR #16977</a>)</li>
<li>Files passed with -embed:relative/path/to/file are not embedded. (<a href="https://github.com/dotnet/fsharp/pull/17068">Issue #16768</a>)</li>
<li>Fix bug in optimization of for-loops over integral ranges with steps and units of measure. (<a href="https://github.com/dotnet/fsharp/issues/17025">Issue #17025</a>, <a href="https://github.com/dotnet/fsharp/pull/17040">PR #17040</a>, <a href="https://github.com/dotnet/fsharp/pull/17048">PR #17048</a>)</li>
<li>Fix calling an overridden virtual static method via the interface (<a href="https://github.com/dotnet/fsharp/pull/17013">PR #17013</a>)</li>
<li>Fix state machines compilation, when big decision trees are involved, by removing code split when resumable code is detected (<a href="https://github.com/dotnet/fsharp/pull/17076">PR #17076</a>)</li>
<li>Fix for exponential runtime in CE builders when using nested implicit yields <a href="https://github.com/dotnet/fsharp/pull/17096">PR #17096</a></li>
<li>Fix several AND operator parser bugs and regressions (<a href="https://github.com/dotnet/fsharp/issues/16447">Issue #16447</a>, <a href="https://github.com/dotnet/fsharp/issues/17134">Issue #17134</a>, <a href="https://github.com/dotnet/fsharp/issues/16309">Issue #16309</a>, <a href="https://github.com/dotnet/fsharp/pull/17113">PR #17113</a>)</li>
<li>Treat exceptions as types in a namespace for graph based type checking (<a href="https://github.com/dotnet/fsharp/issues/17262">Issue #17262</a>, <a href="https://github.com/dotnet/fsharp/pull/17268">PR #17268</a>)</li>
<li>FS0243 - Unrecognized option: '--realsig-' #17561 (<a href="https://github.com/dotnet/fsharp/issues/17561">Issue #17561</a>, <a href="https://github.com/dotnet/fsharp/pull/17562">PR #17268</a>)</li>
</ul>
<h3 data-fsdocs-heading="48"><a name="43.8.400-Added" class="anchor" href="#43.8.400-Added">Added</a></h3>
<ul>
<li>Generate new <code>Equals</code> overload to avoid boxing for structural comparison (<a href="https://github.com/dotnet/fsharp/pull/16857">PR #16857</a>)</li>
<li>Allow #nowarn to support the FS prefix on error codes to disable warnings (<a href="https://github.com/dotnet/fsharp/issues/16447">Issue #17206</a>, <a href="https://github.com/dotnet/fsharp/pull/17209">PR #17209</a>)</li>
<li>Allow ParsedHashDirectives to have argument types other than strings (<a href="https://github.com/dotnet/fsharp/issues/16447">Issue #17240</a>, <a href="https://github.com/dotnet/fsharp/pull/17209">PR #17209</a>)</li>
<li>Parser: better recovery for unfinished patterns (<a href="https://github.com/dotnet/fsharp/pull/17231">PR #17231</a>)</li>
<li>Expose inner exception information of TypeProviders to help diagnostics in IDE (<a href="https://github.com/dotnet/fsharp/pull/17251">PR #17251</a>)</li>
<li>Parser: recover on empty match clause (<a href="https://github.com/dotnet/fsharp/pull/17233">PR #17233</a>)</li>
<li>Support empty-bodied computation expressions. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1232">Language suggestion #1232</a>, <a href="https://github.com/fsharp/fslang-design/pull/774">RFC FS-1144 (PR #774)</a>, <a href="https://github.com/dotnet/fsharp/pull/17352">PR #17352</a>)</li>
</ul>
<h3 data-fsdocs-heading="49"><a name="43.8.400-Changed" class="anchor" href="#43.8.400-Changed">Changed</a></h3>
<ul>
<li>Enforce <code>AttributeTargets.Interface</code> (<a href="https://github.com/dotnet/fsharp/pull/17173">PR #17173</a>)</li>
<li>Minor compiler perf improvements. (<a href="https://github.com/dotnet/fsharp/pull/17130">PR #17130</a>)</li>
<li>Improve error messages for active pattern argument count mismatch (<a href="https://github.com/dotnet/fsharp/pull/16846">PR #16846</a>, <a href="https://github.com/dotnet/fsharp/pull/17186">PR #17186</a>)</li>
<li>AsyncLocal diagnostics context. (<a href="https://github.com/dotnet/fsharp/pull/16779">PR #16779</a>)</li>
<li>Reduce allocations in compiler checking via <code>ValueOption</code> usage (<a href="https://github.com/dotnet/fsharp/pull/16822">PR #16822</a>)</li>
<li>Use AsyncLocal instead of ThreadStatic to hold Cancellable.Token (<a href="https://github.com/dotnet/fsharp/pull/17156">PR #17156</a>)</li>
<li>Showing and inserting correct name of entities from unopened namespace/module (<a href="https://github.com/dotnet/fsharp/issues/14375">Issue #14375</a>, <a href="https://github.com/dotnet/fsharp/pull/17261">PR #17261</a>)</li>
<li>Improve completion after method/property override (<a href="https://github.com/dotnet/fsharp/pull/17292">PR #17292</a>)</li>
<li>Support lazy custom attributes calculation for <code>ILTypeDef</code> public API, improve <code>ExtensionAttribute</code> presence detecting perf. (<a href="https://github.com/dotnet/fsharp/pull/16168">PR #16168</a>)</li>
</ul>

<h2 data-fsdocs-heading="50"><a name="43.8.300" class="anchor" href="#43.8.300">43.8.300 - 2024-05-14</a></h2><a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.8.300" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.8.300-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.8.301" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.8.301-blue"></a><h3 data-fsdocs-heading="51"><a name="43.8.300-Fixed" class="anchor" href="#43.8.300-Fixed">Fixed</a></h3>
<ul>
<li>Fix a false positive of the <code>[&lt;TailCall&gt;]</code> analysis in combination with <code>yield!</code>. (<a href="https://github.com/dotnet/fsharp/pull/16933">PR #16933</a>)</li>
<li>Improve error reporting: ambiguous override method in object expression. (<a href="https://github.com/dotnet/fsharp/pull/16985">PR #16985</a>)</li>
<li>Don't blow the stack when traversing deeply nested sequential expressions. (<a href="https://github.com/dotnet/fsharp/pull/16882">PR #16882</a>)</li>
<li>Fix wrong range start of INTERP_STRING_END. (<a href="https://github.com/dotnet/fsharp/pull/16774">PR #16774</a>, <a href="https://github.com/dotnet/fsharp/pull/16785">PR #16785</a>)</li>
<li>Fix missing warning for recursive calls in list comprehensions. (<a href="https://github.com/dotnet/fsharp/pull/16652">PR #16652</a>)</li>
<li>Code generated files with &gt; 64K methods and generated symbols crash when loaded. Use inferred sequence points for debugging. (<a href="https://github.com/dotnet/fsharp/issues/16399">Issue #16399</a>, <a href="https://github.com/dotnet/fsharp/pull/16514">#PR 16514</a>)</li>
<li><code>nameof Module</code> expressions and patterns are processed to link files in <code>--test:GraphBasedChecking</code>. (<a href="https://github.com/dotnet/fsharp/pull/16550">PR #16550</a>, <a href="https://github.com/dotnet/fsharp/pull/16743">PR #16743</a>)</li>
<li>Graph Based Checking doesn't throw on invalid parsed input so it can be used for IDE scenarios (<a href="https://github.com/dotnet/fsharp/pull/16575">PR #16575</a>, <a href="https://github.com/dotnet/fsharp/pull/16588">PR #16588</a>, <a href="https://github.com/dotnet/fsharp/pull/16643">PR #16643</a>)</li>
<li>Various parenthesization API fixes. (<a href="https://github.com/dotnet/fsharp/pull/16578">PR #16578</a>, <a href="https://github.com/dotnet/fsharp/pull/16666">PR #16666</a>, <a href="https://github.com/dotnet/fsharp/pull/16901">PR #16901</a>, <a href="https://github.com/dotnet/fsharp/pull/16973">PR #16973</a>, <a href="https://github.com/dotnet/fsharp/pull/17012">PR #17012</a>)</li>
<li>Keep parens for problematic exprs (<code>if</code>, <code>match</code>, etc.) in <code>$&quot;{(…):N0}&quot;</code>, <code>$&quot;{(…),-3}&quot;</code>, etc. (<a href="https://github.com/dotnet/fsharp/pull/16578">PR #16578</a>)</li>
<li>Fix crash in DOTNET_SYSTEM_GLOBALIZATION_INVARIANT mode <a href="https://github.com/dotnet/fsharp/pull/16471">#PR 16471</a>)</li>
<li>Fix16572 - Fixed the preview feature enabling Is properties for union case did not work correctly with let .rec and .fsi files (<a href="https://github.com/dotnet/fsharp/pull/16657">PR #16657</a>)</li>
<li><code>[&lt;CliEvent&gt;]</code> member should not produce property symbol. (<a href="https://github.com/dotnet/fsharp/issues/16640">Issue #16640</a>, <a href="https://github.com/dotnet/fsharp/pull/16658">PR #16658</a>)</li>
<li>Fix discriminated union initialization. (<a href="https://github.com/dotnet/fsharp/pull/16661">#PR 16661</a>)</li>
<li>Allow calling method with both Optional and ParamArray. (<a href="https://github.com/dotnet/fsharp/pull/16688">#PR 16688</a>, <a href="https://github.com/fsharp/fslang-suggestions/issues/1120">suggestions #1120</a>)</li>
<li>Return diagnostics that got suppressed by errors in previous files. (<a href="https://github.com/dotnet/fsharp/pull/16719">PR #16719</a>)</li>
<li>Fix release inline optimization, which leads to MethodAccessException if used with `assembly:InternalsVisibleTo`` attribute. (<a href="https://github.com/dotnet/fsharp/issues/16105">Issue #16105</a>, (<a href="https://github.com/dotnet/fsharp/pull/16737">PR #16737</a>)</li>
<li>Enforce AttributeTargets on let values and functions. (<a href="https://github.com/dotnet/fsharp/pull/16692">PR #16692</a>)</li>
<li>Enforce AttributeTargets on union case declarations. (<a href="https://github.com/dotnet/fsharp/pull/16764">PR #16764</a>)</li>
<li>Disallow using base to invoke an abstract base method. (<a href="https://github.com/dotnet/fsharp/issues/13926">Issue #13926</a>, <a href="https://github.com/dotnet/fsharp/pull/16773">PR #16773</a>)</li>
<li>Parser: more unfinished member recovery (<a href="https://github.com/dotnet/fsharp/pull/16835">PR #16835</a>)</li>
<li>Enforce AttributeTargets on implicit constructors. (<a href="https://github.com/dotnet/fsharp/pull/16845/">PR #16845</a>)</li>
<li>Enforce AttributeTargets on structs and classes (<a href="https://github.com/dotnet/fsharp/pull/16790">PR #16790</a>)</li>
<li>Parser: fix pattern range for idents with trivia (<a href="https://github.com/dotnet/fsharp/pull/16824">PR #16824</a>)</li>
<li>Fix broken code completion after a record type declaration (<a href="https://github.com/dotnet/fsharp/pull/16813">PR #16813</a>)</li>
<li>Enforce AttributeTargets on enums (<a href="https://github.com/dotnet/fsharp/pull/16887">PR #16887</a>)</li>
<li>Completion: fix for unfinished record field decl (<a href="https://github.com/dotnet/fsharp/pull/16893">PR #16893</a>)</li>
<li>Enforce AttributeTargets on delegates (<a href="https://github.com/dotnet/fsharp/pull/16891">PR #16891</a>)</li>
<li>Obsolete attribute is ignored in constructor property assignment (<a href="https://github.com/dotnet/fsharp/pull/16900">PR #16900</a>)</li>
<li>Completion: fix completion in empty dot lambda prefix (<a href="https://github.com/dotnet/fsharp/pull/16829">#16829</a>)</li>
<li>Fix StackOverflow when checking non-recursive bindings in module or namespace in <code>fscAnyCpu</code>/<code>fsiAnyCpu</code>. (<a href="https://github.com/dotnet/fsharp/pull/16908">PR #16908</a>)</li>
<li>Removes signature file adjacency check in Transparent Compiler <a href="https://github.com/dotnet/fsharp/issues/17082">Issue #17082</a> <a href="https://github.com/dotnet/fsharp/pull/17085">PR #17085</a></li>
</ul>
<h3 data-fsdocs-heading="52"><a name="43.8.300-Added" class="anchor" href="#43.8.300-Added">Added</a></h3>
<ul>
<li>The stackguard depth for ILPdbWriter.unshadowScopes can be modified via the environment variable <code>FSHARP_ILPdb_UnshadowScopes_StackGuardDepth</code>(<a href="https://github.com/dotnet/fsharp/pull/16583">PR #16583</a>)</li>
<li>Parser recovers on complex primary constructor patterns, better tree representation for primary constructor patterns. (<a href="https://github.com/dotnet/fsharp/pull/16425">PR #16425</a>)</li>
<li>Name resolution: keep type vars in subsequent checks (<a href="https://github.com/dotnet/fsharp/pull/16456">PR #16456</a>)</li>
<li>Higher-order-function-based API for working with the untyped abstract syntax tree. (<a href="https://github.com/dotnet/fsharp/pull/16462">PR #16462</a>)</li>
<li>PrettyNaming: make <code>DoesIdentifierNeedBackticks</code> public (<a href="https://github.com/dotnet/fsharp/pull/16613">PR #16613</a>)</li>
<li>Add switch to generate types and members with  IL visibility that accurately represents their F# visibility. (<a href="https://github.com/dotnet/fsharp/pull/15484">PR #15484</a></li>
<li>Allow returning bool instead of unit option for partial active patterns. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1041">Language suggestion #1041</a>, <a href="https://github.com/dotnet/fsharp/pull/16473">PR #16473</a>)</li>
<li>Symbols: Add GenericArguments to FSharpEntity (<a href="https://github.com/dotnet/fsharp/pull/16470">PR #16470</a>)</li>
<li>Parser: more 'as' pattern recovery (<a href="https://github.com/dotnet/fsharp/pull/16837">PR #16837</a>)</li>
<li>Add extended data for <code>DefinitionsInSigAndImplNotCompatibleAbbreviationsDiffer</code> (FS0318). (<a href="https://github.com/dotnet/fsharp/pull/16811">PR #16811</a>))</li>
<li>Checker/patterns: recover on unresolved long identifiers (<a href="https://github.com/dotnet/fsharp/pull/16842">PR #16842</a>)</li>
<li>SynExprSequentialTrivia (<a href="https://github.com/dotnet/fsharp/issues/16914">Issue #16914</a>, <a href="https://github.com/dotnet/fsharp/pull/16981">PR #16981</a>)</li>
</ul>
<h3 data-fsdocs-heading="53"><a name="43.8.300-Changed" class="anchor" href="#43.8.300-Changed">Changed</a></h3>
<ul>
<li>Autogenerated .Is* members for unions skipped for single-case unions. (<a href="https://github.com/dotnet/fsharp/pull/16571">PR 16571</a>)</li>
<li><code>implicitCtorSynPats</code> in <code>SynTypeDefnSimpleRepr.General</code> is now <code>SynPat option</code> instead of <code>SynSimplePats option</code>. (<a href="https://github.com/dotnet/fsharp/pull/16425">PR #16425</a>)</li>
<li><code>SyntaxVisitorBase&lt;'T&gt;.VisitSimplePats</code> now takes <code>SynPat</code> instead of <code>SynSimplePat list</code>. (<a href="https://github.com/dotnet/fsharp/pull/16425">PR #16425</a>)</li>
<li>Reduce allocations in compiler checking via <code>ValueOption</code> usage (<a href="https://github.com/dotnet/fsharp/pull/16323">PR #16323</a>, <a href="https://github.com/dotnet/fsharp/pull/16567">PR #16567</a>)</li>
<li>Reverted <a href="https://github.com/dotnet/fsharp/pull/16348">#16348</a> <code>ThreadStatic</code> <code>CancellationToken</code> changes to improve test stability and prevent potential unwanted cancellations. (<a href="https://github.com/dotnet/fsharp/pull/16536">PR #16536</a>)</li>
<li>Refactored parenthesization API. ([PR #16461])(https://github.com/dotnet/fsharp/pull/16461))</li>
<li>Optimize some interpolated strings by lowering to string concatenation. (<a href="https://github.com/dotnet/fsharp/pull/16556">PR #16556</a>)</li>
<li>Speed up <code>for x in xs -&gt; …</code> in list &amp; array comprehensions in certain scenarios. (<a href="https://github.com/dotnet/fsharp/pull/16948">PR #16948</a>)</li>
<li>Integral range optimizations. (<a href="https://github.com/dotnet/fsharp/pull/16650">PR #16650</a>, <a href="https://github.com/dotnet/fsharp/pull/16832">PR #16832</a>, <a href="https://github.com/dotnet/fsharp/pull/16947">PR #16947</a>)</li>
</ul>

<h2 data-fsdocs-heading="54"><a name="8.0.202" class="anchor" href="#8.0.202">F# 8.0.202 - Not on NuGet</a></h2><h3 data-fsdocs-heading="55"><a name="8.0.202-Fixed" class="anchor" href="#8.0.202-Fixed">Fixed</a></h3>
<p><code>nameof Module</code> expressions and patterns are processed to link files in <code>--test:GraphBasedChecking</code>. (<a href="https://github.com/dotnet/fsharp/pull/16570">PR #16570</a>, <a href="https://github.com/dotnet/fsharp/pull/16747">PR #16747</a>)</p>

<h2 data-fsdocs-heading="56"><a name="43.8.200" class="anchor" href="#43.8.200">43.8.200 - 2024-02-13</a></h2><a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.8.200" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.8.200-blue"></a><h3 data-fsdocs-heading="57"><a name="43.8.200-Fixed" class="anchor" href="#43.8.200-Fixed">Fixed</a></h3>
<ul>
<li>Correctly handle assembly imports with public key token of 0 length. (<a href="https://github.com/dotnet/fsharp/issues/16359">Issue #16359</a>, <a href="https://github.com/dotnet/fsharp/pull/16363">PR #16363</a>)</li>
<li>Range of <a href="../reference/fsharp-compiler-syntax-synfield.html">SynField</a> (<a href="https://github.com/dotnet/fsharp/pull/16357">PR #16357</a>)</li>
<li>Limit a type to 65K methods, introduce a compile-time error if any class has over approx 64K methods in generated IL. (<a href="https://github.com/dotnet/fsharp/issues/16398">Issue #16398</a>, <a href="https://github.com/dotnet/fsharp/pull/16427">#PR 16427</a>)</li>
</ul>
<h3 data-fsdocs-heading="58"><a name="43.8.200-Added" class="anchor" href="#43.8.200-Added">Added</a></h3>
<ul>
<li>Raise a new error when interfaces with auto properties are implemented on constructor-less types. (<a href="https://github.com/dotnet/fsharp/pull/16352">PR #16352</a>)</li>
<li>Allow usage of <code>[&lt;TailCall&gt;]</code> with older <code>FSharp.Core</code> package versions. (<a href="https://github.com/dotnet/fsharp/pull/16373">PR #16373</a>)</li>
<li>Parser recovers on unfinished <code>as</code> patterns. (<a href="https://github.com/dotnet/fsharp/pull/16404">PR #16404</a>)</li>
<li>Allow type-checking of unfinished object expressions. (<a href="https://github.com/dotnet/fsharp/pull/16413">PR #16413</a>)</li>
<li>Parser recovers on unfinished enum case declarations. (<a href="https://github.com/dotnet/fsharp/pull/16401">PR #16401</a>)</li>
<li>Parser recovers on unfinished record declarations. (<a href="https://github.com/dotnet/fsharp/pull/16357">PR #16357</a>)</li>
<li><code>MutableKeyword</code> to <a href="../reference/fsharp-compiler-syntaxtrivia-synfieldtrivia.html">SynFieldTrivia</a> (<a href="https://github.com/dotnet/fsharp/pull/16357">PR #16357</a>)</li>
<li>Added support for a new parameterless constructor for <code>CustomOperationAttribute</code>, which, when applied, will use method name as keyword for custom operation in computation expression builder. (<a href="https://github.com/dotnet/fsharp/pull/16475">PR #16475</a>, part of implementation for <a href="https://github.com/fsharp/fslang-suggestions/issues/1250">fslang-suggestions/1250</a>)</li>
<li>Compiler service API for getting ranges of unnecessary parentheses. (<a href="https://github.com/dotnet/fsharp/pull/16079">PR #16079</a> et seq.)</li>
</ul>
<h3 data-fsdocs-heading="59"><a name="43.8.200-Changed" class="anchor" href="#43.8.200-Changed">Changed</a></h3>
<ul>
<li>Speed up unused opens handling for empty results. (<a href="https://github.com/dotnet/fsharp/pull/16502">PR #16502</a>)</li>
</ul>

<h2 data-fsdocs-heading="60"><a name="43.8.100" class="anchor" href="#43.8.100">43.8.100 - 2023-11-14</a></h2><a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.8.100" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.8.100-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.8.101" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.8.101-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Compiler.Service/43.8.102" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-43.8.102-blue"></a><h3 data-fsdocs-heading="61"><a name="43.8.100-Fixed" class="anchor" href="#43.8.100-Fixed">Fixed</a></h3>
<ul>
<li>Include the <code>get,set</code> keywords in the range of <code>SynMemberDefn.AutoProperty</code>. (<a href="https://github.com/dotnet/fsharp/pull/15835">PR #15835</a>)</li>
</ul>
