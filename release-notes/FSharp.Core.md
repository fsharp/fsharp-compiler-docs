---
category: Release Notes
categoryindex: 600
index: 3
title: FSharp.Core
---

# FSharp.Core

<h2 data-fsdocs-heading="1"><a name="11.0.200" class="anchor" href="#11.0.200">F# 11.0.200 - Not on NuGet</a></h2><h3 data-fsdocs-heading="2"><a name="11.0.200-Added" class="anchor" href="#11.0.200-Added">Added</a></h3>
<h3 data-fsdocs-heading="3"><a name="11.0.200-Fixed" class="anchor" href="#11.0.200-Fixed">Fixed</a></h3>
<h3 data-fsdocs-heading="4"><a name="11.0.200-Changed" class="anchor" href="#11.0.200-Changed">Changed</a></h3>

<h2 data-fsdocs-heading="5"><a name="11.0.100" class="anchor" href="#11.0.100">11.0.100 - Unlisted on NuGet</a></h2><a href="https://www.nuget.org/packages/FSharp.Core/11.0.100" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-11.0.100-blue"></a><h3 data-fsdocs-heading="6"><a name="11.0.100-Fixed" class="anchor" href="#11.0.100-Fixed">Fixed</a></h3>
<ul>
<li><p>Add <code>inline</code> and <code>[&lt;InlineIfLambda&gt;]</code> to allocation-free <code>List</code> and <code>Array</code> higher-order functions (<code>fold</code>, <code>fold2</code>, <code>foldBack</code>, <code>foldBack2</code>, <code>reduce</code>, <code>reduceBack</code>, <code>iter2</code>, <code>iteri</code>, <code>iteri2</code>, <code>find</code>, <code>findIndex</code>, <code>findBack</code>, <code>findIndexBack</code>, <code>pick</code>, <code>tryPick</code>, <code>exists</code>, <code>exists2</code>, <code>forall</code>, <code>forall2</code>, <code>skipWhile</code>), eliminating the per-call closure allocation when a capturing lambda is passed. Error paths are routed through hidden non-inline helpers so localized exception messages are preserved. (<a href="https://github.com/fsharp/fslang-design/blob/main/RFCs/FS-1115-InlineIfLambda-in-FSharp-Core.md">RFC FS-1115</a>, <a href="https://github.com/dotnet/fsharp/pull/20422">PR #20422</a>)</p>
</li>
<li><p>Mirror compiler-semantic attributes (e.g. <code>[&lt;NoDynamicInvocation&gt;]</code>, <code>[&lt;Experimental&gt;]</code>) in <code>.fsi</code> signature files to match <code>.fs</code> implementations. (<a href="https://github.com/dotnet/fsharp/issues/19560">Issue #19560</a>, <a href="https://github.com/dotnet/fsharp/pull/19880">PR #19880</a>)</p>
</li>
<li><p>Fix <code>Array2D</code> AOT compatibility: <code>create</code>, <code>init</code> and <code>rebase</code> no longer emit <code>IL3050</code>; <code>map</code>, <code>mapi</code> and <code>copy</code> no longer emit it on <code>net10.0</code>; the <code>*Based</code> operations now warn at the call site before the runtime failure they already had. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1454">Language Suggestion #1454</a>, <a href="https://github.com/dotnet/fsharp/pull/20338">PR #20338</a>)</p>
</li>
<li><p>Fix <code>Array.exists2</code> documentation examples to use equal-length arrays; the previous examples would throw <code>ArgumentException</code> at runtime instead of returning the documented <code>false</code>/<code>true</code> values. (<a href="https://github.com/dotnet/fsharp/pull/19672">PR #19672</a>)</p>
</li>
<li><p>Move <code>Async.StartChild</code> to the &quot;Starting Async Computations&quot; docs category alongside <code>Async.StartChildAsTask</code>. (<a href="https://github.com/dotnet/fsharp/issues/19667">Issue #19667</a>)</p>
</li>
<li><p>Add <code>InlineIfLambda</code> to <code>Array.init</code> (<a href="https://github.com/dotnet/fsharp/pull/19869">PR #19869</a>)</p>
</li>
<li><p>Fix array and string slices with extreme reversed bounds to return correctly shaped empty results. (<a href="https://github.com/dotnet/fsharp/issues/20530">Issue #20530</a>, <a href="https://github.com/dotnet/fsharp/pull/20557">PR #20557</a>)</p>
</li>
<li><p>Fix printf handling of -0.0 (negative zero) values for float, float32, and decimal values (<a href="https://github.com/dotnet/fsharp/issues/15557">Issue #15557</a> and <a href="https://github.com/dotnet/fsharp/issues/15558">Issue #15558</a>, <a href="https://github.com/dotnet/fsharp/pull/18147">PR #18147</a>)</p>
</li>
</ul>
<h3 data-fsdocs-heading="7"><a name="11.0.100-Added" class="anchor" href="#11.0.100-Added">Added</a></h3>
<ul>
<li>Add the compiler-recognized <code>StateMachineHelpers.__runtimeAsyncReturn</code> intrinsic to the <code>net10.0</code> FSharp.Core target for .NET runtime-async methods. (<a href="https://github.com/dotnet/fsharp/pull/20235">PR #20235</a>)</li>
<li>Add <code>Unchecked.withNull</code>, an interop escape hatch that re-types any <code>'T</code> to <code>'T | null</code> without the usual <code>not null</code>/<code>not struct</code> constraints, so unconstrained C# nullable-generic APIs (e.g. <code>T? M&lt;T&gt;()</code>) can be implemented and consumed from F#. (<a href="https://github.com/dotnet/fsharp/issues/17734">Issue #17734</a>, <a href="https://github.com/dotnet/fsharp/pull/20232">PR #20232</a>)</li>
<li>Added generic <code>print</code> and <code>printn</code> functions (<code>'T -&gt; unit</code>) to <code>ExtraTopLevelOperators</code> for simple value printing to stdout. (<a href="https://github.com/fsharp/fslang-design/blob/main/RFCs/FS-1125-print-printn-functions.md">RFC FS-1125</a>, <a href="https://github.com/dotnet/fsharp/pull/19265">PR #19265</a>)</li>
<li>Ship <code>FSharp.Core</code> with an additional <code>net10.0</code> target framework (next to <code>netstandard2.0</code> and <code>netstandard2.1</code>). The <code>net</code>-TFM assembly is public-surface-identical to the <code>netstandard2.1</code> one; the target version is a pinned, deliberately advanced knob. (<a href="https://github.com/dotnet/fsharp/pull/20229">PR #20229</a>)</li>
<li>Add <code>Async.Await</code>, mirroring <code>Async.AwaitTask</code> semantics, but elides egregious <code>AggregateException</code> wrapping. Includes <code>ValueTask</code> support, and a SRTP-based overload accepting any Task-like value that supports the <code>GetAwaiter</code> protocol. (<a href="https://github.com/fsharp/fslang-suggestions/issues/840">Language Suggestion #840</a>, <a href="https://github.com/dotnet/fsharp/pull/19785">PR #19785</a>)</li>
<li><code>Async.RunSynchronouslyImmediate</code>: runs work on the calling thread until the first asynchronous suspension (as opposed to <code>RunSynchronously</code>, which immediately offloads if not on a background and/or threadpool thread). (<a href="https://github.com/fsharp/fslang-suggestions/issues/1042">Issue #1042</a>, <a href="https://github.com/dotnet/fsharp/pull/19804">PR #19804</a>)</li>
<li>Added <code>AllowOverloadOnReturnTypeAttribute</code>: applying it to a method causes its return type to be considered during overload resolution, enabling overloads that differ only by return type. (<a href="https://github.com/fsharp/fslang-suggestions/issues/820">fslang-suggestions#820</a>, <a href="https://github.com/dotnet/fsharp/pull/19602">PR #19602</a>)</li>
<li>Add <code>Async.StartTaskImmediate</code>: passes the ambient <code>Async.CancellationToken</code> to a task factory, then await the result using <code>Async.Await</code> semantics. Overloads for <code>Task</code>, <code>Task&lt;'T&gt;</code>, <code>ValueTask</code>, <code>ValueTask&lt;'T&gt;</code> and task-like <code>.GetAwaiter()</code> (via SRTP). (<a href="https://github.com/fsharp/fslang-suggestions/issues/1284">Language Suggestion #1284</a>, <a href="https://github.com/dotnet/fsharp/pull/20258">PR #20258</a>)</li>
<li>Add modules for <code>Async</code>, <code>Task</code> and <code>ValueTask</code> with consistent <code>result</code>, <code>map</code>, <code>bind</code>, <code>ignore</code>, <code>catchWith</code>, <code>catch</code>, and <code>empty</code> functions (<a href="https://github.com/fsharp/fslang-suggestions/issues/1466">LanguageSuggestion #1466</a>, <a href="https://github.com/dotnet/fsharp/pull/19844">PR #19844</a>)</li>
<li>Add conversion functions <code>Task.ofValueTask</code> and <code>ValueTask.ofTask</code>. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1466">LanguageSuggestion #1466</a>, <a href="https://github.com/dotnet/fsharp/pull/19844">PR #19844</a>)</li>
<li>Add <code>parallelLimit</code> to <code>Task</code> and <code>Async</code> modules, for bounded-parallelism execution of async/task computations, flowing cancellation and returning the results as an array. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1467">LanguageSuggestion #1467</a>, <a href="https://github.com/dotnet/fsharp/pull/20294">PR #20294</a>)</li>
<li>Add <code>parallelDoLimit</code> to <code>Task</code> and <code>Async</code> modules, for bounded-parallelism execution of async/task computations, flowing cancellation. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1467">LanguageSuggestion #1467</a>, <a href="https://github.com/dotnet/fsharp/pull/20294">PR #20294</a>)</li>
<li>Prototype a compiler-services entry and reference host for direct runtime-async sequences on the <code>net10.0</code> target, reusing the first enumerator instance.</li>
<li>Add <code>sequentialDo</code> to <code>Task</code> and <code>Async</code>, running computations one at a time, flowing cancellation, without yielding an egregious <code>unit[]</code>. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1467">LanguageSuggestion #1467</a>, <a href="https://github.com/dotnet/fsharp/pull/20294">PR #20294</a>)</li>
<li>Add <code>Task.sequential</code>, running task computations one at a time, flowing cancellation and returning the results as an array. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1467">LanguageSuggestion #1467</a>, <a href="https://github.com/dotnet/fsharp/pull/20294">PR #20294</a>)</li>
<li>Add <code>Task.startAsyncImmediate</code>, starting an <code>Async&lt;'T&gt;</code> on the current thread and returning a <code>Task&lt;'T&gt;</code>, flowing cancellation. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1467">LanguageSuggestion #1467</a>, <a href="https://github.com/dotnet/fsharp/pull/20294">PR #20294</a>)</li>
</ul>
<h3 data-fsdocs-heading="8"><a name="11.0.100-Changed" class="anchor" href="#11.0.100-Changed">Changed</a></h3>
<ul>
<li>Most <code>Array</code> and <code>Array.Parallel</code> functions, and the <code>toArray</code> conversions (<code>List.toArray</code>, <code>Seq.toArray</code>, <code>Array.ofList</code>/<code>ofSeq</code>, <code>Set.toArray</code>, <code>Map.toArray</code>), now return the shared empty-array singleton instead of allocating a fresh zero-length array when the result is empty. Copies of covariant arrays retain their runtime array type. (<a href="https://github.com/dotnet/fsharp/issues/20382">Issue #20382</a>, <a href="https://github.com/dotnet/fsharp/pull/20388">PR #20388</a>)</li>
<li>Deduplicate repeated XML documentation (sort stability/complexity, <code>Parallel.For</code>, dynamic entry points) via the compile-time <code>&lt;include&gt;</code> tag; generated FSharp.Core.xml is unchanged. (<a href="https://github.com/dotnet/fsharp/pull/20231">PR #20231</a>)</li>
<li>Remove trailing whitespace from source files. No functional change: whitespace inside string literals and inactive <code>#if</code> regions is preserved. (<a href="https://github.com/dotnet/fsharp/pull/20355">PR #20355</a>)</li>
</ul>

<h2 data-fsdocs-heading="9"><a name="10.1.300" class="anchor" href="#10.1.300">10.1.300 - 2026-05-12</a></h2><a href="https://www.nuget.org/packages/FSharp.Core/10.1.300" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.1.300-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/10.1.301" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.1.301-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/10.1.302" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.1.302-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/10.1.303" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.1.303-blue"></a><h3 data-fsdocs-heading="10"><a name="10.1.300-Fixed" class="anchor" href="#10.1.300-Fixed">Fixed</a></h3>
<ul>
<li>Optimize Set.intersect performance symmetry and preserve identity from the first set argument. (<a href="https://github.com/dotnet/fsharp/pull/19291">PR #19291</a>) (Fixes #19139)</li>
<li>Fix anonymous record field ordering in LINQ expression conversion to produce consistent expression trees regardless of field declaration order. (<a href="https://github.com/dotnet/fsharp/issues/11131">Issue #11131</a>, <a href="https://github.com/dotnet/fsharp/issues/15648">Issue #15648</a>)</li>
<li>Fix array indexing in LINQ expressions to generate proper array index expressions instead of GetArray method calls, enabling LINQ providers like Azure Cosmos DB to translate array access. (<a href="https://github.com/dotnet/fsharp/issues/16918">Issue #16918</a>)</li>
<li>Fix tuple join conditions and groupBy operations to properly compare tuple keys using structural equality. AnonymousObject types now implement Equals and GetHashCode, enabling inline tuple joins like <code>join b on ((a.Id1, a.Id2) = (b.Id1, b.Id2))</code> to work correctly. (<a href="https://github.com/dotnet/fsharp/issues/7885">Issue #7885</a>, <a href="https://github.com/dotnet/fsharp/issues/47">Issue #47</a>)</li>
<li>Fix tuple/multi-value projections in queries to use Queryable.Select instead of Enumerable.Select when the source is IQueryable, preserving query composition and enabling async operations like ToListAsync() in Entity Framework Core. (<a href="https://github.com/dotnet/fsharp/issues/3782">Issue #3782</a>, <a href="https://github.com/dotnet/fsharp/issues/15133">Issue #15133</a>)</li>
<li>Fix EvaluateQuotation to handle Sequential expressions, void method calls (unit return), and other patterns that were previously throwing NotSupportedException. Also properly handles unit-returning expressions by using Action delegates instead of Func delegates. (<a href="https://github.com/dotnet/fsharp/issues/19099">Issue #19099</a>)</li>
<li>Fix query conditionals without else branch (if-then only) that were causing type mismatch errors. Now properly extracts element type from IQueryable for creating empty sequences. (<a href="https://github.com/dotnet/fsharp/issues/3445">Issue #3445</a>)</li>
<li>Fix <code>Seq.empty</code> rendering as <code>&quot;EmptyEnumerable&quot;</code> in serializers by delegating to <code>System.Linq.Enumerable.Empty&lt;'T&gt;()</code> instead of using a custom DU type. (<a href="https://github.com/dotnet/fsharp/issues/17864">Issue #17864</a>, <a href="https://github.com/dotnet/fsharp/pull/19317">PR #19317</a>)</li>
<li>Fix <code>seq { try/with }</code> handler body executing twice when source throws immediately and handler yields nothing. (<a href="https://github.com/dotnet/fsharp/issues/19660">Issue #19660</a>, <a href="https://github.com/dotnet/fsharp/pull/19661">PR #19661</a>)</li>
<li>Ensure culture-independent parsing of .NET-style interpolated string holes. (<a href="https://github.com/dotnet/fsharp/issues/19367">Issue #19367</a>, <a href="https://github.com/dotnet/fsharp/pull/19370">PR #19370</a>)</li>
</ul>
<h3 data-fsdocs-heading="11"><a name="10.1.300-Added" class="anchor" href="#10.1.300-Added">Added</a></h3>
<ul>
<li>Add <code>List.partitionWith</code>, <code>Array.partitionWith</code>, <code>Set.partitionWith</code>, and <code>Array.Parallel.partitionWith</code> functions that partition a collection using a function that returns <code>Choice&lt;'T1, 'T2&gt;</code>. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1119">Language Suggestion #1119</a>)</li>
</ul>
<h3 data-fsdocs-heading="12"><a name="10.1.300-Changed" class="anchor" href="#10.1.300-Changed">Changed</a></h3>
<ul>
<li>Added complexity documentation (Big-O notation) to all 462 functions across Array, List, Seq, Map, and Set collection modules. (<a href="https://github.com/dotnet/fsharp/pull/19240">PR #19240</a>)</li>
</ul>
<h3 data-fsdocs-heading="13"><a name="10.1.300-Breaking Changes" class="anchor" href="#10.1.300-Breaking Changes">Breaking Changes</a></h3>

<h2 data-fsdocs-heading="14"><a name="10.1.201" class="anchor" href="#10.1.201">10.1.201 - 2026-03-12</a></h2><a href="https://www.nuget.org/packages/FSharp.Core/10.1.201" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.1.201-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/10.1.202" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.1.202-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/10.1.203" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.1.203-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/10.1.204" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.1.204-blue"></a><h3 data-fsdocs-heading="15"><a name="10.1.201-Fixed" class="anchor" href="#10.1.201-Fixed">Fixed</a></h3>
<ul>
<li>Fix IL2091 trimming warning in <code>LazyExtensions.Create</code> by adding <code>DynamicallyAccessedMembers</code> attribute to the generic type parameter. (<a href="https://github.com/dotnet/fsharp/issues/17356">Issue #17356</a>, <a href="https://github.com/dotnet/fsharp/pull/18302">PR #18302</a>)</li>
</ul>
<h3 data-fsdocs-heading="16"><a name="10.1.201-Changed" class="anchor" href="#10.1.201-Changed">Changed</a></h3>
<ul>
<li>Added <code>not null</code> constraints to <code>IDelegateEvent&lt;'Delegate&gt;</code>, <code>IEvent&lt;'Delegate,'Args&gt;</code>, <code>DelegateEvent&lt;'Delegate&gt;</code>, and <code>Event&lt;'Delegate,'Args&gt;</code> types to prevent spurious nullness warnings when implementing CLIEvent properties. (<a href="https://github.com/dotnet/fsharp/issues/18361">Issue #18361</a>, <a href="https://github.com/dotnet/fsharp/issues/18349">Issue #18349</a>, <a href="https://github.com/dotnet/fsharp/pull/19221">PR #19221</a>)</li>
<li>Renamed deprecated <code>or</code> and <code>&amp;</code> operators, but keeping the original compiled names for binary compatibility. (<a href="https://github.com/dotnet/fsharp/pull/19143">PR #19143</a>)</li>
</ul>

<h2 data-fsdocs-heading="17"><a name="10.0.100" class="anchor" href="#10.0.100">10.0.100 - 2025-11-11</a></h2><a href="https://www.nuget.org/packages/FSharp.Core/10.0.100" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.0.100-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/10.0.101" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.0.101-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/10.0.102" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.0.102-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/10.0.103" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.0.103-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/10.0.104" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.0.104-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/10.0.105" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.0.105-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/10.0.106" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.0.106-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/10.0.107" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.0.107-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/10.0.108" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.0.108-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/10.0.109" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.0.109-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/10.0.110" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.0.110-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/10.0.111" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.0.111-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/10.0.112" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.0.112-blue"></a><h3 data-fsdocs-heading="18"><a name="10.0.100-Fixed" class="anchor" href="#10.0.100-Fixed">Fixed</a></h3>
<ul>
<li>Correct a typo in docs for List.sort (<a href="https://github.com/dotnet/fsharp/pull/18938">PR #18938</a>)</li>
</ul>
<h3 data-fsdocs-heading="19"><a name="10.0.100-Added" class="anchor" href="#10.0.100-Added">Added</a></h3>
<ul>
<li>Enable more <code>string</code> optimizations by adding <code>when 'T : Enum</code> library-only library-only static optimization constraint. (<a href="https://github.com/dotnet/fsharp/pull/18546">PR #18546</a>)</li>
</ul>
<h3 data-fsdocs-heading="20"><a name="10.0.100-Changed" class="anchor" href="#10.0.100-Changed">Changed</a></h3>
<ul>
<li>Random functions support for zero element chosen/sampled (<a href="https://github.com/dotnet/fsharp/pull/18568">PR #18568</a>)</li>
<li>Optimize array slicing performance. (<a href="https://github.com/dotnet/fsharp/pull/18778">PR #18778</a>)</li>
</ul>
<h3 data-fsdocs-heading="21"><a name="10.0.100-Breaking Changes" class="anchor" href="#10.0.100-Breaking Changes">Breaking Changes</a></h3>
<ul>
<li>1D array slicing now returns an empty array singleton instead of allocating a new array when the result is empty. (<a href="https://github.com/dotnet/fsharp/pull/18778">PR #18778</a>)</li>
</ul>

<h2 data-fsdocs-heading="22"><a name="9.0.300" class="anchor" href="#9.0.300">9.0.300 - 2025-05-13</a></h2><a href="https://www.nuget.org/packages/FSharp.Core/9.0.300" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-9.0.300-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/9.0.303" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-9.0.303-blue"></a><h3 data-fsdocs-heading="23"><a name="9.0.300-Fixed" class="anchor" href="#9.0.300-Fixed">Fixed</a></h3>
<ul>
<li>Modified the behavior of <code>Array.insertManyAt</code> to return a copy of the original array when inserting an empty array. (<a href="https://github.com/dotnet/fsharp/pull/18353">PR #18353</a>)</li>
</ul>
<h3 data-fsdocs-heading="24"><a name="9.0.300-Added" class="anchor" href="#9.0.300-Added">Added</a></h3>
<ul>
<li>Added nullability annotations to <code>.Using</code> builder method for <code>async</code> and <code>task</code> builders (<a href="https://github.com/dotnet/fsharp/pull/18292">PR #18292</a>)</li>
<li>Support for <code>and!</code> in <code>TaskBuilder</code> (<a href="https://github.com/fsharp/fslang-suggestions/issues/1363">LanguageSuggestion #1363</a>, <a href="https://github.com/dotnet/fsharp/pull/18451">PR #18451</a>)</li>
</ul>
<h3 data-fsdocs-heading="25"><a name="9.0.300-Changed" class="anchor" href="#9.0.300-Changed">Changed</a></h3>
<h3 data-fsdocs-heading="26"><a name="9.0.300-Breaking Changes" class="anchor" href="#9.0.300-Breaking Changes">Breaking Changes</a></h3>
<ul>
<li>Struct unions with overlapping fields now generate mappings needed for reading via reflection (<a href="https://github.com/dotnet/fsharp/issues/17797">Issue #18121</a>, <a href="https://github.com/dotnet/fsharp/pull/18274">PR #18274</a>). Previous versions of FSharp.Core returned incomplete mapping between fields and cases, these older fslib versions will now report an exception.</li>
</ul>

<h2 data-fsdocs-heading="27"><a name="9.0.201" class="anchor" href="#9.0.201">9.0.201 - 2025-02-11</a></h2><a href="https://www.nuget.org/packages/FSharp.Core/9.0.201" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-9.0.201-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/9.0.202" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-9.0.202-blue"></a><h3 data-fsdocs-heading="28"><a name="9.0.201-Fixed" class="anchor" href="#9.0.201-Fixed">Fixed</a></h3>
<ul>
<li>Fix exception on Post after MailboxProcessor was disposed (<a href="https://github.com/dotnet/fsharp/issues/17849">Issue #17849</a>, <a href="https://github.com/dotnet/fsharp/pull/17922">PR #17922</a>)</li>
<li>Fix missing null annotation in Async.SwitchToContext (<a href="https://github.com/dotnet/fsharp/issues/18055">Issue #18055</a>, <a href="https://github.com/dotnet/fsharp/pull/18059">PR #18059</a>)</li>
</ul>
<h3 data-fsdocs-heading="29"><a name="9.0.201-Added" class="anchor" href="#9.0.201-Added">Added</a></h3>
<h3 data-fsdocs-heading="30"><a name="9.0.201-Changed" class="anchor" href="#9.0.201-Changed">Changed</a></h3>
<ul>
<li>String function changed to guarantee a non-null string return type (<a href="https://github.com/dotnet/fsharp/pull/17809">PR #17809</a>)</li>
<li>Add Parameters as valid target for the Struct attribute (<a href="https://github.com/fsharp/fslang-suggestions/issues/1136">Language suggestion #1136</a>, <a href="https://github.com/dotnet/fsharp/pull/18098">PR #18098</a>)</li>
</ul>
<h3 data-fsdocs-heading="31"><a name="9.0.201-Breaking Changes" class="anchor" href="#9.0.201-Breaking Changes">Breaking Changes</a></h3>

<h2 data-fsdocs-heading="32"><a name="9.0.101" class="anchor" href="#9.0.101">9.0.101 - 2025-01-14</a></h2><a href="https://www.nuget.org/packages/FSharp.Core/9.0.101" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-9.0.101-blue"></a><h2>FSharp.Core 9.0.101 did not change compared to version 9.0.100. Below are the changes for FSharp.Core 9.0.100</h2>
<h3 data-fsdocs-heading="33"><a name="9.0.101-Fixed" class="anchor" href="#9.0.101-Fixed">Fixed</a></h3>
<ul>
<li>Struct UnionCase doesn't seem to be a valid target for the DefaultAugmentationAttribute (<a href="https://github.com/dotnet/fsharp/issues/17499">Issue #17499</a>, <a href="https://github.com/dotnet/fsharp/pull/17502">PR #17502</a>)</li>
</ul>
<h3 data-fsdocs-heading="34"><a name="9.0.101-Added" class="anchor" href="#9.0.101-Added">Added</a></h3>
<ul>
<li>Enable C# collection expression support for F# lists &amp; sets. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1355">Language suggestion #1355</a>, <a href="https://github.com/fsharp/fslang-design/pull/776">RFC FS-1145 (PR#776)</a>, <a href="https://github.com/dotnet/fsharp/pull/17359">PR #17359</a>)</li>
<li>Add module functions for converting between <code>'T option</code> and <code>'T voption</code>. (<a href="https://github.com/dotnet/fsharp/pull/17436">PR #17436</a>)</li>
</ul>
<h3 data-fsdocs-heading="35"><a name="9.0.101-Changed" class="anchor" href="#9.0.101-Changed">Changed</a></h3>
<ul>
<li>Change compiler default setting realsig+ when building assemblies (<a href="https://github.com/dotnet/fsharp/issues/17384">Issue #17384</a>, <a href="https://github.com/dotnet/fsharp/pull/17385">PR #17378</a>)</li>
<li>Change compiler default setting for compressedMetadata (<a href="https://github.com/dotnet/fsharp/issues/17379">Issue #17379</a>, <a href="https://github.com/dotnet/fsharp/pull/17383">PR #17383</a>)</li>
<li>Enable FSharp 9.0 Language Version (<a href="https://github.com/dotnet/fsharp/issues/17438">Issue #17497</a>), <a href="https://github.com/dotnet/fsharp/pull/17500">PR</a>))</li>
<li>Struct UnionCase doesn't seem to be a valid target for the DefaultAugmentationAttribute (<a href="https://github.com/dotnet/fsharp/issues/17499">Issue #17499</a>, <a href="https://github.com/dotnet/fsharp/pull/17502">PR #17502</a>)</li>
</ul>
<h3 data-fsdocs-heading="36"><a name="9.0.101-Breaking Changes" class="anchor" href="#9.0.101-Breaking Changes">Breaking Changes</a></h3>

<h2 data-fsdocs-heading="37"><a name="9.0.100" class="anchor" href="#9.0.100">9.0.100 - 2024-11-12</a></h2><a href="https://www.nuget.org/packages/FSharp.Core/9.0.100" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-9.0.100-blue"></a><h3 data-fsdocs-heading="38"><a name="9.0.100-Fixed" class="anchor" href="#9.0.100-Fixed">Fixed</a></h3>
<ul>
<li>Struct UnionCase doesn't seem to be a valid target for the DefaultAugmentationAttribute (<a href="https://github.com/dotnet/fsharp/issues/17499">Issue #17499</a>, <a href="https://github.com/dotnet/fsharp/pull/17502">PR #17502</a>)</li>
</ul>
<h3 data-fsdocs-heading="39"><a name="9.0.100-Added" class="anchor" href="#9.0.100-Added">Added</a></h3>
<ul>
<li>Enable C# collection expression support for F# lists &amp; sets. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1355">Language suggestion #1355</a>, <a href="https://github.com/fsharp/fslang-design/pull/776">RFC FS-1145 (PR#776)</a>, <a href="https://github.com/dotnet/fsharp/pull/17359">PR #17359</a>)</li>
<li>Add module functions for converting between <code>'T option</code> and <code>'T voption</code>. (<a href="https://github.com/dotnet/fsharp/pull/17436">PR #17436</a>)</li>
</ul>
<h3 data-fsdocs-heading="40"><a name="9.0.100-Changed" class="anchor" href="#9.0.100-Changed">Changed</a></h3>
<ul>
<li>Change compiler default setting realsig+ when building assemblies (<a href="https://github.com/dotnet/fsharp/issues/17384">Issue #17384</a>, <a href="https://github.com/dotnet/fsharp/pull/17385">PR #17378</a>)</li>
<li>Change compiler default setting for compressedMetadata (<a href="https://github.com/dotnet/fsharp/issues/17379">Issue #17379</a>, <a href="https://github.com/dotnet/fsharp/pull/17383">PR #17383</a>)</li>
<li>Enable FSharp 9.0 Language Version (<a href="https://github.com/dotnet/fsharp/issues/17438">Issue #17497</a>), <a href="https://github.com/dotnet/fsharp/pull/17500">PR</a>))</li>
<li>Struct UnionCase doesn't seem to be a valid target for the DefaultAugmentationAttribute (<a href="https://github.com/dotnet/fsharp/issues/17499">Issue #17499</a>, <a href="https://github.com/dotnet/fsharp/pull/17502">PR #17502</a>)</li>
</ul>
<h3 data-fsdocs-heading="41"><a name="9.0.100-Breaking Changes" class="anchor" href="#9.0.100-Breaking Changes">Breaking Changes</a></h3>

<h2 data-fsdocs-heading="42"><a name="8.0.400" class="anchor" href="#8.0.400">8.0.400 - 2024-08-13</a></h2><a href="https://www.nuget.org/packages/FSharp.Core/8.0.400" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-8.0.400-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/8.0.401" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-8.0.401-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/8.0.403" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-8.0.403-blue"></a><h3 data-fsdocs-heading="43"><a name="8.0.400-Fixed" class="anchor" href="#8.0.400-Fixed">Fixed</a></h3>
<h3 data-fsdocs-heading="44"><a name="8.0.400-Added" class="anchor" href="#8.0.400-Added">Added</a></h3>
<ul>
<li><code>Random functions for collections</code> (<a href="https://github.com/fsharp/fslang-design/blob/main/RFCs/FS-1135-random-functions-for-collections.md">RFC #1135</a>, <a href="https://github.com/dotnet/fsharp/pull/17277">PR #17277</a>)</li>
</ul>
<h3 data-fsdocs-heading="45"><a name="8.0.400-Changed" class="anchor" href="#8.0.400-Changed">Changed</a></h3>
<ul>
<li>Cache delegate in query extensions. (<a href="https://github.com/dotnet/fsharp/pull/17130">PR #17130</a>)</li>
<li>Update <code>AllowNullLiteralAttribute</code> to also use <code>AttributeTargets.Interface</code> (<a href="https://github.com/dotnet/fsharp/pull/17173">PR #17173</a>)</li>
<li>Update <code>StructAttribute </code> to also use <code>AttributeTargets.Class</code> (<a href="https://github.com/dotnet/fsharp/pull/17207">PR #17207</a>)</li>
</ul>
<h3 data-fsdocs-heading="46"><a name="8.0.400-Breaking Changes" class="anchor" href="#8.0.400-Breaking Changes">Breaking Changes</a></h3>
<ul>
<li>Fixed argument exception throwing inconsistency - accessing an out-of-bounds collection index will now throw <code>ArgumentOutOfRangeException</code> instead of <code>ArgumentException</code> (<a href="https://github.com/dotnet/fsharp/pull/17328">#17328</a>)</li>
</ul>

<h2 data-fsdocs-heading="47"><a name="8.0.300" class="anchor" href="#8.0.300">8.0.300 - 2024-05-14</a></h2><a href="https://www.nuget.org/packages/FSharp.Core/8.0.300" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-8.0.300-blue"></a> <a href="https://www.nuget.org/packages/FSharp.Core/8.0.301" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-8.0.301-blue"></a><h3 data-fsdocs-heading="48"><a name="8.0.300-Added" class="anchor" href="#8.0.300-Added">Added</a></h3>
<ul>
<li>Minor tweaks to inline specifications to support Visibility PR (<a href="https://github.com/dotnet/fsharp/pull/15484">PR #15484</a>, <a href="https://github.com/dotnet/fsharp/pull/15484">#PR 16427</a></li>
<li>Optimize equality in generic contexts. (<a href="https://github.com/dotnet/fsharp/pull/16615">PR #16615</a>)</li>
<li>Add a constructor for <code>MailboxProcessor</code> with a flag denoting that an exception will be thrown when <code>Post</code> is called after the <code>MailboxProcessor</code> has been disposed. (<a href="https://github.com/dotnet/fsharp/pull/13036">PR #13036</a>)</li>
</ul>
<h3 data-fsdocs-heading="49"><a name="8.0.300-Fixed" class="anchor" href="#8.0.300-Fixed">Fixed</a></h3>
<ul>
<li>Preserve original stack traces in resumable state machines generated code if available. (<a href="https://github.com/dotnet/fsharp/pull/16568">PR #16568</a>)</li>
<li>Fix receiving and processing mailbox after Dispose. (<a href="https://github.com/dotnet/fsharp/pull/13036">PR #13036</a>)</li>
<li>Enforce AttributeTargets on structs and classes. Also update <code>RequireQualifiedAccessAttribute</code> and <code>AutoOpenAttribute</code> to use <code>AttributeTargets.Struct</code> (<a href="https://github.com/dotnet/fsharp/pull/16790">PR #16790</a>)</li>
<li>Enforce AttributeTargets on enums. Also update <code>RequireQualifiedAccessAttribute</code> to use <code>AttributeTargets.Enum</code> (<a href="https://github.com/dotnet/fsharp/pull/16887">PR #16887</a>)</li>
<li>Enforce AttributeTargets on delegates. Also update <code>ReflectedDefinitionAttribute</code> to use <code>AttributeTargets.Delegate</code> (<a href="https://github.com/dotnet/fsharp/pull/16891">PR #16891</a>)</li>
</ul>

<h2 data-fsdocs-heading="50"><a name="8.0.200" class="anchor" href="#8.0.200">8.0.200 - 2024-02-13</a></h2><a href="https://www.nuget.org/packages/FSharp.Core/8.0.200" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-8.0.200-blue"></a><h3 data-fsdocs-heading="51"><a name="8.0.200-Added" class="anchor" href="#8.0.200-Added">Added</a></h3>
<ul>
<li>More inlines for Result module. (<a href="https://github.com/dotnet/fsharp/pull/16106">PR #16106</a>)</li>
<li>Added a new parameterless constructor for <code>CustomOperationAttribute</code> (<a href="https://github.com/dotnet/fsharp/pull/16475">PR #16475</a>, part of implementation for <a href="https://github.com/fsharp/fslang-suggestions/issues/1250">fslang-suggestions/1250</a>)</li>
</ul>
