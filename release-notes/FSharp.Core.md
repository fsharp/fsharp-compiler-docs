# FSharp.Core

<h2><a name="9.0.300" class="anchor" href="#9.0.300">9.0.300 - 2025-05-13</a></h2><a href="https://www.nuget.org/packages/FSharp.Core/9.0.300" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-9.0.300-blue"></a><h3><a name="9.0.300-Fixed" class="anchor" href="#9.0.300-Fixed">Fixed</a></h3>
<ul>
<li>Modified the behavior of <code>Array.insertManyAt</code> to return a copy of the original array when inserting an empty array. (<a href="https://github.com/dotnet/fsharp/pull/18353">PR #18353</a>)</li>
</ul>
<h3><a name="9.0.300-Added" class="anchor" href="#9.0.300-Added">Added</a></h3>
<ul>
<li>Added nullability annotations to <code>.Using</code> builder method for <code>async</code> and <code>task</code> builders (<a href="https://github.com/dotnet/fsharp/pull/18292">PR #18292</a>)</li>
<li>Support for <code>and!</code> in <code>TaskBuilder</code> (<a href="https://github.com/fsharp/fslang-suggestions/issues/1363">LanguageSuggestion #1363</a>, <a href="https://github.com/dotnet/fsharp/pull/18451">PR #18451</a>)</li>
</ul>
<h3><a name="9.0.300-Changed" class="anchor" href="#9.0.300-Changed">Changed</a></h3>
<h3><a name="9.0.300-Breaking Changes" class="anchor" href="#9.0.300-Breaking Changes">Breaking Changes</a></h3>
<ul>
<li>Struct unions with overlapping fields now generate mappings needed for reading via reflection (<a href="https://github.com/dotnet/fsharp/issues/17797">Issue #18121</a>, <a href="https://github.com/dotnet/fsharp/pull/18274">PR #18274</a>). Previous versions of FSharp.Core returned incomplete mapping between fields and cases, these older fslib versions will now report an exception.</li>
</ul>

<h2><a name="9.0.200" class="anchor" href="#9.0.200">9.0.200 - Unreleased</a></h2><h3><a name="9.0.200-Fixed" class="anchor" href="#9.0.200-Fixed">Fixed</a></h3>
<ul>
<li>Fix exception on Post after MailboxProcessor was disposed (<a href="https://github.com/dotnet/fsharp/issues/17849">Issue #17849</a>, <a href="https://github.com/dotnet/fsharp/pull/17922">PR #17922</a>)</li>
<li>Fix missing null annotation in Async.SwitchToContext (<a href="https://github.com/dotnet/fsharp/issues/18055">Issue #18055</a>, <a href="https://github.com/dotnet/fsharp/pull/18059">PR #18059</a>)</li>
</ul>
<h3><a name="9.0.200-Added" class="anchor" href="#9.0.200-Added">Added</a></h3>
<h3><a name="9.0.200-Changed" class="anchor" href="#9.0.200-Changed">Changed</a></h3>
<ul>
<li>String function changed to guarantee a non-null string return type (<a href="https://github.com/dotnet/fsharp/pull/17809">PR #17809</a>)</li>
<li>Add Parameters as valid target for the Struct attribute (<a href="https://github.com/fsharp/fslang-suggestions/issues/1136">Language suggestion #1136</a>, <a href="https://github.com/dotnet/fsharp/pull/18098">PR #18098</a>)</li>
</ul>
<h3><a name="9.0.200-Breaking Changes" class="anchor" href="#9.0.200-Breaking Changes">Breaking Changes</a></h3>

<h2><a name="9.0.101" class="anchor" href="#9.0.101">9.0.101 - 2025-01-14</a></h2><a href="https://www.nuget.org/packages/FSharp.Core/9.0.101" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-9.0.101-blue"></a><h2>FSharp.Core 9.0.101 did not change compared to version 9.0.100. Below are the changes for FSharp.Core 9.0.100</h2>
<h3><a name="9.0.101-Fixed" class="anchor" href="#9.0.101-Fixed">Fixed</a></h3>
<ul>
<li>Struct UnionCase doesn't seem to be a valid target for the DefaultAugmentationAttribute (<a href="https://github.com/dotnet/fsharp/issues/17499">Issue #17499</a>, <a href="https://github.com/dotnet/fsharp/pull/17502">PR #17502</a>)</li>
</ul>
<h3><a name="9.0.101-Added" class="anchor" href="#9.0.101-Added">Added</a></h3>
<ul>
<li>Enable C# collection expression support for F# lists &amp; sets. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1355">Language suggestion #1355</a>, <a href="https://github.com/fsharp/fslang-design/pull/776">RFC FS-1145 (PR#776)</a>, <a href="https://github.com/dotnet/fsharp/pull/17359">PR #17359</a>)</li>
<li>Add module functions for converting between <code>'T option</code> and <code>'T voption</code>. (<a href="https://github.com/dotnet/fsharp/pull/17436">PR #17436</a>)</li>
</ul>
<h3><a name="9.0.101-Changed" class="anchor" href="#9.0.101-Changed">Changed</a></h3>
<ul>
<li>Change compiler default setting realsig+ when building assemblies (<a href="https://github.com/dotnet/fsharp/issues/17384">Issue #17384</a>, <a href="https://github.com/dotnet/fsharp/pull/17385">PR #17378</a>)</li>
<li>Change compiler default setting for compressedMetadata (<a href="https://github.com/dotnet/fsharp/issues/17379">Issue #17379</a>, <a href="https://github.com/dotnet/fsharp/pull/17383">PR #17383</a>)</li>
<li>Enable FSharp 9.0 Language Version (<a href="https://github.com/dotnet/fsharp/issues/17438">Issue #17497</a>), <a href="https://github.com/dotnet/fsharp/pull/17500">PR</a>))</li>
<li>Struct UnionCase doesn't seem to be a valid target for the DefaultAugmentationAttribute (<a href="https://github.com/dotnet/fsharp/issues/17499">Issue #17499</a>, <a href="https://github.com/dotnet/fsharp/pull/17502">PR #17502</a>)</li>
</ul>
<h3><a name="9.0.101-Breaking Changes" class="anchor" href="#9.0.101-Breaking Changes">Breaking Changes</a></h3>

<h2><a name="9.0.100" class="anchor" href="#9.0.100">9.0.100 - 2024-11-12</a></h2><a href="https://www.nuget.org/packages/FSharp.Core/9.0.100" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-9.0.100-blue"></a><h3><a name="9.0.100-Fixed" class="anchor" href="#9.0.100-Fixed">Fixed</a></h3>
<ul>
<li>Struct UnionCase doesn't seem to be a valid target for the DefaultAugmentationAttribute (<a href="https://github.com/dotnet/fsharp/issues/17499">Issue #17499</a>, <a href="https://github.com/dotnet/fsharp/pull/17502">PR #17502</a>)</li>
</ul>
<h3><a name="9.0.100-Added" class="anchor" href="#9.0.100-Added">Added</a></h3>
<ul>
<li>Enable C# collection expression support for F# lists &amp; sets. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1355">Language suggestion #1355</a>, <a href="https://github.com/fsharp/fslang-design/pull/776">RFC FS-1145 (PR#776)</a>, <a href="https://github.com/dotnet/fsharp/pull/17359">PR #17359</a>)</li>
<li>Add module functions for converting between <code>'T option</code> and <code>'T voption</code>. (<a href="https://github.com/dotnet/fsharp/pull/17436">PR #17436</a>)</li>
</ul>
<h3><a name="9.0.100-Changed" class="anchor" href="#9.0.100-Changed">Changed</a></h3>
<ul>
<li>Change compiler default setting realsig+ when building assemblies (<a href="https://github.com/dotnet/fsharp/issues/17384">Issue #17384</a>, <a href="https://github.com/dotnet/fsharp/pull/17385">PR #17378</a>)</li>
<li>Change compiler default setting for compressedMetadata (<a href="https://github.com/dotnet/fsharp/issues/17379">Issue #17379</a>, <a href="https://github.com/dotnet/fsharp/pull/17383">PR #17383</a>)</li>
<li>Enable FSharp 9.0 Language Version (<a href="https://github.com/dotnet/fsharp/issues/17438">Issue #17497</a>), <a href="https://github.com/dotnet/fsharp/pull/17500">PR</a>))</li>
<li>Struct UnionCase doesn't seem to be a valid target for the DefaultAugmentationAttribute (<a href="https://github.com/dotnet/fsharp/issues/17499">Issue #17499</a>, <a href="https://github.com/dotnet/fsharp/pull/17502">PR #17502</a>)</li>
</ul>
<h3><a name="9.0.100-Breaking Changes" class="anchor" href="#9.0.100-Breaking Changes">Breaking Changes</a></h3>

<h2><a name="8.0.400" class="anchor" href="#8.0.400">8.0.400 - 2024-08-13</a></h2><a href="https://www.nuget.org/packages/FSharp.Core/8.0.400" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-8.0.400-blue"></a><h3><a name="8.0.400-Fixed" class="anchor" href="#8.0.400-Fixed">Fixed</a></h3>
<h3><a name="8.0.400-Added" class="anchor" href="#8.0.400-Added">Added</a></h3>
<ul>
<li><code>Random functions for collections</code> (<a href="https://github.com/fsharp/fslang-design/blob/main/RFCs/FS-1135-random-functions-for-collections.md">RFC #1135</a>, <a href="https://github.com/dotnet/fsharp/pull/17277">PR #17277</a>)</li>
</ul>
<h3><a name="8.0.400-Changed" class="anchor" href="#8.0.400-Changed">Changed</a></h3>
<ul>
<li>Cache delegate in query extensions. (<a href="https://github.com/dotnet/fsharp/pull/17130">PR #17130</a>)</li>
<li>Update <code>AllowNullLiteralAttribute</code> to also use <code>AttributeTargets.Interface</code> (<a href="https://github.com/dotnet/fsharp/pull/17173">PR #17173</a>)</li>
<li>Update <code>StructAttribute </code> to also use <code>AttributeTargets.Class</code> (<a href="https://github.com/dotnet/fsharp/pull/17207">PR #17207</a>)</li>
</ul>
<h3><a name="8.0.400-Breaking Changes" class="anchor" href="#8.0.400-Breaking Changes">Breaking Changes</a></h3>
<ul>
<li>Fixed argument exception throwing inconsistency - accessing an out-of-bounds collection index will now throw <code>ArgumentOutOfRangeException</code> instead of <code>ArgumentException</code> (<a href="https://github.com/dotnet/fsharp/pull/17328">#17328</a>)</li>
</ul>

<h2><a name="8.0.300" class="anchor" href="#8.0.300">8.0.300 - 2024-05-14</a></h2><a href="https://www.nuget.org/packages/FSharp.Core/8.0.300" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-8.0.300-blue"></a><h3><a name="8.0.300-Added" class="anchor" href="#8.0.300-Added">Added</a></h3>
<ul>
<li>Minor tweaks to inline specifications to support Visibility PR (<a href="https://github.com/dotnet/fsharp/pull/15484">PR #15484</a>, <a href="https://github.com/dotnet/fsharp/pull/15484">#PR 16427</a></li>
<li>Optimize equality in generic contexts. (<a href="https://github.com/dotnet/fsharp/pull/16615">PR #16615</a>)</li>
<li>Add a constructor for <code>MailboxProcessor</code> with a flag denoting that an exception will be thrown when <code>Post</code> is called after the <code>MailboxProcessor</code> has been disposed. (<a href="https://github.com/dotnet/fsharp/pull/13036">PR #13036</a>)</li>
</ul>
<h3><a name="8.0.300-Fixed" class="anchor" href="#8.0.300-Fixed">Fixed</a></h3>
<ul>
<li>Preserve original stack traces in resumable state machines generated code if available. (<a href="https://github.com/dotnet/fsharp/pull/16568">PR #16568</a>)</li>
<li>Fix receiving and processing mailbox after Dispose. (<a href="https://github.com/dotnet/fsharp/pull/13036">PR #13036</a>)</li>
<li>Enforce AttributeTargets on structs and classes. Also update <code>RequireQualifiedAccessAttribute</code> and <code>AutoOpenAttribute</code> to use <code>AttributeTargets.Struct</code> (<a href="https://github.com/dotnet/fsharp/pull/16790">PR #16790</a>)</li>
<li>Enforce AttributeTargets on enums. Also update <code>RequireQualifiedAccessAttribute</code> to use <code>AttributeTargets.Enum</code> (<a href="https://github.com/dotnet/fsharp/pull/16887">PR #16887</a>)</li>
<li>Enforce AttributeTargets on delegates. Also update <code>ReflectedDefinitionAttribute</code> to use <code>AttributeTargets.Delegate</code> (<a href="https://github.com/dotnet/fsharp/pull/16891">PR #16891</a>)</li>
</ul>

<h2><a name="8.0.200" class="anchor" href="#8.0.200">8.0.200 - 2024-02-13</a></h2><a href="https://www.nuget.org/packages/FSharp.Core/8.0.200" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-8.0.200-blue"></a><h3><a name="8.0.200-Added" class="anchor" href="#8.0.200-Added">Added</a></h3>
<ul>
<li>More inlines for Result module. (<a href="https://github.com/dotnet/fsharp/pull/16106">PR #16106</a>)</li>
<li>Added a new parameterless constructor for <code>CustomOperationAttribute</code> (<a href="https://github.com/dotnet/fsharp/pull/16475">PR #16475</a>, part of implementation for <a href="https://github.com/fsharp/fslang-suggestions/issues/1250">fslang-suggestions/1250</a>)</li>
</ul>

<h2><a name="10.0.300" class="anchor" href="#10.0.300">10.0.300 - Unreleased</a></h2><h3><a name="10.0.300-Fixed" class="anchor" href="#10.0.300-Fixed">Fixed</a></h3>
<ul>
<li>Optimize Set.intersect performance symmetry and preserve identity from the first set argument. (<a href="https://github.com/dotnet/fsharp/pull/19291">PR #19291</a>) (Fixes #19139)</li>
<li>Fix anonymous record field ordering in LINQ expression conversion to produce consistent expression trees regardless of field declaration order. (<a href="https://github.com/dotnet/fsharp/issues/11131">Issue #11131</a>, <a href="https://github.com/dotnet/fsharp/issues/15648">Issue #15648</a>)</li>
<li>Fix array indexing in LINQ expressions to generate proper array index expressions instead of GetArray method calls, enabling LINQ providers like Azure Cosmos DB to translate array access. (<a href="https://github.com/dotnet/fsharp/issues/16918">Issue #16918</a>)</li>
<li>Fix tuple join conditions and groupBy operations to properly compare tuple keys using structural equality. AnonymousObject types now implement Equals and GetHashCode, enabling inline tuple joins like <code>join b on ((a.Id1, a.Id2) = (b.Id1, b.Id2))</code> to work correctly. (<a href="https://github.com/dotnet/fsharp/issues/7885">Issue #7885</a>, <a href="https://github.com/dotnet/fsharp/issues/47">Issue #47</a>)</li>
<li>Fix tuple/multi-value projections in queries to use Queryable.Select instead of Enumerable.Select when the source is IQueryable, preserving query composition and enabling async operations like ToListAsync() in Entity Framework Core. (<a href="https://github.com/dotnet/fsharp/issues/3782">Issue #3782</a>, <a href="https://github.com/dotnet/fsharp/issues/15133">Issue #15133</a>)</li>
<li>Fix EvaluateQuotation to handle Sequential expressions, void method calls (unit return), and other patterns that were previously throwing NotSupportedException. Also properly handles unit-returning expressions by using Action delegates instead of Func delegates. (<a href="https://github.com/dotnet/fsharp/issues/19099">Issue #19099</a>)</li>
<li>Fix query conditionals without else branch (if-then only) that were causing type mismatch errors. Now properly extracts element type from IQueryable for creating empty sequences. (<a href="https://github.com/dotnet/fsharp/issues/3445">Issue #3445</a>)</li>
<li>Fix <code>Seq.empty</code> rendering as <code>&quot;EmptyEnumerable&quot;</code> in serializers by delegating to <code>System.Linq.Enumerable.Empty&lt;'T&gt;()</code> instead of using a custom DU type. (<a href="https://github.com/dotnet/fsharp/issues/17864">Issue #17864</a>, <a href="https://github.com/dotnet/fsharp/pull/19317">PR #19317</a>)</li>
<li>Ensure culture-independent parsing of .NET-style interpolated string holes. (<a href="https://github.com/dotnet/fsharp/issues/19367">Issue #19367</a>, <a href="https://github.com/dotnet/fsharp/pull/19370">PR #19370</a>)</li>
</ul>
<h3><a name="10.0.300-Added" class="anchor" href="#10.0.300-Added">Added</a></h3>
<ul>
<li>Add <code>List.partitionWith</code>, <code>Array.partitionWith</code>, <code>Set.partitionWith</code>, and <code>Array.Parallel.partitionWith</code> functions that partition a collection using a function that returns <code>Choice&lt;'T1, 'T2&gt;</code>. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1119">Language Suggestion #1119</a>)</li>
</ul>
<h3><a name="10.0.300-Changed" class="anchor" href="#10.0.300-Changed">Changed</a></h3>
<ul>
<li>Added complexity documentation (Big-O notation) to all 462 functions across Array, List, Seq, Map, and Set collection modules. (<a href="https://github.com/dotnet/fsharp/pull/19240">PR #19240</a>)</li>
</ul>
<h3><a name="10.0.300-Breaking Changes" class="anchor" href="#10.0.300-Breaking Changes">Breaking Changes</a></h3>

<h2><a name="10.0.200" class="anchor" href="#10.0.200">10.0.200 - Unreleased</a></h2><h3><a name="10.0.200-Fixed" class="anchor" href="#10.0.200-Fixed">Fixed</a></h3>
<ul>
<li>Fix IL2091 trimming warning in <code>LazyExtensions.Create</code> by adding <code>DynamicallyAccessedMembers</code> attribute to the generic type parameter. (<a href="https://github.com/dotnet/fsharp/issues/17356">Issue #17356</a>, <a href="https://github.com/dotnet/fsharp/pull/18302">PR #18302</a>)</li>
</ul>
<h3><a name="10.0.200-Changed" class="anchor" href="#10.0.200-Changed">Changed</a></h3>
<ul>
<li>Added <code>not null</code> constraints to <code>IDelegateEvent&lt;'Delegate&gt;</code>, <code>IEvent&lt;'Delegate,'Args&gt;</code>, <code>DelegateEvent&lt;'Delegate&gt;</code>, and <code>Event&lt;'Delegate,'Args&gt;</code> types to prevent spurious nullness warnings when implementing CLIEvent properties. (<a href="https://github.com/dotnet/fsharp/issues/18361">Issue #18361</a>, <a href="https://github.com/dotnet/fsharp/issues/18349">Issue #18349</a>, <a href="https://github.com/dotnet/fsharp/pull/19221">PR #19221</a>)</li>
<li>Renamed deprecated <code>or</code> and <code>&amp;</code> operators, but keeping the original compiled names for binary compatibility. (<a href="https://github.com/dotnet/fsharp/pull/19143">PR #19143</a>)</li>
</ul>

<h2><a name="10.0.100" class="anchor" href="#10.0.100">10.0.100 - 2025-11-11</a></h2><a href="https://www.nuget.org/packages/FSharp.Core/10.0.100" target="_blank"><img alt="Nuget" src="https://img.shields.io/badge/NuGet-10.0.100-blue"></a><h3><a name="10.0.100-Fixed" class="anchor" href="#10.0.100-Fixed">Fixed</a></h3>
<ul>
<li>Correct a typo in docs for List.sort (<a href="https://github.com/dotnet/fsharp/pull/18938">PR #18938</a>)</li>
</ul>
<h3><a name="10.0.100-Added" class="anchor" href="#10.0.100-Added">Added</a></h3>
<ul>
<li>Enable more <code>string</code> optimizations by adding <code>when 'T : Enum</code> library-only library-only static optimization constraint. (<a href="https://github.com/dotnet/fsharp/pull/18546">PR #18546</a>)</li>
</ul>
<h3><a name="10.0.100-Changed" class="anchor" href="#10.0.100-Changed">Changed</a></h3>
<ul>
<li>Random functions support for zero element chosen/sampled (<a href="https://github.com/dotnet/fsharp/pull/18568">PR #18568</a>)</li>
<li>Optimize array slicing performance. (<a href="https://github.com/dotnet/fsharp/pull/18778">PR #18778</a>)</li>
</ul>
<h3><a name="10.0.100-Breaking Changes" class="anchor" href="#10.0.100-Breaking Changes">Breaking Changes</a></h3>
<ul>
<li>1D array slicing now returns an empty array singleton instead of allocating a new array when the result is empty. (<a href="https://github.com/dotnet/fsharp/pull/18778">PR #18778</a>)</li>
</ul>
