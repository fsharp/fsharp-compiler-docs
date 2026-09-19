---
category: Release Notes
categoryindex: 600
index: 2
title: F# Language
---

# F# Language

<h2 data-fsdocs-heading="1"><a name="Preview" class="anchor" href="#Preview">Preview</a></h2><h3 data-fsdocs-heading="2"><a name="Preview-Added" class="anchor" href="#Preview-Added">Added</a></h3>
<ul>
<li><p>Runtime async: <code>task</code>/<code>async</code>-style computation expressions can be compiled to use the .NET runtime async support (RuntimeAsync preview feature). (<a href="https://github.com/dotnet/fsharp/pull/20235">PR #20235</a>)</p>
</li>
<li><p><strong>Extension members for operators and SRTP constraints</strong> (<a href="https://github.com/fsharp/fslang-design/blob/main/RFCs/FS-1043-extension-members-for-operators-and-srtp-constraints.md">RFC FS-1043</a>, <a href="https://github.com/fsharp/fslang-suggestions/issues/230">fslang-suggestions#230</a>, <a href="https://github.com/dotnet/fsharp/pull/19602">PR #19602</a>): Extension methods now participate in SRTP constraint resolution. This allows defining operators on types you don't own via type extensions:</p>
<pre><code class="language-fsharp">type System.String with
    static member (*) (s: string, n: int) = String.replicate n s

let inline multiply (x: ^T) (n: int) = x * n
let result = multiply &quot;ha&quot; 3  // &quot;hahaha&quot;
</code></pre>
<p><strong>Feature flag:</strong> <code>--langversion:preview</code> (feature name: <code>ExtensionConstraintSolutions</code>)</p>
<p><strong>Includes:</strong></p>
<ul>
<li>Extension operators resolve via SRTP constraints (suggestion #230)</li>
<li>Only <strong>public</strong> members solve SRTP constraints — a <code>private</code>, <code>internal</code>, or <code>protected</code> member (even one visible at the definition site, or exposed via <code>InternalsVisibleTo</code>) is not a valid witness and is rejected at compile time</li>
<li>Intrinsic members take priority over extension members</li>
<li>FS1215 warning suppressed when defining extension operators with preview langversion</li>
<li>Weak resolution disabled for inline code, keeping SRTP constraints generic</li>
<li><code>[&lt;AllowOverloadOnReturnType&gt;]</code> attribute for defining overloads that differ only by return type (suggestion #820). When applied, return-type information is used during overload resolution to disambiguate call sites.</li>
<li>Cross-assembly resolution: extension operators defined in referenced assemblies are resolved via SRTP constraints</li>
<li>Extension members solve SRTP constraints but do <em>not</em> satisfy nominal static abstract interface constraints (IWSAMs). These are orthogonal mechanisms.</li>
<li>Tuple type extensions using syntactic tuple notation: <code>type ('T1 * 'T2) with</code> for reference tuples and <code>type struct ('T1 * 'T2) with</code> for struct tuples. These are transformed to <code>System.Tuple&lt;'T1,'T2&gt;</code> and <code>System.ValueTuple&lt;'T1,'T2&gt;</code> extensions respectively.</li>
</ul>
</li>
<li><p>Warn (FS3884) when a function or delegate value is used as an interpolated string argument, since it will be formatted via <code>ToString</code> rather than being applied. (<a href="https://github.com/dotnet/fsharp/pull/19289">PR #19289</a>)</p>
</li>
<li><p>Added <code>MethodOverloadsCache</code> language feature (preview) that caches overload resolution results for repeated method calls, significantly improving compilation performance. (<a href="https://github.com/dotnet/fsharp/pull/19072">PR #19072</a>)</p>
</li>
<li><p>Added <code>ErrorOnMissingSignatureAttribute</code> preview language feature: makes FS3888 (compiler-semantic attribute on the <code>.fs</code> but not on the <code>.fsi</code>) an error instead of a warning. (<a href="https://github.com/dotnet/fsharp/issues/19560">Issue #19560</a>, <a href="https://github.com/dotnet/fsharp/pull/19880">PR #19880</a>)</p>
</li>
<li><p>Support common types of <code>NotNullIfNotNullAttribute</code> usage. If a method parameter is marked with <code>NotNullIfNotNullAttribute</code>, the compiler will now honor this attribute and mark the return type as non-null. (<a href="https://github.com/dotnet/fsharp/pull/19977">PR #19977</a>)</p>
</li>
<li><p>Spread operator for records (<a href="https://github.com/fsharp/fslang-design/pull/805">RFC FS-1151</a>, <a href="https://github.com/dotnet/fsharp/pull/18927">PR #18927</a>)</p>
</li>
<li><p>Added <code>AccessProtectedBaseFieldFromClosure</code> preview language feature: a derived member can now read a <code>protected</code> base-class field from an ordinary closure (lambda, delegate, <code>async</code>/<code>seq</code>/<code>lazy</code>, <code>function</code>, or list/array literal), which previously failed with FS1097 even though direct access compiles. Object expressions remain unsupported — bind the field to a local function or expose it through a member. (<a href="https://github.com/dotnet/fsharp/issues/5302">Issue #5302</a>)</p>
</li>
<li><p>Added <code>ImprovedImpliedArgumentNamesPartTwo</code> language feature: when a function with no recoverable parameter names is coerced to a delegate (e.g. a partial application like <code>System.Func&lt;int, int&gt;((+) 1)</code>), the synthesized <code>Invoke</code> parameters take their names from the delegate's own <code>Invoke</code> signature instead of synthetic <code>delegateArg0</code>, <code>delegateArg1</code>, … names. (<a href="https://github.com/dotnet/fsharp/pull/20001">PR #20001</a>)</p>
</li>
<li><p>Added a &quot;most concrete&quot; tiebreaker for overload resolution: when several overloads of a method, constructor, or generic-type member are equally applicable, the one with more concrete parameter types is preferred instead of reporting an ambiguity. Requires <code>--langversion:preview</code>. (<a href="https://github.com/fsharp/fslang-design/pull/834">RFC FS-1340</a>, <a href="https://github.com/dotnet/fsharp/pull/19277">PR #19277</a>)</p>
</li>
<li><p>Added support for <code>System.Runtime.CompilerServices.OverloadResolutionPriorityAttribute</code> (.NET 9): overloads with a higher priority value are preferred during resolution, matching C#. Requires <code>--langversion:preview</code>. (<a href="https://github.com/fsharp/fslang-design/pull/828">RFC FS-1338</a>, <a href="https://github.com/dotnet/fsharp/pull/19277">PR #19277</a>)</p>
</li>
<li><p>Allow constructing a record via its all-fields constructor, e.g. <code>MyRecord(a, b)</code>, with positional or named arguments (<code>RecordConstructorSyntax</code> preview feature). Accessibility matches <code>{ ... }</code> construction. (<a href="https://github.com/fsharp/fslang-suggestions/issues/722">Suggestion #722</a>, <a href="https://github.com/fsharp/fslang-design/blob/main/RFCs/FS-1073-record-constructors.md">RFC FS-1073</a>, <a href="https://github.com/dotnet/fsharp/pull/19974">PR #19974</a>)</p>
</li>
<li><p>Added support for <code>System.Diagnostics.CodeAnalysis.RequireNamedArgumentsAttribute</code>: annotated methods and constructors require named arguments, and positional calls report FS3923. The compiler recognises the attribute by full type name, including local polyfills, referenced assemblies, and type-provider metadata. <code>nameof</code> remains allowed. Requires <code>--langversion:preview</code>. (<a href="https://github.com/fsharp/fslang-suggestions/issues/414">Suggestion #414</a>, <a href="https://github.com/fsharp/fslang-design/blob/main/drafts/FS-1095-requirenamedargumentattribute.md">RFC FS-1095</a>, <a href="https://github.com/dotnet/runtime/issues/132924#issuecomment-5686007524">Approved API</a>, <a href="https://github.com/dotnet/fsharp/pull/20340">PR #20340</a>)</p>
</li>
</ul>
<h3 data-fsdocs-heading="3"><a name="Preview-Fixed" class="anchor" href="#Preview-Fixed">Fixed</a></h3>
<ul>
<li>Explicit generic type arguments are now unified in constraint-dependency order, so a subtype constraint that references a later type parameter (e.g. <code>Register&lt;'a, 'b when 'a :&gt; I&lt;'b&gt;&gt;</code> called as <code>&lt;Foo, int&gt;</code>) no longer fails with FS0001 when the argument implements the interface at several instantiations. (<a href="https://github.com/dotnet/fsharp/issues/20103">Issue #20103</a>, <a href="https://github.com/dotnet/fsharp/pull/20342">PR #20342</a>)</li>
<li>Bitwise operators (<code>|||</code>, <code>&amp;&amp;&amp;</code>, <code>^^^</code>) on enums whose underlying type is not an integer type (e.g. <code>char</code>) are now a compile-time error (FS0001, consistent with <code>~~~</code>, <code>&lt;&lt;&lt;</code>, <code>&gt;&gt;&gt;</code>) instead of a runtime <code>NotSupportedException</code>. (<a href="https://github.com/dotnet/fsharp/issues/11785">Issue #11785</a>, <a href="https://github.com/dotnet/fsharp/pull/20322">PR #20322</a>)</li>
</ul>
<h3 data-fsdocs-heading="4"><a name="Preview-Changed" class="anchor" href="#Preview-Changed">Changed</a></h3>
<ul>
<li>Inline functions now keep SRTP constraints generic instead of eagerly resolving through weak resolution. This changes inferred types for some inline code — see <a href="https://github.com/fsharp/fslang-design/blob/main/RFCs/FS-1043-extension-members-for-operators-and-srtp-constraints.md">RFC FS-1043 compatibility section</a> for details and workarounds.</li>
<li>Remove the always-on <code>ErrorReportingOnStaticClasses</code> language feature flag. Static-class validation remains unchanged for all supported language versions. (<a href="https://github.com/dotnet/fsharp/issues/20180">Issue #20180</a>, <a href="https://github.com/dotnet/fsharp/pull/20513">PR #20513</a>)</li>
</ul>

<h2 data-fsdocs-heading="5"><a name="11.0" class="anchor" href="#11.0">11.0</a></h2><h3 data-fsdocs-heading="6"><a name="11.0-Added" class="anchor" href="#11.0-Added">Added</a></h3>
<ul>
<li>Simplify implementation of interface hierarchies with equally named abstract slots: when a derived interface provides a Default Interface Member (DIM) implementation for a base interface slot, F# no longer requires explicit interface declarations for the DIM-covered slot. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1430">Language suggestion #1430</a>, <a href="https://github.com/fsharp/fslang-design/pull/826">RFC FS-1336</a>, <a href="https://github.com/dotnet/fsharp/pull/19241">PR #19241</a>)</li>
<li>Support <code>#elif</code> preprocessor directive (<a href="https://github.com/fsharp/fslang-suggestions/issues/1370">Language suggestion #1370</a>, <a href="https://github.com/fsharp/fslang-design/blob/main/RFCs/FS-1334-elif-preprocessor-directive.md">RFC FS-1334</a>, <a href="https://github.com/dotnet/fsharp/pull/XXXXX">PR #XXXXX</a>)</li>
<li>Warn (FS3884) when a function or delegate value is used as an interpolated string argument, since it will be formatted via <code>ToString</code> rather than being applied. (<a href="https://github.com/dotnet/fsharp/pull/19289">PR #19289</a>)</li>
<li>Added <code>MethodOverloadsCache</code> language feature that caches overload resolution results for repeated method calls, significantly improving compilation performance. (<a href="https://github.com/dotnet/fsharp/pull/19072">PR #19072</a>)</li>
<li>Added <code>ErrorOnMissingSignatureAttribute</code> language feature: makes FS3888 (compiler-semantic attribute on the <code>.fs</code> but not on the <code>.fsi</code>) an error instead of a warning. (<a href="https://github.com/dotnet/fsharp/issues/19560">Issue #19560</a>, <a href="https://github.com/dotnet/fsharp/pull/19880">PR #19880</a>)</li>
<li><code>[&lt;OptimizeClosureIfNotInlined&gt;]</code>, paired with <code>[&lt;InlineIfLambda&gt;]</code> on a curried arity 2–5 callback of an inlined function, makes the optimizer adapt the callback once via <code>OptimizedClosures</code> when it is passed opaquely rather than as a known lambda (feature <code>OptimizeClosureIfNotInlined</code>). (<a href="https://github.com/dotnet/fsharp/pull/20422">PR #20422</a>)</li>
<li>Support common types of <code>NotNullIfNotNullAttribute</code> usage. If a method parameter is marked with <code>NotNullIfNotNullAttribute</code>, the compiler will now honor this attribute and mark the return type as non-null. (<a href="https://github.com/dotnet/fsharp/pull/19977">PR #19977</a>)</li>
<li>Spread operator for records (<a href="https://github.com/fsharp/fslang-design/pull/805">RFC FS-1151</a>, <a href="https://github.com/dotnet/fsharp/pull/18927">PR #18927</a>)</li>
<li>Added <code>AccessProtectedBaseFieldFromClosure</code> language feature: a derived member can now read a <code>protected</code> base-class field from an ordinary closure (lambda, delegate, <code>async</code>/<code>seq</code>/<code>lazy</code>, <code>function</code>, or list/array literal), which previously failed with FS1097 even though direct access compiles. Object expressions remain unsupported — bind the field to a local function or expose it through a member. (<a href="https://github.com/dotnet/fsharp/issues/5302">Issue #5302</a>)</li>
<li>Added <code>ImprovedImpliedArgumentNamesPartTwo</code> language feature: when a function with no recoverable parameter names is coerced to a delegate (e.g. a partial application like <code>System.Func&lt;int, int&gt;((+) 1)</code>), the synthesized <code>Invoke</code> parameters take their names from the delegate's own <code>Invoke</code> signature instead of synthetic <code>delegateArg0</code>, <code>delegateArg1</code>, … names. (<a href="https://github.com/dotnet/fsharp/pull/20001">PR #20001</a>)</li>
</ul>
<h3 data-fsdocs-heading="7"><a name="11.0-Fixed" class="anchor" href="#11.0-Fixed">Fixed</a></h3>
<h3 data-fsdocs-heading="8"><a name="11.0-Changed" class="anchor" href="#11.0-Changed">Changed</a></h3>
<ul>
<li>Lines starting with <code>#:</code> are now ignored (<a href="https://github.com/fsharp/fslang-suggestions/issues/1440">Language suggestion 1440</a>, <a href="https://github.com/fsharp/fslang-design/pull/830">RFC FS-1337</a>, <a href="https://github.com/dotnet/fsharp/pull/20212">PR #20212</a>)</li>
<li>Direct delegate construction (<a href="https://github.com/dotnet/fsharp/pull/19993">PR #19993</a>)
<ul>
<li>A delegate built from a method or function now points straight at that method instead of an intermediate closure, so <code>delegate.Method</code> is the real target and no closure class is generated.</li>
<li>Two delegates built from the same method and target now compare equal, where the previous closure form produced distinct instances; this also makes <code>Delegate.Remove</code> (and <code>-=</code> on events) match and remove such a delegate that it previously left in place.</li>
<li>A <code>null</code> instance receiver now faults at delegate construction rather than at the first invoke: an <code>ArgumentException</code> for a non-virtual target (the delegate constructor rejects a null <code>this</code>) or a <code>NullReferenceException</code> for a virtual one (from <code>ldvirtftn</code>), matching how C# builds the same delegate.</li>
</ul>
</li>
</ul>

<h2 data-fsdocs-heading="9"><a name="10.0.200" class="anchor" href="#10.0.200">10.0.200</a></h2><h3 data-fsdocs-heading="10"><a name="10.0.200-Added" class="anchor" href="#10.0.200-Added">Added</a></h3>
<ul>
<li>Add <code>--disableLanguageFeature</code> CLI switch and MSBuild property to selectively disable specific F# language features on a per-project basis. (<a href="https://github.com/dotnet/fsharp/pull/19167">PR #19167</a>)</li>
</ul>

<h2 data-fsdocs-heading="11"><a name="10.0" class="anchor" href="#10.0">10.0</a></h2><h3 data-fsdocs-heading="12"><a name="10.0-Added" class="anchor" href="#10.0-Added">Added</a></h3>
<ul>
<li>Better generic unmanaged structs handling. (<a href="https://github.com/fsharp/fslang-suggestions/issues/692">Language suggestion #692</a>, <a href="https://github.com/dotnet/fsharp/pull/12154">PR #12154</a>)</li>
<li>Deprecate places where <code>seq</code> can be omitted. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1033">Language suggestion #1033</a>, <a href="https://github.com/dotnet/fsharp/pull/17772">PR #17772</a>)</li>
<li>Added type conversions cache, only enabled for compiler runs (<a href="https://github.com/dotnet/fsharp/pull/17668">PR#17668</a>)</li>
<li>Support ValueOption + Struct attribute as optional parameter for methods (<a href="https://github.com/fsharp/fslang-suggestions/issues/1136">Language suggestion #1136</a>, <a href="https://github.com/dotnet/fsharp/pull/18098">PR #18098</a>)</li>
<li>Allow <code>_</code> in <code>use!</code> bindings values (lift FS1228 restriction) (<a href="https://github.com/dotnet/fsharp/pull/18487">PR #18487</a>)</li>
<li>Warn when <code>unit</code> is passed to an <code>obj</code>-typed argument  (<a href="https://github.com/dotnet/fsharp/pull/18330">PR #18330</a>)</li>
<li>Fix parsing errors using anonymous records and units of measures (<a href="https://github.com/dotnet/fsharp/pull/18543">PR #18543</a>)</li>
<li>Scoped Nowarn: added the #warnon compiler directive (<a href="https://github.com/fsharp/fslang-suggestions/issues/278">Language suggestion #278</a>, <a href="https://github.com/fsharp/fslang-design/pull/782">RFC FS-1146 PR</a>, <a href="https://github.com/dotnet/fsharp/pull/18049">PR #18049</a>)</li>
<li>Allow <code>let!</code>, <code>use!</code>, <code>and!</code> type annotations without requiring parentheses ((<a href="https://github.com/dotnet/fsharp/pull/18508">PR #18508</a> and <a href="https://github.com/dotnet/fsharp/pull/18682">PR #18682</a>))</li>
<li>Exception names are now validated for illegal characters using the same mechanism as types/modules/namespaces (<a href="https://github.com/dotnet/fsharp/issues/18763">Issue #18763</a>, <a href="https://github.com/dotnet/fsharp/pull/18768">PR #18768</a>)</li>
<li>Support tail calls in computation expressions (<a href="https://github.com/dotnet/fsharp/pull/18804">PR #18804</a>)</li>
</ul>
<h3 data-fsdocs-heading="13"><a name="10.0-Fixed" class="anchor" href="#10.0-Fixed">Fixed</a></h3>
<ul>
<li>Warn on uppercase identifiers in patterns. (<a href="https://github.com/dotnet/fsharp/pull/15816">PR #15816</a>)</li>
<li>Error on invalid declarations in type definitions.(<a href="https://github.com/dotnet/fsharp/issues/10066">Issue #10066</a>, <a href="https://github.com/dotnet/fsharp/pull/18813">PR #18813</a>)</li>
<li>Fix type erasure logic for <code>nativeptr&lt;'T&gt;</code> overloads to properly preserve element type differences during duplicate member checking. (<a href="https://github.com/dotnet/fsharp/pull/18911">PR #18911</a>)</li>
</ul>
<h3 data-fsdocs-heading="14"><a name="10.0-Changed" class="anchor" href="#10.0-Changed">Changed</a></h3>
<ul>
<li>Removed parsing support for long-deprecated ML constructs and non-light syntax. (<a href="https://github.com/dotnet/fsharp/pull/19143">PR #19143</a>)</li>
<li>Released <code>asr</code>, <code>land</code>, <code>lor</code>, <code>lsl</code>, <code>lsr</code> and <code>lxor</code> as usable keywords (note: <code>mod</code> continues to be reserved). (<a href="https://github.com/dotnet/fsharp/pull/19143">PR #19143</a>)</li>
</ul>

<h2 data-fsdocs-heading="15"><a name="9.0" class="anchor" href="#9.0">9.0</a></h2><h3 data-fsdocs-heading="16"><a name="9.0-Added" class="anchor" href="#9.0-Added">Added</a></h3>
<ul>
<li>Speed up <code>for x in xs -&gt; …</code> in list &amp; array comprehensions in certain scenarios. (<a href="https://github.com/dotnet/fsharp/pull/16948">PR #16948</a>)</li>
<li>Lower integral ranges to fast loops in more cases and optimize list and array construction from ranges. (<a href="https://github.com/dotnet/fsharp/pull/16650">PR #16650</a>, <a href="https://github.com/dotnet/fsharp/pull/16832">PR #16832</a>)</li>
<li>Support for nullable reference types (<a href="https://github.com/dotnet/fsharp/pull/15181">PR #15181</a>)</li>
<li>Bidirectional F#/C# interop for 'unmanaged' constraint. (<a href="https://github.com/dotnet/fsharp/pull/12154">PR #12154</a>)</li>
<li>Make <code>.Is*</code> discriminated union properties visible. (<a href="https://github.com/fsharp/fslang-suggestions/issues/222">Language suggestion #222</a>, <a href="https://github.com/dotnet/fsharp/pull/16341">PR #16341</a>)</li>
<li>Allow returning bool instead of unit option for partial active patterns. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1041">Language suggestion #1041</a>, <a href="https://github.com/dotnet/fsharp/pull/16473">PR #16473</a>)</li>
<li>Allow access modifies to auto properties getters and setters (<a href="https://github.com/fsharp/fslang-suggestions/issues/430">Language suggestion #430</a>, <a href="https://github.com/dotnet/fsharp/pull/16687">PR 16687</a>, <a href="https://github.com/dotnet/fsharp/pull/16861">PR 16861</a>, <a href="https://github.com/dotnet/fsharp/pull/17522">PR 17522</a>)</li>
<li>Allow #nowarn to support the FS prefix on error codes to disable warnings (<a href="https://github.com/dotnet/fsharp/issues/16447">Issue #17206</a>, <a href="https://github.com/dotnet/fsharp/pull/17209">PR #17209</a>)</li>
<li>Allow ParsedHashDirectives to have argument types other than strings (<a href="https://github.com/dotnet/fsharp/issues/16447">Issue #17240</a>, <a href="https://github.com/dotnet/fsharp/pull/17209">PR #17209</a>)</li>
<li>Support empty-bodied computation expressions. (<a href="https://github.com/fsharp/fslang-suggestions/issues/1232">Language suggestion #1232</a>, <a href="https://github.com/dotnet/fsharp/pull/17352">PR #17352</a>)</li>
<li>Allow object expression without overrides. (<a href="https://github.com/fsharp/fslang-suggestions/issues/632">Language suggestion #632</a>, <a href="https://github.com/dotnet/fsharp/pull/17387">PR #17387</a>)</li>
<li>Enable FSharp 9.0 Language Version (<a href="https://github.com/dotnet/fsharp/issues/17438">Issue #17497</a>), <a href="https://github.com/dotnet/fsharp/pull/17500">PR</a>))</li>
</ul>
<h3 data-fsdocs-heading="17"><a name="9.0-Fixed" class="anchor" href="#9.0-Fixed">Fixed</a></h3>
<ul>
<li>Allow extension methods without type attribute work for types from imported assemblies. (<a href="https://github.com/dotnet/fsharp/pull/16368">PR #16368</a>)</li>
<li>Enforce AttributeTargets on let values and functions. (<a href="https://github.com/dotnet/fsharp/pull/16692">PR #16692</a>)</li>
<li>Enforce AttributeTargets on union case declarations. (<a href="https://github.com/dotnet/fsharp/pull/16764">PR #16764</a>)</li>
<li>Enforce AttributeTargets on implicit constructors. (<a href="https://github.com/dotnet/fsharp/pull/16845/">PR #16845</a>)</li>
<li>Enforce AttributeTargets on structs and classes (<a href="https://github.com/dotnet/fsharp/pull/16790">PR #16790</a>)</li>
<li>Ensure consistent interaction between ``#line<code>and</code>#nowarn` directives (<a href="https://github.com/dotnet/fsharp/pull/17649">PR #17649</a>)</li>
<li>Revert EnforceAttributeTargets Feature. (<a href="https://github.com/dotnet/fsharp/pull/18005">PR #18005</a>)</li>
</ul>
<h3 data-fsdocs-heading="18"><a name="9.0-Changed" class="anchor" href="#9.0-Changed">Changed</a></h3>
<ul>
<li>Lower interpolated strings to string concatenation. (<a href="https://github.com/dotnet/fsharp/pull/16556">PR #16556</a>)</li>
</ul>

<h2 data-fsdocs-heading="19"><a name="8.0" class="anchor" href="#8.0">8.0</a></h2><h3 data-fsdocs-heading="20"><a name="8.0-Fixed" class="anchor" href="#8.0-Fixed">Fixed</a></h3>
<ul>
<li>Disallow using base to invoke an abstract base method (<a href="https://github.com/dotnet/fsharp/issues/13926">Issue #13926</a>, <a href="https://github.com/dotnet/fsharp/pull/16773">PR #16773</a>)</li>
</ul>
<h3 data-fsdocs-heading="21"><a name="8.0-Added" class="anchor" href="#8.0-Added">Added</a></h3>
<ul>
<li><code>while!</code> (<a href="https://github.com/fsharp/fslang-suggestions/issues/1038">Language suggestion #1038</a>, <a href="https://github.com/dotnet/fsharp/pull/14238">PR #14238</a>)</li>
</ul>
