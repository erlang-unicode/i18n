

#Module i18n_collation#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`arcusfelis@gmail.com`](mailto:arcusfelis@gmail.com)).
<a name="types"></a>

##Data Types##




###<a name="type-i18n_collation_option">i18n_collation_option()</a>##



<pre>i18n_collation_option() = primary | secondary | tertiary | quantiary | identical | shifted | non_ignorable | lower_first | upper_first | numeric | french_accents | hiragana | normalization</pre>



###<a name="type-i18n_collator">i18n_collator()</a>##



<pre>i18n_collator() = <a href="#type-resource">resource()</a></pre>



###<a name="type-i18n_compare_result">i18n_compare_result()</a>##



<pre>i18n_compare_result() = less | equal | greater</pre>



###<a name="type-i18n_locale_id">i18n_locale_id()</a>##



<pre>i18n_locale_id() = atom()</pre>



###<a name="type-i18n_sort_key">i18n_sort_key()</a>##



<pre>i18n_sort_key() = binary()</pre>



###<a name="type-i18n_string">i18n_string()</a>##



<pre>i18n_string() = binary()</pre>



###<a name="type-resource">resource()</a>##



<pre>resource() = &lt;&lt;&gt;&gt;</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#available_locales-0">available_locales/0</a></td><td></td></tr><tr><td valign="top"><a href="#compare-3">compare/3</a></td><td></td></tr><tr><td valign="top"><a href="#map_sort-2">map_sort/2</a></td><td>Sort a list.</td></tr><tr><td valign="top"><a href="#open-0">open/0</a></td><td></td></tr><tr><td valign="top"><a href="#open-1">open/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td></td></tr><tr><td valign="top"><a href="#open_rules-1">open_rules/1</a></td><td>Get a rules-based collator.</td></tr><tr><td valign="top"><a href="#open_rules-2">open_rules/2</a></td><td>Get a rule-based collator with options.</td></tr><tr><td valign="top"><a href="#sort-2">sort/2</a></td><td></td></tr><tr><td valign="top"><a href="#sort_key-2">sort_key/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="available_locales-0"></a>

###available_locales/0##




<pre>available_locales() -> [<a href="#type-i18n_locale_id">i18n_locale_id()</a>]</pre>
<br></br>


<a name="compare-3"></a>

###compare/3##




<pre>compare(C::<a href="#type-i18n_collator">i18n_collator()</a>, S1::<a href="#type-i18n_string">i18n_string()</a>, S2::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_compare_result">i18n_compare_result()</a></pre>
<br></br>


<a name="map_sort-2"></a>

###map_sort/2##




<pre>map_sort(F::function(), Xx::[any()]) -&gt; [any()]</pre>
<br></br>






Sort a list.

Xx is a list of strings to sort.
F produces a sort keys.<a name="open-0"></a>

###open/0##




<pre>open() -> <a href="#type-i18n_collator">i18n_collator()</a></pre>
<br></br>


<a name="open-1"></a>

###open/1##




<pre>open(L::<a href="#type-i18n_locale_id">i18n_locale_id()</a>) -> <a href="#type-i18n_collator">i18n_collator()</a></pre>
<br></br>


<a name="open-2"></a>

###open/2##




<pre>open(L::<a href="#type-i18n_locale_id">i18n_locale_id()</a>, Options::[<a href="#type-i18n_collation_option">i18n_collation_option()</a>]) -> <a href="#type-i18n_collator">i18n_collator()</a></pre>
<br></br>


<a name="open_rules-1"></a>

###open_rules/1##




<pre>open_rules(Rules::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_collator">i18n_collator()</a></pre>
<br></br>




Get a rules-based collator.<a name="open_rules-2"></a>

###open_rules/2##




<pre>open_rules(Rules::<a href="#type-i18n_string">i18n_string()</a>, Options::[<a href="#type-i18n_collation_option">i18n_collation_option()</a>]) -> <a href="#type-i18n_collator">i18n_collator()</a></pre>
<br></br>




Get a rule-based collator with options.
[More information](http://userguide.icu-project.org/collation/customization)<a name="sort-2"></a>

###sort/2##




<pre>sort(C::<a href="#type-i18n_collator">i18n_collator()</a>, Ss::[<a href="#type-i18n_string">i18n_string()</a>]) -> [<a href="#type-i18n_string">i18n_string()</a>]</pre>
<br></br>


<a name="sort_key-2"></a>

###sort_key/2##




<pre>sort_key(C::<a href="#type-i18n_collator">i18n_collator()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_sort_key">i18n_sort_key()</a></pre>
<br></br>


