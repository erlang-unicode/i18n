Module i18n_collation
=====================


<h1>Module i18n_collation</h1>

* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`freeakk@gmail.com`](mailto:freeakk@gmail.com)).


<h2><a name="types">Data Types</a></h2>





<h3 class="typedecl"><a name="type-i18n_collation_option">i18n_collation_option()</a></h3>




<pre>i18n_collation_option() = primary | secondary | tertiary | quantiary | identical | shifted | non_ignorable | lower_first | upper_first | numeric | french_accents | hiragana | normalization</pre>



<h3 class="typedecl"><a name="type-i18n_collator">i18n_collator()</a></h3>




<pre>i18n_collator() = <a href="#type-resource">resource()</a></pre>



<h3 class="typedecl"><a name="type-i18n_compare_result">i18n_compare_result()</a></h3>




<pre>i18n_compare_result() = less | equal | greater</pre>



<h3 class="typedecl"><a name="type-i18n_locale_id">i18n_locale_id()</a></h3>




<pre>i18n_locale_id() = atom()</pre>



<h3 class="typedecl"><a name="type-i18n_sort_key">i18n_sort_key()</a></h3>




<pre>i18n_sort_key() = binary()</pre>



<h3 class="typedecl"><a name="type-i18n_string">i18n_string()</a></h3>




<pre>i18n_string() = binary()</pre>



<h3 class="typedecl"><a name="type-resource">resource()</a></h3>




<pre>resource() = <<>></pre>


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#available_locales-0">available_locales/0</a></td><td></td></tr><tr><td valign="top"><a href="#compare-3">compare/3</a></td><td></td></tr><tr><td valign="top"><a href="#map_sort-2">map_sort/2</a></td><td>Sort a list.</td></tr><tr><td valign="top"><a href="#open-0">open/0</a></td><td></td></tr><tr><td valign="top"><a href="#open-1">open/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td></td></tr><tr><td valign="top"><a href="#sort-2">sort/2</a></td><td></td></tr><tr><td valign="top"><a href="#sort_key-2">sort_key/2</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="available_locales-0"></a>

<h3>available_locales/0</h3>





<pre>available_locales() -> [<a href="#type-i18n_locale_id">i18n_locale_id()</a>]</pre>
<br></br>


<a name="compare-3"></a>

<h3>compare/3</h3>





<pre>compare(C::<a href="#type-i18n_collator">i18n_collator()</a>, S1::<a href="#type-i18n_string">i18n_string()</a>, S2::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_compare_result">i18n_compare_result()</a></pre>
<br></br>


<a name="map_sort-2"></a>

<h3>map_sort/2</h3>





<pre>map_sort(F::function(), Xx::[any()]) -> [any()]</pre>
<br></br>






Sort a list.

Xx is a list of strings to sort.
F produces a sort keys.<a name="open-0"></a>

<h3>open/0</h3>





<pre>open() -> <a href="#type-i18n_collator">i18n_collator()</a></pre>
<br></br>


<a name="open-1"></a>

<h3>open/1</h3>





<pre>open(L::<a href="#type-i18n_locale_id">i18n_locale_id()</a>) -> <a href="#type-i18n_collator">i18n_collator()</a></pre>
<br></br>


<a name="open-2"></a>

<h3>open/2</h3>





<pre>open(L::<a href="#type-i18n_locale_id">i18n_locale_id()</a>, Options::[<a href="#type-i18n_collation_option">i18n_collation_option()</a>]) -> <a href="#type-i18n_collator">i18n_collator()</a></pre>
<br></br>


<a name="sort-2"></a>

<h3>sort/2</h3>





<pre>sort(C::<a href="#type-i18n_collator">i18n_collator()</a>, Ss::[<a href="#type-i18n_string">i18n_string()</a>]) -> [<a href="#type-i18n_string">i18n_string()</a>]</pre>
<br></br>


<a name="sort_key-2"></a>

<h3>sort_key/2</h3>





<pre>sort_key(C::<a href="#type-i18n_collator">i18n_collator()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_sort_key">i18n_sort_key()</a></pre>
<br></br>


