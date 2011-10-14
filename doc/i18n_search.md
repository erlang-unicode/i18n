Module i18n_search
==================


<h1>Module i18n_search</h1>

* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`freeakk@gmail.com`](mailto:freeakk@gmail.com)).


<h2><a name="types">Data Types</a></h2>





<h3 class="typedecl"><a name="type-i18n_collator">i18n_collator()</a></h3>




<pre>i18n_collator() = <a href="#type-resource">resource()</a></pre>



<h3 class="typedecl"><a name="type-i18n_searcher">i18n_searcher()</a></h3>




<pre>i18n_searcher() = <a href="#type-resource">resource()</a></pre>



<h3 class="typedecl"><a name="type-i18n_string">i18n_string()</a></h3>




<pre>i18n_string() = binary()</pre>



<h3 class="typedecl"><a name="type-resource">resource()</a></h3>




<pre>resource() = <<>></pre>


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#index-2">index/2</a></td><td></td></tr><tr><td valign="top"><a href="#match-2">match/2</a></td><td></td></tr><tr><td valign="top"><a href="#match_all-2">match_all/2</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td></td></tr><tr><td valign="top"><a href="#test-2">test/2</a></td><td>Test matches.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="index-2"></a>

<h3>index/2</h3>





<pre>index(Searcher::<a href="#type-i18n_searcher">i18n_searcher()</a>, String::<a href="#type-i18n_string">i18n_string()</a>) -> [{Start::non_neg_integer(), Length::non_neg_integer()}]</pre>
<br></br>


<a name="match-2"></a>

<h3>match/2</h3>





<pre>match(Searcher::<a href="#type-i18n_searcher">i18n_searcher()</a>, String::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a> | false</pre>
<br></br>


<a name="match_all-2"></a>

<h3>match_all/2</h3>





<pre>match_all(Searcher::<a href="#type-i18n_searcher">i18n_searcher()</a>, String::<a href="#type-i18n_string">i18n_string()</a>) -> [<a href="#type-i18n_string">i18n_string()</a>]</pre>
<br></br>


<a name="open-2"></a>

<h3>open/2</h3>





<pre>open(Col::<a href="#type-i18n_collator">i18n_collator()</a>, Pattern::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_searcher">i18n_searcher()</a></pre>
<br></br>


<a name="test-2"></a>

<h3>test/2</h3>





<pre>test(Searcher::<a href="#type-i18n_searcher">i18n_searcher()</a>, String::<a href="#type-i18n_string">i18n_string()</a>) -> boolean()</pre>
<br></br>




Test matches.