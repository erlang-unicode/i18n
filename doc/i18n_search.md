

#Module i18n_search#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`freeakk@gmail.com`](mailto:freeakk@gmail.com)).
<a name="types"></a>

##Data Types##




###<a name="type-i18n_collator">i18n_collator()</a>##



<pre>i18n_collator() = [resource()](#type-resource)</pre>



###<a name="type-i18n_searcher">i18n_searcher()</a>##



<pre>i18n_searcher() = [resource()](#type-resource)</pre>



###<a name="type-i18n_string">i18n_string()</a>##



<pre>i18n_string() = binary()</pre>



###<a name="type-resource">resource()</a>##



<pre>resource() = &lt;&lt;&gt;&gt;</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#index-2">index/2</a></td><td></td></tr><tr><td valign="top"><a href="#match-2">match/2</a></td><td></td></tr><tr><td valign="top"><a href="#match_all-2">match_all/2</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td></td></tr><tr><td valign="top"><a href="#test-2">test/2</a></td><td>Test matches.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="index-2"></a>

###index/2##




<pre>index(Searcher::[i18n_searcher()](#type-i18n_searcher), String::[i18n_string()](#type-i18n_string)) -&gt; [{Start::non_neg_integer(), Length::non_neg_integer()}]</pre>
<br></br>


<a name="match-2"></a>

###match/2##




<pre>match(Searcher::[i18n_searcher()](#type-i18n_searcher), String::[i18n_string()](#type-i18n_string)) -&gt; [i18n_string()](#type-i18n_string) | false</pre>
<br></br>


<a name="match_all-2"></a>

###match_all/2##




<pre>match_all(Searcher::[i18n_searcher()](#type-i18n_searcher), String::[i18n_string()](#type-i18n_string)) -&gt; [[i18n_string()](#type-i18n_string)]</pre>
<br></br>


<a name="open-2"></a>

###open/2##




<pre>open(Col::[i18n_collator()](#type-i18n_collator), Pattern::[i18n_string()](#type-i18n_string)) -&gt; [i18n_searcher()](#type-i18n_searcher)</pre>
<br></br>


<a name="test-2"></a>

###test/2##




<pre>test(Searcher::[i18n_searcher()](#type-i18n_searcher), String::[i18n_string()](#type-i18n_string)) -&gt; boolean()</pre>
<br></br>




Test matches.