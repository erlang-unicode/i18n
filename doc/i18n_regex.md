Module i18n_regex
=================


<h1>Module i18n_regex</h1>

* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`freeakk@gmail.com`](mailto:freeakk@gmail.com)).


<h2><a name="types">Data Types</a></h2>





<h3 class="typedecl"><a name="type-i18n_regex">i18n_regex()</a></h3>




<pre>i18n_regex() = <a href="#type-resource">resource()</a></pre>



<h3 class="typedecl"><a name="type-i18n_string">i18n_string()</a></h3>




<pre>i18n_string() = binary()</pre>



<h3 class="typedecl"><a name="type-resource">resource()</a></h3>




<pre>resource() = <<>></pre>


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#match-2">match/2</a></td><td>Return first match as a list.</td></tr><tr><td valign="top"><a href="#match_all-2">match_all/2</a></td><td>Return all matches as a list of lists.</td></tr><tr><td valign="top"><a href="#open-1">open/1</a></td><td>Parse a message to a resourse.</td></tr><tr><td valign="top"><a href="#replace-3">replace/3</a></td><td>Replace first element of the text by the pattern.</td></tr><tr><td valign="top"><a href="#replace_all-3">replace_all/3</a></td><td>Replace all finded elements.</td></tr><tr><td valign="top"><a href="#split-2">split/2</a></td><td>Split a string to a list.</td></tr><tr><td valign="top"><a href="#test-2">test/2</a></td><td>Test matches.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="match-2"></a>

<h3>match/2</h3>





<pre>match(Re::<a href="#type-i18n_regex">i18n_regex()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> [<a href="#type-i18n_string">i18n_string()</a>]</pre>
<br></br>




Return first match as a list.<a name="match_all-2"></a>

<h3>match_all/2</h3>





<pre>match_all(Re::<a href="#type-i18n_regex">i18n_regex()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> [[<a href="#type-i18n_string">i18n_string()</a>]]</pre>
<br></br>




Return all matches as a list of lists.<a name="open-1"></a>

<h3>open/1</h3>





<pre>open(S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_regex">i18n_regex()</a></pre>
<br></br>




Parse a message to a resourse.<a name="replace-3"></a>

<h3>replace/3</h3>





<pre>replace(Re::<a href="#type-i18n_regex">i18n_regex()</a>, Pattern::<a href="#type-i18n_string">i18n_string()</a>, Source::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>




Replace first element of the text by the pattern.<a name="replace_all-3"></a>

<h3>replace_all/3</h3>





<pre>replace_all(Re::<a href="#type-i18n_regex">i18n_regex()</a>, Pattern::<a href="#type-i18n_string">i18n_string()</a>, Source::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>




Replace all finded elements.<a name="split-2"></a>

<h3>split/2</h3>





<pre>split(Re::<a href="#type-i18n_regex">i18n_regex()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> [<a href="#type-i18n_string">i18n_string()</a>]</pre>
<br></br>




Split a string to a list.<a name="test-2"></a>

<h3>test/2</h3>





<pre>test(Re::<a href="#type-i18n_regex">i18n_regex()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> boolean()</pre>
<br></br>




Test matches.