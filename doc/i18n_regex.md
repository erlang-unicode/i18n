

#Module i18n_regex#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`arcusfelis@gmail.com`](mailto:arcusfelis@gmail.com)).
<a name="types"></a>

##Data Types##




###<a name="type-i18n_regex">i18n_regex()</a>##



<pre>i18n_regex() = <a href="#type-resource">resource()</a></pre>



###<a name="type-i18n_string">i18n_string()</a>##



<pre>i18n_string() = binary()</pre>



###<a name="type-resource">resource()</a>##



<pre>resource() = &lt;&lt;&gt;&gt;</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#match-2">match/2</a></td><td>Return first match as a list.</td></tr><tr><td valign="top"><a href="#match_all-2">match_all/2</a></td><td>Return all matches as a list of lists.</td></tr><tr><td valign="top"><a href="#open-1">open/1</a></td><td>Parse a message to a resourse.</td></tr><tr><td valign="top"><a href="#replace-3">replace/3</a></td><td>Replace first element of the text by the pattern.</td></tr><tr><td valign="top"><a href="#replace_all-3">replace_all/3</a></td><td>Replace all finded elements.</td></tr><tr><td valign="top"><a href="#split-2">split/2</a></td><td>Split a string to a list.</td></tr><tr><td valign="top"><a href="#test-2">test/2</a></td><td>Test matches.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="match-2"></a>

###match/2##




<pre>match(Re::<a href="#type-i18n_regex">i18n_regex()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> [<a href="#type-i18n_string">i18n_string()</a>]</pre>
<br></br>




Return first match as a list.<a name="match_all-2"></a>

###match_all/2##




<pre>match_all(Re::<a href="#type-i18n_regex">i18n_regex()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> [[<a href="#type-i18n_string">i18n_string()</a>]]</pre>
<br></br>




Return all matches as a list of lists.<a name="open-1"></a>

###open/1##




<pre>open(S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_regex">i18n_regex()</a></pre>
<br></br>




Parse a message to a resourse.<a name="replace-3"></a>

###replace/3##




<pre>replace(Re::<a href="#type-i18n_regex">i18n_regex()</a>, Pattern::<a href="#type-i18n_string">i18n_string()</a>, Source::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>




Replace first element of the text by the pattern.<a name="replace_all-3"></a>

###replace_all/3##




<pre>replace_all(Re::<a href="#type-i18n_regex">i18n_regex()</a>, Pattern::<a href="#type-i18n_string">i18n_string()</a>, Source::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>




Replace all finded elements.<a name="split-2"></a>

###split/2##




<pre>split(Re::<a href="#type-i18n_regex">i18n_regex()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> [<a href="#type-i18n_string">i18n_string()</a>]</pre>
<br></br>




Split a string to a list.<a name="test-2"></a>

###test/2##




<pre>test(Re::<a href="#type-i18n_regex">i18n_regex()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> boolean()</pre>
<br></br>




Test matches.