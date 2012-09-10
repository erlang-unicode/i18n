

#Module i18n_iterator#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Text Boundary Analysis (Break Iteration).



Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`arcusfelis@gmail.com`](mailto:arcusfelis@gmail.com)).
<a name="types"></a>

##Data Types##




###<a name="type-i18n_iterator">i18n_iterator()</a>##



<pre>i18n_iterator() = <a href="#type-resource">resource()</a></pre>



###<a name="type-i18n_locale_id">i18n_locale_id()</a>##



<pre>i18n_locale_id() = atom()</pre>



###<a name="type-i18n_string_iterator_type">i18n_string_iterator_type()</a>##



<pre>i18n_string_iterator_type() = grapheme | word | sentence | line | word_only</pre>



###<a name="type-resource">resource()</a>##



<pre>resource() = &lt;&lt;&gt;&gt;</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#available_locales-0">available_locales/0</a></td><td></td></tr><tr><td valign="top"><a href="#open-1">open/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="available_locales-0"></a>

###available_locales/0##




<pre>available_locales() -> [<a href="#type-i18n_locale_id">i18n_locale_id()</a>]</pre>
<br></br>


<a name="open-1"></a>

###open/1##




<pre>open(T::<a href="#type-i18n_string_iterator_type">i18n_string_iterator_type()</a>) -> <a href="#type-i18n_iterator">i18n_iterator()</a></pre>
<br></br>


<a name="open-2"></a>

###open/2##




<pre>open(L::<a href="#type-i18n_locale_id">i18n_locale_id()</a>, T::<a href="#type-i18n_string_iterator_type">i18n_string_iterator_type()</a>) -> <a href="#type-i18n_iterator">i18n_iterator()</a></pre>
<br></br>


