

#Module i18n_transliteration#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Text Trasliteration.



Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`arcusfelis@gmail.com`](mailto:arcusfelis@gmail.com)).
<a name="types"></a>

##Data Types##




###<a name="type-i18n_direction">i18n_direction()</a>##



<pre>i18n_direction() = forward | reverse</pre>



###<a name="type-i18n_string">i18n_string()</a>##



<pre>i18n_string() = binary()</pre>



###<a name="type-i18n_transliterator">i18n_transliterator()</a>##



<pre>i18n_transliterator() = <a href="#type-resource">resource()</a></pre>



###<a name="type-i18n_transliterator_id">i18n_transliterator_id()</a>##



<pre>i18n_transliterator_id() = atom()</pre>



###<a name="type-resource">resource()</a>##



<pre>resource() = &lt;&lt;&gt;&gt;</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#available_ids-0">available_ids/0</a></td><td></td></tr><tr><td valign="top"><a href="#do-2">do/2</a></td><td></td></tr><tr><td valign="top"><a href="#open-1">open/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="available_ids-0"></a>

###available_ids/0##




<pre>available_ids() -> [<a href="#type-i18n_transliterator_id">i18n_transliterator_id()</a>]</pre>
<br></br>


<a name="do-2"></a>

###do/2##




<pre>do(T::<a href="#type-i18n_transliterator">i18n_transliterator()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="open-1"></a>

###open/1##




<pre>open(Id::<a href="#type-i18n_transliterator_id">i18n_transliterator_id()</a>) -> <a href="#type-i18n_transliterator">i18n_transliterator()</a></pre>
<br></br>


<a name="open-2"></a>

###open/2##




<pre>open(Id::<a href="#type-i18n_transliterator_id">i18n_transliterator_id()</a>, Dir::<a href="#type-i18n_direction">i18n_direction()</a>) -> <a href="#type-i18n_transliterator">i18n_transliterator()</a></pre>
<br></br>


