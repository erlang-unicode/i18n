

#Module i18n_transliteration#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Text Trasliteration.



Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`freeakk@gmail.com`](mailto:freeakk@gmail.com)).
<a name="types"></a>

##Data Types##




###<a name="type-i18n_direction">i18n_direction()</a>##



<pre>i18n_direction() = forward | reverse</pre>



###<a name="type-i18n_string">i18n_string()</a>##



<pre>i18n_string() = binary()</pre>



###<a name="type-i18n_transliterator">i18n_transliterator()</a>##



<pre>i18n_transliterator() = [resource()](#type-resource)</pre>



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




<pre>available_ids() -&gt; [[i18n_transliterator_id()](#type-i18n_transliterator_id)]</pre>
<br></br>


<a name="do-2"></a>

###do/2##




<pre>do(T::[i18n_transliterator()](#type-i18n_transliterator), S::[i18n_string()](#type-i18n_string)) -&gt; [i18n_string()](#type-i18n_string)</pre>
<br></br>


<a name="open-1"></a>

###open/1##




<pre>open(Id::[i18n_transliterator_id()](#type-i18n_transliterator_id)) -&gt; [i18n_transliterator()](#type-i18n_transliterator)</pre>
<br></br>


<a name="open-2"></a>

###open/2##




<pre>open(Id::[i18n_transliterator_id()](#type-i18n_transliterator_id), Dir::[i18n_direction()](#type-i18n_direction)) -&gt; [i18n_transliterator()](#type-i18n_transliterator)</pre>
<br></br>


