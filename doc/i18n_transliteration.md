Module i18n_transliteration
===========================


<h1>Module i18n_transliteration</h1>

* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Text Trasliteration.



Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`freeakk@gmail.com`](mailto:freeakk@gmail.com)).


<h2><a name="types">Data Types</a></h2>





<h3 class="typedecl"><a name="type-i18n_direction">i18n_direction()</a></h3>




<pre>i18n_direction() = forward | reverse</pre>



<h3 class="typedecl"><a name="type-i18n_string">i18n_string()</a></h3>




<pre>i18n_string() = binary()</pre>



<h3 class="typedecl"><a name="type-i18n_transliterator">i18n_transliterator()</a></h3>




<pre>i18n_transliterator() = <a href="#type-resource">resource()</a></pre>



<h3 class="typedecl"><a name="type-i18n_transliterator_id">i18n_transliterator_id()</a></h3>




<pre>i18n_transliterator_id() = atom()</pre>



<h3 class="typedecl"><a name="type-resource">resource()</a></h3>




<pre>resource() = <<>></pre>


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#available_ids-0">available_ids/0</a></td><td></td></tr><tr><td valign="top"><a href="#do-2">do/2</a></td><td></td></tr><tr><td valign="top"><a href="#open-1">open/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="available_ids-0"></a>

<h3>available_ids/0</h3>





<pre>available_ids() -> [<a href="#type-i18n_transliterator_id">i18n_transliterator_id()</a>]</pre>
<br></br>


<a name="do-2"></a>

<h3>do/2</h3>





<pre>do(T::<a href="#type-i18n_transliterator">i18n_transliterator()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="open-1"></a>

<h3>open/1</h3>





<pre>open(Id::<a href="#type-i18n_transliterator_id">i18n_transliterator_id()</a>) -> <a href="#type-i18n_transliterator">i18n_transliterator()</a></pre>
<br></br>


<a name="open-2"></a>

<h3>open/2</h3>





<pre>open(Id::<a href="#type-i18n_transliterator_id">i18n_transliterator_id()</a>, Dir::<a href="#type-i18n_direction">i18n_direction()</a>) -> <a href="#type-i18n_transliterator">i18n_transliterator()</a></pre>
<br></br>


