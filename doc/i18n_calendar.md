Module i18n_calendar
====================


<h1>Module i18n_calendar</h1>

* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`freeakk@gmail.com`](mailto:freeakk@gmail.com)).


<h2><a name="types">Data Types</a></h2>





<h3 class="typedecl"><a name="type-i18n_calendar">i18n_calendar()</a></h3>




<pre>i18n_calendar() = <a href="#type-resource">resource()</a></pre>



<h3 class="typedecl"><a name="type-i18n_calendar_type">i18n_calendar_type()</a></h3>




<pre>i18n_calendar_type() = gregorian | traditional</pre>



<h3 class="typedecl"><a name="type-i18n_locale_id">i18n_locale_id()</a></h3>




<pre>i18n_locale_id() = atom()</pre>



<h3 class="typedecl"><a name="type-i18n_string">i18n_string()</a></h3>




<pre>i18n_string() = binary()</pre>



<h3 class="typedecl"><a name="type-i18n_timezone">i18n_timezone()</a></h3>




<pre>i18n_timezone() = atom() | <a href="#type-i18n_string">i18n_string()</a></pre>



<h3 class="typedecl"><a name="type-resource">resource()</a></h3>




<pre>resource() = <<>></pre>


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#available_locales-0">available_locales/0</a></td><td></td></tr><tr><td valign="top"><a href="#open-0">open/0</a></td><td></td></tr><tr><td valign="top"><a href="#open-1">open/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td></td></tr><tr><td valign="top"><a href="#open-3">open/3</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="available_locales-0"></a>

<h3>available_locales/0</h3>





<pre>available_locales() -> [<a href="#type-i18n_locale_id">i18n_locale_id()</a>]</pre>
<br></br>


<a name="open-0"></a>

<h3>open/0</h3>





<pre>open() -> <a href="#type-i18n_calendar">i18n_calendar()</a></pre>
<br></br>


<a name="open-1"></a>

<h3>open/1</h3>





<pre>open(Locale::<a href="#type-i18n_locale_id">i18n_locale_id()</a>) -> <a href="#type-i18n_calendar">i18n_calendar()</a></pre>
<br></br>


<a name="open-2"></a>

<h3>open/2</h3>





<pre>open(Locale::<a href="#type-i18n_locale_id">i18n_locale_id()</a>, TZ::<a href="#type-i18n_timezone">i18n_timezone()</a>) -> <a href="#type-i18n_calendar">i18n_calendar()</a></pre>
<br></br>


<a name="open-3"></a>

<h3>open/3</h3>





<pre>open(Locale::<a href="#type-i18n_locale_id">i18n_locale_id()</a>, TZ::<a href="#type-i18n_timezone">i18n_timezone()</a>, Type::<a href="#type-i18n_calendar_type">i18n_calendar_type()</a>) -> <a href="#type-i18n_calendar">i18n_calendar()</a></pre>
<br></br>


