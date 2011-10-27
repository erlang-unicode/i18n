

#Module i18n_calendar#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`freeakk@gmail.com`](mailto:freeakk@gmail.com)).
<a name="types"></a>

##Data Types##




###<a name="type-i18n_calendar">i18n_calendar()</a>##



<pre>i18n_calendar() = [resource()](#type-resource)</pre>



###<a name="type-i18n_calendar_type">i18n_calendar_type()</a>##



<pre>i18n_calendar_type() = gregorian | traditional</pre>



###<a name="type-i18n_locale_id">i18n_locale_id()</a>##



<pre>i18n_locale_id() = atom()</pre>



###<a name="type-i18n_string">i18n_string()</a>##



<pre>i18n_string() = binary()</pre>



###<a name="type-i18n_timezone">i18n_timezone()</a>##



<pre>i18n_timezone() = atom() | [i18n_string()](#type-i18n_string)</pre>



###<a name="type-resource">resource()</a>##



<pre>resource() = &lt;&lt;&gt;&gt;</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#available_locales-0">available_locales/0</a></td><td></td></tr><tr><td valign="top"><a href="#open-0">open/0</a></td><td></td></tr><tr><td valign="top"><a href="#open-1">open/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td></td></tr><tr><td valign="top"><a href="#open-3">open/3</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="available_locales-0"></a>

###available_locales/0##




<pre>available_locales() -&gt; [[i18n_locale_id()](#type-i18n_locale_id)]</pre>
<br></br>


<a name="open-0"></a>

###open/0##




<pre>open() -&gt; [i18n_calendar()](#type-i18n_calendar)</pre>
<br></br>


<a name="open-1"></a>

###open/1##




<pre>open(Locale::[i18n_locale_id()](#type-i18n_locale_id)) -&gt; [i18n_calendar()](#type-i18n_calendar)</pre>
<br></br>


<a name="open-2"></a>

###open/2##




<pre>open(Locale::[i18n_locale_id()](#type-i18n_locale_id), TZ::[i18n_timezone()](#type-i18n_timezone)) -&gt; [i18n_calendar()](#type-i18n_calendar)</pre>
<br></br>


<a name="open-3"></a>

###open/3##




<pre>open(Locale::[i18n_locale_id()](#type-i18n_locale_id), TZ::[i18n_timezone()](#type-i18n_timezone), Type::[i18n_calendar_type()](#type-i18n_calendar_type)) -&gt; [i18n_calendar()](#type-i18n_calendar)</pre>
<br></br>


