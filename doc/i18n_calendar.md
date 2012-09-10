

#Module i18n_calendar#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`arcusfelis@gmail.com`](mailto:arcusfelis@gmail.com)).
<a name="types"></a>

##Data Types##




###<a name="type-i18n_calendar">i18n_calendar()</a>##



<pre>i18n_calendar() = <a href="#type-resource">resource()</a></pre>



###<a name="type-i18n_calendar_type">i18n_calendar_type()</a>##



<pre>i18n_calendar_type() = gregorian | traditional</pre>



###<a name="type-i18n_locale_id">i18n_locale_id()</a>##



<pre>i18n_locale_id() = atom()</pre>



###<a name="type-i18n_string">i18n_string()</a>##



<pre>i18n_string() = binary()</pre>



###<a name="type-i18n_timezone">i18n_timezone()</a>##



<pre>i18n_timezone() = atom() | <a href="#type-i18n_string">i18n_string()</a></pre>



###<a name="type-resource">resource()</a>##



<pre>resource() = &lt;&lt;&gt;&gt;</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#available_locales-0">available_locales/0</a></td><td></td></tr><tr><td valign="top"><a href="#available_timezones-0">available_timezones/0</a></td><td></td></tr><tr><td valign="top"><a href="#open-0">open/0</a></td><td></td></tr><tr><td valign="top"><a href="#open-1">open/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td></td></tr><tr><td valign="top"><a href="#open-3">open/3</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="available_locales-0"></a>

###available_locales/0##




<pre>available_locales() -> [<a href="#type-i18n_locale_id">i18n_locale_id()</a>]</pre>
<br></br>


<a name="available_timezones-0"></a>

###available_timezones/0##




<pre>available_timezones() -> [<a href="#type-i18n_timezone">i18n_timezone()</a>]</pre>
<br></br>


<a name="open-0"></a>

###open/0##




<pre>open() -> <a href="#type-i18n_calendar">i18n_calendar()</a></pre>
<br></br>


<a name="open-1"></a>

###open/1##




<pre>open(Locale::<a href="#type-i18n_locale_id">i18n_locale_id()</a>) -> <a href="#type-i18n_calendar">i18n_calendar()</a></pre>
<br></br>


<a name="open-2"></a>

###open/2##




<pre>open(Locale::<a href="#type-i18n_locale_id">i18n_locale_id()</a>, TZ::<a href="#type-i18n_timezone">i18n_timezone()</a>) -> <a href="#type-i18n_calendar">i18n_calendar()</a></pre>
<br></br>


<a name="open-3"></a>

###open/3##




<pre>open(Locale::<a href="#type-i18n_locale_id">i18n_locale_id()</a>, TZ::<a href="#type-i18n_timezone">i18n_timezone()</a>, Type::<a href="#type-i18n_calendar_type">i18n_calendar_type()</a>) -> <a href="#type-i18n_calendar">i18n_calendar()</a></pre>
<br></br>


