

#Module i18n_locale#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


This module containg functions to manage the locale of the process.



Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`freeakk@gmail.com`](mailto:freeakk@gmail.com)).
<a name="types"></a>

##Data Types##




###<a name="type-i18n_locale_id">i18n_locale_id()</a>##



<pre>i18n_locale_id() = atom()</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#base_name-1">base_name/1</a></td><td><code>ru_RU@col=COL</code> -> <code>ru_RU</code></td></tr><tr><td valign="top"><a href="#get_locale-0">get_locale/0</a></td><td>Extract the locale of this process.</td></tr><tr><td valign="top"><a href="#parent_locale-1">parent_locale/1</a></td><td><code>ru_RU</code> -> <code>ru</code></td></tr><tr><td valign="top"><a href="#set_default_locale-1">set_default_locale/1</a></td><td>Extract the locale of all processes, in which
there is no call of <code>set_locale/1</code>.</td></tr><tr><td valign="top"><a href="#set_locale-1">set_locale/1</a></td><td>Set the locale of this process.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="base_name-1"></a>

###base_name/1##




<pre>base_name(LocaleId::[i18n_locale_id()](#type-i18n_locale_id)) -&gt; [i18n_locale_id()](#type-i18n_locale_id)</pre>
<br></br>




`ru_RU@col=COL` -> `ru_RU`<a name="get_locale-0"></a>

###get_locale/0##




<pre>get_locale() -&gt; [i18n_locale_id()](#type-i18n_locale_id)</pre>
<br></br>




Extract the locale of this process<a name="parent_locale-1"></a>

###parent_locale/1##




<pre>parent_locale(Locale::[i18n_locale_id()](#type-i18n_locale_id)) -&gt; [i18n_locale_id()](#type-i18n_locale_id)</pre>
<br></br>




`ru_RU` -> `ru`<a name="set_default_locale-1"></a>

###set_default_locale/1##




<pre>set_default_locale(Value::[i18n_locale_id()](#type-i18n_locale_id)) -&gt; [i18n_locale_id()](#type-i18n_locale_id)</pre>
<br></br>




Extract the locale of all processes, in which
there is no call of `set_locale/1`.<a name="set_locale-1"></a>

###set_locale/1##




<pre>set_locale(Value::[i18n_locale_id()](#type-i18n_locale_id)) -&gt; [i18n_locale_id()](#type-i18n_locale_id)</pre>
<br></br>




Set the locale of this process.
It will affect on all case-sensitive operations
when the locale parameter will be skipped.