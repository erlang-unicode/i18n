

#Module i18n_message#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`freeakk@gmail.com`](mailto:freeakk@gmail.com)).
<a name="types"></a>

##Data Types##




###<a name="type-i18n_locale_id">i18n_locale_id()</a>##



<pre>i18n_locale_id() = atom()</pre>



###<a name="type-i18n_msg_arg">i18n_msg_arg()</a>##



<pre>i18n_msg_arg() = any()</pre>



###<a name="type-i18n_msg_format">i18n_msg_format()</a>##



<pre>i18n_msg_format() = [resource()](#type-resource)</pre>



###<a name="type-i18n_msg_param">i18n_msg_param()</a>##



<pre>i18n_msg_param() = {atom(), [i18n_msg_arg()](#type-i18n_msg_arg)}</pre>



###<a name="type-i18n_string">i18n_string()</a>##



<pre>i18n_string() = binary()</pre>



###<a name="type-resource">resource()</a>##



<pre>resource() = &lt;&lt;&gt;&gt;</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format-2">format/2</a></td><td></td></tr><tr><td valign="top"><a href="#format-3">format/3</a></td><td></td></tr><tr><td valign="top"><a href="#open-1">open/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td>Parse a message to a resourse.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="format-2"></a>

###format/2##




<pre>format(M::[i18n_msg_format()](#type-i18n_msg_format), P::[[i18n_msg_param()](#type-i18n_msg_param)]) -&gt; [i18n_string()](#type-i18n_string)</pre>
<br></br>


<a name="format-3"></a>

###format/3##




<pre>format(M::[i18n_msg_format()](#type-i18n_msg_format), P::[[i18n_msg_param()](#type-i18n_msg_param)], A::[i18n_string()](#type-i18n_string)) -&gt; [i18n_string()](#type-i18n_string)</pre>
<br></br>


<a name="open-1"></a>

###open/1##




<pre>open(S::[i18n_string()](#type-i18n_string)) -&gt; [i18n_msg_format()](#type-i18n_msg_format)</pre>
<br></br>


<a name="open-2"></a>

###open/2##




<pre>open(L::[i18n_locale_id()](#type-i18n_locale_id), S::[i18n_string()](#type-i18n_string)) -&gt; [i18n_msg_format()](#type-i18n_msg_format)</pre>
<br></br>




Parse a message to a resourse.