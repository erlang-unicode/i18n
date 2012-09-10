

#Module i18n_message#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`arcusfelis@gmail.com`](mailto:arcusfelis@gmail.com)).
<a name="types"></a>

##Data Types##




###<a name="type-i18n_locale_id">i18n_locale_id()</a>##



<pre>i18n_locale_id() = atom()</pre>



###<a name="type-i18n_msg_arg">i18n_msg_arg()</a>##



<pre>i18n_msg_arg() = any()</pre>



###<a name="type-i18n_msg_format">i18n_msg_format()</a>##



<pre>i18n_msg_format() = <a href="#type-resource">resource()</a></pre>



###<a name="type-i18n_msg_param">i18n_msg_param()</a>##



<pre>i18n_msg_param() = {atom(), <a href="#type-i18n_msg_arg">i18n_msg_arg()</a>}</pre>



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




<pre>format(M::<a href="#type-i18n_msg_format">i18n_msg_format()</a>, P::[<a href="#type-i18n_msg_param">i18n_msg_param()</a>]) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="format-3"></a>

###format/3##




<pre>format(M::<a href="#type-i18n_msg_format">i18n_msg_format()</a>, P::[<a href="#type-i18n_msg_param">i18n_msg_param()</a>], A::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="open-1"></a>

###open/1##




<pre>open(S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_msg_format">i18n_msg_format()</a></pre>
<br></br>


<a name="open-2"></a>

###open/2##




<pre>open(L::<a href="#type-i18n_locale_id">i18n_locale_id()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_msg_format">i18n_msg_format()</a></pre>
<br></br>




Parse a message to a resourse.