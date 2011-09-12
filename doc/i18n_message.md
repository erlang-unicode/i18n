Module i18n_message
===================


<h1>Module i18n_message</h1>

* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)







<h2><a name="types">Data Types</a></h2>





<h3 class="typedecl"><a name="type-i18n_locale_id">i18n_locale_id()</a></h3>




<pre>i18n_locale_id() = atom()</pre>



<h3 class="typedecl"><a name="type-i18n_msg_arg">i18n_msg_arg()</a></h3>




<pre>i18n_msg_arg() = any()</pre>



<h3 class="typedecl"><a name="type-i18n_msg_format">i18n_msg_format()</a></h3>




<pre>i18n_msg_format() = <a href="#type-resource">resource()</a></pre>



<h3 class="typedecl"><a name="type-i18n_msg_param">i18n_msg_param()</a></h3>




<pre>i18n_msg_param() = {atom(), <a href="#type-i18n_msg_arg">i18n_msg_arg()</a>}</pre>



<h3 class="typedecl"><a name="type-i18n_string">i18n_string()</a></h3>




<pre>i18n_string() = binary()</pre>



<h3 class="typedecl"><a name="type-resource">resource()</a></h3>




<pre>resource() = <<>></pre>


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format-2">format/2</a></td><td></td></tr><tr><td valign="top"><a href="#format-3">format/3</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#open-1">open/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td>Parse a message to a resourse.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="format-2"></a>

<h3>format/2</h3>





<pre>format(Mesage::<a href="#type-i18n_msg_format">i18n_msg_format()</a>, Parameters::[<a href="#type-i18n_msg_param">i18n_msg_param()</a>]) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="format-3"></a>

<h3>format/3</h3>





<pre>format(Mesage::<a href="#type-i18n_msg_format">i18n_msg_format()</a>, Parameters::[<a href="#type-i18n_msg_param">i18n_msg_param()</a>], AppendTo::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="init-0"></a>

<h3>init/0</h3>





`init() -> any()`

<a name="open-1"></a>

<h3>open/1</h3>





<pre>open(S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_msg_format">i18n_msg_format()</a></pre>
<br></br>


<a name="open-2"></a>

<h3>open/2</h3>





<pre>open(L::<a href="#type-i18n_locale_id">i18n_locale_id()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_msg_format">i18n_msg_format()</a></pre>
<br></br>




Parse a message to a resourse.