Module i18n_string
==================


<h1>Module i18n_string</h1>

* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)







<h2><a name="types">Data Types</a></h2>





<h3 class="typedecl"><a name="type-binary_string">binary_string()</a></h3>




<pre>binary_string() = binary()</pre>



<h3 class="typedecl"><a name="type-i18n_iterator">i18n_iterator()</a></h3>




<pre>i18n_iterator() = <a href="#type-resource">resource()</a></pre>



<h3 class="typedecl"><a name="type-i18n_locale_id">i18n_locale_id()</a></h3>




<pre>i18n_locale_id() = atom()</pre>



<h3 class="typedecl"><a name="type-i18n_string">i18n_string()</a></h3>




<pre>i18n_string() = binary()</pre>



<h3 class="typedecl"><a name="type-i18n_string_iterator_type">i18n_string_iterator_type()</a></h3>




<pre>i18n_string_iterator_type() = grapheme | word | sentence | line</pre>



<h3 class="typedecl"><a name="type-resource">resource()</a></h3>




<pre>resource() = <<>></pre>


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#append-2">append/2</a></td><td></td></tr><tr><td valign="top"><a href="#from-1">from/1</a></td><td></td></tr><tr><td valign="top"><a href="#from_utf8-1">from_utf8/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_iterator-1">get_iterator/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_iterator-2">get_iterator/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#len-2">len/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_lower-1">to_lower/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_lower-2">to_lower/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_title-1">to_title/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_title-2">to_title/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_upper-1">to_upper/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_upper-2">to_upper/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_utf8-1">to_utf8/1</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="append-2"></a>

<h3>append/2</h3>





<pre>append(BinUTF16Beginning::<a href="#type-i18n_string">i18n_string()</a>, BinUTF16Ending::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="from-1"></a>

<h3>from/1</h3>





`from(B) -> any()`

<a name="from_utf8-1"></a>

<h3>from_utf8/1</h3>





<pre>from_utf8(BinUTF8::<a href="#type-binary_string">binary_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="get_iterator-1"></a>

<h3>get_iterator/1</h3>





<pre>get_iterator(T::<a href="#type-i18n_string_iterator_type">i18n_string_iterator_type()</a>) -> <a href="#type-i18n_iterator">i18n_iterator()</a></pre>
<br></br>


<a name="get_iterator-2"></a>

<h3>get_iterator/2</h3>





<pre>get_iterator(Locale::<a href="#type-i18n_locale_id">i18n_locale_id()</a>, Type::<a href="#type-i18n_string_iterator_type">i18n_string_iterator_type()</a>) -> <a href="#type-i18n_iterator">i18n_iterator()</a></pre>
<br></br>


<a name="init-0"></a>

<h3>init/0</h3>





`init() -> any()`

<a name="len-2"></a>

<h3>len/2</h3>





<pre>len(IterResource::<a href="#type-i18n_iterator">i18n_iterator()</a>, String::<a href="#type-i18n_string">i18n_string()</a>) -> non_neg_integer()</pre>
<br></br>


<a name="to_lower-1"></a>

<h3>to_lower/1</h3>





<pre>to_lower(S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="to_lower-2"></a>

<h3>to_lower/2</h3>





<pre>to_lower(Locale::<a href="#type-i18n_locale_id">i18n_locale_id()</a>, String::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="to_title-1"></a>

<h3>to_title/1</h3>





<pre>to_title(S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="to_title-2"></a>

<h3>to_title/2</h3>





<pre>to_title(Locale::<a href="#type-i18n_locale_id">i18n_locale_id()</a>, String::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="to_upper-1"></a>

<h3>to_upper/1</h3>





<pre>to_upper(S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="to_upper-2"></a>

<h3>to_upper/2</h3>





<pre>to_upper(Locale::<a href="#type-i18n_locale_id">i18n_locale_id()</a>, String::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="to_utf8-1"></a>

<h3>to_utf8/1</h3>





<pre>to_utf8(BinUTF16::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-binary_string">binary_string()</a></pre>
<br></br>


