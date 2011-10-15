Module i18n_string
==================


<h1>Module i18n_string</h1>

* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`freeakk@gmail.com`](mailto:freeakk@gmail.com)).


<h2><a name="types">Data Types</a></h2>





<h3 class="typedecl"><a name="type-i18n_iterator">i18n_iterator()</a></h3>




<pre>i18n_iterator() = <a href="#type-resource">resource()</a></pre>



<h3 class="typedecl"><a name="type-i18n_locale_id">i18n_locale_id()</a></h3>




<pre>i18n_locale_id() = atom()</pre>



<h3 class="typedecl"><a name="type-i18n_string">i18n_string()</a></h3>




<pre>i18n_string() = binary()</pre>



<h3 class="typedecl"><a name="type-resource">resource()</a></h3>




<pre>resource() = <<>></pre>



<h3 class="typedecl"><a name="type-unicode_binary">unicode_binary()</a></h3>




<pre>unicode_binary() = binary()</pre>


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#concat-2">concat/2</a></td><td></td></tr><tr><td valign="top"><a href="#from-1">from/1</a></td><td></td></tr><tr><td valign="top"><a href="#from_utf8-1">from_utf8/1</a></td><td></td></tr><tr><td valign="top"><a href="#len-1">len/1</a></td><td>Count of code paints.</td></tr><tr><td valign="top"><a href="#len-2">len/2</a></td><td>Count the length og the string with help of an iterator.</td></tr><tr><td valign="top"><a href="#split-2">split/2</a></td><td></td></tr><tr><td valign="top"><a href="#split_index-2">split_index/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_lower-1">to_lower/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_lower-2">to_lower/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_nfc-1">to_nfc/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_nfd-1">to_nfd/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_nfkc-1">to_nfkc/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_nfkd-1">to_nfkd/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_title-1">to_title/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_title-2">to_title/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_upper-1">to_upper/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_upper-2">to_upper/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_utf8-1">to_utf8/1</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="concat-2"></a>

<h3>concat/2</h3>





<pre>concat(B1::<a href="#type-i18n_string">i18n_string()</a>, B2::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="from-1"></a>

<h3>from/1</h3>





`from(B) -> any()`

<a name="from_utf8-1"></a>

<h3>from_utf8/1</h3>





<pre>from_utf8(B::<a href="#type-unicode_binary">unicode_binary()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="len-1"></a>

<h3>len/1</h3>





<pre>len(S::<a href="#type-i18n_string">i18n_string()</a>) -> non_neg_integer()</pre>
<br></br>




Count of code paints.<a name="len-2"></a>

<h3>len/2</h3>





<pre>len(I::<a href="#type-i18n_iterator">i18n_iterator()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> non_neg_integer()</pre>
<br></br>






Count the length og the string with help of an iterator.

<pre>  i18n_string:len(i18n_iterator:open('grapheme'), ?ISTR("Example"));</pre><a name="split-2"></a>

<h3>split/2</h3>





`split(I, S) -> any()`

<a name="split_index-2"></a>

<h3>split_index/2</h3>





`split_index(I, S) -> any()`

<a name="to_lower-1"></a>

<h3>to_lower/1</h3>





<pre>to_lower(S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="to_lower-2"></a>

<h3>to_lower/2</h3>





<pre>to_lower(L::<a href="#type-i18n_locale_id">i18n_locale_id()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="to_nfc-1"></a>

<h3>to_nfc/1</h3>





<pre>to_nfc(B::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-unicode_binary">unicode_binary()</a></pre>
<br></br>


<a name="to_nfd-1"></a>

<h3>to_nfd/1</h3>





<pre>to_nfd(B::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-unicode_binary">unicode_binary()</a></pre>
<br></br>


<a name="to_nfkc-1"></a>

<h3>to_nfkc/1</h3>





<pre>to_nfkc(B::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-unicode_binary">unicode_binary()</a></pre>
<br></br>


<a name="to_nfkd-1"></a>

<h3>to_nfkd/1</h3>





<pre>to_nfkd(B::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-unicode_binary">unicode_binary()</a></pre>
<br></br>


<a name="to_title-1"></a>

<h3>to_title/1</h3>





<pre>to_title(S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="to_title-2"></a>

<h3>to_title/2</h3>





<pre>to_title(L::<a href="#type-i18n_locale_id">i18n_locale_id()</a> | <a href="#type-i18n_iterator">i18n_iterator()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="to_upper-1"></a>

<h3>to_upper/1</h3>





<pre>to_upper(S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="to_upper-2"></a>

<h3>to_upper/2</h3>





<pre>to_upper(L::<a href="#type-i18n_locale_id">i18n_locale_id()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="to_utf8-1"></a>

<h3>to_utf8/1</h3>





<pre>to_utf8(B::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-unicode_binary">unicode_binary()</a></pre>
<br></br>


