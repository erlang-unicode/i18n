

#Module i18n_string#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`arcusfelis@gmail.com`](mailto:arcusfelis@gmail.com)).
<a name="types"></a>

##Data Types##




###<a name="type-i18n_iterator">i18n_iterator()</a>##



<pre>i18n_iterator() = <a href="#type-resource">resource()</a></pre>



###<a name="type-i18n_locale_id">i18n_locale_id()</a>##



<pre>i18n_locale_id() = atom()</pre>



###<a name="type-i18n_string">i18n_string()</a>##



<pre>i18n_string() = binary()</pre>



###<a name="type-resource">resource()</a>##



<pre>resource() = &lt;&lt;&gt;&gt;</pre>



###<a name="type-unicode_binary">unicode_binary()</a>##



<pre>unicode_binary() = binary()</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#case_compare-2">case_compare/2</a></td><td></td></tr><tr><td valign="top"><a href="#case_compare-3">case_compare/3</a></td><td></td></tr><tr><td valign="top"><a href="#compare-2">compare/2</a></td><td></td></tr><tr><td valign="top"><a href="#concat-2">concat/2</a></td><td>Concatinate two strings.</td></tr><tr><td valign="top"><a href="#from-1">from/1</a></td><td>Convert a term to a UTF-16 string.</td></tr><tr><td valign="top"><a href="#from_utf8-1">from_utf8/1</a></td><td>Convert a UTF-8 string to a UTF-16 string.</td></tr><tr><td valign="top"><a href="#len-1">len/1</a></td><td>Count of code elements.</td></tr><tr><td valign="top"><a href="#len-2">len/2</a></td><td>Count the length og the string with help of an iterator.</td></tr><tr><td valign="top"><a href="#split-2">split/2</a></td><td>This functions breaks a string on parts.</td></tr><tr><td valign="top"><a href="#split_index-2">split_index/2</a></td><td>Return the list of indexes of beginnings of parts of a string.</td></tr><tr><td valign="top"><a href="#to_lower-1">to_lower/1</a></td><td>Convert a string to lower case.</td></tr><tr><td valign="top"><a href="#to_lower-2">to_lower/2</a></td><td>Convert a string to lower case.</td></tr><tr><td valign="top"><a href="#to_nfc-1">to_nfc/1</a></td><td>Convert a string to Unicode Normalization Form NFC.</td></tr><tr><td valign="top"><a href="#to_nfd-1">to_nfd/1</a></td><td>Convert a string to Unicode Normalization Form NFD.</td></tr><tr><td valign="top"><a href="#to_nfkc-1">to_nfkc/1</a></td><td>Convert a string to Unicode Normalization Form NFKD.</td></tr><tr><td valign="top"><a href="#to_nfkd-1">to_nfkd/1</a></td><td>Convert a string to Unicode Normalization Form NFKD.</td></tr><tr><td valign="top"><a href="#to_title-1">to_title/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_title-2">to_title/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_upper-1">to_upper/1</a></td><td>Convert a string to upper case.</td></tr><tr><td valign="top"><a href="#to_upper-2">to_upper/2</a></td><td>Convert a string to upper case.</td></tr><tr><td valign="top"><a href="#to_utf8-1">to_utf8/1</a></td><td>Convert a UTF-16 string to a UTF-8 string.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="case_compare-2"></a>

###case_compare/2##




<pre>case_compare(S1::<a href="#type-i18n_string">i18n_string()</a>, S2::<a href="#type-i18n_string">i18n_string()</a>) -> less | greater | equal</pre>
<br></br>


<a name="case_compare-3"></a>

###case_compare/3##




<pre>case_compare(L::<a href="#type-i18n_locale_id">i18n_locale_id()</a>, S1::<a href="#type-i18n_string">i18n_string()</a>, S2::<a href="#type-i18n_string">i18n_string()</a>) -> less | greater | equal</pre>
<br></br>


<a name="compare-2"></a>

###compare/2##




<pre>compare(S1::<a href="#type-i18n_string">i18n_string()</a>, S2::<a href="#type-i18n_string">i18n_string()</a>) -> less | greater | equal</pre>
<br></br>


<a name="concat-2"></a>

###concat/2##




<pre>concat(B1::<a href="#type-i18n_string">i18n_string()</a>, B2::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>




Concatinate two strings.<a name="from-1"></a>

###from/1##




<pre>from(B::binary() | list() | atom()) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>




Convert a term to a UTF-16 string.<a name="from_utf8-1"></a>

###from_utf8/1##




<pre>from_utf8(B::<a href="#type-unicode_binary">unicode_binary()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>




Convert a UTF-8 string to a UTF-16 string.<a name="len-1"></a>

###len/1##




<pre>len(S::<a href="#type-i18n_string">i18n_string()</a>) -> non_neg_integer()</pre>
<br></br>




Count of code elements. Code elements are neither code points nor
graphemes.<a name="len-2"></a>

###len/2##




<pre>len(I::<a href="#type-i18n_iterator">i18n_iterator()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> non_neg_integer()</pre>
<br></br>






Count the length og the string with help of an iterator.

<pre>  i18n_string:len(i18n_iterator:open('grapheme'), ?ISTR("Example"));</pre><a name="split-2"></a>

###split/2##




<pre>split(I::<a href="#type-i18n_iterator">i18n_iterator()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> [<a href="#type-i18n_string">i18n_string()</a>]</pre>
<br></br>




This functions breaks a string on parts.<a name="split_index-2"></a>

###split_index/2##




<pre>split_index(I::<a href="#type-i18n_iterator">i18n_iterator()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> [non_neg_integer()]</pre>
<br></br>




Return the list of indexes of beginnings of parts of a string.<a name="to_lower-1"></a>

###to_lower/1##




<pre>to_lower(S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>




Convert a string to lower case.<a name="to_lower-2"></a>

###to_lower/2##




<pre>to_lower(L::<a href="#type-i18n_locale_id">i18n_locale_id()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>




Convert a string to lower case.<a name="to_nfc-1"></a>

###to_nfc/1##




<pre>to_nfc(B::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-unicode_binary">unicode_binary()</a></pre>
<br></br>




Convert a string to Unicode Normalization Form NFC.<a name="to_nfd-1"></a>

###to_nfd/1##




<pre>to_nfd(B::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-unicode_binary">unicode_binary()</a></pre>
<br></br>




Convert a string to Unicode Normalization Form NFD.<a name="to_nfkc-1"></a>

###to_nfkc/1##




<pre>to_nfkc(B::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-unicode_binary">unicode_binary()</a></pre>
<br></br>




Convert a string to Unicode Normalization Form NFKD.<a name="to_nfkd-1"></a>

###to_nfkd/1##




<pre>to_nfkd(B::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-unicode_binary">unicode_binary()</a></pre>
<br></br>




Convert a string to Unicode Normalization Form NFKD.<a name="to_title-1"></a>

###to_title/1##




<pre>to_title(S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="to_title-2"></a>

###to_title/2##




<pre>to_title(L::<a href="#type-i18n_locale_id">i18n_locale_id()</a> | <a href="#type-i18n_iterator">i18n_iterator()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>


<a name="to_upper-1"></a>

###to_upper/1##




<pre>to_upper(S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>




Convert a string to upper case.<a name="to_upper-2"></a>

###to_upper/2##




<pre>to_upper(L::<a href="#type-i18n_locale_id">i18n_locale_id()</a>, S::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-i18n_string">i18n_string()</a></pre>
<br></br>




Convert a string to upper case.<a name="to_utf8-1"></a>

###to_utf8/1##




<pre>to_utf8(B::<a href="#type-i18n_string">i18n_string()</a>) -> <a href="#type-unicode_binary">unicode_binary()</a></pre>
<br></br>




Convert a UTF-16 string to a UTF-8 string.