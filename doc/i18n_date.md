

#Module i18n_date#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`arcusfelis@gmail.com`](mailto:arcusfelis@gmail.com)).
<a name="types"></a>

##Data Types##




###<a name="type-double">double()</a>##



<pre>double() = number()</pre>



###<a name="type-fields">fields()</a>##



<pre>fields() = [{<a href="#type-i18n_date_field">i18n_date_field()</a>, <a href="#type-double">double()</a>}]</pre>



###<a name="type-i18n_calendar">i18n_calendar()</a>##



<pre>i18n_calendar() = <a href="#type-resource">resource()</a></pre>



###<a name="type-i18n_date">i18n_date()</a>##



<pre>i18n_date() = <a href="#type-double">double()</a></pre>



###<a name="type-i18n_date_field">i18n_date_field()</a>##



<pre>i18n_date_field() = era | year | month | week_of_year | date | day_of_year | day_of_week | am_pm | hour | hour_of_day | minute | second | millisecond | zone_offset | dst_offset | day_of_week_in_month</pre>



###<a name="type-resource">resource()</a>##



<pre>resource() = &lt;&lt;&gt;&gt;</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-1">add/1</a></td><td>Append <code>double()</code> to the field value.</td></tr><tr><td valign="top"><a href="#add-2">add/2</a></td><td></td></tr><tr><td valign="top"><a href="#add-3">add/3</a></td><td></td></tr><tr><td valign="top"><a href="#clear-2">clear/2</a></td><td>Clear the field value (values).</td></tr><tr><td valign="top"><a href="#clear-3">clear/3</a></td><td>Clear the field value (values).</td></tr><tr><td valign="top"><a href="#compare-3">compare/3</a></td><td></td></tr><tr><td valign="top"><a href="#compare-4">compare/4</a></td><td>If D1 and D2 are too close, then they are equal with the precision of Field.</td></tr><tr><td valign="top"><a href="#difference-3">difference/3</a></td><td></td></tr><tr><td valign="top"><a href="#difference-4">difference/4</a></td><td></td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Get the value of the field or fields.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Create date from fields' values (YMD).</td></tr><tr><td valign="top"><a href="#new-4">new/4</a></td><td>Create date from fields' values (YMD).</td></tr><tr><td valign="top"><a href="#new-6">new/6</a></td><td>Create date from fields' values (YMDHMS).</td></tr><tr><td valign="top"><a href="#new-7">new/7</a></td><td>Create date from fields' values (YMDHMS).</td></tr><tr><td valign="top"><a href="#now-0">now/0</a></td><td>Return the timestamp
(count of milliseconds from starting of the 1970 year).</td></tr><tr><td valign="top"><a href="#roll-1">roll/1</a></td><td>This function and <code>add</code> function are same, but
<code>roll</code> will not modify more significant fields in the calendar.</td></tr><tr><td valign="top"><a href="#roll-2">roll/2</a></td><td></td></tr><tr><td valign="top"><a href="#roll-3">roll/3</a></td><td></td></tr><tr><td valign="top"><a href="#set-1">set/1</a></td><td>Set the value of the field or fields for now().</td></tr><tr><td valign="top"><a href="#set-2">set/2</a></td><td>Set the value of the field or fields for date.</td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td>Set the value of the field or fields for date.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="add-1"></a>

###add/1##




<pre>add(Fields::<a href="#type-fields">fields()</a>) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>




Append `double()` to the field value.<a name="add-2"></a>

###add/2##




<pre>add(Date::<a href="#type-i18n_calendar">i18n_calendar()</a> | <a href="#type-i18n_date">i18n_date()</a>, Fields::<a href="#type-fields">fields()</a>) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>


<a name="add-3"></a>

###add/3##




<pre>add(Cal::<a href="#type-i18n_calendar">i18n_calendar()</a>, Date::<a href="#type-i18n_date">i18n_date()</a>, Fields::<a href="#type-fields">fields()</a>) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>


<a name="clear-2"></a>

###clear/2##




<pre>clear(Date::<a href="#type-i18n_date">i18n_date()</a>, Fields::[<a href="#type-i18n_date_field">i18n_date_field()</a>]) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>




Clear the field value (values).<a name="clear-3"></a>

###clear/3##




<pre>clear(Cal::<a href="#type-i18n_calendar">i18n_calendar()</a>, Date::<a href="#type-i18n_date">i18n_date()</a>, Fields::[<a href="#type-i18n_date_field">i18n_date_field()</a>]) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>




Clear the field value (values).<a name="compare-3"></a>

###compare/3##




<pre>compare(Field::<a href="#type-i18n_date_field">i18n_date_field()</a>, D1::<a href="#type-i18n_date">i18n_date()</a>, D2::<a href="#type-i18n_date">i18n_date()</a>) -> boolean()</pre>
<br></br>


<a name="compare-4"></a>

###compare/4##




<pre>compare(Cal::<a href="#type-i18n_calendar">i18n_calendar()</a>, Field::<a href="#type-i18n_date_field">i18n_date_field()</a>, D1::<a href="#type-i18n_date">i18n_date()</a>, D2::<a href="#type-i18n_date">i18n_date()</a>) -> boolean()</pre>
<br></br>




If D1 and D2 are too close, then they are equal with the precision of Field.<a name="difference-3"></a>

###difference/3##




`difference(FromDate, ToDate, Fields) -> any()`

<a name="difference-4"></a>

###difference/4##




<pre>difference(Cal::<a href="#type-i18n_calendar">i18n_calendar()</a>, FromDate::<a href="#type-i18n_date">i18n_date()</a>, ToDate::<a href="#type-i18n_date">i18n_date()</a>, Field::[<a href="#type-i18n_date_field">i18n_date_field()</a>] | <a href="#type-i18n_date_field">i18n_date_field()</a>) -> [{<a href="#type-i18n_date_field">i18n_date_field()</a>, integer()}] | integer()</pre>
<br></br>


<a name="get-1"></a>

###get/1##




<pre>get(Fields::[<a href="#type-i18n_date_field">i18n_date_field()</a>] | <a href="#type-i18n_date_field">i18n_date_field()</a>) -> [integer()] | integer()</pre>
<br></br>




Get the value of the field or fields.<a name="get-2"></a>

###get/2##




<pre>get(Date::<a href="#type-i18n_calendar">i18n_calendar()</a> | <a href="#type-i18n_date">i18n_date()</a>, Fields::[<a href="#type-i18n_date_field">i18n_date_field()</a>] | <a href="#type-i18n_date_field">i18n_date_field()</a>) -> [integer()] | integer()</pre>
<br></br>


<a name="get-3"></a>

###get/3##




<pre>get(Cal::<a href="#type-i18n_calendar">i18n_calendar()</a>, Date::<a href="#type-i18n_date">i18n_date()</a>, Fields::[<a href="#type-i18n_date_field">i18n_date_field()</a>] | <a href="#type-i18n_date_field">i18n_date_field()</a>) -> [integer()] | integer()</pre>
<br></br>


<a name="new-3"></a>

###new/3##




<pre>new(Year::integer(), Month::integer(), Day::integer()) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>




Create date from fields' values (YMD).<a name="new-4"></a>

###new/4##




<pre>new(Cal::<a href="#type-i18n_calendar">i18n_calendar()</a>, Year::integer(), Month::integer(), Day::integer()) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>




Create date from fields' values (YMD).<a name="new-6"></a>

###new/6##




<pre>new(Year::integer(), Month::integer(), Day::integer(), Hour::integer(), Minute::integer(), Second::integer()) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>




Create date from fields' values (YMDHMS).<a name="new-7"></a>

###new/7##




<pre>new(Cal::<a href="#type-i18n_calendar">i18n_calendar()</a>, Year::integer(), Month::integer(), Day::integer(), Hour::integer(), Minute::integer(), Second::integer()) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>




Create date from fields' values (YMDHMS).<a name="now-0"></a>

###now/0##




<pre>now() -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>




Return the timestamp
(count of milliseconds from starting of the 1970 year).<a name="roll-1"></a>

###roll/1##




<pre>roll(Fields::<a href="#type-fields">fields()</a>) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>




This function and `add` function are same, but
`roll` will not modify more significant fields in the calendar.<a name="roll-2"></a>

###roll/2##




<pre>roll(Date::<a href="#type-i18n_calendar">i18n_calendar()</a> | <a href="#type-i18n_date">i18n_date()</a>, Fields::<a href="#type-fields">fields()</a>) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>


<a name="roll-3"></a>

###roll/3##




<pre>roll(Cal::<a href="#type-i18n_calendar">i18n_calendar()</a>, Date::<a href="#type-i18n_date">i18n_date()</a>, Fields::<a href="#type-fields">fields()</a>) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>


<a name="set-1"></a>

###set/1##




<pre>set(Fields::<a href="#type-fields">fields()</a>) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>




Set the value of the field or fields for now().<a name="set-2"></a>

###set/2##




<pre>set(Date::<a href="#type-i18n_calendar">i18n_calendar()</a> | <a href="#type-i18n_date">i18n_date()</a>, Fields::<a href="#type-fields">fields()</a>) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>




Set the value of the field or fields for date.<a name="set-3"></a>

###set/3##




<pre>set(Cal::<a href="#type-i18n_calendar">i18n_calendar()</a>, Date::<a href="#type-i18n_date">i18n_date()</a>, Fields::<a href="#type-fields">fields()</a>) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>




Set the value of the field or fields for date.
Take a calendar as argument.