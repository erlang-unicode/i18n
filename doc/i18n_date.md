

#Module i18n_date#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`freeakk@gmail.com`](mailto:freeakk@gmail.com)).
<a name="types"></a>

##Data Types##




###<a name="type-double">double()</a>##



<pre>double() = number()</pre>



###<a name="type-fields">fields()</a>##



<pre>fields() = [{[i18n_date_field()](#type-i18n_date_field), [double()](#type-double)}]</pre>



###<a name="type-i18n_calendar">i18n_calendar()</a>##



<pre>i18n_calendar() = [resource()](#type-resource)</pre>



###<a name="type-i18n_date">i18n_date()</a>##



<pre>i18n_date() = [double()](#type-double)</pre>



###<a name="type-i18n_date_field">i18n_date_field()</a>##



<pre>i18n_date_field() = era | year | month | week_of_year | date | day_of_year | day_of_week | am_pm | hour | hour_of_day | minute | second | millisecond | zone_offset | dst_offset | day_of_week_in_month</pre>



###<a name="type-resource">resource()</a>##



<pre>resource() = &lt;&lt;&gt;&gt;</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-1">add/1</a></td><td>Append <code>double()</code> to the field value.</td></tr><tr><td valign="top"><a href="#add-2">add/2</a></td><td></td></tr><tr><td valign="top"><a href="#add-3">add/3</a></td><td></td></tr><tr><td valign="top"><a href="#clear-2">clear/2</a></td><td>Clear the field value (values).</td></tr><tr><td valign="top"><a href="#clear-3">clear/3</a></td><td>Clear the field value (values).</td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Get the value of the field or fields.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td></td></tr><tr><td valign="top"><a href="#is_weekend-0">is_weekend/0</a></td><td>Returns the atom <code>true</code> if there is weekend now.</td></tr><tr><td valign="top"><a href="#is_weekend-1">is_weekend/1</a></td><td>Check if the date is weekend.</td></tr><tr><td valign="top"><a href="#is_weekend-2">is_weekend/2</a></td><td>Returns the atom <code>true</code> if the given date is in the weekend in this
calendar system.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Create date from fields' values (YMD).</td></tr><tr><td valign="top"><a href="#new-4">new/4</a></td><td>Create date from fields' values (YMD).</td></tr><tr><td valign="top"><a href="#new-6">new/6</a></td><td>Create date from fields' values (YMDHMS).</td></tr><tr><td valign="top"><a href="#new-7">new/7</a></td><td>Create date from fields' values (YMDHMS).</td></tr><tr><td valign="top"><a href="#now-0">now/0</a></td><td>Return the timestamp
(count of milliseconds from starting of the 1970 year).</td></tr><tr><td valign="top"><a href="#roll-1">roll/1</a></td><td>This function and <code>add</code> function are same, but
<code>roll</code> will not modify more significant fields in the calendar.</td></tr><tr><td valign="top"><a href="#roll-2">roll/2</a></td><td></td></tr><tr><td valign="top"><a href="#roll-3">roll/3</a></td><td></td></tr><tr><td valign="top"><a href="#set-1">set/1</a></td><td>Set the value of the field or fields for now().</td></tr><tr><td valign="top"><a href="#set-2">set/2</a></td><td>Set the value of the field or fields for date.</td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td>Set the value of the field or fields for date.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="add-1"></a>

###add/1##




<pre>add(Fields::[fields()](#type-fields)) -&gt; [i18n_date()](#type-i18n_date)</pre>
<br></br>




Append `double()` to the field value.<a name="add-2"></a>

###add/2##




<pre>add(Date::[i18n_calendar()](#type-i18n_calendar) | [i18n_date()](#type-i18n_date), Fields::[fields()](#type-fields)) -&gt; [i18n_date()](#type-i18n_date)</pre>
<br></br>


<a name="add-3"></a>

###add/3##




<pre>add(Cal::[i18n_calendar()](#type-i18n_calendar), Date::[i18n_date()](#type-i18n_date), Fields::[fields()](#type-fields)) -&gt; [i18n_date()](#type-i18n_date)</pre>
<br></br>


<a name="clear-2"></a>

###clear/2##




<pre>clear(Date::[i18n_date()](#type-i18n_date), Fields::[[i18n_date_field()](#type-i18n_date_field)]) -&gt; [i18n_date()](#type-i18n_date)</pre>
<br></br>




Clear the field value (values).<a name="clear-3"></a>

###clear/3##




<pre>clear(Cal::[i18n_calendar()](#type-i18n_calendar), Date::[i18n_date()](#type-i18n_date), Fields::[[i18n_date_field()](#type-i18n_date_field)]) -&gt; [i18n_date()](#type-i18n_date)</pre>
<br></br>




Clear the field value (values).<a name="get-1"></a>

###get/1##




<pre>get(Fields::[[i18n_date_field()](#type-i18n_date_field)] | [i18n_date_field()](#type-i18n_date_field)) -&gt; [integer()] | integer()</pre>
<br></br>




Get the value of the field or fields.<a name="get-2"></a>

###get/2##




<pre>get(Date::[i18n_calendar()](#type-i18n_calendar) | [i18n_date()](#type-i18n_date), Fields::[[i18n_date_field()](#type-i18n_date_field)] | [i18n_date_field()](#type-i18n_date_field)) -&gt; [integer()] | integer()</pre>
<br></br>


<a name="get-3"></a>

###get/3##




<pre>get(Cal::[i18n_calendar()](#type-i18n_calendar), Date::[i18n_date()](#type-i18n_date), Fields::[[i18n_date_field()](#type-i18n_date_field)] | [i18n_date_field()](#type-i18n_date_field)) -&gt; [integer()] | integer()</pre>
<br></br>


<a name="is_weekend-0"></a>

###is_weekend/0##




<pre>is_weekend() -&gt; boolean()</pre>
<br></br>




Returns the atom `true` if there is weekend now.
Function is locale-sensitive: the calendar will be selected
according this process locale.<a name="is_weekend-1"></a>

###is_weekend/1##




<pre>is_weekend(Date::[i18n_calendar()](#type-i18n_calendar) | [i18n_date()](#type-i18n_date)) -&gt; boolean()</pre>
<br></br>




Check if the date is weekend. If Arg1 is the calendar, then the date is
`now()`.<a name="is_weekend-2"></a>

###is_weekend/2##




<pre>is_weekend(Cal::[i18n_calendar()](#type-i18n_calendar), Date::[i18n_date()](#type-i18n_date)) -&gt; boolean()</pre>
<br></br>




Returns the atom `true` if the given date is in the weekend in this
calendar system.<a name="new-3"></a>

###new/3##




<pre>new(Year::integer(), Month::integer(), Day::integer()) -&gt; [i18n_date()](#type-i18n_date)</pre>
<br></br>




Create date from fields' values (YMD).<a name="new-4"></a>

###new/4##




<pre>new(Cal::[i18n_calendar()](#type-i18n_calendar), Year::integer(), Month::integer(), Day::integer()) -&gt; [i18n_date()](#type-i18n_date)</pre>
<br></br>




Create date from fields' values (YMD).<a name="new-6"></a>

###new/6##




<pre>new(Year::integer(), Month::integer(), Day::integer(), Hour::integer(), Minute::integer(), Second::integer()) -&gt; [i18n_date()](#type-i18n_date)</pre>
<br></br>




Create date from fields' values (YMDHMS).<a name="new-7"></a>

###new/7##




<pre>new(Cal::[i18n_calendar()](#type-i18n_calendar), Year::integer(), Month::integer(), Day::integer(), Hour::integer(), Minute::integer(), Second::integer()) -&gt; [i18n_date()](#type-i18n_date)</pre>
<br></br>




Create date from fields' values (YMDHMS).<a name="now-0"></a>

###now/0##




<pre>now() -&gt; [i18n_date()](#type-i18n_date)</pre>
<br></br>




Return the timestamp
(count of milliseconds from starting of the 1970 year).<a name="roll-1"></a>

###roll/1##




<pre>roll(Fields::[fields()](#type-fields)) -&gt; [i18n_date()](#type-i18n_date)</pre>
<br></br>




This function and `add` function are same, but
`roll` will not modify more significant fields in the calendar.<a name="roll-2"></a>

###roll/2##




<pre>roll(Date::[i18n_calendar()](#type-i18n_calendar) | [i18n_date()](#type-i18n_date), Fields::[fields()](#type-fields)) -&gt; [i18n_date()](#type-i18n_date)</pre>
<br></br>


<a name="roll-3"></a>

###roll/3##




<pre>roll(Cal::[i18n_calendar()](#type-i18n_calendar), Date::[i18n_date()](#type-i18n_date), Fields::[fields()](#type-fields)) -&gt; [i18n_date()](#type-i18n_date)</pre>
<br></br>


<a name="set-1"></a>

###set/1##




<pre>set(Fields::[fields()](#type-fields)) -&gt; [i18n_date()](#type-i18n_date)</pre>
<br></br>




Set the value of the field or fields for now().<a name="set-2"></a>

###set/2##




<pre>set(Date::[i18n_calendar()](#type-i18n_calendar) | [i18n_date()](#type-i18n_date), Fields::[fields()](#type-fields)) -&gt; [i18n_date()](#type-i18n_date)</pre>
<br></br>




Set the value of the field or fields for date.<a name="set-3"></a>

###set/3##




<pre>set(Cal::[i18n_calendar()](#type-i18n_calendar), Date::[i18n_date()](#type-i18n_date), Fields::[fields()](#type-fields)) -&gt; [i18n_date()](#type-i18n_date)</pre>
<br></br>




Set the value of the field or fields for date.
Take a calendar as argument.