Module i18n_date
================


<h1>Module i18n_date</h1>

* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`freeakk@gmail.com`](mailto:freeakk@gmail.com)).


<h2><a name="types">Data Types</a></h2>





<h3 class="typedecl"><a name="type-double">double()</a></h3>




<pre>double() = number()</pre>



<h3 class="typedecl"><a name="type-fields">fields()</a></h3>




<pre>fields() = [{<a href="#type-i18n_date_field">i18n_date_field()</a>, integer()}]</pre>



<h3 class="typedecl"><a name="type-i18n_calendar">i18n_calendar()</a></h3>




<pre>i18n_calendar() = <a href="#type-resource">resource()</a></pre>



<h3 class="typedecl"><a name="type-i18n_date">i18n_date()</a></h3>




<pre>i18n_date() = <a href="#type-double">double()</a></pre>



<h3 class="typedecl"><a name="type-i18n_date_field">i18n_date_field()</a></h3>




<pre>i18n_date_field() = era | year | month | week_of_year | date | day_of_year | day_of_week | am_pm | hour | hour_of_day | minute | second | millisecond | zone_offset | dst_offset | day_of_week_in_month</pre>



<h3 class="typedecl"><a name="type-resource">resource()</a></h3>




<pre>resource() = <<>></pre>


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-1">add/1</a></td><td>Append <code>double()</code> to the field value.</td></tr><tr><td valign="top"><a href="#add-2">add/2</a></td><td></td></tr><tr><td valign="top"><a href="#add-3">add/3</a></td><td></td></tr><tr><td valign="top"><a href="#clear-2">clear/2</a></td><td>Clear the field value.</td></tr><tr><td valign="top"><a href="#clear-3">clear/3</a></td><td></td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Get the value of the field or fields.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td></td></tr><tr><td valign="top"><a href="#is_weekend-0">is_weekend/0</a></td><td>Returns the atom <code>true</code> if there is weekend now.</td></tr><tr><td valign="top"><a href="#is_weekend-1">is_weekend/1</a></td><td>Check if the date is weekend.</td></tr><tr><td valign="top"><a href="#is_weekend-2">is_weekend/2</a></td><td>Returns the atom <code>true</code> if the given date is in the weekend in this
calendar system.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Constructors.</td></tr><tr><td valign="top"><a href="#new-4">new/4</a></td><td></td></tr><tr><td valign="top"><a href="#new-6">new/6</a></td><td></td></tr><tr><td valign="top"><a href="#new-7">new/7</a></td><td></td></tr><tr><td valign="top"><a href="#now-0">now/0</a></td><td>Return the timestamp
(count of milliseconds from starting of the 1970 year).</td></tr><tr><td valign="top"><a href="#roll-1">roll/1</a></td><td>This function and <code>add</code> function are same, but
<code>roll</code> will not modify more significant fields in the calendar.</td></tr><tr><td valign="top"><a href="#roll-2">roll/2</a></td><td></td></tr><tr><td valign="top"><a href="#roll-3">roll/3</a></td><td></td></tr><tr><td valign="top"><a href="#set-1">set/1</a></td><td>Set the value of the field or fields.</td></tr><tr><td valign="top"><a href="#set-2">set/2</a></td><td></td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="add-1"></a>

<h3>add/1</h3>





<pre>add(Fields::<a href="#type-fields">fields()</a>) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>




Append `double()` to the field value.<a name="add-2"></a>

<h3>add/2</h3>





<pre>add(Date::<a href="#type-i18n_calendar">i18n_calendar()</a> | <a href="#type-i18n_date">i18n_date()</a>, Fields::<a href="#type-fields">fields()</a>) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>


<a name="add-3"></a>

<h3>add/3</h3>





<pre>add(Cal::<a href="#type-i18n_calendar">i18n_calendar()</a>, Date::<a href="#type-i18n_date">i18n_date()</a>, Fields::<a href="#type-fields">fields()</a>) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>


<a name="clear-2"></a>

<h3>clear/2</h3>





<pre>clear(Date::<a href="#type-i18n_date">i18n_date()</a>, Fields::[<a href="#type-i18n_date_field">i18n_date_field()</a>]) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>




Clear the field value.<a name="clear-3"></a>

<h3>clear/3</h3>





<pre>clear(Cal::<a href="#type-i18n_calendar">i18n_calendar()</a>, Date::<a href="#type-i18n_date">i18n_date()</a>, Fields::[<a href="#type-i18n_date_field">i18n_date_field()</a>]) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>


<a name="get-1"></a>

<h3>get/1</h3>





<pre>get(Fields::[<a href="#type-i18n_date_field">i18n_date_field()</a>] | <a href="#type-i18n_date_field">i18n_date_field()</a>) -> [integer()] | integer()</pre>
<br></br>




Get the value of the field or fields.<a name="get-2"></a>

<h3>get/2</h3>





<pre>get(Date::<a href="#type-i18n_calendar">i18n_calendar()</a> | <a href="#type-i18n_date">i18n_date()</a>, Fields::[<a href="#type-i18n_date_field">i18n_date_field()</a>] | <a href="#type-i18n_date_field">i18n_date_field()</a>) -> [integer()] | integer()</pre>
<br></br>


<a name="get-3"></a>

<h3>get/3</h3>





<pre>get(Cal::<a href="#type-i18n_calendar">i18n_calendar()</a>, Date::<a href="#type-i18n_date">i18n_date()</a>, Fields::[<a href="#type-i18n_date_field">i18n_date_field()</a>] | <a href="#type-i18n_date_field">i18n_date_field()</a>) -> [integer()] | integer()</pre>
<br></br>


<a name="is_weekend-0"></a>

<h3>is_weekend/0</h3>





<pre>is_weekend() -> boolean()</pre>
<br></br>




Returns the atom `true` if there is weekend now.
Function is locale-sensitive: the calendar will be selected
according this process locale.<a name="is_weekend-1"></a>

<h3>is_weekend/1</h3>





<pre>is_weekend(Date::<a href="#type-i18n_calendar">i18n_calendar()</a> | <a href="#type-i18n_date">i18n_date()</a>) -> boolean()</pre>
<br></br>




Check if the date is weekend. If Arg1 is the calendar, then the date is
`now()`.<a name="is_weekend-2"></a>

<h3>is_weekend/2</h3>





<pre>is_weekend(Cal::<a href="#type-i18n_calendar">i18n_calendar()</a>, Date::<a href="#type-i18n_date">i18n_date()</a>) -> boolean()</pre>
<br></br>




Returns the atom `true` if the given date is in the weekend in this
calendar system.<a name="new-3"></a>

<h3>new/3</h3>





`new(Year, Month, Day) -> any()`



Constructors<a name="new-4"></a>

<h3>new/4</h3>





`new(Cal, Year, Month, Day) -> any()`

<a name="new-6"></a>

<h3>new/6</h3>





`new(Year, Month, Day, Hour, Minute, Second) -> any()`

<a name="new-7"></a>

<h3>new/7</h3>





`new(Cal, Year, Month, Day, Hour, Minute, Second) -> any()`

<a name="now-0"></a>

<h3>now/0</h3>





<pre>now() -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>




Return the timestamp
(count of milliseconds from starting of the 1970 year).<a name="roll-1"></a>

<h3>roll/1</h3>





<pre>roll(Fields::<a href="#type-fields">fields()</a>) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>




This function and `add` function are same, but
`roll` will not modify more significant fields in the calendar.<a name="roll-2"></a>

<h3>roll/2</h3>





<pre>roll(Date::<a href="#type-i18n_calendar">i18n_calendar()</a> | <a href="#type-i18n_date">i18n_date()</a>, Fields::<a href="#type-fields">fields()</a>) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>


<a name="roll-3"></a>

<h3>roll/3</h3>





<pre>roll(Cal::<a href="#type-i18n_calendar">i18n_calendar()</a>, Date::<a href="#type-i18n_date">i18n_date()</a>, Fields::<a href="#type-fields">fields()</a>) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>


<a name="set-1"></a>

<h3>set/1</h3>





<pre>set(Fields::<a href="#type-fields">fields()</a>) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>




Set the value of the field or fields.<a name="set-2"></a>

<h3>set/2</h3>





<pre>set(Date::<a href="#type-i18n_calendar">i18n_calendar()</a> | <a href="#type-i18n_date">i18n_date()</a>, Fields::<a href="#type-fields">fields()</a>) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>


<a name="set-3"></a>

<h3>set/3</h3>





<pre>set(Cal::<a href="#type-i18n_calendar">i18n_calendar()</a>, Date::<a href="#type-i18n_date">i18n_date()</a>, Fields::<a href="#type-fields">fields()</a>) -> <a href="#type-i18n_date">i18n_date()</a></pre>
<br></br>


