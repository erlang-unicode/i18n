; -- Mode: Markdown; -- ; vim: filetype=markdown tw=76 expandtab shiftwidth=4 tabstop=4


i18n: ICU for Erlang
====================

__License__: [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.html)

__Author__: Uvarov Michael ([`freeakk@gmail.com`](mailto:freeakk@gmail.com))


Module for working with strings.
A string is a binary UTF-16 string.


All actions with Unicode were described in the [Unicode Standards](http://www.unicode.org/reports/).
ICU Documentation [API Reference](http://icu-project.org/apiref/icu4c/).
Erlang NIF [erl_nif API](http://www.erlang.org/doc/man/erl_nif.html).



Motivation
==========

There are many operation which are locale-dependible. Each locale has its
own rules for working with dates, with collation or even with the case
transform. All these rules must be stored in the global memory store.

But when we talk about the Erlang memory management, we talk about own
copy of data for each Erlang process. There is no the global store (maybe
ETS, but it is not pure Erlang).

Also, there is no the common string format for Erlang. There are lists and
binaries. A string as a list are is actually a list of Unicode codepaints. 
A binary string is a binary representation of the string (often it is in
UTF-8 encoding).

Lists are very helpful, when we work with codepaints. But they are slow.
Binaries are useful, when we store or transmit information.

One of the most powerful library for working with Unicode is ICU. We will
use icu4c. It is version of ICU for C and C++ languages. Reasons why we use
it are: it is fast, it has optimizied global data store, it is well tested
and it works into the multithread enviroment very well.



Design
======


Almost all locale-dependible modules of ICU uses resources. It can be an
iterator, a collator, a parser, a transliterator and so on. The creation of
the resource is take some time, but its using is fast. So, if you have
locale-dependible process or gen\_server, create resources and store them
(one resource of same type for one longtime process).

We use ICU in the multithread enviroment, so all resources are cloned for
each os thread by this library. When garbage collector deallocates the
resource, all its copies will also deallocated. `cloner.c` does this work.

Any resource is immutable: you cannot change its internal state from Erlang
code. If you load new version of the library, old resouces will be here. But
you cannot send a resouce to another node. 

The nif module is one, because same resources are used in different modules
(for example, an `iterator` resource can be used in `string` and `collation` modules).

Strings are represented as binaries in `UTF-16` form (two bytes per
codepaint). It can be `UTF-16be` (big endian) or `UTF-16le` (little endian). If you use nodes with different endianess, you must convert strings from one form to another.

There are two macroses in `i18n/include/i18n.hrl`:

```erlang
-define(ISTR(X), i18n_string:from(X)).
-define(ITS(X),  i18n_string:to_utf8(X)).
```

When you get a string from the user or from the database, it is often in
`UTF-8` encoding. You can transform it `UTF-16`, process this string and
transform it back to `UTF-8`. You can do it in two ways:

```erlang
Len   = i18n_string:len(?ISTR(Utf8Str)),
Up    = ?ITS(i18n_string:to_upper(?ISTR(Up))),
UpLen = i18n_string:len(?ISTR(Up)).
```

```erlang
Str   = ?ISTR(Utf8Str),
Len   = i18n_string:len(Str),
Up    = i18n_string:to_upper(Str),
UpLen = i18n_string:len(Up).
```

In this example we will get the length of the string and the length of the
uppercase version of the string. In first example we convert from one form
to another very often, this example will be longer. 

Main advice is to convert string when you get it, store string in this form
as long as you need it for processing and then encode strings back to `UTF-8`.

Processing of long strings and first resource allocations can stop other erlang processes, which are scheduled in the same thread. Dirty schedulers can fix this
problem, so we wait R15, when they are planned.


Examples
========

String case modifications
-------------------------

```erlang
S=i18n_string:from_utf8(<<"the quick brown Fox jumps over the lazy Dog.">>).
i18n_string:to_utf8(i18n_string:to_upper(S)).                               
i18n_string:to_utf8(i18n_string:to_title(S)).                               

I=i18n_iterator:open(sentence).                                       
i18n_string:to_utf8(i18n_string:to_title(I, S)).                            
```


Searching
---------

```erlang
CS = i18n_collation:open([secondary]). 
CT = i18n_collation:open(). 
S = i18n:from("abcd ABCD"). 
P = i18n:from("a"). 
SCSP = i18n_search:open(CS, P). 
SCTP = i18n_search:open(CT, P). 
i18n_search:index(SCSP, S).
i18n_search:index(SCTP, S).           
i18n_search:match_all(SCSP, S).           
```


Modules
-------

* [i18n_calendar](https://github.com/freeakk/i18n/blob/master/doc/i18n_calendar.md)
* [i18n_collation](https://github.com/freeakk/i18n/blob/master/doc/i18n_collation.md)
* [i18n_date](https://github.com/freeakk/i18n/blob/master/doc/i18n_date.md)
* [i18n_iterator](https://github.com/freeakk/i18n/blob/master/doc/i18n_iterator.md)
* [i18n_locale](https://github.com/freeakk/i18n/blob/master/doc/i18n_locale.md)
* [i18n_message](https://github.com/freeakk/i18n/blob/master/doc/i18n_message.md)
* [i18n_regex](https://github.com/freeakk/i18n/blob/master/doc/i18n_regex.md)
* [i18n_search](https://github.com/freeakk/i18n/blob/master/doc/i18n_search.md)
* [i18n_string](https://github.com/freeakk/i18n/blob/master/doc/i18n_string.md)
* [i18n_transliteration](https://github.com/freeakk/i18n/blob/master/doc/i18n_transliteration.md)
