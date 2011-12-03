; -- Mode: Markdown; -- ; vim: filetype=markdown tw=76 expandtab shiftwidth=4 tabstop=4


i18n: ICU for Erlang
====================

__License__: [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.html)

__Author__: Uvarov Michael ([`freeakk@gmail.com`](mailto:freeakk@gmail.com))


Module for working with strings and dates.
A string is a binary UTF-16 string.


All actions with Unicode were described in the [Unicode Standards](http://www.unicode.org/reports/).
ICU Documentation [API Reference](http://icu-project.org/apiref/icu4c/).
Erlang NIF [erl_nif API](http://www.erlang.org/doc/man/erl_nif.html).




Installation
============

__i18n__ is based on [ICU 4.2 or newer](http://icu-project.org/), which you 
need to have installed:

`brew install icu4c` with Homebrew on OSX;

`port install icu` with MacPorts on OSX;

`apt-get install libicu42 libicu-dev` on Ubuntu 10.10;

`apt-get install libicu44 libicu-dev` on Ubuntu 11.10.

Also you need to have gcc for compilation :).



Enviroment Variables
====================

You can configure the compilation process with environment variables.
This application uses rebar for building, it also uses 
[this patch](https://github.com/basho/rebar/pull/129) for checking env vars.



Try it
======

Run in the terminal:

```
./start-dev.sh
```




Motivation
==========

There are many operation which are locale-dependable. Each locale has its
own rules for working with dates, with collation or even with the case
transform. All these rules must be stored in the global memory store.

But when we talk about the Erlang memory management, we talk about own
copy of data for each Erlang process. There is no the global store (maybe
ETS, but it is not pure Erlang).

Also, there is no the common string format for Erlang. There are lists and
binaries. A string as a list are is actually a list of Unicode code points. 
A binary string is a binary representation of the string (often it is in
UTF-8 encoding).

Lists are very helpful, when we work with code points. But they are slow.
Binaries are useful, when we store or transmit information.

One of the most powerful library for working with Unicode is ICU. We will
use icu4c. It is version of ICU for C and C++ languages. Reasons why we use
it are: it is fast, it has fast global data store, it is well tested
and it works into the multithread environment very well.



Design
======


Almost all locale-dependable modules of ICU uses resources. It can be an
iterator, a collator, a parser, a transliterator and so on. The creation of
the resource is take some time, but its using is fast. So, if you have
locale-dependable process or gen\_server, create resources and store them
(one resource of same type for one longtime process).

We use ICU in the multithread enviroment, so all resources are cloned for
each OS's thread by this library. When garbage collector deallocates the
resource, all its copies will also deallocated. `cloner.c` does this work.

Any resource is immutable: you cannot change its internal state from Erlang
code. If you load new version of the library, old resources will be here. 
But you cannot send a resource to another node. 

The NIF module is one, because same resources are used in different modules
(for example, an `iterator` resource can be used in `string` and `collation` modules).

Strings are represented as binaries in `UTF-16` form (two bytes per
code point). It can be `UTF-16be` (big endian) or `UTF-16le` (little endian). 
If you use nodes with different endianess, you must convert strings from 
one form to another.

There are two macroses in `i18n/include/i18n.hrl`:

```erlang
-define(ISTR(X), i18n_string:from(X)).
-define(ITS(X),  i18n_string:to_utf8(X)).
```

When you get a string from the user or from the database, it is often in
`UTF-8` encoding. You can transform it `UTF-16`, process this string and
transform it back to `UTF-8`. You can do it in two ways:

```erlang
% Bad example

Len   = i18n_string:len(?ISTR(Utf8Str)),
Up    = ?ITS(i18n_string:to_upper(?ISTR(Up))),
UpLen = i18n_string:len(?ISTR(Up)).
```

```erlang
% Good example

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

Processing of long strings or first resource allocations can stop other 
erlang processes, which are scheduled in the same thread. Dirty schedulers 
can fix this problem, so I am waiting __R15__.

Functions can throw `badarg` or exceptions:

```erlang
{i18n_error, Code :: atom(), tuple()}
```

For example:

```erlang
(i18n@delta)1> i18n_regex:open(i18n:from("[[]")).
** exception error: {i18n_error,{'U_REGEX_MISSING_CLOSE_BRACKET',{line,1},
                                                                 {offset,3}}}


(i18n@delta)2>  i18n_message:open(i18n:from("{rr")).
** exception error: {i18n_error,{'U_UNMATCHED_BRACES',{line,0},{offset,3}}}
     in function  i18n_message:open/2
```



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

Rule-based collation
--------------------

```erlang
R1 = i18n_collation:open().
R2 = i18n_collation:open_rules(i18n:from("& g <<< ca")). 
F = fun(R, L) -> 
    lists:map(fun i18n:to/1, 
        i18n_collation:sort(R,
            lists:map(fun i18n:from/1, L))) end.
L = ["ca", "h", "f", "cà"].
{F(R1, L), F(R2, L)}.
{[<<"ca">>,<<"cÃ ">>,<<"f">>,<<"h">>],
 [<<"cÃ ">>,<<"f">>,<<"ca">>,<<"h">>]}
```

where:

```erlang
io:format("~ts", [<<"cÃ ">>]).
cà
```


Length of the string
--------------------

With `word` iterator, punctuation and space characters are counted as "words"
(see partitions for more information).

```erlang
GI = i18n_iterator:open(grapheme),
WI = i18n_iterator:open(word),
WOI = i18n_iterator:open(word_only).

1> i18n_string:len(GI, i18n:from("Длина длинной строки.")).
21

(i18n@delta)14> i18n_string:len(WOI, i18n:from("Count of the words.")). 
4

(i18n@delta)15> i18n_string:len(WO, i18n:from("Count of the words.")).
8
```


Extracting  words
-----------------

```erlang
1> lists:map(fun i18n:to/1,
    i18n_string:split(i18n_iterator:open(word_only), 
        i18n:from("This string contains 5 words."))).
[<<"This">>,<<"string">>,<<"contains">>,<<"5">>,<<"words">>]
```



Partitions of the string
------------------------

```erlang
1> lists:map(fun i18n:to/1,
    i18n_string:split(i18n_iterator:open(word), 
        i18n:from("This string contains 5 words."))).
[<<"This">>,<<" ">>,<<"string">>,<<" ">>,<<"contains">>,
 <<" ">>,<<"5">>,<<" ">>,<<"words">>,<<".">>]

2> Out = lists:map(fun i18n:to/1,
    i18n_string:split(i18n_iterator:open(grapheme), 
        i18n:from("Erlang är ett generellt programspråk som från början (år
1987) utvecklades på forskningsavdelningen hos telebolaget Ericsson vid
utvärderingen av olika programspråk för implementation av styrsystemen i
telefonväxlar."))).

3> io:format("~w", [Out]). 
[<<69>>,<<114>>,<<108>>,<<97>>,<<110>>,<<103>>,<<32>>,<<195,164>>,<<114>>,
 <<32>>,<<101>>,<<116>>,<<116>>,<<32>>,<<103>>,<<101>>,<<110>>,<<101>>,...].
```



Message format
--------------

Simple message format:

```erlang
M = i18n_message:open(i18n:from("Hello, {name}. Now {now, time, full}.")),
R = i18n_message:format(M, [
        {'name', i18n:from("Username")},
        {'now',  i18n_date:now()}
    ]),
io:format("~n~ts~n", [i18n_string:to_utf8(R)]).
```

Out:

```
Hello, Username. Now 17:32:11 GMT+04:00.
```


Date format:

```erlang
MLong = i18n_message:open(i18n:from("{0,date,long}")).
MShort = i18n_message:open(i18n:from("{0,date,short}")).
Epoch = i18n_date:new(1970,1,1).
NewDate = i18n_date:add(Epoch, [{day, 3}]).
i18n:to(i18n_message:format(MLong, [NewDate])).
i18n:to(i18n_message:format(MShort, [NewDate])).
```

Out:

```erlang
(i18n@delta)1> MLong = i18n_message:open(i18n:from("{0,date,long}")).
<<>>
(i18n@delta)2> MShort = i18n_message:open(i18n:from("{0,date,short}")).
<<>>
(i18n@delta)3> Epoch = i18n_date:new(1970,1,1).
39487338.0
(i18n@delta)4> NewDate = i18n_date:add(Epoch, [{day, 3}]).
298687338.0
(i18n@delta)5> i18n:to(i18n_message:format(MLong, [NewDate])).
<<"1970 1 4">>
(i18n@delta)6> i18n:to(i18n_message:format(MShort, [NewDate])).
<<"1970-01-04">>
```


If you want to use ICU messages with gettext, then see
[l10n](https://github.com/freeakk/l10n/).


Using Unicode strings in source code
------------------------------------

Because a list can be both a list of bytes (used in source files) and a list 
of code points (used by default), I suggest use the next form of writing
Unicode strings in your code:

```erlang
?ISTR(<<"Строка Unicode">>).
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
