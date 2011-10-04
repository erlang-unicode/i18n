; -- Mode: Markdown; -- ; vim: filetype=markdown tw=76 expandtab shiftwidth=4 tabstop=4


i18n: ICU for Erlang
====================

__License__: [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.html)

__Author__: Uvarov Michael ([`freeakk@gmail.com`](mailto:freeakk@gmail.com))

[Read edoc documentation](https://github.com/freeakk/i18n/blob/master/doc/README.md)

Module for working with strings.
A string is a binary UTF-16 string.


All actions with Unicode were described in the [Unicode Standards](http://www.unicode.org/reports/).
ICU Documentation [API Reference](http://icu-project.org/apiref/icu4c/).
Erlang NIF [erl_nif API](http://www.erlang.org/doc/man/erl_nif.html).



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


