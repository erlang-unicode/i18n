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
1> S=i18n_string:from_utf8(<<"the quick brown Fox jumps over the lazy Dog.">>).
<<116,0,104,0,101,0,32,0,113,0,117,0,105,0,99,0,107,0,32,
  0,98,0,114,0,111,0,119,0,110,...>>

2> i18n_string:to_utf8(i18n_string:to_upper(S)).                               
<<"THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG.">>

3> i18n_string:to_utf8(i18n_string:to_title(S)).                               
<<"The Quick Brown Fox Jumps Over The Lazy Dog.">>

4> I=i18n_iterator:open(sentence).                                       
<<>>

5> i18n_string:to_utf8(i18n_string:to_title(I, S)).                            
<<"The quick brown fox jumps over the lazy dog.">>
```
