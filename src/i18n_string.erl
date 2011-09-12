% vim: set filetype=erlang shiftwidth=4 tabstop=4 expandtab tw=80:

%%% =====================================================================
%%%   Copyright 2011 Uvarov Michael 
%%%
%%%   Licensed under the Apache License, Version 2.0 (the "License");
%%%   you may not use this file except in compliance with the License.
%%%   You may obtain a copy of the License at
%%%
%%%       http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%   Unless required by applicable law or agreed to in writing, software
%%%   distributed under the License is distributed on an "AS IS" BASIS,
%%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%   See the License for the specific language governing permissions and
%%%   limitations under the License.
%%%
%%% $Id$
%%%
%%% @copyright 2010-2011 Michael Uvarov
%%% @author Michael Uvarov <freeakk@gmail.com>
%%% =====================================================================

-module(i18n_string).
-include("i18n.hrl").

-type i18n_locale_id() :: atom(). 

%% UTF-8 string
-type unicode_binary() :: binary(). 

%% UTF-16 string
-type i18n_string() :: binary().   

-type resource() :: <<>>.   
-type i18n_iterator() :: resource().   

-type i18n_string_iterator_type() :: 'grapheme' | 'word' | 'sentence' | 'line'.

% NIFs
-export([from/1]).
-export([from_utf8/1, to_utf8/1]).
-export([concat/2]).


-export([to_lower/2, to_upper/2, to_title/2]).
-export([to_lower/1, to_upper/1, to_title/1]).
-export([get_iterator/1, get_iterator/2, len/2]).


from(B) 
    when is_binary(B) ->
    from_utf8(B);
from(L) 
    when is_list(L) ->
    B = unicode:characters_to_binary(L),
    from_utf8(B);
from(A) 
    when is_atom(A) ->
    L = erlang:atom_to_list(A),
    B = erlang:atom_to_binary(L),
    from_utf8(B).


-spec from_utf8(unicode_binary()) -> i18n_string().
from_utf8(B) ->
    ?TRY_STR(?IM:from_utf8(B)).

-spec to_utf8(i18n_string()) -> unicode_binary().
to_utf8(B) ->
    ?TRY_BIN(?IM:to_utf8(B)).

-spec concat(i18n_string(), i18n_string()) -> i18n_string().
concat(B1, B2) -> 
    ?TRY_STR(?IM:concat(B1, B2)).


-spec to_lower(i18n_string()) -> i18n_string().
to_lower(S) ->
    L = i18n_locale:get_locale(),
    to_lower(L, S).

-spec to_upper(i18n_string()) -> i18n_string().
to_upper(S) ->
    L = i18n_locale:get_locale(),
    to_upper(L, S).

-spec to_title(i18n_string()) -> i18n_string().
to_title(S) ->
    L = i18n_locale:get_locale(),
    to_title(L, S).

-spec get_iterator(i18n_string_iterator_type()) -> i18n_iterator().
get_iterator(T) -> 
    L = i18n_locale:get_locale(),
    get_iterator(L, T).

%% @doc Count of code paints.
-spec len(i18n_string()) -> non_neg_integer().
len(S) when is_integer(S) -> 
    size(S) div 2.

%% @doc Count the length og the string with help of an iterator.
%%
%% ```
%% i18n_string:len(i18n_string:get_iterator('grapheme'), ?ISTR("Example"));
%% '''
-spec len(i18n_iterator(), i18n_string()) -> non_neg_integer().
len(I, S) -> 
    ?TRY_INT(?IM:len(I, S)).

-spec to_lower(i18n_locale_id(), i18n_string()) -> i18n_string().
to_lower(L, S) when is_atom(L) ->
    ?TRY_STR(?IM:to_lower(L, S)).

-spec to_upper(i18n_locale_id(), i18n_string()) -> i18n_string().
to_upper(L, S) when is_atom(L) ->
    ?TRY_STR(?IM:to_upper(L, S)).

-spec to_title(i18n_locale_id() | i18n_iterator(), i18n_string()) -> i18n_string().
to_title(L, S) ->
    ?TRY_STR(?IM:to_title(L, S)).

-spec get_iterator(i18n_locale_id(), i18n_string_iterator_type()) -> i18n_iterator().
get_iterator(S, T) when is_atom(T) -> 
    ?TRY_RES(?IM:get_iterator(S, T)).


