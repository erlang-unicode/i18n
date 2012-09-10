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
%%% @author Michael Uvarov <arcusfelis@gmail.com>
%%% =====================================================================

%%% @doc Text Boundary Analysis (Break Iteration) 

-module(i18n_iterator).
-include("i18n.hrl").

-export([open/1, open/2]).
-export([available_locales/0]).

-type i18n_locale_id() :: atom(). 

-type resource() :: <<>>.   
-type i18n_iterator() :: resource().   


-type i18n_string_iterator_type() :: 'grapheme' | 'word' | 'sentence' | 'line'
            | 'word_only'.

-spec open(i18n_string_iterator_type()) -> i18n_iterator().
open(T) -> 
    L = i18n_locale:get_locale(),
    open(L, T).

-spec open(i18n_locale_id(), i18n_string_iterator_type()) -> i18n_iterator().
open(L, T) when is_atom(T) -> 
    ?TRY_RES(?IM:get_iterator(L, T)).

-spec available_locales() -> [i18n_locale_id()].
available_locales() ->
	?TRY_LIST(?IM:iterator_locales()).

