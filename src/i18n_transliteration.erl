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

%%% @doc Text Trasliteration


-module(i18n_transliteration).
-include("i18n.hrl").

-export([open/1, open/2]).
-export([available_ids/0]).
-export([do/2]).

-type i18n_transliterator_id() :: atom(). 

%% UTF-16 string
-type i18n_string() :: binary().   

-type resource() :: <<>>.   
-type i18n_transliterator() :: resource().   

-type i18n_direction() :: 'forward' | 'reverse'.



-spec open(i18n_transliterator_id()) -> i18n_transliterator().
open(Id) -> 
	Dir = 'forward',
    open(Id, Dir).


-spec open(i18n_transliterator_id(), i18n_direction()) -> 
		i18n_transliterator().

open(Id, Dir) when is_atom(Id), is_atom(Dir) -> 
    ?TRY_RES(?IM:get_transliterator(Id, Dir)).




-spec available_ids() -> [i18n_transliterator_id()].
available_ids() ->
    ?TRY_LIST(?IM:trans_ids()).

	
-spec do(i18n_transliterator(), i18n_string()) ->
		i18n_string().

do(T, S) ->
    ?TRY_STR(?IM:trans(T, S)).
