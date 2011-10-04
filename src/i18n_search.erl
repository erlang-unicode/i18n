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

-module(i18n_search).
-include_lib("i18n.hrl").

-export([open/2]).
-export([index/2, match_all/2, test/2, match/2]).


%%
%% Types
%%

-type i18n_string() :: binary().

-type resource() :: <<>>.
-type i18n_collator() :: resource().
-type i18n_searcher() :: resource().
-type i18n_search_option() :: atom().


%%
%% API
%%

open(Col, Pattern) ->
    ?TRY_RES(?IM:search_open(Col, Pattern)).


index(Searcher, String) ->
    ?TRY_LIST(?IM:search_index(Searcher, String)).

match_all(Searcher, String) ->
    ?TRY_LIST(?IM:search_match_all(Searcher, String)).
    
%% @doc Test matches.
test(Searcher, String) ->
    ?TRY_ATOM(?IM:search_index(Searcher, String)).

match(Searcher, String) ->
    ?TRY_STR_OR_ATOM(?IM:search_match(Searcher, String)).
