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

-export([index/3, match_all/3, test/3, match/3]).


%%
%% Types
%%

-type i18n_string() :: binary().

-type resource() :: <<>>.
-type i18n_collator() :: resource().
-type i18n_search_option() :: atom().


%%
%% API
%%


index(Col, Pattern, String) ->
    ?TRY_LIST(?IM:search_index(Col, Pattern, String)).

match_all(Col, Pattern, String) ->
    ?TRY_LIST(?IM:search_match_all(Col, Pattern, String)).
    
-spec test(i18n_collator(), i18n_string(), i18n_string()) -> boolean().
%% @doc Test matches.
test(Col, Pattern, String) ->
    ?TRY_ATOM(?IM:search_test(Col, Pattern, String)).

match(Col, Pattern, String) ->
    ?TRY_STR_OR_ATOM(?IM:search_match(Col, Pattern, String)).
