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

-module(i18n_regex).
-include("i18n.hrl").

-type i18n_string() :: binary(). 

-type resource() :: <<>>.   
-type i18n_regex() :: resource().   

% NIFs
-export([open/1]).
-export([split/2, replace/3, replace_all/3, test/2, match/2, match_all/2]).



-spec open(i18n_string()) -> i18n_regex().
%% @doc Parse a message to a resourse.
open(S) ->
    ?TRY_RES(?IM:open_regex(S)).

%% @doc Split a string to a list.
-spec split(i18n_regex(), i18n_string()) -> [i18n_string()].
split(Re, S) ->
    ?TRY_LIST(?IM:regex_split(Re, S)).

%% @doc Replace first element of the text by the pattern.
-spec replace(i18n_regex(), Pattern::i18n_string(), Source::i18n_string()) -> 
        i18n_string().
replace(Re, R, S) ->
    ?TRY_STR(?IM:regex_replace(Re, R, S)).

%% @doc Replace all finded elements.
-spec replace_all(i18n_regex(), Pattern::i18n_string(), Source::i18n_string()) -> 
        i18n_string().
replace_all(Re, R, S) ->
    ?TRY_STR(?IM:regex_replace_all(Re, R, S)).

-spec test(i18n_regex(), i18n_string()) -> boolean().
%% @doc Test matches.
test(Re, S) ->
    ?TRY_ATOM(?IM:regex_test(Re, S)).

%% @doc Return first match as a list.
-spec match(i18n_regex(), i18n_string()) -> [i18n_string()].
match(Re, S) ->
    ?TRY_LIST(?IM:regex_match(Re, S)).

%% @doc Return all matches as a list of lists.
-spec match_all(i18n_regex(), i18n_string()) -> [[i18n_string()]].
match_all(Re, S) ->
    ?TRY_LIST(?IM:regex_match_all(Re, S)).
