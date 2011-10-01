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

-module(i18n_collation).
-include_lib("i18n.hrl").


% NIFs
-export([open/0, open/1, open/2]).
-export([sort_key/2, compare/3, sort/2]).

-export([map_sort/2]).


%%
%% Types
%%

-type i18n_locale_id() :: atom(). 
-type i18n_string() :: binary().


-type i18n_collation_option() ::
      'primary'
    | 'secondary'
    | 'tertiary'
    | 'quantiary'
    | 'identical'
    | 'shifted'
    | 'non_ignorable'
    | 'lower_first'
    | 'upper_first'
    | 'numeric'
    | 'french_accents'
    | 'hiragana'
    | 'normalization'.

-type resource() :: <<>>.
-type  i18n_collator() :: resource().
-type  i18n_sort_key() :: binary().
-type  i18n_compare_result() ::'less' | 'equal' | 'greater'.



%%
%% API
%%

-spec open() -> i18n_collator().
open() ->
    L = i18n_locale:get_locale(),
    open(L).

-spec open(i18n_locale_id()) -> i18n_collator().
open(L) when is_atom(L) ->
    ?TRY_RES(?IM:get_collator(L)).

-spec open(i18n_locale_id(), [i18n_collation_option()]) -> i18n_collator().
open(L, Options) ->
    ?TRY_RES(?IM:get_collator(L, lists:map(fun options/1, Options))).

-spec sort_key(i18n_collator(), i18n_string()) -> i18n_sort_key().
sort_key(C, S) ->
    ?TRY_BIN(?IM:sort_key(C, S)).

-spec compare(i18n_collator(), i18n_string(), i18n_string()) -> 
        i18n_compare_result().
compare(C, S1, S2) ->
    ?TRY_ATOM(?IM:compare(C, S1, S2)).


-spec sort(i18n_collator(),[i18n_string()]) -> 
        [i18n_string()].
sort(C, Ss) ->
    % Step 1: produce array of sort keys
    F = fun(S) -> 
            sort_key(C, S)
        end,
    map_sort(F, Ss).


%% @doc Xx list of strings to sort.
%%      F produces a sort keys.
-spec map_sort(fun(), [any()]) -> [any()].
map_sort(F, Xx) 
    when is_function(F) ->
    % Step 1: produce array of sort keys
    PairFn = fun(X) -> 
            Key = F(X),
            {Key, X} 
        end,
    Keys = lists:map(PairFn, Xx),

    % Step 2: sort array
    SortedKeys = lists:keysort(1, Keys),

    % Step 3: Return result
    RetFn = fun({_Key, X}) -> X end,
    lists:map(RetFn, SortedKeys).




%%
%% Helpers
%%

options(H) 
    when   H=:='primary'
    orelse H=:='secondary'
    orelse H=:='tertiary'
    orelse H=:='quanternary'
    orelse H=:='identical' ->
    {'strength', H};
options(H) 
    when   H=:='shifted'
    orelse H=:='non_ignorable' ->
    {'alternate', H};
options(H) 
    when   H=:='lower_first'
    orelse H=:='upper_first' ->
    {'case_first', H};
options(H) 
    when   H=:='numeric' % natural sort
    orelse H=:='french_accents' % bachwards on level 2
    orelse H=:='hiragana' % on level 4
    orelse H=:='normalization' ->
    {H, 'on'};
options(H) -> H.
