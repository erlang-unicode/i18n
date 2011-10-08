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

%%% @doc This module containg functions to manage the locale of the process.

-module(i18n_locale).
-include("i18n.hrl").
-define(SERVER, 'i18n_locale_server').

-export([set_locale/1, set_default_locale/1, get_locale/0]).
-export([base_name/1, parent_locale/1]).

-type i18n_locale_id() :: atom(). 


%% @doc Set the locale of this process.
%%      It will affect on all case-sensitive operations
%%      when the locale parameter will be skipped.
-spec set_locale(i18n_locale_id()) -> i18n_locale_id().

set_locale(Value) -> 
    LName = ?TRY_ATOM(?IM:locale_name(Value)),
    set_value('i18n_locale', fix_locale(LName)).


%% @doc Extract the locale of all processes, in which
%%      there is no call of `set_locale/1'.
-spec set_default_locale(i18n_locale_id()) -> i18n_locale_id().

set_default_locale(Value) -> 
    set_default_value('i18n_locale', fix_locale(Value)).


%% @doc Extract the locale of this process
-spec get_locale() -> i18n_locale_id().

get_locale() -> 
    get_value('i18n_locale').





%% @doc `ru_RU@col=COL' -> `ru_RU'
-spec base_name(i18n_locale_id()) -> i18n_locale_id().

base_name(LocaleId) ->
    fix_locale(?TRY_ATOM(?IM:locale_base_name(LocaleId))).


%% @doc `ru_RU' -> `ru'
-spec parent_locale(i18n_locale_id()) -> i18n_locale_id().

parent_locale(Locale) ->
    fix_locale(?TRY_ATOM(?IM:locale_parent(Locale))).


%%
%% Helpers
%%

fix_locale('') -> 'root';
fix_locale(X) -> X.


set_value(Key, Value) ->
    erlang:put(Key, Value),
    Value.

set_default_value(Key, Value) ->
    ?SERVER:set_default(Key, Value),
    Value.



get_value(Key) ->
    case erlang:get(Key) of
    'undefined' ->
        % Get a global value
        Value = ?SERVER:get_default(Key),
        erlang:put(Key, Value),
        Value;
    Value ->
        Value
    end.
