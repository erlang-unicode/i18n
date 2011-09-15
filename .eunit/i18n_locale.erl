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

-module(i18n_locale).
-include("i18n.hrl").
-export([set_locale/1, set_default_locale/1, get_locale/0]).
-export([base_name/1]).

-define(SERVER, 'i18n_locale_server').

set_locale(Value) -> 
    LName = ?TRY_ATOM(?IM:locale_name(Value)),
    set_value('i18n_locale', LName).

set_default_locale(Value) -> 
    set_default_value('i18n_locale', Value).

get_locale() -> 
    get_value('i18n_locale').







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

base_name(LocaleId) ->
    ?TRY_ATOM(?IM:locale_base_name(LocaleId)).
