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

-module(i18n_calendar).
-include("i18n.hrl").
-export([open/0, open/1, open/2, open/3]).
-export([available_locales/0]).

-type i18n_locale_id() :: atom(). 
-type resource() :: <<>>.   
-type i18n_calendar() :: resource(). 
-type i18n_timezone() :: atom() | i18n_string(). 
-type i18n_calendar_type() :: .

-spec open() -> i18n_calendar().

open() ->
    Locale = i18n_locale:get_locale(),
    open(Locale).


-spec open(i18n_locale()) -> i18n_calendar().

open(Locale) ->
	?TRY_RES(?IM:open_calendar(Locale)).

-spec open(i18n_locale(), i18n_timezone()) -> i18n_calendar().

open(Locale, TZ) when is_atom(TZ) ->
	?TRY_RES(?IM:open_calendar(Locale, i18n_string:from(TZ)));
open(Locale, TZ) ->
	?TRY_RES(?IM:open_calendar(Locale, TZ)).


-spec open(i18n_locale(), i18n_timezone(), i18n_calendar_type()) -> 
        i18n_calendar().

open(Locale, TZ, Type) when is_atom(TZ) ->
	?TRY_RES(?IM:open_calendar(Locale, i18n_string:from(TZ), Type));
open(Locale, TZ, Type) ->
	?TRY_RES(?IM:open_calendar(Locale, TZ, Type)).


-spec available_locales() -> [i18n_locale_id()].
available_locales() ->
	?TRY_LIST(?IM:calendar_locales()).

