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

%%% @private
%%% Private functions. Use API functions from modules instead.

-module(i18n_nif).
-include("i18n.hrl").
-on_load(init/0).

-export([init/0]).


% NIFs
-export([from_utf8/1, to_utf8/1]).
-export([concat/2]).

-export([to_lower/2, to_upper/2, to_title/2]).
-export([get_iterator/2, len/2]).


-export([get_collator/1]).
-export([sort_key/2, compare/3]).
% Bad function
-export([collator_set_attr/3]).

-export([open_format/2, format/2, format/3]).

init() ->
    i18n:start(),
    Nif = ?I18N_NIF_PATH(?MODULE),
    erlang:load_nif(Nif, 0).



from_utf8(_BinUTF8) ->
      ?I18N_NIF_NOT_LOADED.

to_utf8(_BinUTF16) ->
      ?I18N_NIF_NOT_LOADED.

append(_BinUTF16Beginning, _BinUTF16Ending) -> 
      ?I18N_NIF_NOT_LOADED.

len(_IterResource, _String) -> 
    ?I18N_NIF_NOT_LOADED.

to_lower(_Locale, _String) ->
    ?I18N_NIF_NOT_LOADED.

to_upper(_Locale, _String) ->
    ?I18N_NIF_NOT_LOADED.

to_title(_Locale, _String) ->
    ?I18N_NIF_NOT_LOADED.

get_iterator(_Locale, _Type) -> 
    ?I18N_NIF_NOT_LOADED.



%%
%% Collation
%%

get_collator(_Locale) ->
    ?I18N_NIF_NOT_LOADED.

sort_key(_CollatorResource, _String) ->
    ?I18N_NIF_NOT_LOADED.

compare(_CollatorResource, _String, _String2) ->
    ?I18N_NIF_NOT_LOADED.

%% Destructive function!
collator_set_attr(_CollationResource, _Key, _Value) ->
    ?I18N_NIF_NOT_LOADED.





%%
%% Message Format
%%

open_format(_L, _S) ->
    ?I18N_NIF_NOT_LOADED.

format(_Mesage, _Parameters) ->
    ?I18N_NIF_NOT_LOADED.

format(_Mesage, _Parameters, _AppendTo) ->
    ?I18N_NIF_NOT_LOADED.
