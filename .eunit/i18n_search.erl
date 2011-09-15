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

% NIFs
-export([open/0, open/1, open/2]).




%%
%% Types
%%

-type i18n_locale_id() :: atom(). 
-type i18n_string() :: binary().

-type resource() :: <<>>.
-type i18n_collator() :: resource().
-type i18n_search_option() :: atom().


%%
%% API
%%


-spec open() -> i18n_collator().
open() ->
    L = i18n_locale:get_locale(),
    open(L).

-spec open(i18n_locale_id()) -> i18n_collator().
open(_Locale) ->
    ?I18N_NIF_NOT_LOADED.

-spec open(i18n_locale_id(), [i18n_search_option()]) -> i18n_collator().
open(L, Options) ->
    C = open(L).
%   do_options(C, Options).

