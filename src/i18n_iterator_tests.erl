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
-module(i18n_iterator_tests).
-include_lib("i18n/include/i18n.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").
    


simple_open_test(L) ->
    i18n_iterator:open('grapheme'),
    i18n_iterator:open('word'),
    i18n_iterator:open('line'),
    i18n_iterator:open('sentence'),
    ok.

advanced_open_test() ->
    lists:map(fun open_iterator_with_locale/1,
        i18n_iterator:available_locales()),
    ok.

%%
%% Helpers
%%

open_iterator_with_locale(L) ->
    i18n_iterator:open(L, 'grapheme'),
    i18n_iterator:open(L, 'word'),
    i18n_iterator:open(L, 'line'),
    i18n_iterator:open(L, 'sentence'),
    ok.

-endif.
