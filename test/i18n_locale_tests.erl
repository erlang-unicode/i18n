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
%%% @author Michael Uvarov <arcusfelis@gmail.com>
%%% =====================================================================

%%% @private
-module(i18n_locale_tests).
-include_lib("i18n/include/i18n.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").

locale_test_() ->
    {spawn, fun() ->

        ?assertEqual(i18n_locale:set_locale(''), 'root'),
        ?assertEqual(i18n_locale:get_locale(), 'root'),

        ?assertEqual(i18n_locale:set_locale('ru_RU'), 'ru_RU'),
        ?assertEqual(i18n_locale:get_locale(), 'ru_RU'),

        ?assertEqual(i18n_locale:set_locale('root'), 'root'),
        ?assertEqual(i18n_locale:get_locale(), 'root'),
        ok
        end}.

locale_test() ->
    i18n_locale:set_default_locale('ru_RU'),
    'ok'.

base_name_test_() ->
    F = fun i18n_locale:base_name/1,
    [?_assertEqual(F('ru_RU@col=COL'), 'ru_RU')
    ].

parent_locale_test_() ->
    F = fun i18n_locale:parent_locale/1,
    [?_assertEqual(F('ru_RU'), 'ru')
    ,?_assertEqual(F('ru'), 'root')
    ].
    
-endif.
