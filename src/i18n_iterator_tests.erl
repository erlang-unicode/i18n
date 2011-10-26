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
    i18n_iterator:open('word_only'),
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
    i18n_iterator:open(L, 'word_only'),
    i18n_iterator:open(L, 'line'),
    i18n_iterator:open(L, 'sentence'),
    ok.

len_test_() ->
    F = fun(X) ->
        I = i18n_iterator:open(X),
        i18n_string:len(I, ?ISTR("Test my string.")) end,

    [?_assertEqual(F('grapheme'), 15)
    ,?_assertEqual(F('word'), 6)
    ,?_assertEqual(F('word_only'), 3)
    ,?_assertEqual(F('line'), 3)
    ,?_assertEqual(F('sentence'), 1)].
    
split_test_() ->
    F = fun(X) ->
        I = i18n_iterator:open(X),
        lists:map(fun i18n:to/1, 
            i18n_string:split(I, 
                ?ISTR("Test my string."))) end,

    [?_assertEqual(F('grapheme'), 
        [<<"T">>,<<"e">>,<<"s">>,<<"t">>,<<" ">>,<<"m">>,<<"y">>,
         <<" ">>,<<"s">>,<<"t">>,<<"r">>,<<"i">>,<<"n">>,<<"g">>,
         <<".">>])
    ,?_assertEqual(F('word'), 
        [<<"Test">>,<<" ">>,<<"my">>,<<" ">>,<<"string">>,<<".">>])
    ,?_assertEqual(F('word_only'), 
        [<<"Test">>,<<"my">>,<<"string">>])
    ,?_assertEqual(F('line'), 
        [<<"Test ">>,<<"my ">>,<<"string.">>])
    ,?_assertEqual(F('sentence'), 
        [<<"Test my string.">>])].

-endif.
