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
-module(i18n_search_tests).
-include_lib("i18n/include/i18n.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").

    

index_test_() ->
    C = i18n_collation:open(),
    S = i18n_search:open(C, ?ISTR("test")),
    F = fun i18n_search:index/2,
    [?_assertEqual(F(S, ?ISTR("open test")), [{5, 4}])
    ,?_assertEqual(F(S, ?ISTR("test test")), [{0, 4}, {5, 4}])
    ].

match_test_() ->
    C = i18n_collation:open(),
    S = i18n_search:open(C, ?ISTR("test")),
    F = fun i18n_search:match/2,
    [?_assertEqual(F(S, ?ISTR("open test")), ?ISTR("test"))
    ].

nocase_match_test_() ->
    C = i18n_collation:open([primary]),
    S = i18n_search:open(C, ?ISTR("test")),
    F = fun i18n_search:match/2,
    [?_assertEqual(F(S, ?ISTR("open Test")), ?ISTR("Test"))
    ].

nocase_match_all_test_() ->
    C = i18n_collation:open([primary]),
    S = i18n_search:open(C, ?ISTR("test")),
    F = fun i18n_search:match_all/2,
    [?_assertEqual(F(S, ?ISTR("test and Test")), [?ISTR("test"), ?ISTR("Test")])
    ].

nocase_test_() ->
    C = i18n_collation:open([primary]),
    S = i18n_search:open(C, ?ISTR("test")),
    F = fun i18n_search:test/2,
    [?_assert(F(S, ?ISTR("Test")))
    ,?_assert(F(S, ?ISTR("test")))
    ,?_assert(F(S, ?ISTR("it is a test")))
    ,?_assert(not F(S, ?ISTR("it is an error")))
    ].

-endif.
