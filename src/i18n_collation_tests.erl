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
-module(i18n_collation_tests).
-include_lib("i18n/include/i18n.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").

    
simple_open_test() ->
    i18n_collation:open().
    
for_all_locales_open_test_() ->
    ?_assert(
        lists:all(fun erlang:is_binary/1, 
            lists:map(fun i18n_collation:open/1, 
                i18n_collation:available_locales()))).

simple_sort_key_test() ->
    C = i18n_collation:open(),
    i18n_collation:sort_key(C, ?ISTR("A")).

simple_sort_test_() ->
    C = i18n_collation:open(),
    F = fun(X) -> i18n_collation:sort(C,X) end,
    {"Simple input data.",
        ?_assertEqual(F([?ISTR("A"), ?ISTR("C"), ?ISTR("B")]), [?ISTR("A"), ?ISTR("B"), ?ISTR("C")])}.

natural_sort_test_() ->
    Unsorted = ["1000X Radonius Maximus",
                "10X Radonius",
                "200X Radonius",
                "20X Radonius",
                "20X Radonius Prime",
                "30X Radonius",
                "40X Radonius",
                "Allegia 50 Clasteron",
                "Allegia 500 Clasteron",
                "Allegia 51 Clasteron",
                "Allegia 51B Clasteron",
                "Allegia 52 Clasteron",
                "Allegia 60 Clasteron",
                "Alpha 100",
                "Alpha 2",
                "Alpha 200",
                "Alpha 2A",
                "Alpha 2A-8000",
                "Alpha 2A-900",
                "Callisto Morphamax",
                "Callisto Morphamax 500",
                "Callisto Morphamax 5000",
                "Callisto Morphamax 600",
                "Callisto Morphamax 700",
                "Callisto Morphamax 7000",
                "Callisto Morphamax 7000 SE",
                "Callisto Morphamax 7000 SE2",
                "QRS-60 Intrinsia Machine",
                "QRS-60F Intrinsia Machine",
                "QRS-62 Intrinsia Machine",
                "QRS-62F Intrinsia Machine",
                "Xiph Xlater 10000",
                "Xiph Xlater 2000",
                "Xiph Xlater 300",
                "Xiph Xlater 40",
                "Xiph Xlater 5",
                "Xiph Xlater 50",
                "Xiph Xlater 500",
                "Xiph Xlater 5000",
                "Xiph Xlater 58"],

    Sorted = ["10X Radonius",
         "20X Radonius",
         "20X Radonius Prime",
         "30X Radonius",
         "40X Radonius",
         "200X Radonius",
         "1000X Radonius Maximus",
         "Allegia 50 Clasteron",
         "Allegia 51 Clasteron",
         "Allegia 51B Clasteron",
         "Allegia 52 Clasteron",
         "Allegia 60 Clasteron",
         "Allegia 500 Clasteron",
         "Alpha 2",
         "Alpha 2A",
         "Alpha 2A-900",
         "Alpha 2A-8000",
         "Alpha 100",
         "Alpha 200",
         "Callisto Morphamax",
         "Callisto Morphamax 500",
         "Callisto Morphamax 600",
         "Callisto Morphamax 700",
         "Callisto Morphamax 5000",
         "Callisto Morphamax 7000",
         "Callisto Morphamax 7000 SE",
         "Callisto Morphamax 7000 SE2",
         "QRS-60 Intrinsia Machine",
         "QRS-60F Intrinsia Machine",
         "QRS-62 Intrinsia Machine",
         "QRS-62F Intrinsia Machine",
         "Xiph Xlater 5",
         "Xiph Xlater 40",
         "Xiph Xlater 50",
         "Xiph Xlater 58",
         "Xiph Xlater 300",
         "Xiph Xlater 500",
         "Xiph Xlater 2000",
         "Xiph Xlater 5000",
         "Xiph Xlater 10000"],

    C = i18n_collation:open('root', [numeric]),
    KeyFn = fun(X) -> i18n_collation:sort_key(C, ?ISTR(X)) end,
    F = fun(X) -> i18n_collation:map_sort(KeyFn, X) end,


    [{"Using official test strings from Dave Koelle", 
        ?_assertEqual(F(Unsorted), Sorted)}
    ].



prop_sort_key_test_() ->
	{"sort_key prop testing.",
		{timeout, 60,
    		fun() -> triq:check(prop_sort_key()) end}}.

prop_compare_test_() ->
	{"compare prop testing.",
		{timeout, 60,
    		fun() -> triq:check(prop_compare()) end}}.


prop_sort_key() ->
    C = i18n_collation:open(),
    F = fun(Xs) ->
    	S = i18n_string:from_utf8(Xs),
    	Key = i18n_collation:sort_key(C, S),
    	is_binary(Key)
    	end,
   	?FORALL({Xs},{unicode_binary(100)}, F(Xs)).

prop_compare() ->
    C = i18n_collation:open(),
    F = fun(Xs) ->
    	S = i18n_string:from_utf8(Xs),
    	equal =:= i18n_collation:compare(C, S, S)
    	end,
   	?FORALL({Xs},{unicode_binary(100)}, F(Xs)).

-endif.
