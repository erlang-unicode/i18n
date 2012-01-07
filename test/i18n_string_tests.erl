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
-module(i18n_string_tests).
-include_lib("i18n/include/i18n.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").

-define(UBINARY, unicode_binary(20)).
    
from_test_() ->
    B = i18n_string:from(<<"F">>),
    L = i18n_string:from("F"),
    A = i18n_string:from('F'),
    % Atom A can be processed by parse_transform. Then some code will be 
    % not covered by tests.
    AA = i18n_string:from(f_atom()),
    
    [?_assertEqual(B, L), ?_assertEqual(B, A), ?_assertEqual(AA, A)].

f_atom() -> 'F'.
    
from_utf8_test() ->
    i18n_string:from_utf8(<<"F">>),
    ok.

simple_normalization_test() ->
    S = i18n_string:from_utf8(<<"Simple test">>),
    i18n_string:to_nfc(S),
    i18n_string:to_nfd(S),
    i18n_string:to_nfkc(S),
    i18n_string:to_nfkd(S),
    ok.

case_compare2_test_() ->
    F = fun i18n_string:case_compare/3,
    [?_assertEqual('greater', F(tr, ?ISTR("I"),   ?ISTR("i")))
    ,?_assertEqual('equal',   F(tr, ?ISTR("L"),   ?ISTR("l")))
    ,?_assertEqual('equal',   F(root, ?ISTR("L"), ?ISTR("l")))
    ,?_assertEqual('greater', F(root, ?ISTR("z"), ?ISTR("i")))
    ,?_assertEqual('less',    F(root, ?ISTR("i"), ?ISTR("z")))
    ,?_assertEqual('equal',   F(root, ?ISTR("word"), ?ISTR("word")))
    ].

case_compare_test_() ->
    F = fun i18n_string:case_compare/2,
    [?_assertEqual('equal',   F(?ISTR("L"), ?ISTR("l")))
    ,?_assertEqual('greater', F(?ISTR("z"), ?ISTR("i")))
    ,?_assertEqual('less',    F(?ISTR("i"), ?ISTR("z")))
    ,?_assertEqual('equal',   F(?ISTR("word"), ?ISTR("word")))
    ].

compare_test_() ->
    F = fun i18n_string:compare/2,
    [?_assertEqual('less',    F(?ISTR("I"), ?ISTR("i")))
    ,?_assertEqual('greater', F(?ISTR("z"), ?ISTR("i")))
    ,?_assertEqual('less',    F(?ISTR("i"), ?ISTR("z")))
    ,?_assertEqual('equal',   F(?ISTR("word"), ?ISTR("word")))
    ].

len_test_() ->
    F = fun i18n_string:len/1,
    [?_assertEqual(F(?ISTR("I")), 1)
    ,?_assertEqual(F(?ISTR("Test")), 4)
    ,?_assertEqual(F(?ISTR("Another test")), 12)
    ].


prop_from_char_test_() ->
	{"1 char conversation prop testing.",
		{timeout, 60,
			fun() -> triq:check(prop_from_char()) end}}.

prop_from_utf8_test_() ->
	{"from_utf8 and to_utf8 prop testing.",
		{timeout, 60,
    		fun() -> triq:check(prop_from_utf8()) end}}.

prop_concat_test_() ->
	{"Simple concat prop testing.",
		{timeout, 60,
    		fun() -> triq:check(prop_concat()) end}}.

prop_case_test_() ->
    {"to_lower and to_upper prop testing.",
    	{timeout, 60,
    		fun() -> triq:check(prop_case()) end}}.


prop_byte_len_test_() ->
    {"len prop testing (byte_size).",
    	{timeout, 60,
    		fun() -> triq:check(prop_len()) end}}.

prop_iterator_len_test_() ->
    {"len prop testing with a grapheme iterator.",
    	{timeout, 60,
    		fun() -> triq:check(prop_len2()) end}}.


prop_get_iterator_parallel_test_() ->
    {"Cloner for the iterator type prop testing.",
    	{timeout, 60,
    		fun() -> triq:check(prop_get_iterator_parallel()) end}}.
    


prop_from_char() ->
   ?FORALL({Xs},{unicode_char()},
   	    i18n_string:to_utf8(i18n_string:from([Xs])) =:= [Xs]).

prop_from_utf8() ->
   ?FORALL({Xs},{?UBINARY},
   	    i18n_string:to_utf8(i18n_string:from_utf8(Xs)) =:= Xs).

prop_concat() ->
   ?FORALL({Xs, Ys},{?UBINARY, unicode_binary(100)},
   	    is_binary(i18n_string:concat(
                i18n_string:from_utf8(Xs),
                i18n_string:from_utf8(Ys)))).
    
prop_case() ->
	I = i18n_iterator:open('grapheme'),
	F = fun(Xs) ->
		S = i18n_string:from_utf8(Xs),
		i18n_string:to_lower(S) =:= i18n_string:to_lower(i18n_string:to_upper(S))
    	andalso
    	i18n_string:to_upper(S) =:= i18n_string:to_upper(i18n_string:to_upper(S))
    	andalso
    	i18n_string:to_title(S) =:= i18n_string:to_title(i18n_string:to_upper(S))
    	andalso
    	i18n_string:to_title(I, S) =:= i18n_string:to_title(I, i18n_string:to_upper(S))
		end,
   	?FORALL({Xs},{?UBINARY}, F(Xs)).

    
prop_len() ->
	F = fun(Xs) ->
		S = i18n_string:from_utf8(Xs),
		is_integer(i18n_string:len(S))
		end,
   	?FORALL({Xs},{?UBINARY}, F(Xs)).

prop_len2() ->
	I = i18n_iterator:open('grapheme'),
	F = fun(Xs) ->
		S = i18n_string:from_utf8(Xs),
		is_integer(i18n_string:len(I, S))
		end,
   	?FORALL({Xs},{?UBINARY}, F(Xs)).

    
prop_get_iterator_parallel() ->
	I = i18n_iterator:open('grapheme'),
	F = fun(Xs) ->
		S = i18n_string:from_utf8(Xs),
		is_integer(i18n_string:len(I, S))
		end,
    FF = fun(F2, Xs) -> 
        spawn(fun() -> F2(Xs) end), 
        true 
        end,
   	?FORALL({Xs},{?UBINARY}, FF(F, Xs)).

normalization_test_() ->
    [?_assertEqual(i18n_string:to_nfc(i18n_string:to_nfd(<<199,0>>)),
                   i18n_string:to_nfc(<<199,0>>))
    ].

-endif.
