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
    
from_test_() ->
    B = i18n_string:from(<<"F">>),
    L = i18n_string:from("F"),
    A = i18n_string:from('F'),
    
    [?_assertEqual(B, L), ?_assertEqual(B, A)].
    
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

prop_len_test_() ->
    {"len prop testing.",
    	{timeout, 60,
    		fun() -> triq:check(prop_len()) end}}.

prop_get_iterator_parallel_test_() ->
    {"Cloner for the iterator type prop testing.",
    	{timeout, 60,
    		fun() -> triq:check(prop_get_iterator_parallel()) end}}.
    


prop_from_char() ->
   ?FORALL({Xs},{unicode_char()},
   	    i18n_string:to_utf8(i18n_string:from([Xs])) =:= [Xs]).

prop_from_utf8() ->
   ?FORALL({Xs},{unicode_binary(100)},
   	    i18n_string:to_utf8(i18n_string:from_utf8(Xs)) =:= Xs).

prop_concat() ->
   ?FORALL({Xs, Ys},{unicode_binary(100), unicode_binary(100)},
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
   	?FORALL({Xs},{unicode_binary(100)}, F(Xs)).
    
prop_len() ->
	I = i18n_iterator:open('grapheme'),
	F = fun(Xs) ->
		S = i18n_string:from_utf8(Xs),
		is_integer(i18n_string:len(I, S))
		end,
   	?FORALL({Xs},{unicode_binary(100)}, F(Xs)).
    
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
   	?FORALL({Xs},{unicode_binary(100)}, FF(F, Xs)).

-endif.
