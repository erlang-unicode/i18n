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
-module(i18n_message_tests).
-include_lib("i18n/include/i18n.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").
    
%%% argType = "number" | "date" | "time" | "spellout" | "ordinal" | "duration"
%%% argStyle = "short" | "medium" | "long" | "full" | "integer" | "currency" |
%%%    "percent" | argStyleText

prop_message_string_test_() ->
	{"message with string prop testing.",
		{timeout, 60,
    		fun() -> triq:check(prop_message_string()) end}}.
    
prop_message_int_test_() ->
	{"message with int prop testing.",
		{timeout, 60,
    		fun() -> triq:check(prop_message_int()) end}}.

prop_message_string() ->
    S = i18n_string:from("String {0}."),
    M = i18n_message:open(S),

    ?FORALL({Xs},{unicode_binary(100)},
   	    is_binary(i18n_message:format(M, [i18n_string:from_utf8(Xs)]))).

%% argType = number
%% argStyle = integer
prop_message_int() ->
    S = i18n_string:from("String {0,number, integer}."),
    M = i18n_message:open(S),

    ?FORALL({Xs},{int()},
   	    is_binary(i18n_message:format(M, [Xs]))).


-endif.
