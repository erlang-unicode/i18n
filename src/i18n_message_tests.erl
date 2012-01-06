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

equal([H, H|T]) ->
    equal([H|T]);
equal([]) ->
    true;
equal([H]) ->
    true;
equal(_) ->
    false.

equal_test_() ->
    [?_assert(equal([1,1,1]))
    ,?_assert(equal([1,1]))
    ,?_assert(not equal([1,2]))
    ,?_assert(not equal([1,1,3]))
    ].

%message_test_() ->
%    M = i18n_message:open(?ISTR("Hello, {name}. Now {now, time, full}.")),
%    R = i18n_message:format(M, [
%            {'name', ?ISTR("Username")},
%            {'now',  i18n_date:now()}
%        ]),
%    io:format(user, "~n~ts~n", [i18n_string:to_utf8(R)]),
%
%    [?_assert(is_binary(R))].
%
%%%% argType = "number" | "date" | "time" | "spellout" | "ordinal" | "duration"
%%%% argStyle = "short" | "medium" | "long" | "full" | "integer" | "currency" |
%%%%    "percent" | argStyleText
%
%get_checker_format(S) ->
%    M = i18n_message:open(S),
%
%    % This fun returns boolean
%    fun(X) ->
%        B = i18n_message:format(M, [X]),
%        equal([B,
%               i18n_message:format(M, [{0, X}]),
%               i18n_message:format(M, [{'0', X}])
%              ]) 
%        andalso is_binary(B)
%    end.
%
%prop_message_string_test_() ->
%    {"message with string prop testing.",
%    	{timeout, 60,
%    		fun() -> triq:check(prop_message_string()) end}}.
%    
%prop_message_int_test_() ->
%    {"message with int prop testing.",
%    	{timeout, 60,
%    		fun() -> triq:check(prop_message_int()) end}}.
%    
%prop_message_double_test_() ->
%    {"message with double prop testing.",
%    	{timeout, 60,
%    		fun() -> triq:check(prop_message_double()) end}}.
%    
%prop_message_date_test_() ->
%    {"message with date prop testing.",
%    	{timeout, 60,
%    		fun() -> triq:check(prop_message_date()) end}}.
%
%prop_message_string() ->
%    S = ?ISTR("String {0}."),
%    F = get_checker_format(S),
%    ?FORALL({Xs}, {unicode_binary()}, F(?ISTR(Xs))).
%
%prop_message_int() ->
%    S = i18n_string:from("String {0, number, integer}."),
%    F = get_checker_format(S),
%    ?FORALL({Xs}, {int()}, F(Xs)).
%
%prop_message_double() ->
%    S = i18n_string:from("String {0, number, double}."),
%    F = get_checker_format(S),
%    ?FORALL({Xs}, {real()}, F(Xs)).
%
%prop_message_date() ->
%    S = i18n_string:from("String {0, date}."),
%    F = get_checker_format(S),
%    ?FORALL({Xs}, {real()}, F(Xs)).

message_order_test_() ->
    OF = fun i18n_message:open/1,
    FF = fun i18n_message:format/2,
    Date = i18n_date:new(2011,12,8),
    MNums = OF(?ISTR("{0,date} {1,number}")),
    MNames = OF(?ISTR("{d,date} {n,number}")),
    Res1 = ?ISTR("2011 12 8 3.3"),

    [?_assertEqual(Res1, FF(MNums, [Date, 3.3]))
    ,?_assertEqual(Res1, FF(MNums, [{0, Date}, {1, 3.3}]))
    ,?_assertEqual(Res1, FF(MNums, [{'0', Date}, {'1', 3.3}]))
    ,?_assertEqual(Res1, FF(MNums, 
                        [{?ISTR("0"), Date}, {?ISTR("1"), 3.3}]))
    ,?_assertEqual(Res1, FF(MNums, [{'1', 3.3}, {'0', Date}]))
    ,?_assertEqual(Res1, FF(MNums, [{?ISTR("1"), 3.3}, {?ISTR("0"), Date}]))

    ,?_assertEqual(Res1, FF(MNames, [{d, Date}, {n, 3.3}]))
%   ,?_assertEqual(Res1, FF(MNames, [{n, 3.3}, {d, Date}]))
%   ,?_assertEqual(Res1, FF(MNames, [{?ISTR("n"), 3.3}, {?ISTR("d"), Date}]))
    ].

-endif.
