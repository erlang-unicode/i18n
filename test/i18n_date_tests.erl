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
-module(i18n_date_tests).
-include_lib("i18n/include/i18n.hrl").
-define(M, i18n_date).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").

simple_add_test() ->
    ?M:add([{day, 1}]).

compare_test_() ->
    % Year Month Day
    [?_assert(?M:compare('month', 
            ?M:new(1990, 12, 5),
            ?M:new(1990, 12, 19)))
    ,?_assert(not ?M:compare('month', 
            ?M:new(1991, 12, 19),
            ?M:new(1990, 12, 19)))
    ].

difference_test_() ->
    F = fun ?M:difference/3,
    Now = ?M:now(),

    [?_assertEqual(F(Now, Now, day), 0)
    ,?_assertEqual(F(Now, Now, [day]), [{day, 0}])
    ,?_assertEqual(F(Now, Now, [day, year]), [{year, 0}, {day, 0}])
    ,?_assertEqual(F(i18n_date:new(0,12,31),
                     i18n_date:new(0,1,1), day), -365)
% This function has different behavior with different versions of ICU.
    ,?_assertEqual(F(i18n_date:new(2000,12,31),
                     i18n_date:new(0,1,1), [day, year]), 
                   [{year,-2000},{day,-365}])
    ,?_assertEqual(F(i18n_date:new(2000,1,1),
                      i18n_date:new(2000,12,31), [day]), 
                   [{day,365}])

    % ICU has 'bad' support of negative years.
%   ,?_assertException(error, {i18n_error,'U_UNSUPPORTED_ERROR'},
%       F(i18n_date:new(0,1,1), i18n_date:new(2000,12,31), [year]))
    % Fixed for lists (use extended_year instead of year by default).
    ,?_assertEqual(F(i18n_date:new(0,1,1),
                     i18n_date:new(2000,12,31), [year]), [{year,2000}])
    % Atoms still have the native ICU's behaviour.
%   ,?_assertException(error, {i18n_error,'U_UNSUPPORTED_ERROR'},
%       F(i18n_date:new(0,1,1), i18n_date:new(2000,12,31), year))

    ,?_assertEqual(F(i18n_date:new(0,1,1),
                     i18n_date:new(2000,12,31), extended_year), 2000)
    ,?_assertEqual(F(i18n_date:new(1999,1,1),
                     i18n_date:new(2000,1,1), [extended_year]), 
                   [{extended_year,1}])
    ,?_assertEqual(F(i18n_date:new(1999,1,1),
                     i18n_date:new(2000,1,1), [extended_year, day]), 
                   [{extended_year,1},{day,0}])
    ,?_assertEqual(F(i18n_date:new(0,1,1),
                     i18n_date:new(2000,12,31), [day_of_year, extended_year]), 
                   [{extended_year,2000},{day_of_year,365}])
    ,?_assertEqual(F(i18n_date:new(0,1,1),
                      i18n_date:new(2000,12,31), [day, extended_year]), 
                   [{extended_year,2000},{day,365}])
    ].

add_date_test_() ->
    F = fun ?M:add/1,
    {"Uses only fields.",
        do_add_common(F)}.

add2_date_test_() ->
    F = fun ?M:add/2,
    Now = ?M:now(),
    Zero = ?M:new(0,1,1),
    Y2k = ?M:new(2000,1,1),
    FF = fun(Fields) -> F(Now, Fields) end,
    
    {"First argument is a date.",
        [?_assert(F(Zero, [{extended_year,1999}])<Y2k)
        ,?_assert(F(Zero, [{extended_year,2012}])>Y2k)
        ] ++ do_add_common(FF)}.

add2_calendar_test_() ->
    F = fun ?M:add/2,
    Cal = i18n_calendar:open(),
    {"First argument is a calendar.",
        [?_assert(F(Cal, [{day, 10}])>F(Cal, [{day, -10}]))]}.

do_add_common(F) ->
    Now = ?M:now(),

    [?_assert(is_float(F([{day,0}])))
    ,?_assert(is_float(F([{day,10}, {year,1}])))
    ,?_assert(is_float(F([{day,1000}])))
    ,?_assert(F([{day, 1}])>Now)
    ,?_assert(F([{day,-1}])<Now)].

clear_test_() ->
    F = fun ?M:clear/2,
    Date = ?M:new(2000, 12, 5,   09, 45, 15),

    lists:map(fun(Field) ->
        ?_assertEqual(?M:get(F(Date, [Field]), Field), 0)
    end, [minute, second]).

new_test_() ->
    Cal = i18n_calendar:open(),
    Date1 = ?M:new(Cal, 2000, 12, 5,   09, 45, 15),
    Date2 = ?M:new(Cal, 2000, 12, 5),
    [?_assertEqual(?M:get(Cal, Date1, year), ?M:get(Cal, Date2, year))
    ,?_assertEqual(?M:get(Cal, Date1, month), ?M:get(Cal, Date2, month))
    ,?_assertEqual(?M:get(Cal, Date1, day), ?M:get(Cal, Date2, day))
    ].

get3_list_test() ->
    Cal = i18n_calendar:open(),
    Date1 = ?M:new(Cal, 2000, 12, 5,   09, 45, 15),

    F = fun ?M:get/3,
    [{hour, 9}, {minute, 45}, {second, 15}] 
        = F(Cal, Date1, [hour, minute, second]).

get2_list_test_() ->
    Cal = i18n_calendar:open(),

    F = fun ?M:get/2,
    [{hour, H}, {minute, M}, {second, S}] 
        = F(Cal, [hour, minute, second]),

    [?_assert(is_integer(H))
    ,?_assert(is_integer(M))
    ,?_assert(is_integer(S))
    ].

get1_list_test_() ->

    F = fun ?M:get/1,
    [{hour, H}, {minute, M}, {second, S}] 
        = F([hour, minute, second]),

    [?_assert(is_integer(H))
    ,?_assert(is_integer(M))
    ,?_assert(is_integer(S))
    ].

roll3_test_() ->
    Cal = i18n_calendar:open(),
    Date = ?M:new(Cal, 2000, 12, 5,   09, 45, 15),
    RolledDate1 = ?M:roll(Cal, Date, [{second, 55}]),
    RolledDate2 = ?M:roll(Cal, Date, [{second, -55}]),

    [?_assertEqual(?M:get(Cal, RolledDate1, [second, minute]), 
        [{second, 10}, {minute, 45}])
    ,?_assertEqual(?M:get(Cal, RolledDate1, second), 10)
    ,?_assertEqual(?M:get(Cal, RolledDate1, minute), 45)

    ,?_assertEqual(?M:get(Cal, RolledDate2, [second, minute]), 
        [{second, 20}, {minute, 45}])
    ,?_assertEqual(?M:get(Cal, RolledDate2, second), 20)
    ,?_assertEqual(?M:get(Cal, RolledDate2, minute), 45)
    ].

roll_test_() ->
    Cal = i18n_calendar:open(),
    Date = ?M:new(Cal, 2000, 12, 5,   09, 45, 15),
    RolledDate1 = ?M:roll(Date, [{second, 55}]),
    _RolledDate2 = ?M:roll(Cal,  [{second, 55}]),
    _RolledDate3 = ?M:roll([{second, 55}]),

    [?_assertEqual(?M:get(Cal, RolledDate1, second), 10)
    ,?_assertEqual(?M:get(Cal, RolledDate1, minute), 45)
    ].

set_test_() ->
    Date = ?M:new(2000, 12, 5,   09, 45, 15),
    NewDate = ?M:set(Date, [{minute, 30}]),

    
    Cal = i18n_calendar:open(),
    HalfHour1 = ?M:set(Cal, [{minute, 30}]),
    HalfHour2 = ?M:set([{minute, 30}]),
    

    [?_assertEqual(45, ?M:get(Date, minute))
    ,?_assertEqual(30, ?M:get(NewDate, minute))


    % Are not changed
    ,?_assertEqual(2000, ?M:get(NewDate, year))
    ,?_assertEqual(12, ?M:get(NewDate, month))
    ,?_assertEqual(05, ?M:get(NewDate, day))
    ,?_assertEqual(09, ?M:get(NewDate, hour))
    ,?_assertEqual(15, ?M:get(NewDate, second))


    ,?_assertEqual(30, ?M:get(HalfHour1, minute))
    ,?_assertEqual(30, ?M:get(HalfHour2, minute))
    ].

-endif.
