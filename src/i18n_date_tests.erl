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

test() ->
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
    ,?_assertEqual(F(i18n_date:new(2000,12,31),
                      i18n_date:new(0,1,1), [day, year]), 
                   [{year,-2000},{day,-365}])
    ,?_assertEqual(F(i18n_date:new(0,1,1),
                      i18n_date:new(2000,12,31), [day, year]), 
                   [{year,2000},{day,365}])
    ].

-endif.
