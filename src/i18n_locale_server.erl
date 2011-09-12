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
-module(i18n_locale_server).

-export([start_link/0]).
-export([init/1, terminate/2, 
    handle_call/3]).
-export([set_default/2, get_default/1]).

-behavior(gen_server).

-define(CFG_TABLE_NAME, 'i18n_settings').

%% You can set a default locale as a compiler's parameter 
-ifdef(I18N_LOCALE).
-else.
-define(I18N_LOCALE, 'root').
-endif.

%% Exported Client Functions
%% Operation & Maintenance API
start_link() ->
    Arguments = [],
    Opts = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Arguments, Opts).

init([]) ->
    E = ets:new(?CFG_TABLE_NAME, [{read_concurrency, true}, named_table]),
    true = ets:insert(E, {'i18n_locale', ?I18N_LOCALE}),
    {ok, []}.

terminate(_Reason, _LoopData) ->
    ok.


handle_call({set_default, Key, Value}, _From, LoopData) ->
    % Update inherited values
    Reply = ets:insert(?CFG_TABLE_NAME, {Key, Value}),
    {reply, Reply, LoopData}.

    
%%
%% API
%%

get_default(Key) ->
    lookup(Key).

set_default(Key, Value) ->
    call({set_default, Key, Value}).
        

%%
%% Helpers
%%

call(Cmd) ->
    try
        gen_server:call(?MODULE, Cmd)
    catch
        exit:{noproc, _Stack} ->
        i18n:start(),
        gen_server:call(?MODULE, Cmd)
    end.

lookup(Key) ->
    try
    [{Key, Value}] = ets:lookup(?CFG_TABLE_NAME, Key),
    Value
    catch
%%      error:badarg -> 
%%      % Server is not running.
%%      i18n:start(),
%%      lookup(Key);

        error:{badmatch, _V} ->
        % Key is not found.
        'undefined'
    end.

