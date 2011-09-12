%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc i18n.
%% @private

-module(i18n).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).
-export([repeat/2]).

%% @spec start() -> ok
%% @doc Start the ux server.
start() ->
    application:start(i18n).

repeat(N, F) when N>0 -> 
F(), repeat(N-1, F);
repeat(N, F) -> ok.

%% @spec stop() -> ok
%% @doc Stop the ux server.
stop() ->
    application:stop(i18n).
