%% @doc i18n.
%% @private

-module(i18n).
-export([start/0, stop/0]).
-export([from/1, to/1, re/1]).

%% @spec start() -> ok
%% @doc Start the ux server.
start() ->
    application:start(i18n).

%% @spec stop() -> ok
%% @doc Stop the ux server.
stop() ->
    application:stop(i18n).

from(X) -> i18n_string:from(X).
to(X) -> i18n_string:to_utf8(X).
re(X) -> i18n_regex:open(X).
