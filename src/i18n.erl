%% @doc i18n.
%% @private

-module(i18n).
-export([start/0, stop/0]).

% Short names for the console
-export([from/1, to/1, re/1, write/1]).
% Helpers for the console
-export([repeat/2]).

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
write(X) -> io:format("~ts", [to(X)]).
repeat(C, F) when C >= 0, is_function(F) ->
	do_repeat(C, F).

-spec do_repeat(non_neg_integer(), fun()) -> ok.
do_repeat(C, F) when C>0 ->
	F(),
	do_repeat(C - 1, F);
do_repeat(0, _F) ->
	ok.
