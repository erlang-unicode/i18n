%% @doc i18n.

-module(i18n).
-include("i18n.hrl").
-include_lib("i18n/include/i18n.hrl").
-export([start/0, stop/0]).
-export([print/1, print_date/1, print_time/1, print_number/1]).

% Short names for the console
-export([from/1, to/1, re/1]).
% Helpers for the console
-export([repeat/2]).

-export([icu_version/0, unicode_version/0]).


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

repeat(C, F) when C >= 0, is_function(F) ->
	do_repeat(C, F).

-spec do_repeat(non_neg_integer(), fun()) -> ok.
do_repeat(C, F) when C>0 ->
	F(),
	do_repeat(C - 1, F);
do_repeat(0, _F) ->
	ok.

icu_version() ->
    ?TRY_ATOM(?IM:icu_version()).

unicode_version() ->
    ?TRY_ATOM(?IM:unicode_version()).

%%
%% Print Helpers
%%

print(IStr) ->
    io:format("~ts", [to(IStr)]).

print_date(Date) ->
    M = i18n_message:open(?ISTR("{0,date}")),
    i18n:print(i18n_message:format(M, [Date])).

print_time(Date) ->
    M = i18n_message:open(?ISTR("{0,time}")),
    i18n:print(i18n_message:format(M, [Date])).

print_number(Num) ->
    M = i18n_message:open(?ISTR("{0,number}")),
    i18n:print(i18n_message:format(M, [float(Num)])).


