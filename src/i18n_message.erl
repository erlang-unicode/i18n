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
%%% @author Michael Uvarov <arcusfelis@gmail.com>
%%% =====================================================================

-module(i18n_message).
-include("i18n.hrl").

-type i18n_string() :: binary(). 
-type i18n_locale_id() :: atom(). 

-type resource() :: <<>>.   
-type i18n_msg_format() :: resource().   
-type i18n_msg_param() :: {atom(), i18n_msg_arg()}.
-type i18n_msg_arg() :: any().

% NIFs
-export([open/1, open/2]).
-export([format/2, format/3]).



-spec open(i18n_locale_id(), i18n_string()) -> i18n_msg_format().
%% @doc Parse a message to a resourse.
open(L, S) ->
    ?TRY_RES(?IM:open_format(L, S)).

-spec open(i18n_string()) -> i18n_msg_format().
open(S) ->
    L = i18n_locale:get_locale(),
    open(L, S).

-spec format(i18n_msg_format(), [i18n_msg_param()]) -> i18n_string().
format(M, P) ->
    ?TRY_STR(?IM:format(M, P)).
    
-spec format(i18n_msg_format(), [i18n_msg_param()], i18n_string()) -> i18n_string().
format(M, P, A) ->
    ?TRY_STR(?IM:format(M, P, A)).
