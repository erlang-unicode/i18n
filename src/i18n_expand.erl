%%% @private
%%% Used in compiling time.

-module(i18n_expand).
-export([parse_transform/2]).

-import(ct_expand, [function/4]).


parse_transform(Forms, Options) ->
    function({i18n_string, from, 1},
	     fun(Form, _Context) ->
			try
			    [Expr] = erl_syntax:application_arguments(Form),
			    {value, Value, _} = erl_eval:exprs(revert_tree([Expr]), []),
				io:format("Expanding: ~w ~n", [Value]),
			    erl_syntax:abstract(i18n_string:from(Value))
			catch error:Reason ->
				io:format("Skip expanding: ~w ~n", [Reason]),
				Form
			end 
	     end, Forms, Options).

revert_tree(Tree) ->
    [erl_syntax:revert(T) || T <- lists:flatten(Tree)].
