-module(i18n_expand).

-export([parse_transform/2]).

parse_transform(AST, Options) ->
	ct_expand:parse_transform(AST, Options).
