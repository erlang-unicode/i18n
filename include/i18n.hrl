
-compile({parse_transform, i18n_expand}). 

% X is a variable.
-define(_ISTR(X), i18n_string:from(X)).

% X is a conctant.
-define(ISTR(X), 
		ct_expand:term(
	   		i18n_string:native_from(X)
		)).
