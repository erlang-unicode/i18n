-compile({parse_transform, i18n_expand}). 

-define(ISTR(X), i18n_string:from(X)).
-define(ITS(X),  i18n_string:to_utf8(X)).
