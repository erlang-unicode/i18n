%%% Testing of Unicode support for source code.
-module(i18n_source).
-export([list/0, string/0, binary/0]).

list() ->
	[1058,1077,1089,1090,1086,1074,1072,1103,32,1089,1090,1088,1086,1082,1072].

string() ->
	"Тестовая строка".

binary() ->
	<<"Тестовая строка">>.
