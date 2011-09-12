#!/bin/sh
cd `dirname $0`
./rebar compile
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa $PWD/ebin edit $PWD/deps/*/ebin \
    -boot start_sasl \
    -sname i18n \
#   -s i18n_nif init \
#   -s reloader

