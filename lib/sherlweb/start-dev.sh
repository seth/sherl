#!/bin/sh
cd `dirname $0`
exec erl -mnesia dir '"/Users/seth/temp/sherl-DB"' -pa $PWD/ebin $PWD/deps/*/ebin $PWD/deps/*/deps/*/ebin $PWD/../sherl/ebin -boot start_sasl -s reloader -s sherlweb
