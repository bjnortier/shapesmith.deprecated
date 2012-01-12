#!/bin/sh
cd `dirname $0`
exec erl -config session -pa $PWD/ebin $PWD/deps/*/ebin -s reloader -s node
