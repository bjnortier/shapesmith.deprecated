#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/apps/*/ebin $PWD/deps/*/ebin -s reloader -s lager -s worker_master -s worker -name 'master@127.0.0.1'
