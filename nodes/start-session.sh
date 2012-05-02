#!/bin/sh
cd `dirname $0`
exec erl -config session -pa $PWD/apps/*/ebin $PWD/deps/*/ebin -name 'master@127.0.0.1' -s reloader -s lager -s worker_master -s worker -s api
