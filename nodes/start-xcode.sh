#!/bin/sh
# This dev startup script is used to debug the worker process in the XCode debugger
cd `dirname $0`
exec erl -config xcode-worker -pa $PWD/apps/*/ebin $PWD/deps/*/ebin -name 'master@127.0.0.1' -s reloader -s lager -s worker_master -s worker -s api
