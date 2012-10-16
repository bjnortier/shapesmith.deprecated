#!/bin/bash

CONFIG_ARGS=""
if [ $# -eq 0 ]
    then
        echo "Starting with development config..."
    else
        echo "Starting with $1 config..."
        CONFIG_ARGS="-config $1"

fi
cd `dirname $0`
exec erl $CONFIG_ARGS -pa $PWD/*/ebin $PWD/deps/*/ebin -name 'master@127.0.0.1' -s reloader -s lager -s worker_master -s worker -s api -s ui
