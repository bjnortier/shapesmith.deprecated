#!/bin/sh
cd `dirname $0`
exec erl -config ec2 -pa $PWD/ebin $PWD/deps/*/ebin -s reloader -s node
