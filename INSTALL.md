Installing Shapesmith
=====================

Prerequisites
-------------

 * MacOSX or Ubuntu Linux (other variants of Linux should be fine, but there are not specific instructions for installing the dependencies on these yet).
 * Riak 0.14.1 http://downloads.basho.com/riak/CURRENT/
 * OpenCASCADE Community Edition, Erlang and some libraries those require. Follow the instructions for installing these in either INSTALL.MacOSX or INSTALL.Ubuntu

Clone the Shapesmith repo from github
-------------------------------------

 * git clone git://github.com/bjnortier/shapesmith.git
 * cd shapesmith

Build the worker process
------------------------

The worker is a C++ executable that executes the geometry modelling operations. The shapesmith application will use one or more worker instances.

From within the cloned shapesmith repo:

 * cd worker
 * mkdir build
 * cd build
 * cmake ..
 * make

You should now have a usable worker process

Build the Erlang application
----------------------------

 * cd shapesmith/node
 * make
 * ./rebar eunit ct skip_deps=true # This runs the tests. Make sure riak is running before you run tests
 
If all the tests pass, run shapesmith:

 * ./start.sh

Point your browser to

 * http://localhost:8000 