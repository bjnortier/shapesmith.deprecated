Installing Shapesmith
=====================

Prerequisites
-------------

 * MacOSX or Ubuntu Linux (other variants of Linux should be fine, but there are not specific instructions for installing the dependencies on these yet).
 * Riak 0.14.1 http://downloads.basho.com/riak/CURRENT/
 * Libraries for OpenCASCADE, Erlang etc: *Follow the instructions for installing these in either INSTALL.MacOSX or INSTALL.Ubuntu*

Install OCE v 0.4
-----------------
 
 * clone https://github.com/tpaviot/oce.git
 * git checkout -b 0.4.0 OCE-0.4.0
 * cd oce
 * mkdir build
 * cmake ..
 * make -j4
 * sudo make install/strip

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