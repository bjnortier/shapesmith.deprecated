Installing Shapesmith
=====================

Prerequisites
-------------

 * MacOSX or Ubuntu Linux (other variants of Linux should be fine, but there are not specific instructions for installing the dependencies on these yet).
 * Erlang R14 or R15.
 * Libraries for OpenCASCADE, Erlang etc: *Follow the instructions for installing these in either INSTALL.MacOSX or INSTALL.Ubuntu*

Install libraries
-----------------

 * Refer to the MacOSX or Ubuntu install instructions

Install Erlang
--------------

 * Refer to the MacOSX or Ubuntu install instructions

Install OCE v 0.4
-----------------
 
 * git clone https://github.com/tpaviot/oce.git
 * cd oce
 * git checkout -b 0. 4.0 OCE-0.4.0
 * mkdir build
 * cd build
 * cmake ..
 * make -j4
 * sudo make install/strip 
 * sudo ldconfig /usr/local/lib # Ubuntu only

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

NB. Because of the cookies used in the session authentication tests, add the following line to the /etc/hosts file:

``127.0.0.1       localhost.shapesmith.net``

Run the tests:

 * ./rebar eunit ct skip_deps=true # This runs the tests. 

If all the tests pass, run shapesmith:

 * ./start-dev.sh

Point your browser to

 * http://localhost:8000 