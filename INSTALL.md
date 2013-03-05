# Installing Shapesmith

 *If you experience ANY problems or have suggestions for improving these instructions, please send an email to shapesmith-user@googlegroups.com*

## Prerequisites

 * MacOSX or Ubuntu Linux (other variants of Linux should be fine, but there are not specific instructions for installing the dependencies on these yet. Volunteers?).

## Install Erlang and libraries

 * Please refer to the INSTALL.MacOSX.md or INSTALL.Ubuntu.md for platform-specific install instructions.

## Install OCE v 0.4
 
    $ git clone https://github.com/tpaviot/oce.git
    $ cd oce
    $ git checkout -b 0.4.0 OCE-0.4.0
    $ mkdir build
    $ cd build
    $ cmake ..
    $ make -j4
    $ sudo make install/strip 
    $ sudo ldconfig /usr/local/lib # Ubuntu only

## Clone the Shapesmith repo from github

    $ git clone git://github.com/bjnortier/shapesmith.git
    $ cd shapesmith

## Build the worker process

The worker is a C++ executable that executes the geometry modelling operations. The shapesmith application will use one or more worker instances.

From within the cloned shapesmith repo:

    $ cd nodes/apps/worker/priv
    $ ./regen_build.sh

If all the tests pass you should now have a usable worker process. (Note: if the tests do not compile, and you are running Ubuntu 12x, see the notes about compiling gtest in INSTALL.ubuntu.md).

## Optional Installs

Refer to INSTALL.Optional.md for optional components.

## Build the Erlang application

From within shapesmith/nodes:

    .../shapesmith/nodes$ make

NB. Because of the cookies used in the session authentication tests, add the following line to the /etc/hosts file:

    127.0.0.1       localhost.shapesmith.net

Run the tests:

    $ ./rebar compile eunit ct skip_deps=true

If all the tests pass, run shapesmith:

    $ ./start-dev.sh

Point your browser to

    $ http://localhost:8000 


## Note: Use the local 'rebar' instance

The latest rebar does not work with the one of the dependencies (erlang-bcrypt). So, even if you have rebar installed, be sure to use the rebar that comes with this project. This means, when the instructions below say:

    $ ./rebar foo

Do not forget the "./" in front of the 'rebar' command, this insures that you are running this program from the current directory. Whereas, running:

    $ rebar foo

will run rebar from wherever it is installed on your system...and if your version of rebar is recent, then rebar might not compile your project correctly (as is currently the case with erlang-bcrypt).

See: 
    https://github.com/smarkets/erlang-bcrypt/pull/6
