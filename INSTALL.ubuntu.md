# Install dependencies on Ubuntu

Erlang and various other libraries are required:

    $ sudo apt-get install build-essential libncurses5-dev libssl-dev git-core cmake libx11-dev libfreetype6-dev libftgl-dev libxmu-dev libboost-dev libgtest-dev erlang

Recent versions of Ubuntu have JSON Spirit available as a package, so installing it is as easy as

    $ sudo apt-get install libjson-spirit-dev

For older version, you can install JSON Spirit from source:

    $ wget https://launchpad.net/ubuntu/+archive/primary/+files/json-spirit_4.04.orig.tar.gz
    $ tar xzf json-spirit_4.04.orig.tar.gz
    $ cd json_spirit_v4.04/
    $ mkdir build
    $ cd build
    $ cmake ..
    $ make
    $ sudo make install

## Ubuntu 12.x specific notes

The google test libraries, 'gtest', no longer install the binaries by default.

So, if you receive the following error when building, then you need to compile the gtest libraries locally.

    CMake Error at
    /usr/share/cmake-2.8/Modules/FindPackageHandleStandardArgs.cmake:91
    (MESSAGE):
        Could NOT find GTest (missing: GTEST_LIBRARY GTEST_MAIN_LIBRARY) 

This thread describes the issue: 

    http://askubuntu.com/questions/145887/why-no-library-files-installed-for-google-test-on-12-04

gtest build instructions:

    cd /usr/src/gtest
    sudo cmake .
    sudo make
    sudo mv libg* /usr/lib/
