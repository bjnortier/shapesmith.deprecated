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
