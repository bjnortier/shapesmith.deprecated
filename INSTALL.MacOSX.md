# Install dependencies on Mac OSX

## Install MacPorts + dependencies

 * Install MacPorts from http://www.macports.org/
 * sudo port install cmake ftgl json_spirit

## 10.5.8 (32-bit) Dependencies
These instructions have been verified on a circa-2006 Core Duo (32-bit) MacBook Pro running Leopard (10.5.8). YMMV.

### Erlang (R140B3)
Erlang is the language in which Shapesmith is written.

*Do not install erlang via MacPorts; it is a later non-compatible version!*

A 32-bit compatible version of Erlang/OTP (R140B3) can only be installed from source. It requires these libraries:

* /opt/local/lib/libpng14.14.dylib
* /opt/local/lib/libjpeg.8.dylib

If you don't have them, make symlinks to create them before building:

* sudo ln -s /opt/local/lib/libpng14.14.dylib /opt/local/lib/libpng12.0.dylib
* sudo ln -s /opt/local/lib/libjpeg.8.dylib /opt/local/lib/libjpeg.7.dylib

To build from source, you must have XCode tools installed from the CD that came with your Mac or from [Apple’s Developer](http://developer.apple.com/ "Apple Developer website") website.

First, download and unpack the source:

* curl -O http://erlang.org/download/otp_src_R14B03.tar.gz
* tar zxvf otp_src_R14B03.tar.gz
* cd otp_src_R14B03

Next, configure Erlang for a 32-bit OS:

* ./configure --enable-hipe --enable-smp-support --enable-threads --enable-kernel-poll

Now build and install (you will be prompted for your password):

* make && sudo make install

Once installed, validate:

* which escript
* escript is /usr/local/bin/escript

For further information see: http://wiki.basho.com/Installing-Erlang.html

### Riak (1.0.1)
Riak is the NoSQL database used by Shapesmith.

A 32-bit compatible version of Riak (1.0.1) can only be built from source. To build from source, you must have XCode tools installed from the CD that came with your Mac or from [Apple’s Developer](http://developer.apple.com/ "Apple Developer website") website.

Download and unpack the source distribution:

* curl -O http://downloads.basho.com/riak/riak-1.0.1/riak-1.0.1.tar.gz
* tar zxvf riak-1.0.1.tar.gz
* cd riak-1.0.1

Now build:

* make rel

Once built, validate:

* cd rel/riak/bin
* sudo ./riak start
* sudo ./riak ping
* pong

For further information see: http://wiki.basho.com/Installing-on-Mac-OS-X.html

Now continue with [INSTALL.md](https://github.com/bjnortier/shapesmith/blob/master/INSTALL.md), omitting the Erlang and Riak installation instructions.

