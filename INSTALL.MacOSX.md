Install dependencies on Mac OSX
===============================

Install MacPorts + dependencies
-------------------------------

 * Install MacPorts from http://www.macports.org/
 * sudo port install cmake ftgl json_spirit

Install OCE v 0.4
-----------------
 
 * clone https://github.com/tpaviot/oce.git
 * git checkout -b 0.4.0 OCE-0.4.0
 * cd oce
 * mkdir build
 * cmake ..
 * make -j4
 * sudo make install/strip

Install Erlang > R14
--------------------

 * Download the source from http://www.erlang.org/download.html
 * tar xzf otp_src_R14B03.tar.gz
 * cd otp_src_R14B03
 * ./configure
 * make
 * sudo make install


