# Optional installs

## GTS and mesh coarsening

For STL imports, Shapesmith will automatically detect whether the 'coarsen' executable exists on the system and simplify imported meshes.

For mesh simplification we user the GTS library: [http://gts.sourceforge.net/](http://gts.sourceforge.net/). 

'coarsen' is a GTS example and is not part of the standard GTS package. Thus it is easier to build this locally and symlink the 'coarsen' command.

### On Ubuntu:

    $ sudo apt-get install libglib2.0-dev

### On MacOSX:

    $ brew install glib

### Both:

    $ wget http://downloads.sourceforge.net/project/gts/gts/0.7.6/gts-0.7.6.tar.gz
    $ tar xzf gts-0.7.6.tar.gz
    $ cd gts-0.7.6
    $ ./configure
    $ make
    $ sudo make install
    $ sudo ln -s $PWD/examples/coarsen /usr/local/bin/coarsen

### Verify

You can verify the installation by coarsening a mesh:

    $ cd /tmp
    $ wget http://gts.sourceforge.net/samples/x_wing.gts.gz
    $ gunzip x_wing.gts.gz
    $ cat x_wing.gts | gts2stl > x_wing.stl # Make an STL file from GTS file
    $ stl2gts < x_wing.stl # Make a GTS file from an STL
    $ coarsen < x_wing.gts # Should output a GTS file






