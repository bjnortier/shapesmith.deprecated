define([
        'lathe/primitives/cube',
        'lathe/primitives/sphere',
        'lathe/polygon3d',
        'lathe/bsp',
        'lathe/conv',
    ], 
    function(
        Cube,
        Sphere,
        Polygon3D,
        BSP,
        Conv) {
   
    var createSphere = function(x,y,z,r) {
        return new Sphere(x,y,z,r,24);
    }

    var createCube = function(x,y,z,w,d,h) {
        return new Cube(x,y,z,w,d,h);
    }

    var subtract = function(a,b) {
        return BSP.difference(a, b, Polygon3D);
    }

    var toBrep = function(bsp) {
        return Conv.bspToBrep(bsp);
    }

    return {
        createCube  : createCube,
        createSphere: createSphere,
        subtract    : subtract,
        toBrep      : toBrep,
    }

});