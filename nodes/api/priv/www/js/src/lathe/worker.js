importScripts('/ui/js/lib/require.js');

var worker = self;

requirejs.config({
    baseUrl: "/ui/js",
    paths: {
        'lathe': 'node_modules/lathe/lib',
    },
});
requirejs([
        'lathe/primitives/cube',
        'lathe/primitives/sphere',
        'lathe/conv',
    ],
    function(Cube, Sphere, Conv) {

        function construct(constructor, args) {
            function F() {
                return constructor.apply(this, args);
            }
            F.prototype = constructor.prototype;
            return new F();
        }

        this.addEventListener('message', function(e) {
            var bsp;
            // Create new with the arguments
            if (e.data.sphere) {
                bsp = construct(Sphere, e.data.sphere).bsp;
            } else if (e.data.cube) {
                bsp = construct(Cube, e.data.cube).bsp;
            } else {
                postMessage({error: 'unknown worker message:' + JSON.stringify(e.data)});
            }

            if (bsp) {
                var brep = Conv.bspToBrep(bsp);
                var polygons = brep.map(function(p) {
                    return p.toVertices().map(function(v) {
                        return v.toCoordinate();
                    });
                });
                postMessage({id: e.data.id, polygons: polygons});
            } 
        }, false);
    }
);