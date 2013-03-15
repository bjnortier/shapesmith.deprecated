importScripts('/ui/js/lib/require.js');

var worker = self;

requirejs.config({
    baseUrl: "/ui/js",
    paths: {
        'underscore': 'node_modules/underscore/underscore',
        'backbone-events': 'node_modules/backbone-events/lib/backbone-events',
        'backbone': 'node_modules/backbone/backbone',
        'lathe': 'node_modules/lathe/lib',
    },
    shim: {
        'underscore': {
            exports: '_'
        },
        'backbone': {
            deps: ['underscore', 'jquery'],
            exports: 'Backbone'
        },
    },
});
requirejs([
        'lathe/bsp',
        'lathe/primitives/cube',
        'lathe/primitives/sphere',
        'lathe/conv',
        'src/lathe/bspdb',
    ],
    function(BSP, Cube, Sphere, Conv, BSPDB) {

        var infoHandler = function(a,b,c,d) {
            postMessage({info: [a,b,c,d].join('')});
        }
        var errorHandler = function(a,b,c,d) {
            postMessage({error: [a,b,c,d].join('')});
        }
        var bspdb = new BSPDB(infoHandler, errorHandler);

        this.addEventListener('message', function(e) {
            var bsp;
            // Create new with the arguments
            if (e.data.sphere) {
                bsp = new Sphere(e.data.sphere).bsp;
            } else if (e.data.cube) {
                bsp = new Cube(e.data.cube).bsp;
            } else if (e.data.subtract) {
                var a = BSP.deserialize(e.data.subtract[0]);
                var b = BSP.deserialize(e.data.subtract[1]);
                var bsp = BSP.difference(a,b);
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
                var jobResult = {
                    id: e.data.id, 
                    sha: e.data.sha,
                    bsp: BSP.serialize(bsp),
                    polygons: polygons,
                }
                postMessage(jobResult);
                
                bspdb.write(jobResult, function(err) {
                    if (err) {
                        postMessage({error: 'error writing to BSP DB' + err});
                    }
                })
            } 
        }, false);
    }
);