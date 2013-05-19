importScripts('/lib/require.js');

var worker = self;

requirejs.config({
    baseUrl: "..",
    paths: {
        'underscore': '../node_modules/underscore/underscore',
        'backbone-events': '../node_modules/backbone-events/lib/backbone-events',
        'backbone': '../node_modules/backbone/backbone',
        'lathe': '../node_modules/lathe/lib',
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
    ],
    function(BSP, Cube, Sphere, Conv) {

        var infoHandler = function(a,b,c,d) {
            postMessage({info: [a,b,c,d].join('')});
        }
        
        var errorHandler = function(a,b,c,d) {
            postMessage({error: [a,b,c,d].join('')});
        }

        var returnResult = function(id, sha, bsp) {
            if (!bsp) {
                postMessage({error: 'no BSP for ' + id});
                return;
            }

            var brep = Conv.bspToBrep(bsp);
            var polygons = brep.map(function(p) {
                return p.toVertices().map(function(v) {
                    return v.toCoordinate();
                });
            });
            var jobResult = {
                id: id,
                sha: sha,
                bsp: BSP.serialize(bsp),
                polygons: polygons,
            }
            postMessage(jobResult);
            
        }

        this.addEventListener('message', function(e) {

            // Create new with the arguments
            if (e.data.sphere) {
                var bsp = new Sphere(e.data.sphere).bsp;
                returnResult(e.data.id, e.data.sha, bsp);
            } else if (e.data.cube) {
                var bsp = new Cube(e.data.cube).bsp;
                returnResult(e.data.id, e.data.sha, bsp);
            } else if (e.data.subtract) {

                // The child BSPs start off as an array of SHAs, 
                // and each SHA is replaced with the BSP from the DB
                var childBSPs = e.data.subtract;
                var remaining = childBSPs.length;

                var a = BSP.deserialize(childBSPs[0]);
                var b = BSP.deserialize(childBSPs[1]);
                var bsp = BSP.difference(a,b);
                returnResult(e.data.id, e.data.sha, bsp);

            } else {
                postMessage({error: 'unknown worker message:' + JSON.stringify(e.data)});
            }

        }, false);

        postMessage('initialized');
    }
);