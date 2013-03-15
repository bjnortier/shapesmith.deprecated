// Functions for normalizing geometry vertices to a representation
// that can be used for meshing
define([
        'src/calculations',
        'src/geometrygraphsingleton',
    ], function(calc, geometryGraph) { 


    var normalizeCube = function(cube) {

        var points = geometryGraph.childrenOf(cube);
        var positions = points.map(function(p) {
            return calc.objToVector(
                p.parameters.coordinate, geometryGraph, THREE.Vector3);
        });

        var h = Math.abs(geometryGraph.evaluate(cube.parameters.height));
        return {
            x: Math.min(positions[0].x, positions[1].x),
            y: Math.min(positions[0].y, positions[1].y),
            z: Math.min(positions[0].z, positions[0].z + h),
            w: Math.abs(positions[1].x - positions[0].x),
            d: Math.abs(positions[1].y - positions[0].y),
            h: h,
        }

    }

    var normalizeSphere = function(sphere) {
        var points = geometryGraph.childrenOf(sphere);
        var center = calc.objToVector(points[0].parameters.coordinate, geometryGraph, THREE.Vector3);
        var radius = geometryGraph.evaluate(sphere.parameters.radius);

        return {
            x: center.x, 
            y: center.y,
            z: center.z,
            r: radius,
        }
    }

    var normalizeVertex = function(vertex) {
        switch (vertex.type) {
            case 'cube':
                return normalizeCube(vertex);
            case 'sphere':
                return normalizeSphere(vertex);
            default:
                throw Error('no normalization defined for ' + vertex.id)
        }
    }

    return {
        normalizeVertex: normalizeVertex
    }

});