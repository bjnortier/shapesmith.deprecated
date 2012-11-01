define(['src/geometrygraphsingleton', 'src/pointwrapper', 'src/polylinewrapper'], 
    function(geometryGraph, point, polyline) {
    
    var models = {};
    var wrappers = {
        'point'    : point,
        'polyline' : polyline,
    }

    geometryGraph.on('vertexAdded', function(vertex) {
        addVertex(vertex);
    });

    geometryGraph.on('vertexRemoved', function(vertex) {
        removeVertex(vertex);
    });

    geometryGraph.on('vertexReplaced', function(original, replacement) {
        removeVertex(original);
        addVertex(replacement);
    });

    var addVertex = function(vertex) {
        if (vertex.editing) {
            models[vertex.id] = new wrappers[vertex.type].EditingModel(vertex);
        } else {
            models[vertex.id] = new wrappers[vertex.type].DisplayModel(vertex);
        }
    }


    var removeVertex = function(vertex) {
        if (models[vertex.id]) {
            models[vertex.id].destroy();
            delete models[vertex.id];
        }
    }


});