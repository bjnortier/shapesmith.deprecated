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
        } else if (vertex.isNamed()) {
            models[vertex.id] = new wrappers[vertex.type].DisplayModel(vertex);

            var updateAncestors = function(v) {
                geometryGraph.parentsOf(v).map(function(parent) {
                    if (models[parent.id]) {
                        models[parent.id].destroy();
                        models[parent.id] = new wrappers[parent.type].DisplayModel(parent); 
                    }
                    updateAncestors(parent);
                });
            }
            updateAncestors(vertex);
        }
    }


    var removeVertex = function(vertex) {
        if (models[vertex.id]) {
            models[vertex.id].destroy();
            delete models[vertex.id];
        }
    }


});