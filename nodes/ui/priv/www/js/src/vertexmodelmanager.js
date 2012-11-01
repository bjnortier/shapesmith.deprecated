define(['src/geometrygraphsingleton', 'src/pointwrapper', 'src/pointwrapper2', 'src/polylinewrapper'], 
    function(geometryGraph, point, point2, polyline) {
    
    var models = {};
    var wrappers = {
        'point'    : point2,
        'polyline' : polyline,
    }

    geometryGraph.on('vertexAdded', function(vertex) {
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
    });

    geometryGraph.on('vertexRemoved', function(vertex) {
        if (models[vertex.id]) {
            models[vertex.id].destroy();
            delete models[vertex.id];
        }
    });

});