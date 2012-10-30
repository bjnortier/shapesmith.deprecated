define(['src/geometrygraphsingleton', 'src/pointwrapper', 'src/polylinewrapper'], 
    function(geometryGraph, point, polyline) {
    
    var models = {};
    var wrappers = {
        'point'    : point,
        'polyline' : polyline,
    }

    geometryGraph.on('vertexAdded', function(vertex) {
        if (geometryGraph.parentsOf(vertex).length === 0) {
            if (vertex.editing) {
                models[vertex.id] = new wrappers[vertex.type].EditingModel(vertex);
            } else {
                models[vertex.id] = new wrappers[vertex.type].DisplayModel(vertex);
            }
        }
    });

    geometryGraph.on('vertexRemoved', function(vertex) {
        if (models[vertex.id]) {
            models[vertex.id].destroy();
            delete models[vertex.id];
        }
    });

});