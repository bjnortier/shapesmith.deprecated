define(['src/geometrygraphsingleton', 'src/selection', 'src/pointwrapper', 'src/polylinewrapper'], 
    function(geometryGraph, selection, point, polyline) {
    
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

    selection.on('selected', function(ids, selection) {
        updateEditingForSelected(selection);
    });

    selection.on('deselected', function(ids, selection) {
        updateEditingForSelected(selection);
    });

    var updateEditingForSelected = function(selection) {
        if (selection.length === 1) {
            geometryGraph.editById(selection[0]);
        } else {
            geometryGraph.commitIfEditing();
        }
    }

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