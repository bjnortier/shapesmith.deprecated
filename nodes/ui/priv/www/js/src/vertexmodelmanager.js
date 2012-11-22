define([
        'src/geometrygraphsingleton', 
        'src/selection', 
        'src/variableMV',
        'src/pointMV', 
        'src/polylineMV',
        'src/extrudewrapper',
    ], 
    function(
        geometryGraph, 
        selectionManager, 
        Variable,
        Point, 
        Polyline,
        Extrude) {
    
    var models = {};
    var wrappers = {
        'variable' : Variable,
        'point'    : Point,
        'polyline' : Polyline,
        'extrude'  : Extrude,
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

    selectionManager.on('selected', function(ids, selectionManager) {
        updateEditingForSelected(selectionManager);
    });

    selectionManager.on('deselected', function(ids, selectionManager) {
        updateEditingForSelected(selectionManager);
    });

    var updateEditingForSelected = function(selectionManager) {
        if (selectionManager.length === 1) {
            geometryGraph.editById(selectionManager[0]);
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
        if (!models[vertex.id]) {
            throw Error('no model for vertex:' + vertex.id);
        }
        var model = models[vertex.id];
        model.destroy();
        delete models[vertex.id];
        return model;
    }

    return wrappers;

});