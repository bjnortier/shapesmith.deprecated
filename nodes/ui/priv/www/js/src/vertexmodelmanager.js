define([
        'src/geometrygraphsingleton', 
        'src/selection', 
        'src/workplaneMV',
        'src/variableMV',
        'src/pointMV', 
        'src/polylineMV',
        'src/extrudeMV',
    ], 
    function(
        geometryGraph, 
        selectionManager, 
        Workplane,
        Variable,
        Point, 
        Polyline,
        Extrude) {
    
    var models = {};
    var wrappers = {
        'workplane': Workplane,
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

    selectionManager.on('selected', function(ids, selection) {
        updateEditingForSelected(selection);
    });

    selectionManager.on('deselected', function(ids, selection) {
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
        // Try to find the editing parent model of an implicit child
        var editingParentModel = undefined;
        if (vertex.implicit) {
            var parents = geometryGraph.parentsOf(vertex);
            var editingParent = _.find(parents, function(parent) { return parent.editing; });
            if (editingParent) {
                editingParentModel = models[editingParent.id];
            }
        }

        if (vertex.editing) {
            models[vertex.id] = new wrappers[vertex.type].EditingModel(vertex, editingParentModel);
        } else {
            models[vertex.id] = new wrappers[vertex.type].DisplayModel(vertex, editingParentModel);
        }
    }

    var removeVertex = function(vertex) {
        if (!models[vertex.id]) {
            throw Error('no model for vertex:' + vertex.id);
        }
        var model = models[vertex.id];
        model.destroy();
        delete models[vertex.id];
    }

    return wrappers;

});