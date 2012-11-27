define([
        'src/geometrygraphsingleton', 
        'src/selection', 
        'src/vertexMV',
        'src/workplaneMV',
        'src/variableMV',
        'src/pointMV2', 
        'src/implicitPointMV', 
        'src/polylineMV2',
        'src/extrudeMV',
    ], 
    function(
        geometryGraph, 
        selectionManager, 
        VertexMV,
        Workplane,
        Variable,
        Point, 
        ImplicitPoint,
        Polyline,
        Extrude) {
    
    var models = {};
    var wrappers = {
        'workplane'      : Workplane,
        'variable'       : Variable,
        'point'          : Point,
        'implicit_point' : ImplicitPoint,
        'polyline'       : Polyline,
        'extrude'        : Extrude,
    }

    geometryGraph.on('vertexAdded', function(vertex) {
        addVertex(vertex);
    });

    geometryGraph.on('vertexRemoved', function(vertex) {
        removeVertex(vertex);
    });

    geometryGraph.on('vertexReplaced', function(original, replacement) {
        if (!original.editing && !replacement.editing) {
            var model = VertexMV.getModelForVertex(original);
            model.replaceDisplayVertex(original, replacement);
        }
    });

    // selectionManager.on('selected', function(ids, selection) {
    //     updateEditingForSelected(selection);
    // });

    // selectionManager.on('deselected', function(ids, selection) {
    //     updateEditingForSelected(selection);
    // });

    // var updateEditingForSelected = function(selection) {
    //     if (selection.length === 1) {
    //         geometryGraph.editById(selection[0]);
    //     } else {
    //         geometryGraph.commitIfEditing();
    //     }
    // }

    var addVertex = function(vertex) {
        // Try to find the editing parent model of an implicit child
        // var editingParentModel = undefined;
        // if (vertex.implicit) {
        //     var parents = geometryGraph.parentsOf(vertex);
        //     var editingParent = _.find(parents, function(parent) { return parent.editing; });
        //     if (editingParent) {
        //         editingParentModel = models[editingParent.id];
        //     }
        // }

        if (vertex.editing) {
            if (!vertex.implicit) {
                new wrappers[vertex.type].EditingModel(undefined, vertex);
            }
        } else {
            new wrappers[vertex.type].DisplayModel(vertex);
        }
    }

    var removeVertex = function(vertex) {
        var implicitEditing = vertex.implicit && vertex.editing;
        if (!implicitEditing) {
            var model = VertexMV.getModelForVertex(vertex);
            if (!model) {
                throw Error('no model for vertex:' + vertex.id);
            }
            model.destroy();
        }
    }

    return wrappers;

});