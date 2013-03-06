define([
        'src/geometrygraphsingleton', 
        'src/selection', 
        'src/vertexMV',
        'src/workplaneMV',
        'src/variableMV',
        'src/pointMV', 
        'src/polylineMV',
        'src/extrudeMV',
    ], 
    function(
        geometryGraph, 
        selectionManager, 
        VertexMV,
        Workplane,
        Variable,
        Point, 
        Polyline,
        Extrude) {
    
    var models = {};
    var wrappers = {
        'workplane'      : Workplane,
        'variable'       : Variable,
        'point'          : Point,
        'polyline'       : Polyline,
        'extrude'        : Extrude,
    }

    geometryGraph.on('vertexAdded', function(vertex) {
        addVertex(vertex);
    });

    geometryGraph.on('vertexRemoved', function(vertex) {
        removeVertex(vertex);
    });

    // Editing replacements are handled by the models
    geometryGraph.on('vertexReplaced', function(original, replacement) {
        if (!original.editing && !replacement.editing) {
            var model = VertexMV.getModelForVertex(original);
            model.replaceWithDisplay(original, replacement);
        }
    }); 

    var addVertex = function(vertex) {
        var implicitEditing = vertex.implicit && vertex.editing;
        if (implicitEditing) {
            return
        }

        if (vertex.editing) {
            new wrappers[vertex.type].EditingModel({vertex: vertex});
        } else {
            new wrappers[vertex.type].DisplayModel({vertex: vertex});
        }
    }

    var removeVertex = function(vertex) {
        var implicitEditing = vertex.implicit && vertex.editing;
        if (implicitEditing) {
            return
        }

        var model = VertexMV.getModelForVertex(vertex);
        if (!model) {
            throw Error('no model for vertex:' + vertex.id);
        }
        model.destroy();
    }

    return wrappers;

});