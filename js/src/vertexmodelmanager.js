define([
        'src/geometrygraphsingleton', 
        'src/selection', 
        'src/vertexMV',
        'src/workplaneMV',
        'src/variableMV',
        'src/pointMV', 
        'src/polylineMV',
        'src/modelviews/CubeMV',
        'src/modelviews/SphereMV',
        'src/extrudeMV',
        'src/modelviews/subtractMV',
    ], 
    function(
        geometryGraph, 
        selectionManager, 
        VertexMV,
        WorkplaneMV,
        VariableMV,
        PointMV, 
        PolylineMV,
        CubeMV,
        SphereMV,
        ExtrudeMV,
        SubtractMV) {
    
    var models = {};
    var wrappers = {
        'workplane' : WorkplaneMV,
        'variable'  : VariableMV,
        'point'     : PointMV,
        'polyline'  : PolylineMV,
        'cube'      : CubeMV,
        'sphere'    : SphereMV,
        'extrude'   : ExtrudeMV,
        'subtract'  : SubtractMV,
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
        // Implciit editing vertices are handles by the parent editing model
        var implicitEditing = vertex.implicit && vertex.editing;
        if (implicitEditing) {
            return;
        }

        if (vertex.editing) {
            new wrappers[vertex.type].EditingModel({vertex: vertex});
        } else {
            new wrappers[vertex.type].DisplayModel({vertex: vertex});
        }
    }

    var removeVertex = function(vertex) {
        // Implicit editing vertices are handles by the parent editing model
        var implicitEditing = vertex.implicit && vertex.editing;
        if (implicitEditing) {
            return;
        }

        var model = VertexMV.getModelForVertex(vertex);
        if (!model) {
            throw Error('no model for vertex:' + vertex.id);
        }

        model.destroy();
    }

    return wrappers;

});