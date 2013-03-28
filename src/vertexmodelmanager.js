define([
        'geometrygraphsingleton', 
        'selection', 
        'vertexMV',
        'workplaneMV',
        'variableMV',
        // 'pointMV', 
        // 'polylineMV',
        // 'modelviews/CubeMV',
        // 'modelviews/SphereMV',
        // 'extrudeMV',
        // 'modelviews/subtractMV',
    ], 
    function(
        geometryGraph, 
        selectionManager, 
        VertexMV,
        WorkplaneMV,
        VariableMV) {
        // PointMV, 
        // PolylineMV,
        // CubeMV,
        // SphereMV,
        // ExtrudeMV,
        // SubtractMV) {
    
    var wrappers = {
        'workplane' : WorkplaneMV,
        'variable'  : VariableMV,
        // 'point'     : PointMV,
        // 'polyline'  : PolylineMV,
        // 'cube'      : CubeMV,
        // 'sphere'    : SphereMV,
        // 'extrude'   : ExtrudeMV,
        // 'subtract'  : SubtractMV,
    }

    geometryGraph.on('vertexAdded', function(vertex) {
        if (vertex.category !== 'geometry') {
            addVertex(vertex);
        }
    });

    geometryGraph.on('vertexRemoved', function(vertex) {
        if (vertex.category !== 'geometry') {
            removeVertex(vertex);
        }
    });

    // Editing replacements are handled by the models
    geometryGraph.on('vertexReplaced', function(original, replacement) {
        if (replacement.category !== 'geometry') {
            if (!original.editing && !replacement.editing) {
                var model = VertexMV.getModelForVertex(original);
                model.replaceWithDisplay(original, replacement);
            }
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