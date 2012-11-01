define(['lib/underscore-require', 'lib/backbone-require', 'src/graph', 'src/geomnode'], 
    function(_, Backbone, graphLib, geomNode) {

    var GeometryGraph = function() {

        _.extend(this, Backbone.Events);
        var graph = new graphLib.Graph();

        this.commit = function(vertex) {
            var nonEditingReplacement = vertex.cloneNonEditing();
            graph.replaceVertex(vertex, nonEditingReplacement);
            this.trigger('vertexReplaced', vertex, nonEditingReplacement);
            this.trigger('committed');
        }

        // When editing, the original vertex is kept 
        // for the cancel operation
        var originals = {};

        this.edit = function(vertex) {
            var editingReplacement = vertex.cloneEditing();
            graph.replaceVertex(vertex, editingReplacement);
            originals[vertex.id] = vertex;
            this.trigger('vertexReplaced', vertex, editingReplacement);
        }

        this.cancel = function(vertex) {
            if (vertex.proto) {
                // Initial creation, not editing an existing vertex
                if (originals[vertex.id]) {
                    throw Error('Vertex is prototype but original replacement found');
                }
                graph.removeVertex(vertex);
                this.trigger('vertexRemoved', vertex);

            } else {    
                // Editing an existing node
                if (!originals[vertex.id]) {
                    throw Error('No original vertex found');
                }
                graph.replaceVertex(vertex, originals[vertex.id]);
                this.trigger('vertexReplaced', vertex, originals[vertex.id]);

                delete originals[vertex.id];
            }
        }

        this.cancelIfEditing = function() {
            var editingVertex = this.getEditingVertex();
            if (editingVertex) {
                this.cancel(editingVertex);
            }
        }

        // ---------- Prototypes ----------
       
        this.createPointPrototype = function() {
            var pointVertex = new geomNode.Point({
                editing      : true,
                proto        : true,
                nameFromId   : true,
                addAnotherFn : 'createPointPrototype',
            });
            graph.addVertex(pointVertex);
            this.trigger('vertexAdded', pointVertex);
            return pointVertex;
        }

        this.createPolylinePrototype = function() {
            var pointVertex = new geomNode.Point({});
            var polylineVertex = new geomNode.Polyline({
                editing      : true,
                proto        : true,
                nameFromId   : true,
                addAnotherFn : 'createPolylinePrototype',
            });

            graph.addVertex(pointVertex);
            graph.addVertex(polylineVertex);
            graph.addEdge(polylineVertex, pointVertex);
            this.trigger('vertexAdded', pointVertex);
            this.trigger('vertexAdded', polylineVertex);

            return polylineVertex;
        }

        // ---------- Mutations ----------

        this.addPointToPolyline = function(polyline, point) {
            if (point === undefined) {
                var pointVertex = new geomNode.Point({});
                graph.addVertex(pointVertex);
                graph.addEdge(polyline, pointVertex);
                this.trigger('vertexAdded', pointVertex);
            } else {
                graph.addEdge(polyline, point);
            }
            return pointVertex;
        }

        this.removeLastPointFromPolyline = function(polyline) {
            var children = this.childrenOf(polyline);
            if (children.length === 0) {
                throw Error('Cannot remove last point from empty polyline');
            }
            var vertex = children[children.length - 1];
            graph.removeVertex(vertex);
            this.trigger('vertexRemoved', vertex);
        }

        // ---------- Graph functions ----------

        this.childrenOf = function(vertex) {
            return graph.outgoingEdgesOf(vertex).map(function(id) {
                return graph.vertexById(id);
            });
        }

        this.parentsOf = function(vertex) {
            return graph.incomingEdgesOf(vertex).map(function(id) {
                return graph.vertexById(id);
            });
        }

        this.isEditing = function() {
            return this.getEditingVertex() !== undefined;
        }

        this.getEditingVertex = function() {
            editingVertices = _.reject(graph.vertices(), function(vertex) { 
                return !vertex.editing;
            });
            if (editingVertices.length === 0) {
                return undefined;
            } else if (editingVertices.length > 1) {
                throw Error('More than one editing vertex in graph');
            } else {
                return editingVertices[0];
            }
        }
    }

    return {
        Graph: GeometryGraph
    }

});