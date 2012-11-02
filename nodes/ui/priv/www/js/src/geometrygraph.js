define(['lib/underscore-require', 'lib/backbone-require', 'src/graph', 'src/geomnode'], 
    function(_, Backbone, graphLib, geomNode) {

    var GeometryGraph = function() {

        _.extend(this, Backbone.Events);
        var graph = new graphLib.Graph();

        this.commit = function(vertex) {
            var nonEditingReplacement = vertex.cloneNonEditing();
            this.replace(vertex, nonEditingReplacement);
            this.trigger('committed', nonEditingReplacement);
        }

        // When editing, the original vertex is kept 
        // for the cancel operation
        var originals = {};

        this.edit = function(vertex) {
            var editingReplacement = vertex.cloneEditing();
            this.replace(vertex, editingReplacement);
            originals[vertex.id] = vertex;
        }

        this.cancel = function(vertex) {
            // Vertices being edited will have originals, new vertices
            // will not have originals
            if (originals[vertex.id]) {
                this.replace(vertex, originals[vertex.id]);
                delete originals[vertex.id];
            } else {

                // Remove implicit children that are not editing
                var that = this;
                this.childrenOf(vertex).map(function(child) {
                    if (child.implicit && !child.editing) {
                        that.remove(child);
                    }
                });

                this.remove(vertex);
            }
        }

        this.cancelIfEditing = function() {
            var that = this;
            this.getEditingVertices().map(function(vertex) {
                that.cancel(vertex);
            });
        }

        // ---------- Prototypes ----------
       
        this.createPointPrototype = function(options) {
            var options = _.extend(options || {}, {
                editing      : true,
                proto        : true,
                nameFromId   : true,
            });
            var pointVertex = new geomNode.Point(options);
            this.add(pointVertex);
            return pointVertex;
        }

        this.createPolylinePrototype = function(pointOptions) {
            var pointOptions = pointOptions || {};
            var pointVertex = this.createPointPrototype({implicit: true});
            var polylineVertex = new geomNode.Polyline({
                editing      : true,
                proto        : true,
                nameFromId   : true,
            });
            // Add the vertex but add the edge as well before triggering notifications 
            this.add(polylineVertex, function() {
                graph.addEdge(polylineVertex, pointVertex);
            });
            return polylineVertex;
        }

        // ---------- Mutations ----------

        this.addPointToPolyline = function(polyline, point) {
            if (point === undefined) {
                point = this.createPointPrototype({implicit: true});
            } 
            graph.addEdge(polyline, point);
            return point;
        }

        this.removeLastPointFromPolyline = function(polyline) {
            var children = this.childrenOf(polyline);
            if (children.length === 0) {
                throw Error('Cannot remove last point from empty polyline');
            }
            this.remove(children[children.length - 1]);
        }

        // ---------- Graph functions ----------

        this.add = function(vertex, beforeNotifyFn) {
            graph.addVertex(vertex);
            if (beforeNotifyFn) {
                beforeNotifyFn();
            }
            vertex.on('change', this.vertexChanged, this);
            this.trigger('vertexAdded', vertex);
        }

        this.remove = function(vertex) {
            graph.removeVertex(vertex);
            vertex.off('change', this.vertexChanged, this);
            this.trigger('vertexRemoved', vertex);
        }

        this.replace = function(original, replacement) {
            graph.replaceVertex(original, replacement);
            original.off('change', this.vertexChanged, this);
            replacement.on('change', this.vertexChanged, this);
            this.trigger('vertexReplaced', original, replacement);
        }

        this.vertexChanged = function(vertex) {
            var that = this;
            var notifyAncestors = function(v) {
                that.parentsOf(v).map(function(parent) {
                    parent.trigger('descendantChanged', vertex);
                    notifyAncestors(parent);
                });
            }
            notifyAncestors(vertex);
        }

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
            return this.getEditingVertices().length > 0;
        }

        this.getEditingVertices = function() {
            return _.reject(graph.vertices(), function(vertex) { 
                return !vertex.editing;
            });
        }
    }

    return {
        Graph: GeometryGraph
    }

});