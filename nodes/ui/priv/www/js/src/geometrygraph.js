define([
    'lib/underscore-require',
    'lib/backbone-require',
    'lib/sha1',
    'src/graph',
    'src/geomnode',
    'src/commandstack',
    ], 
    function(_, Backbone, crypto, graphLib, geomNode, commandStack) {

    var GeometryGraph = function() {

        _.extend(this, Backbone.Events);
        var graph = new graphLib.Graph();

        this.commit = function(vertex) {
            var nonEditingReplacement = vertex.cloneNonEditing();

            var url = '/' + SS.session.username + '/' + SS.session.design + '/vertex/';
            var that = this;
            $.ajax({
                type: 'POST',
                url: url,
                contentType: 'application/json',
                data: nonEditingReplacement.toJSON(),
                dataType: 'json',
                success: function(sha) {
                    
                    nonEditingReplacement.sha = sha;
                    that.replace(vertex, nonEditingReplacement);

                    url = '/' + SS.session.username + '/' + SS.session.design + '/graph/';
                    $.ajax({
                        type: 'POST',
                        url: url,
                        contentType: 'application/json',
                        data: JSON.stringify(that.serialize()),
                        dataType: 'json',
                        success: function(sha) {
                            that.trigger('committed', nonEditingReplacement);
                        },
                        error: function(jqXHR, textStatus, errorThrown) {
                            commandStack.error(jqXHR.responseText);
                        },

                    })

                },
                error: function(jqXHR, textStatus, errorThrown) {
                    commandStack.error(jqXHR.responseText);
                }
            });
        }

        // When editing, the original vertex is kept 
        // for the cancel operation
        var originals = {};

        this.edit = function(vertex) {
            var editingReplacement = vertex.cloneEditing();
            this.replace(vertex, editingReplacement);
            originals[vertex.id] = vertex;

            var that = this;
            if (vertex.type === 'polyline') {
                this.childrenOf(vertex).forEach(function(point) {
                    that.edit(point);
                });
            }
        }

        this.editById = function(id) {
            this.edit(graph.vertexById(id));
        }

        this.cancel = function(vertex) {
            // Vertices being edited will have originals, new vertices
            // will not have originals
            if (originals[vertex.id]) {
                this.replace(vertex, originals[vertex.id]);
                delete originals[vertex.id];
            } else {

                // Remove implicit children that are not editing
                // for prototype objects, but only remove them once
                // and only if they are not shared with other parents
                var that = this;
                var removed = [];
                var children = this.childrenOf(vertex);
                children.map(function(child) {
                    var parents = that.parentsOf(child);
                    var hasOtherParent = _.any(parents, function(parent) {
                        parent.id !== vertex.id;
                    });
                    if (child.implicit && !child.editing && !hasOtherParent) {
                        if(removed.indexOf(child) === -1) {
                            that.remove(child);
                            removed.push(child);
                        }
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

        this.commitIfEditing = function() {
            var that = this;
            this.getEditingVertices().map(function(vertex) {
                that.commit(vertex);
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

        this.addChildTo = function(parent, child) {
            this.add(child, function() {
                graph.addEdge(parent, child);
            });
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
            replacement.trigger('change', replacement);
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
            return graph.outgoingVerticesOf(vertex).map(function(id) {
                return graph.vertexById(id);
            });
        }

        this.parentsOf = function(vertex) {
            return graph.incomingVerticesOf(vertex).map(function(id) {
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

        this.serialize = function() {
            var vertices = graph.vertices();
            var result = {edges:{}};
            var that = this;
            vertices.forEach(function(vertex) {
                result.edges[vertex.sha] = that.childrenOf(vertex).map(function(child) {
                    return child.sha;
                });
            });
            return result;
        }
    }

    return {
        Graph: GeometryGraph
    }

});