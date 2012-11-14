define([
    'lib/underscore-require',
    'lib/backbone-require',
    'lib/sha1',
    'src/graph',
    'src/geomnode',
    'src/command',
    'src/commandstack',
    ], 
    function(_, Backbone, crypto, graphLib, geomNode, Command, commandStack) {

    var post = function(url, data, successFn, errorFn) {
        $.ajax({
            type: 'POST',
            url: url,
            contentType: 'application/json',
            data: data,
            dataType: 'json',
            success: successFn,
            error: errorFn,
        });
    }

    var get = function(url, successFn, errorFn) {
        $.ajax({
            type: 'GET',
            url: url,
            dataType: 'json',
            success: successFn,
            error: errorFn,
        });
    }

    var GeometryGraph = function() {

        _.extend(this, Backbone.Events);
        var graph = new graphLib.Graph();
        var that = this;

        var captureVertices = function(vertices, callback) {
            var url = '/' + SS.session.username + '/' + SS.session.design + '/vertex/';

            var shas = vertices.map(function() {
                return undefined;
            })
            vertices.forEach(function(v, i) {
                post(url, v.toJSON(), function(sha) {
                    v.sha = sha;
                    shas[i] = sha;
                    var someRemaining = _.any(shas, function(sha) {
                        return sha === undefined;
                    });
                    if (!someRemaining) {
                        callback(shas);
                    }
                });
            });

        }

        var captureGraph = function(callback) {
            var url = '/' + SS.session.username + '/' + SS.session.design + '/graph/';
            post(url, JSON.stringify(that.serialize()), callback);
        }

        this.commitCreate = function(editingVertex) {
            var vertex = editingVertex.cloneNonEditing();
            var that = this;
            var children = this.childrenOf(vertex);

            var doFn = function(commandSuccessFn, commandErrorFn) {
                
                captureVertices([vertex], function() {
                    // When a polyline is ended with
                    // a double-click, the vertex is captured async, and is removed
                    // synchronously from the graph. So if it's no longer in the graph,
                    // don't do the replacement
                    if (graph.vertexById(vertex.id) !== undefined) {
                        that.replace(editingVertex, vertex);
                        if (!vertex.implicit) {
                            captureGraph(commandSuccessFn);
                            that.trigger('committed', [vertex]);
                        }
                    }
                });
            }

            var undoFn = function() {
                that.remove(vertex);
                children.forEach(function(child) {
                    if (child.implicit) {
                        that.remove(child);
                    } 
                })
            }
            var redoFn = function() {
                children.forEach(function(child) {
                    if (child.implicit) {
                        that.add(child);
                    }
                })
                that.add(vertex, function() {
                    children.forEach(function(child) {
                        graph.addEdge(vertex, child);
                    });
                });
            }

            var command = new Command(doFn, undoFn, redoFn);
            commandStack.do(command);
        }

        this.commitEdit = function() {
            var editingVertices = this.getEditingVertices();
            var nonEditingVertices = editingVertices.map(function(v) {
                return v.cloneNonEditing();
            })
            var originalVertices = nonEditingVertices.map(function(v) {
                return originals[v.id];
            }); 

            var doFn = function(commandSuccessFn, commandErrorFn) {

                captureVertices(nonEditingVertices, function(shas) {
                    for (var i = 0; i < editingVertices.length; ++i) {
                        that.replace(editingVertices[i], nonEditingVertices[i]);
                    }
                    captureGraph(commandSuccessFn);
                    that.trigger('committed', nonEditingVertices);
                });
            }

            var undoFn = function() {
                originalVertices.map(function(originalVertex, i) {
                    that.replace(nonEditingVertices[i], originalVertex);
                });
            }

            var redoFn = function() {
                originalVertices.map(function(originalVertex, i) {
                    that.replace(originalVertex, nonEditingVertices[i]);
                });
            }

            var command = new Command(doFn, undoFn, redoFn);
            commandStack.do(command);
        }

        this.removeAll = function() {
            graph.vertices().forEach(function(vertex) {
                that.remove(vertex);
            });
        }

        this.createGraph = function(deserializedGraph, shasToVertices) {
            var handledSHAs = [];
            while(_.keys(deserializedGraph.edges).length > 0) {
                for(parentSHA in deserializedGraph.edges) {
                    var childrenSHAs = deserializedGraph.edges[parentSHA];
                    var canHandle = _.intersection(childrenSHAs, handledSHAs).length == childrenSHAs.length;
                    if (canHandle) {

                        var parentVertex = shasToVertices[parentSHA];
                        var childVertices = childrenSHAs.map(function(childSHA) {
                            return shasToVertices[childSHA];
                        });
                        that.add(parentVertex, function() {
                            childVertices.map(function(childvertex) {
                                graph.addEdge(parentVertex, childvertex);
                            });
                        });
                        handledSHAs.push(parentSHA);
                        delete deserializedGraph.edges[parentSHA];
                    }
                }
            }
        }


        this.loadFromCommit = function(commit) {
            var url = '/' + SS.session.username + '/' + SS.session.design + '/graph/' + commit;
            this.removeAll();
            get(url, function(graph) {
                var vertexSHAsToLoad = _.keys(graph.edges);
                var remaining = vertexSHAsToLoad.length;
                shasToVertices = {};
                vertexSHAsToLoad.forEach(function(sha) {
                    get('/' + SS.session.username + '/' + SS.session.design + '/vertex/' + sha,
                        function(data) {
                            var vertex = new geomNode.constructors[data.type](data);
                            vertex.sha = sha;
                            shasToVertices[sha] = vertex;
                            --remaining;
                            if (remaining == 0) {
                                that.createGraph(graph, shasToVertices);
                            }
                        });
                });

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
            var editingVertices = this.getEditingVertices();
            var allAreSame = true;
            for (var i = 0; i < editingVertices.length; ++i) {
                if (!originals[editingVertices[i].id].hasSameJSON(editingVertices[i])) {
                    allAreSame = false;
                    break;
                }
            }

            if (allAreSame) {
                editingVertices.map(function(editingVertex, i) {
                    that.replace(editingVertex, originals[editingVertex.id]);
                });
            } else {
                this.commitEdit();
            }
        }

        // ---------- Prototypes ----------
       
        this.createPointPrototype = function(options) {
            var options = _.extend(options || {}, {
                editing      : true,
                proto        : true,
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
            });
            // Add the vertex but add the edge as well before triggering notifications 
            this.add(polylineVertex, function() {
                graph.addEdge(polylineVertex, pointVertex);
            });
            return polylineVertex;
        }

        this.createExtrudePrototype = function(childVertexId) {
            var child = this.vertexById(childVertexId);
            var extrudeVertex = new geomNode.Extrude({
                editing      : true,
                proto        : true,
            });
            this.add(extrudeVertex, function() {
                graph.addEdge(extrudeVertex, child);
            });
            return extrudeVertex;
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

        this.vertexById = function(id) {
            return graph.vertexById(id);
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
                // Ignore the prototype vertices when the serialization happens
                if (vertex.sha) {
                    result.edges[vertex.sha] = that.childrenOf(vertex).map(function(child) {
                        return child.sha;
                    });
                }
            });
            return result;
        }
    }

    return {
        Graph: GeometryGraph
    }

});