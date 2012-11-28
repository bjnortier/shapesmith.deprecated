define([
    'lib/underscore-require',
    'lib/backbone-require',
    'lib/sha1',
    'src/graph',
    'src/geomnode',
    'src/variablegraph',
    ], 
    function(_, Backbone, crypto, graphLib, geomNode, variableGraphLib) {

    var GeometryGraph = function() {

        _.extend(this, Backbone.Events);
        var graph = new graphLib.Graph();
        var varGraph = new variableGraphLib.Graph(graph);
        var that = this;

        this.createGraph = function(shaGraph, shasToVertices) {
            var handledSHAs = [];
            while(_.keys(shaGraph.edges).length > 0) {
                var handledOne = false;
                for(parentSHA in shaGraph.edges) {
                    var childrenSHAs = shaGraph.edges[parentSHA];
                    var uniqueChildrenSHAs = _.uniq(childrenSHAs);
                    var canHandle = _.intersection(uniqueChildrenSHAs, handledSHAs).length == uniqueChildrenSHAs.length;
                    if (canHandle) {
                        handledOne = true;
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
                        delete shaGraph.edges[parentSHA];
                        break;
                    }
                }
                if (!handledOne) {
                    throw Error('nothing to handle');
                }
            }
        }

        this.removeAll = function() {
            graph.vertices().forEach(function(vertex) {
                that.remove(vertex);
            });
        }

        // ---------- Validation ----------

        this.validate = function(vertex) {
            if (vertex.type === 'variable') {
                if (varGraph.canAdd(vertex)) {
                    return true;
                } else {
                    vertex.errors = {name: 'invalid', expression: 'invalid'};
                    vertex.trigger('change', vertex);
                    return false;
                }
            } else if (vertex.type === 'workplane') {
                var snapStr = vertex.parameters.snap;
                try {
                    var snap = parseFloat(snapStr);
                    if (_.isNaN(snap) || (snap <= 0)) {
                        vertex.errors = {snap: 'must be > 0'};
                        vertex.trigger('change', vertex);
                        return false;     
                    }
                    vertex.parameters.snap = snap;
                    return true;
                } catch (e) {
                    vertex.errors = {snap: 'not a number'};
                    vertex.trigger('change', vertex);
                    return false;     
                }
            } else {
                return true;
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

        this.createPolylinePrototype = function(options) {
            var pointVertex = new geomNode.ImplicitPoint({
                editing: true,
                proto: true,
                implicit: true, 
                workplane: options.workplane,
            });
            this.add(pointVertex);
            
            var polylineOptions = _.extend(options || {}, {
                editing      : true,
                proto        : true,
            });
            var polylineVertex = new geomNode.Polyline(polylineOptions);
            
            this.add(polylineVertex, function() {
                graph.addEdge(polylineVertex, pointVertex);
            });
            return polylineVertex;
        }

        this.createExtrudePrototype = function(child, height) {
            var extrudeVertex = new geomNode.Extrude({
                editing    : true,
                proto      : true,
                parameters : {vector: {u: '0', v:'0', n:'1'}, h: height},
            });
            this.add(extrudeVertex, function() {
                graph.addEdge(extrudeVertex, child);
            });
            return extrudeVertex;
        }

        this.createVariablePrototype = function() {
            var vertex = new geomNode.Variable({
                name: 'placeholder', 
                editing      : true,
                proto        : true,
                parameters: {expression: ''}});
            this.add(vertex);
        }

        // ---------- Mutations ----------

        this.addPointToPolyline = function(polyline, point) {
            if (point === undefined) {
                point = new geomNode.ImplicitPoint({
                    editing: true,
                    proto: true,
                    implicit: true, 
                    workplane: polyline.workplane,
                });
                this.add(point, function() {
                    graph.addEdge(polyline, point);
                });
            } else {
                graph.addEdge(polyline, point);
            }
            return point;
        }

        this.removeLastPointFromPolyline = function(polyline) {
            var children = this.childrenOf(polyline);
            if (children.length === 0) {
                throw Error('Cannot remove last point from empty polyline');
            }
            this.remove(children[children.length - 1]);
        }

        this.addEdge = function(from, to) {
            graph.addEdge(from, to);
        }

        // ---------- Variable functions ----------

        this.evaluate = function(expression) {
            try {
                return varGraph.evaluate(expression);
            } catch (e) {
                if (e instanceof variableGraphLib.ParseError) {
                    console.error('Exception when evaluating expression', expression, e);
                } else {
                    throw e;
                }
            }
        }

        // ---------- Graph functions ----------

        this.updateVariableEdges = function(vertex) {
            if (!vertex.editing) {
                var variableChildren = this.childrenOf(vertex).filter(function(v) {
                    return v.type === 'variable';
                });
                variableChildren.map(function(child) {
                    graph.removeEdge(vertex, child);
                });
                var expressions = vertex.getExpressions();
                var newVariableChildren = [];
                expressions.forEach(function(expr) {
                    newVariableChildren = newVariableChildren.concat(varGraph.getExpressionChildren(expr));
                })
                newVariableChildren.forEach(function(child) {
                    graph.addEdge(vertex, child);
                });
            }
        }

        this.add = function(vertex, beforeNotifyFn) {
            graph.addVertex(vertex);
            if (beforeNotifyFn) {
                beforeNotifyFn();
            }
            this.updateVariableEdges(vertex);
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
            this.updateVariableEdges(replacement);
            this.setupEventsForReplacement(original, replacement);
        }

        this.setupEventsForReplacement = function(original, replacement) {
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

        this.filteredVertices = function(filterFn) {
            return graph.vertices().filter(filterFn);
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