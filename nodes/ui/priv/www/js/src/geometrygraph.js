define(['lib/underscore-require', 'lib/backbone-require', 'src/graph', 'src/geomnode'], 
    function(_, Backbone, graphLib, geomNode) {

    var GeometryGraph = function() {

        _.extend(this, Backbone.Events);
        var graph = new graphLib.Graph();

        this.commit = function(vertex) {
            var nonEditingReplacement = vertex.cloneNonEditing();
            graph.replaceVertex(vertex, nonEditingReplacement);

            this.trigger('vertexRemoved', vertex);
            this.trigger('vertexAdded', nonEditingReplacement);
        }

        this.cancelPrototype = function(vertex) {
            graph.removeVertex(vertex);
            this.trigger('vertexRemoved', vertex);
        }
       
        this.createPointPrototype = function() {
            var pointVertex = new geomNode.Point({
                editing: true,
                nameFromId: true,
                addAnotherFn: 'createPointPrototype',
            });
            graph.addVertex(pointVertex);
            this.trigger('vertexAdded', pointVertex);
            return pointVertex;
        }

        this.createPolylinePrototype = function() {
            var pointVertex = new geomNode.Point({});
            var polylineVertex = new geomNode.Polyline({
                editing: true,
                nameFromId: true,
                addAnotherFn: 'createPolylinePrototype',
            });

            graph.addVertex(pointVertex);
            graph.addVertex(polylineVertex);
            graph.addEdge(polylineVertex, pointVertex);
            this.trigger('vertexAdded', pointVertex);
            this.trigger('vertexAdded', polylineVertex);

            return polylineVertex;
        }

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
            return _.contains(_.pluck(graph.vertices(), 'editing'), true);
        }

        this.getPointCoordinates = function(id) {
            var vertex = _.find(graph.vertices, function(vertex) { 
                return vertex.id === id 
            });
            if (!vertex) {
                throw Error('not vertex for id: ' + id);
            }
            return {
                x: vertex.parameters.x,
                y: vertex.parameters.y,
                z: vertex.parameters.z,
            }
        }
    }

    return {
        Graph: GeometryGraph
    }

});