define(['lib/underscore-require', 'lib/backbone-require', 'src/graph', 'src/geomnode'], 
    function(_, Backbone, graphLib, geomNode) {

    var GeometryGraph = function() {

        _.extend(this, Backbone.Events);
        var that = this;
        var graph = new graphLib.Graph();

        var addVertex = function(vertex) {
            graph.addVertex(vertex);
            that.trigger('vertexAdded', vertex);
        }

        var removeVertex = function(vertex) {
            graph.removeVertex(vertex);
            that.trigger('vertexRemoved', vertex);
        }
       
        this.createPointPrototype = function() {
            var pointVertex = new geomNode.Point({
                editing: true,
                addAnotherFn: 'createPointPrototype',
            });
            addVertex(pointVertex);
            return pointVertex;
        }

        this.createPolylinePrototype = function() {
            var pointVertex = new geomNode.Point({});
            var polylineVertex = new geomNode.Polyline({
                editing: true,
                addAnotherFn: 'createPolylinePrototype',
            });
            addVertex(pointVertex);
            addVertex(polylineVertex);
            graph.addEdge(polylineVertex, pointVertex);
            return polylineVertex;
        }

        this.addPointToPolyline = function(polyline) {
            var pointVertex = new geomNode.Point({});
            addVertex(pointVertex);
            graph.addEdge(polyline, pointVertex);
        }

        this.childrenOf = function(vertex) {
            return graph.outgoingEdgesOf(vertex).map(function(id) {
                return graph.vertexById(id);
            });
        }

        this.isEditing = function() {
            return _.contains(_.pluck(graph.vertices, 'editing'), true);
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