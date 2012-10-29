define(['lib/underscore-require', 'lib/backbone-require', 'src/geomnode'], function(_, Backbone, geomNode) {

    var GeometryGraph = function() {
        _.extend(this, Backbone.Events);
        var vertices = [], outgoingEdges = {}, incomingEdges = {};

        this.addVertex = function(vertex) {
            vertices.push(vertex);
            outgoingEdges[vertex.id] = [];
            incomingEdges[vertex.id] = [];
            this.trigger('vertexAdded', vertex);
        }

        this.removeVertex = function(vertex) {
            var index = vertices.indexOf(vertex);
            if (index === -1) {
                throw Error('Cannot remove non-existant vertex: ' + vertex.id);
            }
            vertices.splice(index, 1);

            outgoingEdges[vertex.id].forEach(function(toId) {
                index = incomingEdges[toId].indexOf(vertex.id);
                incomingEdges[toId].splice(index, 1);
            });
            outgoingEdges[vertex.id] = [];

            incomingEdges[vertex.id].forEach(function(fromId) {
                index = outgoingEdges[fromId].indexOf(vertex.id);
                outgoingEdges[fromId].splice(index, 1);
            });
            incomingEdges[vertex.id] = [];

            this.trigger('vertexRemoved', vertex);
        }

        this.addEdge = function(from, to) {
            outgoingEdges[from.id].push(to.id);
            incomingEdges[to.id].push(from.id);
        }

        this.vertexCount = function() {
            return vertices.length;
        }

        this.outgoingEdgesOf = function(from) {
            return outgoingEdges[from.id] ? outgoingEdges[from.id] : [];
        }

        this.incomingEdgesOf = function(to) {
            return incomingEdges[to.id] ? incomingEdges[to.id] : [];
        }

        this.createPointPrototype = function() {
            var pointVertex = new geomNode.Point({
                editing: true,
                addAnotherFn: 'createPointPrototype',
            });
            this.addVertex(pointVertex);
            return pointVertex;
        }

        this.createPolylinePrototype = function() {
            var pointVertex = new geomNode.Point({});
            var polylineVertex = new geomNode.Polyline({
                editing: true,
                addAnotherFn: 'createPolylinePrototype',
            });
            this.addVertex(pointVertex);
            this.addVertex(polylineVertex);
            this.addEdge(polylineVertex, pointVertex);
            return polylineVertex;
        }

        this.addPointToPolyline = function(polyline) {
            var pointVertex = new geomNode.Point({});
            this.addVertex(pointVertex);

            this.addEdge(polyline, pointVertex);
        }

        this.isEditing = function() {
            return _.contains(_.pluck(vertices, 'editing'), true);
        }

        this.getPointCoordinates = function(id) {
            var vertex = _.find(vertices, function(vertex) { return vertex.id === id });
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