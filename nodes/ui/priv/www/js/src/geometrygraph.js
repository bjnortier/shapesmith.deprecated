define(['lib/underscore-require', 'lib/backbone-require', 'src/geomnode'], function(_, Backbone, geomNode) {

    var GeometryGraph = function() {
        _.extend(this, Backbone.Events);
        var vertices = [], edges = {};

        this.addVertex = function(vertex) {
            vertices.push(vertex);
            this.trigger('vertexAdded', vertex);
        }

        this.removeVertex = function(vertex) {
            var index = vertices.indexOf(vertex);
            if (index >= 0) {
                vertices.splice(index, 1);
            }
            this.trigger('vertexRemoved', vertex);
        }

        var addEdge = function(from, to) {
            if (edges[from.id]) {
                edges[from.id].push(to.id);
            } else {
                edges[from.id] = [to.id];
            }
        }

        this.vertexCount = function() {
            return vertices.length;
        }

        this.getOutgoingEdgesOf = function(from) {
            return edges[from.id] ? edges[from.id] : [];
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
            addEdge(polylineVertex, pointVertex);
            return polylineVertex;
        }

        this.addPointToPolyline = function(polyline) {
            var pointVertex = new geomNode.Point({});
            this.addVertex(pointVertex);

            addEdge(polyline, pointVertex);
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