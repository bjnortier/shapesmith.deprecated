define(['lib/underscore-require'], function(_) {
    
    var Graph = function() {
        
        var vertices = {}, outgoingVertices = {}, incomingVertices = {}, size = 0;

        this.addVertex = function(vertex) {
            if (vertices[vertex.id]) {
                throw Error('Vertex ' + vertex.id + ' already in graph');
            }
            vertices[vertex.id] = vertex;
            outgoingVertices[vertex.id] = [];
            incomingVertices[vertex.id] = [];
            ++size;
        }

        this.removeVertex = function(vertex) {
            if (!vertices[vertex.id]) {
                throw Error('Cannot remove non-existant vertex: ' + vertex.id);
            }
            delete vertices[vertex.id];

            outgoingVertices[vertex.id].forEach(function(toId) {
                var index = incomingVertices[toId].indexOf(vertex.id);
                incomingVertices[toId].splice(index, 1);
            });
            outgoingVertices[vertex.id] = [];

            incomingVertices[vertex.id].forEach(function(fromId) {
                var index = outgoingVertices[fromId].indexOf(vertex.id);
                outgoingVertices[fromId].splice(index, 1);
            });
            incomingVertices[vertex.id] = [];
            --size;
        }

        this.replaceVertex = function(oldVertex, newVertex) {
            if (oldVertex.id === newVertex.id) {
                vertices[newVertex.id] = newVertex;
                return;
            }

            if (!vertices[oldVertex.id]) {
                throw Error('Cannot remove non-existant vertex: ' + oldVertex.id);
            }
            delete vertices[oldVertex.id];
            --size;
            this.addVertex(newVertex);

            var outgoingVerticesCopy = outgoingVertices[oldVertex.id].slice(0);
            var incomingVerticesCopy = incomingVertices[oldVertex.id].slice(0);

            outgoingVerticesCopy.forEach(function(toId) {
                var index = incomingVertices[toId].indexOf(oldVertex.id);
                incomingVertices[toId].splice(index, 1, newVertex.id);
            });

            incomingVerticesCopy.forEach(function(fromId) {
                var index = outgoingVertices[fromId].indexOf(oldVertex.id);
                outgoingVertices[fromId].splice(index, 1, newVertex.id);
            });

            outgoingVertices[newVertex.id] = outgoingVerticesCopy;
            incomingVertices[newVertex.id] = incomingVerticesCopy;


        }

        this.vertexById = function(id) {
            return vertices[id];
        }

        this.vertices = function() {
            return _.values(vertices);
        }

        this.size = function() {
            return size;
        }

        this.addEdge = function(from, to) {
            outgoingVertices[from.id].push(to.id);
            incomingVertices[to.id].push(from.id);
        }

        this.removeEdge = function(from, to) {
            var outgoing = outgoingVertices[from.id];
            var index = outgoing.indexOf(to.id);
            if (index === -1) {
                throw Error('no edge from ' + from.id + ' to ' + to.id);
            }
            outgoing.splice(index, 1);

            var incoming = incomingVertices[to.id];
            index = incoming.indexOf(from.id);
            incoming.splice(index, 1);
        }

        this.vertexCount = function() {
            return vertices.length;
        }

        this.outgoingVerticesOf = function(from) {
            return outgoingVertices[from.id] ? outgoingVertices[from.id] : [];
        }

        this.incomingVerticesOf = function(to) {
            return incomingVertices[to.id] ? incomingVertices[to.id] : [];
        }

        this.leafFirstSearch = function(listener) {

            var remainingIds = _.pluck(vertices, 'id');
            var visitedIds   = [];

            // For each vertex in the graph, check if all the children
            // have been visited. If yes, then visit it.
            while(remainingIds.length > 0) {
                for (var i in remainingIds) {
                    var vertex = this.vertexById(remainingIds[i]);
                    var outgoingVertexIds = outgoingVertices[vertex.id];
                    if (_.intersection(outgoingVertexIds, visitedIds).length == outgoingVertexIds.length) {
                        listener(vertex);
                        visitedIds.push(vertex.id);
                        remainingIds.splice(remainingIds.indexOf(vertex.id), 1)
                        break;
                    }
                }
            }

        }

       
    }

    return {
        Graph: Graph
    }


});