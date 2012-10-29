define([], function() {
    
    var Graph = function() {
        
        var vertices = {}, outgoingEdges = {}, incomingEdges = {}, size = 0;

        this.addVertex = function(vertex) {
            if (vertices[vertex.id]) {
                throw Error('Vertex ' + vertex.id + ' already in graph');
            }
            vertices[vertex.id] = vertex;
            outgoingEdges[vertex.id] = [];
            incomingEdges[vertex.id] = [];
            ++size;
        }

        this.removeVertex = function(vertex) {
            if (!vertices[vertex.id]) {
                throw Error('Cannot remove non-existant vertex: ' + vertex.id);
            }
            delete vertices[vertex.id];

            outgoingEdges[vertex.id].forEach(function(toId) {
                var index = incomingEdges[toId].indexOf(vertex.id);
                incomingEdges[toId].splice(index, 1);
            });
            outgoingEdges[vertex.id] = [];

            incomingEdges[vertex.id].forEach(function(fromId) {
                var index = outgoingEdges[fromId].indexOf(vertex.id);
                outgoingEdges[fromId].splice(index, 1);
            });
            incomingEdges[vertex.id] = [];
            --size;
        }

        this.replaceVertex = function(oldVertex, newVertex) {
            this.addVertex(newVertex);
            if (!vertices[oldVertex.id]) {
                throw Error('Cannot remove non-existant vertex: ' + oldVertex.id);
            }
            delete vertices[oldVertex.id];
            --size;

            var outgoingEdgesCopy = outgoingEdges[oldVertex.id].slice(0);
            var incomingEdgesCopy = incomingEdges[oldVertex.id].slice(0);

            outgoingEdgesCopy.forEach(function(toId) {
                var index = incomingEdges[toId].indexOf(oldVertex.id);
                incomingEdges[toId].splice(index, 1, newVertex.id);
            });

            incomingEdgesCopy.forEach(function(fromId) {
                var index = outgoingEdges[fromId].indexOf(oldVertex.id);
                outgoingEdges[fromId].splice(index, 1, newVertex.id);
            });

            outgoingEdges[newVertex.id] = outgoingEdgesCopy;
            incomingEdges[newVertex.id] = incomingEdgesCopy;


        }

        this.vertexById = function(id) {
            return vertices[id];
        }

        this.size = function() {
            return size;
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

        this.stringify = function() {
            var str = '\n';
            var toString = function(obj) {
                if (Object.prototype.toString.call(obj) === '[object Array]') {
                    if (obj.length === 0) {
                        return '[]';
                    }
                }
                return obj.toString();
            }
            for (var key in vertices) {
                str += key + ':' + vertices[key] + '\n'; 
            }
            for (var key in outgoingEdges) {
                str += key + ' → ' + toString(outgoingEdges[key]) + '\n';
            }
            for (var key in incomingEdges) {
                str += key + ' ← ' + toString(incomingEdges[key]) + '\n';
            }
            return str;
        }

    }

    return {
        Graph: Graph
    }


});