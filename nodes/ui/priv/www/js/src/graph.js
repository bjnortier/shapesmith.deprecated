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

    }

    return {
        Graph: Graph
    }


});