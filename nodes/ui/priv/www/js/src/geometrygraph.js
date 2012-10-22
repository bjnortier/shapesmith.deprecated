define(['src/geomnode'], function(geomNode) {

    var GeometryGraph = function() {

        _.extend(this, Backbone.Events);
        this.vertices = [];

    }

    GeometryGraph.prototype.addVertex = function(vertex) {
        this.vertices.push(vertex);
        this.trigger('vertexAdded', vertex);
    }

    GeometryGraph.prototype.removeVertex = function(vertex) {
        var index = this.vertices.indexOf(vertex);
        if (index >= 0) {
            this.vertices.splice(index, 1);
        }
        this.trigger('vertexRemoved', vertex);
    }

    GeometryGraph.prototype.createPointPrototype = function() {
        var vertex = new geomNode.Node({
            parameters: {x: 0, y: 0, z:0}, 
            editing: true,
            addAnotherFn: 'createPointPrototype',
        });
        this.addVertex(vertex);
    }

    return new GeometryGraph();

});