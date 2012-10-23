define(['src/geomnode'], function(geomNode) {

    var GeometryGraph = function() {
        _.extend(this, Backbone.Events);
        var vertices = [];

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

        this.createPointPrototype = function() {
            var vertex = new geomNode.Node({
                type: 'point',
                parameters: {x: 0, y: 0, z:0}, 
                editing: true,
                addAnotherFn: 'createPointPrototype',
            });
            this.addVertex(vertex);
        }

         this.createLinePrototype = function() {
            var vertex = new geomNode.Node({
                type: 'line',
                parameters: [{x: 0, y: 0, z:0}], 
                editing: true,
                addAnotherFn: 'createLinePrototype',
            });
            this.addVertex(vertex);
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

    return new GeometryGraph();

});