 define(['src/geometrygraph'], function(geometryGraph) {

    var Coordinator = function() {
        _.extend(this, Backbone.Events);

        var that = this;
        window.addEventListener('keydown', function(event) {
            that.trigger('keydown', event);
        }, false);
        $('#scene').mousemove(function(event) {
            that.trigger('mousemove', event);
        });
        $('#scene').mouseup(function(event) {
            that.trigger('mouseup', event);
        });
        $('#scene').mousedown(function(event) {
            that.trigger('mousedown', event);
        });
        $('#scene').mousewheel(function(event) {
            that.trigger('mousewheel', event);
        });
        $('#scene').click(function(event) {
            that.trigger('click', event);
        });

        geometryGraph.graph.on('vertexAdded', this.vertexAdded, this);
        geometryGraph.graph.on('vertexRemoved', this.vertexRemoved, this);
        
    }

    Coordinator.prototype.initiateTool = function(name) {
        if (name === 'point') {
            geometryGraph.graph.createPoint();
        }
    }

    Coordinator.prototype.vertexAdded = function(vertex) {
        $('.toolbar').slideUp(100);
    }

    Coordinator.prototype.vertexRemoved = function(vertex) {
        $('.toolbar').slideDown(100);
    }       

    return new Coordinator();
});