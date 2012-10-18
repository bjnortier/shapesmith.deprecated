define(['src/geometrygraph', 'src/vertexwrapper'], function(geometrygraph, vertexWrapper) {

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
            that.click(event);
        });

        geometrygraph.graph.on('vertexAdded', this.vertexAdded, this);
        geometrygraph.graph.on('vertexRemoved', this.vertexRemoved, this);
        
    }

    Coordinator.prototype.initiateTool = function(name) {
        if (name === 'point') {
            geometrygraph.graph.createPoint();
        }
    }

    Coordinator.prototype.vertexAdded = function(vertex) {
        this.editingModel = new vertexWrapper.Model(vertex);
        $('.toolbar').slideUp(100);
    }

    Coordinator.prototype.vertexRemoved = function(vertex) {
        this.editingModel = undefined;
        $('.toolbar').slideDown(100);
    }

    var coordinator = new Coordinator();
    return coordinator;

});