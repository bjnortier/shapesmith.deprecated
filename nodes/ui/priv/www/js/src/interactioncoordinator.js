define(['src/geometrygraph', 'src/vertexwrapper'], function(geometrygraph, vertexWrapper) {

    var Coordinator = function() {
        _.extend(this, Backbone.Events);
        this.initiatedTool = undefined;

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

    Coordinator.prototype.initiateTool = function(name, cursor) {
        this.initiatedTool = name;
        this.click(name);
        // this.trigger('toolInitiated', name);
        // $('#scene').css('cursor', 'url(' + cursor + '), crosshair');
    }

    Coordinator.prototype.uninitiateTool = function() {
        // this.initiatedTool = undefined;
        // this.trigger('toolInitiated', undefined);
        // $('#scene').css('cursor', '');
    }

    Coordinator.prototype.vertexAdded = function(vertex) {
        // if (this.initiatedTool) {
        //     this.uninitiateTool(this.initiatedTool);
        // }
        this.editingModel = new vertexWrapper.Model(vertex);
        $('.toolbar').hide();
    }

    Coordinator.prototype.vertexRemoved = function(vertex) {
        this.editingModel = undefined;
        $('.toolbar').show();
        this.initiatedTool = undefined;
    }

    Coordinator.prototype.click = function() {
        if (this.initiatedTool && !this.editingModel) {
            if (this.initiatedTool === 'point') {
                geometrygraph.graph.createPoint();
            }
        }
    }

    Coordinator.prototype.hasInitiatedTool = function(name) {
        return this.initiatedTool !== undefined;
    }

    var coordinator = new Coordinator();
    return coordinator;

});