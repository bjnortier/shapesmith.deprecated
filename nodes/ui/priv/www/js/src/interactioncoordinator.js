define(['src/geometrygraph'], function(geometrygraph) {

    var Coordinator = function() {
        _.extend(this, Backbone.Events);
        this.activeTool = undefined;

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

        geometrygraph.on('vertexAdded', this.vertexAdded, this);
        
    }

    Coordinator.prototype.activateTool = function(name, cursor) {
        this.activeTool = name;
        this.trigger('toolActivated', name);
        $('#scene').css('cursor', 'url(' + cursor + '), crosshair');
    }

    Coordinator.prototype.deactivateTool = function(name) {
        this.activeTool = undefined;
        this.trigger('toolActivated', undefined);
        $('#scene').css('cursor', '');
    }

    Coordinator.prototype.vertexAdded = function() {
        if (this.activeTool) {
            this.deactivateTool(this.activeTool);
        }
    }

    Coordinator.prototype.click = function() {
        if (this.activeTool) {
            if (this.activeTool === 'point') {
                geometrygraph.createPoint();
            }
        }
    }

    Coordinator.prototype.hasActiveTool = function(name) {
        return this.activeTool !== undefined;
    }

    var coordinator = new Coordinator();
    return coordinator;

});