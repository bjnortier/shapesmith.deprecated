define([], function() {

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
        
    }

    Coordinator.prototype.activateTool = function(name) {
        this.activeTool = name;
        this.trigger('toolActivated', name);
    }

    Coordinator.prototype.deactivateTool = function(name) {
        this.activeTool = undefined;
        this.trigger('toolActivated', undefined);
    }

    Coordinator.prototype.hasActiveTool = function(name) {
        return this.activeTool !== undefined;
    }

    var coordinator = new Coordinator();
    return coordinator;

});