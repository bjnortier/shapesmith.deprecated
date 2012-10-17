define([], function() {

    var Coordinator = function() {
        _.extend(this, Backbone.Events);
        this.toolState = undefined;

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
        this.toolState = name;
        this.trigger('toolActivated', name);
        return true;
    }

    Coordinator.prototype.hasActiveTool = function(name) {
        return this.toolState !== undefined;
    }

    return {
        Coordinator: Coordinator,
    };

});