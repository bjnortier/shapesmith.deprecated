define([], function() {

    var Coordinator = function() {
        _.extend(this, Backbone.Events);

        this.toolState = undefined;
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