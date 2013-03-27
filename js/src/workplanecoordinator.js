define(['underscore', 'backbone'], function(_, Backbone) {

    var Coordinator = function() {

        _.extend(this, Backbone.Events);

        var shouldRenderAtZero = true;

        this.shouldRenderAtZero = function() {
            return shouldRenderAtZero;
        }        

        this.setShouldRenderAtZero = function(shouldRender) {
            shouldRenderAtZero = shouldRender;
            this.trigger('change');
        }

    }

    return new Coordinator;

});