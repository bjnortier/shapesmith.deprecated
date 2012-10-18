define([
        'src/geometrygraph', 
        'src/interactioncoordinator', 
        'src/workplane',
    ], 
    function(geometryGraph, coordinator, workplane) {

    var Model = Backbone.Model.extend({

        initialize: function(vertex) {
            this.vertex = vertex;
            this.views = [
                new DOMView({model: this})
            ];
        },

        destroy: function() {
            this.views.forEach(function(view) {
                view.remove();
            });
            this.views = [];
        },  

        ok: function() {
            geometryGraph.graph.removeVertex(this.vertex);
            this.destroy();
        },

        cancel: function() {
            geometryGraph.graph.removeVertex(this.vertex);
            this.destroy();
        },

    });

    var DOMView = Backbone.View.extend({

        className: 'vertex',

        initialize: function() {
            this.render();
            $('body').append(this.$el);
        },

        remove: function() {
            Backbone.View.prototype.remove.call(this);
        },

        events: {
            'click .okcancel .ok' : 'ok',
            'click .okcancel .cancel' : 'cancel',
        },

        render: function() {
            var template = 
                '<div class="title"><img src="/ui/images/icons/point32x32.png"/>' +
                '<div class="name">Vertex</div>' + 
                '<span class="okcancel">' + 
                '<span class="ok button"><img src="/ui/images/icons/ok24x24.png"/></span>' +
                '<span class="cancel button"><img src="/ui/images/icons/cancel24x24.png"/></span>' +
                '</span>' + 
                '</div>' + 
                '<div class="coordinate">' +
                '<span class="x">{{x}}</span><span class="y">{{y}}</span><span class="z">{{z}}</span>' +
                '</div>';
            var view = {
                x: this.model.vertex.parameters.x,
                y: this.model.vertex.parameters.y,
                z: this.model.vertex.parameters.z,
            }
            this.$el.html($.mustache(template, view));
            return this;
        },

        ok: function() {
            this.model.ok();
        },

        cancel: function() {
            this.model.cancel();
        },

    });

    var editingModel = undefined;
    geometryGraph.graph.on('vertexAdded', function(vertex) {
        editingModel = new Model(vertex);
    });
    geometryGraph.graph.on('vertexRemoved', function(vertex) {
        editingModel = undefined;
    }, this);


});