define(['src/geometrygraph'], function(geometryGraph) {

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

        },

        cancel: function() {
            geometryGraph.graph.removeVertex(this.vertex);
            this.destroy();
        },

    });

    var DOMView = Backbone.View.extend({

        className: 'vertex',

        initialize: function() {
            $('body').append(this.$el);
            this.render();
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
                '<div class="okcancel">' + 
                '<div class="ok"><img src="/ui/images/icons/ok24x24.png"/></div>' +
                '<div class="cancel"><img src="/ui/images/icons/cancel24x24.png"/></div>' +
                '</div>' + 
                '<div class="title"><img src="/ui/images/icons/point32x32.png"/>' +
                '<div class="name">Vertex</div>' + 
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

    return {
        Model: Model
    };

});