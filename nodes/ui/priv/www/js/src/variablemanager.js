define([
    'lib/backbone-require',
    'src/geometrygraphsingleton', 
    ], function(Backbone, geometryGraph) {

    var Model = Backbone.Model.extend({

        initialize: function(vertex) {
            this.view = new DOMView({model: this});
        },

    });

    var DOMView = Backbone.View.extend({

        id: 'variable-manager',
        tagName: "tr",

        initialize: function() {
            this.render();
            $('#variables').prepend(this.$el);
        },

        render: function() {
            var template = 
                '<td colspan="2" class="title">' +
                '<div class="name">Variables <span class="add">+</span></div>' + 
                '</td>'; 
            var view = {};
            this.$el.html($.mustache(template, view));
            return this;
        },

        events: {
            'click .add' : 'addVariable',
        },

        addVariable: function() {
            geometryGraph.addVariable('', '');
        },

    });

    return new Model();


});