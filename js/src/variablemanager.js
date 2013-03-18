define([
    'backbone',
    'jquery',
    'lib/jquery.mustache',
    'src/geometrygraphsingleton', 
    ], function(Backbone, $, __$, geometryGraph) {

    var Model = Backbone.Model.extend({

        initialize: function(vertex) {
            this.views = [
                new TitleView({model: this}),
            ];
        },

    });

    var TitleView = Backbone.View.extend({

        id: 'variable-manager',
        tagName: 'tr',

        initialize: function() {
            this.render();
            $('#variables').append(this.$el);
        },

        render: function() {
            var template = 
                '<td colspan="3" class="title">' +
                '<div class="TitleView">Variables <span class="add">+</span></div>' + 
                '</td>'; 
            var view = {};
            this.$el.html($.mustache(template, view));
            return this;
        },

        events: {
            'click .add': 'addVariableProto',
        },

        addVariableProto: function(event) {
            event.stopPropagation();
            if (!geometryGraph.isEditing()) {
                geometryGraph.createVariablePrototype();
            }
        },

    });
    
    return new Model();

});