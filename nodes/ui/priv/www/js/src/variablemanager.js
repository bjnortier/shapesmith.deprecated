define([
    'lib/backbone-require',
    'src/geometrygraphsingleton', 
    ], function(Backbone, geometryGraph) {

    var Model = Backbone.Model.extend({

        initialize: function(vertex) {
            this.views = [
                new TitleView({model: this}),
                new NewVarView({model: this}),
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
                '<td colspan="2" class="title">' +
                '<div class="name">Variables</div>' + 
                '</td>'; 
            var view = {};
            this.$el.html($.mustache(template, view));
            return this;
        },

    });

    var NewVarView = Backbone.View.extend({

        tagName: 'tr',
        className: 'new-variable',

        initialize: function() {
            this.render();
            $('#variables').append(this.$el);
        },

        render: function() {
            var template = 
                '<td class="name">' +  
                '<input class="field var" placeholder="var" type="text" value="{{name}}"></input>' +
                '</td>' +
                '<td class="expression">' +  
                '<input class="field expr" placeholder="expr" type="text" value="{{expression}}"></input>' +
                '</td>';
            var view = {};
            this.$el.html($.mustache(template, view));
            return this;
        },

        events: {
            'focusout .field' : 'addVariable',
        },

        addVariable: function() {
            var name = this.$el.find('.var').val();
            var expr = this.$el.find('.expr').val();
            var vertex = geometryGraph.addVariable(name, expr);
            if (vertex) {
                geometryGraph.commitCreate(vertex);
                this.$el.find('.var').val('');
                this.$el.find('.expr').val('');
                this.$el.removeClass('error');
            } else {
                if ((name !== '') || (expr !== '')) {
                    this.$el.addClass('error');
                } else {
                    this.$el.removeClass('error');
                }
            }

        },


    });

    return new Model();


});