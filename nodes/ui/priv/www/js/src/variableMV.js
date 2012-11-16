define([
    'src/vertexwrapper',
    'src/geometrygraphsingleton', 
    ], function(vertexWrapper, geometryGraph) {

    var DisplayModel = vertexWrapper.Model.extend({

        initialize: function(vertex) {
            vertexWrapper.Model.prototype.initialize.call(this, vertex);
            this.views = [
                new DisplayDOMView({model: this})
            ]
        },

        destroy: function() {
            vertexWrapper.Model.prototype.destroy.call(this);
        },

    });

    var DisplayDOMView = vertexWrapper.EditingDOMView.extend({

        initialize: function() {
            vertexWrapper.EditingDOMView.prototype.initialize.call(this);
            $('#variables .new-variable').before(this.$el);
            $('.field').autoGrowInput();
        },

        render: function() {
            var template = 
                '<td class="name">' +  
                '<input class="field var" placeholder="var" type="text" value="{{name}}"></input>' +
                '</td>' +
                '<td class="expression">' +  
                '<input class="field expr" placeholder="expr" type="text" value="{{expression}}"></input>' +
                '</td>';
            var view = {
                id: this.model.vertex.id,   
                name:  this.model.vertex.name,
                expression: this.model.vertex.parameters.expression,
            }
            this.$el.html($.mustache(template, view));
            return this;
        },

        events: {
            'focusout .field' : 'updateFromDOM',
        },

        updateFromDOM: function() {
            var oldName = this.model.vertex.id;
            var newName = this.$el.find('.var').val();
            var expr = this.$el.find('.expr').val();
            var vertex = geometryGraph.updateVariable(oldName, newName, expr);
            if (vertex) {
                this.$el.removeClass('error');
                this.model.vertex.editing = true;
                geometryGraph.commitEditWithReplacement(this.model.vertex, vertex);
            } else {
                this.$el.addClass('error');
            }
        },

    });


    return {
        DisplayModel: DisplayModel,
    }

});