define([
    'src/vertexwrapper',
    ], function(vertexWrapper) {

    var EditingModel = vertexWrapper.EditingModel.extend({

        initialize: function(vertex) {
            vertexWrapper.EditingModel.prototype.initialize.call(this, vertex);
            this.view = new EditingDOMView({model: this});
        },

        destroy: function() {
            vertexWrapper.EditingModel.prototype.initialize.call(this);
            this.view.remove();
        },

    });

    var EditingDOMView = vertexWrapper.EditingDOMView.extend({

        initialize: function() {
            vertexWrapper.EditingDOMView.prototype.initialize.call(this);
            $('#variables .new-variable').before(this.$el);
            $('.field').autoGrowInput();
        },

        render: function() {
            var template = 
                '<td class="name">' +  
                '<input class="field" placeholder="var" type="text" value="{{name}}"></input>' +
                '</td>' +
                '<td class="expression">' +  
                '<input class="field" placeholder="expr" type="text" value="{{expression}}"></input>' +
                '</td>';
            var view = {
                id: this.model.vertex.id,   
                name:  this.model.vertex.name,
                expression: this.model.vertex.parameters.expression,
            }
            this.$el.html($.mustache(template, view));
            this.update();
            return this;
        },

        update: function() {

        },

        updateFromDOM: function() {

        },

    });


    return {
        DisplayModel: EditingModel,
    }

});