define([
    'src/vertexwrapper',
    'src/geometrygraphsingleton', 
    'src/interactioncoordinator',
    ], function(vertexWrapper, geometryGraph, coordinator) {

    // ---------- Editing ----------

    var EditingModel = vertexWrapper.EditingModel.extend({

        initialize: function(vertex) {
            vertexWrapper.EditingModel.prototype.initialize.call(this, vertex);
            this.views = [
                new EditingView({model: this})
            ]
        },

        destroy: function() {
            vertexWrapper.EditingModel.prototype.destroy.call(this);
        },

    });

    var EditingView = vertexWrapper.EditingDOMView.extend({

        tagName: 'tr',

        initialize: function() {
            vertexWrapper.EditingDOMView.prototype.initialize.call(this);
            this.render();
            this.$el.addClass('variable');
            $('#variables').append(this.$el);
            $('.field').autoGrowInput();
            coordinator.on('sceneClick', this.sceneClick, this);
        },

        remove: function() {
            vertexWrapper.EditingDOMView.prototype.remove.call(this);
            coordinator.off('sceneClick', this.sceneClick, this);
        },

        render: function() {
            var template = 
                '<td class="name">' +  
                '<input class="field var" placeholder="var" type="text" value="{{name}}"></input>' +
                '</td>' +
                '<td class="expression">' +  
                '<input class="field expr" placeholder="expr" type="text" value="{{expression}}"></input>' +
                '</td>' +
                '<td><div class="delete"></div></td>';
            var view = {};
            if (!this.model.vertex.proto) {
                view.name = this.model.vertex.name;
                view.expression = this.model.vertex.parameters.expression;
            }
            this.$el.html($.mustache(template, view));
            return this;
        },

        events: {
            'click .delete'  : 'delete',
        },

        update: function() {
            if (this.model.vertex.errors) {
                this.$el.addClass('error');
            } else {
                this.$el.removeClass('error');
            }
        },

        delete: function() {
            if (this.model.vertex.proto) {
                this.model.cancel();
            }
        },

        sceneClick: function() {
            console.log(event.originalEvent);
            var name = this.$el.find('.var').val();
            var expr = this.$el.find('.expr').val();
            this.model.vertex.name = name;
            this.model.vertex.parameters.expression = expr;
            if (this.model.vertex.proto) {
                this.model.okCreate();
            } else {
                this.model.okEdit();
            }

        },

    });

    // ---------- Editing ----------

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

    var DisplayDOMView = vertexWrapper.DisplayDOMView.extend({

        initialize: function() {
            vertexWrapper.DisplayDOMView.prototype.initialize.call(this);
            $('#variables').append(this.$el);
            $('.field').autoGrowInput();
        },

        render: function() {
            var template = 
                '<td class="name">{{name}}</input></td>' +
                '<td class="expression">{{expression}}</td>';
            var view = {
                id: this.model.vertex.id,   
                name:  this.model.vertex.name,
                expression: this.model.vertex.parameters.expression,
            }
            this.$el.html($.mustache(template, view));
            return this;
        },

        events: {
            'click' : 'click',
        },

        click: function(event) {
            geometryGraph.edit(this.model.vertex);
        },

    });


    return {
        EditingModel: EditingModel,
        DisplayModel: DisplayModel,
    }

});