define([
    'src/vertexwrapper',
    'src/geometrygraphsingleton', 
    'src/interactioncoordinator',
    ], function(vertexWrapper, geometryGraph, coordinator) {


    var lastIndex = {}

    var addToTable = function(vertex, el) {
        var vertexId = vertex.id;
        var lastVertexRowIndex = lastIndex[vertexId];
        if ((lastVertexRowIndex !== undefined) && 
            ($('#variables tr').length > lastVertexRowIndex)) {
            $($('#variables tr')[lastIndex[vertexId]]).before(el);
        } else {
            $('#variables').append(el);
        }
    }

    var saveRowIndex = function(vertexId, el) {
        var rowIndex = el.closest('tr').prevAll().length;
        lastIndex[vertexId] = rowIndex;
    }

    // ---------- Editing ----------

    var EditingModel = vertexWrapper.EditingModel.extend({

        initialize: function(vertex) {
            vertexWrapper.EditingModel.prototype.initialize.call(this, vertex);
            this.views.push(new EditingView({model: this}));
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
            addToTable(this.model.vertex, this.$el);
            $('.field').autoGrowInput();
            coordinator.on('sceneClick', this.sceneClick, this);
        },

        remove: function() {
            saveRowIndex(this.model.vertex.id, this.$el);
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
            var view = {
                id: this.model.vertex.id,
            };
            if (!this.model.vertex.proto) {
                view.name = this.model.vertex.name;
                view.expression = this.model.vertex.parameters.expression;
            }
            this.$el.html($.mustache(template, view));
            return this;
        },

        update: function() {
            if (this.model.vertex.errors) {
                this.$el.addClass('error');
            } else {
                this.$el.removeClass('error');
            }
        },

        updateFromDOM: function() {

        },

        tryCommit: function() {
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
            this.views.push(new DisplayDOMView({model: this}));
        },

        destroy: function() {
            vertexWrapper.Model.prototype.destroy.call(this);
        },

    });

    var DisplayDOMView = vertexWrapper.DisplayDOMView.extend({

        initialize: function() {
            vertexWrapper.DisplayDOMView.prototype.initialize.call(this);
            addToTable(this.model.vertex, this.$el);
            $('.field').autoGrowInput();
        },

        remove: function() {
            saveRowIndex(this.model.vertex.id, this.$el);
            vertexWrapper.DisplayDOMView.prototype.remove.call(this);
        },

        render: function() {
            var template = 
                '<td class="name">{{name}}</input></td>' +
                '<td class="expression">{{expression}}</td>' +
                '<td><div class="delete"></div></td>';
            var view = {
                id: this.model.vertex.id,   
                name:  this.model.vertex.name,
                expression: this.model.vertex.parameters.expression,
            }
            this.$el.html($.mustache(template, view));
            return this;
        },

        events: {
            'click .name' : 'click',
            'click .expression' : 'click',
            'click .delete' : 'delete',
        },

        delete: function() {
            geometryGraph.commitDelete(this.model.vertex);
        },

        click: function() {
            geometryGraph.edit(this.model.vertex);
        },

    });


    return {
        EditingModel: EditingModel,
        DisplayModel: DisplayModel,
    }

});