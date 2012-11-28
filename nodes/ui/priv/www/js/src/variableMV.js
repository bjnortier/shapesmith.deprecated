define([
        'src/vertexMV',
        'src/geometrygraphsingleton', 
        'src/interactioncoordinator',
        'src/asyncAPI',
    ], function(
        VertexMV, 
        geometryGraph, 
        coordinator,
        AsyncAPI) {


    // var lastIndex = {}

    // var addToTable = function(vertex, el) {
    //     var vertexId = vertex.id;
    //     var lastVertexRowIndex = lastIndex[vertexId];
    //     if ((lastVertexRowIndex !== undefined) && 
    //         ($('#variables tr').length > lastVertexRowIndex)) {
    //         $($('#variables tr')[lastIndex[vertexId]]).before(el);
    //     } else {
    //         $('#variables').append(el);
    //     }
    // }

    // var saveRowIndex = function(vertexId, el) {
    //     var rowIndex = el.closest('tr').prevAll().length;
    //     lastIndex[vertexId] = rowIndex;
    // }

    // ---------- Editing ----------

    var EditingModel = VertexMV.EditingModel.extend({

        initialize: function(original, vertex) {
            this.displayModelConstructor = DisplayModel;
            VertexMV.EditingModel.prototype.initialize.call(this, original, vertex);
            this.views.push(new EditingView({model: this}));
            coordinator.on('sceneClick', this.tryCommit, this);
        },

        destroy: function() {
            VertexMV.EditingModel.prototype.destroy.call(this);
            coordinator.off('sceneClick', this.tryCommit, this);
        },

    });

    var EditingView = VertexMV.EditingDOMView.extend({

        tagName: 'tr',

        initialize: function() {
            VertexMV.EditingDOMView.prototype.initialize.call(this);
            this.render();
            this.$el.addClass('variable');
            $('#variables').append(this.$el);
            $('.field').autoGrowInput();

        },

        remove: function() {
            VertexMV.EditingDOMView.prototype.remove.call(this);
            coordinator.off('sceneClick', this.model.tryCommit, this.model);
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
            var name = this.$el.find('.var').val();
            var expr = this.$el.find('.expr').val();
            this.model.vertex.name = name;
            this.model.vertex.parameters.expression = expr;
        },

    });

    // ---------- Editing ----------

    var DisplayModel = VertexMV.DisplayModel.extend({

        initialize: function(vertex) {
            this.displayModelConstructor = DisplayModel;
            this.editingModelConstructor = EditingModel;
            VertexMV.DisplayModel.prototype.initialize.call(this, vertex);
            this.views.push(new DisplayDOMView({model: this}));
        },

        destroy: function() {
            VertexMV.DisplayModel.prototype.destroy.call(this);
        },

    });

    var DisplayDOMView = VertexMV.DisplayDOMView.extend({

        initialize: function() {
            VertexMV.DisplayDOMView.prototype.initialize.call(this);
            $('#variables').append(this.$el);
            $('.field').autoGrowInput();
        },

        remove: function() {
            VertexMV.DisplayDOMView.prototype.remove.call(this);
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
            'click .delete': 'delete',
        },

        click: function() {
            this.model.destroy();
            var editingVertex = AsyncAPI.edit(this.model.vertex);
            new this.model.editingModelConstructor(this.model.vertex, editingVertex);
        },

        delete: function(event) {
            event.stopPropagation();
            this.model.tryDelete();
        },

    });


    return {
        EditingModel: EditingModel,
        DisplayModel: DisplayModel,
    }

});