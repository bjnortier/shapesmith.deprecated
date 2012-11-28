define([
        'src/colors',
        'src/scenevieweventgenerator',
        'src/workplaneMV',
        'src/vertexMV',
        'src/selection',
        'src/geometrygraphsingleton',
        'src/asyncAPI',
    ], function(
        colors, 
        sceneViewEventGenerator, 
        Workplane, 
        VertexMV,
        selection,
        geometryGraph,
        AsyncAPI) {


    // ---------- Editing ----------

    var EditingModel = VertexMV.EditingModel.extend({

        initialize: function(options) {
            this.currentWorkplaneModel = Workplane.getCurrent();
            VertexMV.EditingModel.prototype.initialize.call(this, options);

            this.currentWorkplaneModel.on('positionChanged', this.workplanePositionChanged, this);
            this.currentWorkplaneModel.on('click', this.workplaneClick, this);
            this.currentWorkplaneModel.on('dblclick', this.workplaneDblClick, this);
            sceneViewEventGenerator.on('sceneViewClick', this.sceneViewClick, this);
            sceneViewEventGenerator.on('sceneViewDblClick', this.sceneViewDblClick, this);
        },

        destroy: function() {
            VertexMV.EditingModel.prototype.destroy.call(this);
            this.currentWorkplaneModel.off('positionChanged', this.workplanePositionChanged, this);
            this.currentWorkplaneModel.off('click', this.workplaneClick, this);
            this.currentWorkplaneModel.off('dblclick', this.workplaneDblClick, this);
            sceneViewEventGenerator.off('sceneViewClick', this.sceneViewClick, this);
            sceneViewEventGenerator.off('sceneViewDblClick', this.sceneViewDblClick, this);
        },  

    });

    var EditingDOMView = VertexMV.EditingDOMView.extend({

        initialize: function() {
            VertexMV.EditingDOMView.prototype.initialize.call(this);
            if (!this.model.vertex.implicit) {
                VertexMV.replaceOrAppendInTable(this, '#geometry'); 
            }
            $('.field').autoGrowInput();
        },

        remove: function() {
            VertexMV.EditingDOMView.prototype.remove.call(this);
        },

    });

    var EditingSceneView = VertexMV.SceneView.extend({

        initialize: function() {
            this.color = colors.geometry.editing;
            VertexMV.SceneView.prototype.initialize.call(this);
            this.model.vertex.on('change', this.render, this);
        },

        remove: function() {
            VertexMV.SceneView.prototype.remove.call(this);
            this.model.vertex.off('change', this.render, this);
        },

    });

    // ---------- Display ----------

    var DisplayModel = VertexMV.DisplayModel.extend({ 

        canSelect: function() {
            return true;
        },

        selectParentOnClick: function() {
            return false;
        },

        select: function(ids, selection) {
            VertexMV.DisplayModel.prototype.select.call(this, ids, selection);
            if ((selection.length === 1) && this.selected) {
                VertexMV.replaceWithEditing(this.vertex, AsyncAPI.edit(this.vertex));
            }
        },

    });

    var DisplayDOMView = VertexMV.DisplayDOMView.extend({

        tagName: "tr",
        className: 'vertex display',

        initialize: function() {
            VertexMV.DisplayDOMView.prototype.initialize.call(this);
            this.$el.addClass(this.model.vertex.name);  
            VertexMV.replaceOrAppendInTable(this, '#geometry'); 
            this.updateSelection();
        },

        render: function() {
            var view = {
                name: this.model.vertex.name,
                type: this.model.vertex.type,
            }
            var template = 
                '<td class="title">' + 
                '<img src="/ui/images/icons/{{type}}32x32.png"/>' + 
                '<div class="name">{{name}}</div>' + 
                '<div class="delete"></div>' +
                '</td>';
            this.$el.html($.mustache(template, view));
            return this;
        },        

        events: {
            'click .title' : 'clickTitle',
            'click .delete': 'delete',
        },

        clickTitle: function(event) {
            if (this.model.canSelect()) {
                if (event.shiftKey || event.ctrlKey || event.metaKey) {
                    // selection.addToSelection(this.model.vertex.id);
                    selection.selectOnly(this.model.vertex.id);
                } else {
                    selection.selectOnly(this.model.vertex.id);
                }
            }
        },

        delete: function(event) {
            event.stopPropagation();
            this.model.tryDelete();
        },

        updateSelection: function() {
            if (this.model.selected) {
                this.$el.addClass('selected');
            } else {
                this.$el.removeClass('selected');
            }
        },

    });

    var DisplaySceneView =  VertexMV.SceneView.extend({

        initialize: function() {
            this.color = colors.geometry.default;
            VertexMV.SceneView.prototype.initialize.call(this);
            this.model.on('updateSelection', this.updateSelection, this);
            this.on('mouseEnter', this.highlight, this);
            this.on('mouseLeave', this.unhighlight, this);
            this.on('click', this.click, this);
            this.on('dblclick', this.dblclick, this);
            this.model.vertex.on('change', this.render, this);
            this.updateSelection();
        },

        remove: function() {
            this.model.off('updateSelection', this.updateSelection, this);

            VertexMV.SceneView.prototype.remove.call(this);
            this.off('mouseEnter', this.highlight, this);
            this.off('mouseLeave', this.unhighlight, this);
            this.off('click', this.click, this);
            this.off('dblclick', this.dblclick, this);
            this.model.vertex.off('change', this.render, this);
        },

        isClickable: function() {
            return true;
        },

        updateSelection: function() {
            if (this.model.selected) {
                this.selectedColor = colors.geometry.selected;
                this.selectedAmbient = colors.geometry.selectedAmbient;
            } else {
                delete this.selectedColor;
                delete this.selectedAmbient;
            }
            this.render();
        },

        highlight: function() {
            this.highlightAmbient = colors.geometry.highlightAmbient;
            this.render();
        },

        unhighlight: function() {
            delete this.highlightAmbient;
            this.render();
        },

        click: function() {
            var vertexToSelect, parents;
            if (this.model.canSelect()) {
                if (this.model.selectParentOnClick()) {
                    var parents = _.uniq(geometryGraph.parentsOf(this.model.vertex));
                    if (parents.length === 1) {
                        vertexToSelect = parents[0];
                    }
                } else {
                    vertexToSelect = this.model.vertex;
                }
            }
            if (vertexToSelect) {
                if (event.shiftKey || event.ctrlKey || event.metaKey) {
                    selection.selectOnly(vertexToSelect.id);
                    // selection.addToSelection(vertexToSelect.id);
                } else {
                    selection.selectOnly(vertexToSelect.id);
                }

            }
        },

    });


    // ---------- Module ----------

    return {
        EditingModel     : EditingModel,
        EditingDOMView   : EditingDOMView,
        EditingSceneView : EditingSceneView,
        DisplayModel     : DisplayModel, 
        DisplayDOMView   : DisplayDOMView,
        DisplaySceneView : DisplaySceneView,
    }

});
