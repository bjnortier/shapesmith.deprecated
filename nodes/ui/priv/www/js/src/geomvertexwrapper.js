define([
    'src/colors',
    'src/calculations',
    'src/geometrygraphsingleton', 
    'src/interactioncoordinator', 
    'src/scenevieweventgenerator',
    'src/selection',
    'src/scene',
    'src/workplaneMV',
    'src/vertexwrapper',
    ], function(
        colors,
        calc,
        geometryGraph,
        coordinator,
        sceneViewEventGenerator,
        selection,
        sceneModel,
        Workplane,
        vertexWrapper) {

    var lastIndex = {}

    var addToTable = function(vertex, el) {
        var placeholderSelector = '._' + vertex.id;
        if (vertex.implicit && ($(placeholderSelector).length > 0)) {
            // el.replaceAll(placeholderSelector);
        } else {
            var vertexId = vertex.id;
            var lastVertexRowIndex = lastIndex[vertexId];
            if ((lastVertexRowIndex !== undefined) && 
                ($('#geometry tr').length > lastVertexRowIndex)) {
                $($('#geometry tr')[lastIndex[vertexId]]).before(el);
            } else {
                $('#geometry').append(el);
            }
        }
    }

    var saveRowIndex = function(vertexId, el) {
        var rowIndex = el.closest('tr').prevAll().length;
        lastIndex[vertexId] = rowIndex;
    }

    // ---------- Editing ----------

    var EditingModel = vertexWrapper.EditingModel.extend({

        initialize: function(vertex) {
            this.currentWorkplaneModel = Workplane.getCurrent();
            vertexWrapper.EditingModel.prototype.initialize.call(this, vertex);
            this.currentWorkplaneModel.on('positionChanged', this.workplanePositionChanged, this);
            this.currentWorkplaneModel.on('click', this.workplaneClick, this);
            this.currentWorkplaneModel.on('dblclick', this.workplaneDblClick, this);
            sceneViewEventGenerator.on('sceneViewClick', this.sceneViewClick, this);
            sceneViewEventGenerator.on('sceneViewDblClick', this.sceneViewDblClick, this);
        },

        destroy: function() {
            vertexWrapper.EditingModel.prototype.destroy.call(this);
            this.currentWorkplaneModel.off('positionChanged', this.workplanePositionChanged, this);
            this.currentWorkplaneModel.off('click', this.workplaneClick, this);
            this.currentWorkplaneModel.off('dblclick', this.workplaneDblClick, this);
            sceneViewEventGenerator.off('sceneViewClick', this.sceneViewClick, this);
            sceneViewEventGenerator.off('sceneViewDblClick', this.sceneViewDblClick, this);
        },  

    });

    var EditingDOMView = vertexWrapper.EditingDOMView.extend({


        initialize: function() {
            vertexWrapper.EditingDOMView.prototype.initialize.call(this);
            if (this.model.appendElement) {
                this.model.appendElement.append(this.$el);
            } else {
                addToTable(this.model.vertex, this.$el);
            }
            $('.field').autoGrowInput();
        },

        remove: function() {
            saveRowIndex(this.model.vertex.id, this.$el);
            vertexWrapper.EditingDOMView.prototype.remove.call(this);
        },

    });

    var EditingSceneView = vertexWrapper.SceneView.extend({

        initialize: function() {
            this.color = colors.geometry.editing;
            vertexWrapper.SceneView.prototype.initialize.call(this);
            this.model.vertex.on('change', this.render, this);
        },

        remove: function() {
            vertexWrapper.SceneView.prototype.remove.call(this);
            this.model.vertex.off('change', this.render, this);
        },

    });

    // ---------- Display ----------

    var DisplayModel = vertexWrapper.Model.extend({ 

        initialize: function(vertex) {
            vertexWrapper.Model.prototype.initialize.call(this, vertex);
            this.selected = selection.isSelected(vertex.id);
            selection.on('selected', this.select, this);
            selection.on('deselected', this.deselect, this);
        },

        destroy: function() {
            vertexWrapper.Model.prototype.destroy.call(this);
            selection.off('selected', this.selected, this);
            selection.off('deselected', this.deselected, this);
        },  

        canSelect: function() {
            return true;
        },

        selectParentOnClick: function() {
            return false;
        },

        select: function(ids) {
            if (ids.indexOf(this.vertex.id) !== -1) {
                this.selected = true;
                this.trigger('updateSelection');
            }
        },

        deselect: function(ids) {
            if (ids.indexOf(this.vertex.id) !== -1) {
                this.selected = false;
                this.trigger('updateSelection');
            }
        },

    });

    var DisplayDOMView = vertexWrapper.DisplayDOMView.extend({

        tagName: "tr",
        className: 'vertex display',

        initialize: function() {
            vertexWrapper.DisplayDOMView.prototype.initialize.call(this);
            if (this.model.appendElement) {
                this.model.appendElement.append(this.$el);
            } else {
                addToTable(this.model.vertex, this.$el);
            }
            this.$el.addClass(this.model.vertex.name);  
            this.updateSelection();
        },

        remove: function() {
            saveRowIndex(this.model.vertex.id, this.$el);
            vertexWrapper.EditingDOMView.prototype.remove.call(this);
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
            'click .title .name' : 'clickTitle',
            'click .title img' : 'clickTitle',
            'click .delete' : 'delete',
        },

        clickTitle: function(event) {
            if (this.model.canSelect()) {
                if (event.shiftKey || event.ctrlKey || event.metaKey) {
                    selection.addToSelection(this.model.vertex.id);
                } else {
                    selection.selectOnly(this.model.vertex.id);
                }
            }
        },

        delete: function() {
            geometryGraph.commitDelete(this.model.vertex);
        },

        updateSelection: function() {
            if (this.model.selected) {
                this.$el.addClass('selected');
            } else {
                this.$el.removeClass('selected');
            }
        },

    });

    var DisplaySceneView =  vertexWrapper.SceneView.extend({

        clickable: true,   

        initialize: function() {
            this.color = colors.geometry.default;
            vertexWrapper.SceneView.prototype.initialize.call(this);
            this.model.on('updateSelection', this.updateSelection, this);
            this.on('mouseEnter', this.highlight, this);
            this.on('mouseLeave', this.unhighlight, this);
            this.on('click', this.click, this);
            this.on('dblclick', this.dblclick, this);
            this.model.vertex.on('change', this.render, this);
            this.updateSelection();
        },

        remove: function() {
            vertexWrapper.SceneView.prototype.remove.call(this);
            this.model.off('updateSelection', this.updateSelection, this);
            this.off('mouseEnter', this.highlight, this);
            this.off('mouseLeave', this.unhighlight, this);
            this.off('click', this.click, this);
            this.off('dblclick', this.dblclick, this);
            this.model.vertex.off('change', this.render, this);
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
                vertexToSelect = this.model.vertex;
            } else if (this.model.selectParentOnClick()) {
                parents = geometryGraph.parentsOf(this.model.vertex);
                if (parents.length === 1) {
                    vertexToSelect = parents[0];
                }
            }
            if (vertexToSelect) {
                if (event.shiftKey || event.ctrlKey || event.metaKey) {
                    selection.addToSelection(vertexToSelect.id);
                } else {
                    selection.selectOnly(vertexToSelect.id);
                }

            }
        },

    });

    return {
        EditingModel     : EditingModel,
        EditingDOMView   : EditingDOMView,
        EditingSceneView : EditingSceneView,
        DisplayModel     : DisplayModel,
        DisplayDOMView   : DisplayDOMView,
        DisplaySceneView : DisplaySceneView,
    }

});