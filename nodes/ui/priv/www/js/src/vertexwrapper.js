define([
        'src/calculations',
        'src/geometrygraphsingleton', 
        'src/interactioncoordinator', 
        'src/scenevieweventgenerator',
        'src/selection',
        'src/scene',
        'src/workplane',
    ], 
    function(calc, geometryGraph, coordinator, sceneViewEventGenerator, selection, sceneModel, workplane) {

    // ---------- Common ----------
    
    var Model = Backbone.Model.extend({

        initialize: function(vertex) {
            this.vertex = vertex;
        },

        destroy: function() {
            this.views.forEach(function(view) {
                view.remove();
            });
            this.views = [];
        },  

    });
    
    var SceneView = Backbone.View.extend({

        initialize: function() {
            this.scene = sceneModel.view.scene;
            this.render();
        },

        remove: function() {
            this.scene.remove(this.sceneObject);
            sceneViewEventGenerator.deregister(this);
            sceneModel.view.updateScene = true;
        },

        render: function() {
            if (this.sceneObject) {
                this.scene.remove(this.sceneObject);
                sceneViewEventGenerator.deregister(this);
            }
            // Each scene view has two objects, the one that is part of
            // the scene, and an object that is never added to the scene
            // but is only used for selections. E.g. an edge has cylinders 
            // that are used for selection
            this.sceneObject = new THREE.Object3D();
            this.hiddenSelectionObject = new THREE.Object3D();
            this.scene.add(this.sceneObject);

            sceneViewEventGenerator.register(this);
            sceneModel.view.updateScene = true;
        },

        isDraggable: function() {
            return false;
        },

    });


    // ---------- Editing ----------

    var EditingModel = Model.extend({

        initialize: function(vertex) {
            Model.prototype.initialize.call(this, vertex);
            this.set('stage', vertex.proto ? 0 : undefined);
            workplane.on('positionChanged', this.workplanePositionChanged, this);
            coordinator.on('keydown', this.keydown, this);
            workplane.on('click', this.workplaneClick, this);
            sceneViewEventGenerator.on('sceneViewClick', this.sceneViewClick, this);
            this.views = [];
        },

        destroy: function() {
            Model.prototype.destroy.call(this);
            workplane.off('positionChanged', this.workplanePositionChanged, this);
            coordinator.off('keydown', this.keydown, this);
            workplane.off('click', this.workplaneClick, this);
            sceneViewEventGenerator.off('sceneViewClick', this.sceneViewClick, this);
        },  

        ok: function() {
            geometryGraph.commit(this.vertex);
        },

        cancel: function() {
            geometryGraph.cancel(this.vertex);
        },

    });

    var EditingDOMView = Backbone.View.extend({

        className: 'vertex editing',
        tagName: "tr",

        initialize: function() {
            this.render();
            $('#graph').prepend(this.$el);
            this.model.on('change:stage', this.stageChanged, this);
            this.model.on('parametersChanged', this.updateParams, this);
        },

        remove: function() {
            Backbone.View.prototype.remove.call(this);
            this.model.off('change:stage', this.stageChanged, this);
            this.model.off('parametersChanged', this.updateParams, this);
        },

        events: function() {
            return {
                'click .okcancel .ok' : 'ok',
                'click .okcancel .cancel' : 'cancel',
            }
        },

        stageChanged: function(stage) {
            if (this.model.canComplete()) {
                this.$el.find('.ok').removeClass('disabled');
            } else {
                this.$el.find('.ok').addClass('disabled');
            }
        },

        ok: function() {
            if (this.model.canComplete()) {
                this.model.ok();
            }
        },

        cancel: function() {
            this.model.cancel();
        },

    });

    var EditingSceneView = SceneView.extend({

        initialize: function() {
            this.color = 0x94dcfc;
            SceneView.prototype.initialize.call(this);
            this.model.on('change:stage', this.render, this);
            this.model.on('parametersChanged', this.render, this);
        },

        remove: function() {
            SceneView.prototype.remove.call(this);
            this.model.off('parametersChanged', this.render, this);
            this.model.off('change:stage', this.render, this);
        },

    });

    // ---------- Display ----------

    var DisplayModel = Model.extend({ 

        initialize: function(vertex) {
            Model.prototype.initialize.call(this, vertex);
            this.selected = false;
            selection.on('selected', this.select, this);
            selection.on('deselected', this.deselect, this);
            this.views = [];
        },

        destroy: function() {
            Model.prototype.destroy.call(this);
            selection.off('selected', this.selected, this);
            selection.off('deselected', this.deselected, this);
        },  

        select: function(ids) {
            if (ids.indexOf(this.vertex.id) !== -1) {
                this.selected = true;
                this.trigger('selected');
            }
        },

        deselect: function(ids) {
            if (ids.indexOf(this.vertex.id) !== -1) {
                this.selected = false;
                this.trigger('deselected');
            }
        },

        editIfOnlySelection: function() {
            if ((selection.selected.length === 1) 
                &&
                (selection.selected[0] === this.vertex.id)) {
                geometryGraph.edit(this.vertex);
            }
        },

    });

    var DisplayDOMView = Backbone.View.extend({

        tagName: "tr",

        initialize: function() {
            this.render();
            $('#graph').prepend(this.$el);
            this.model.on('selected', this.select, this);
            this.model.on('deselected', this.deselect, this);
        },

        remove: function() {
            Backbone.View.prototype.remove.call(this);
            this.model.off('selected', this.select, this);
            this.model.off('deselected', this.deselect, this);
        },

        events: {
            'click .vertex'    : 'click',
            'dblclick .vertex' : 'dblclick',
        },

        click: function(event) {
            if (!geometryGraph.isEditing()) {
                if (event.shiftKey || event.ctrlKey || event.metaKey) {
                    selection.addToSelection(this.model.vertex.id);
                } else {
                    selection.selectOnly(this.model.vertex.id);
                }
            }
        },

        dblclick: function(event) {
            this.model.editIfOnlySelection();
        },

        select: function() {
            this.$el.find('.vertex').addClass('selected');
        },

        deselect: function() {
            this.$el.find('.vertex').removeClass('selected');
        },

    });

    var DisplaySceneView = SceneView.extend({

        clickable: true,   

        initialize: function() {
            this.color = this.unselectedColor;
            SceneView.prototype.initialize.call(this);
            this.model.on('selected', this.select, this);
            this.model.on('deselected', this.deselect, this);
            this.on('mouseEnter', this.highlight, this);
            this.on('mouseLeave', this.unhighlight, this);
            this.on('click', this.click, this);
            this.on('dblclick', this.dblclick, this);
        },

        remove: function() {
            SceneView.prototype.remove.call(this);
            this.model.off('selected', this.select, this);
            this.model.off('deselected', this.deselect, this);
            this.off('mouseEnter', this.highlight, this);
            this.off('mouseLeave', this.unhighlight, this);
            this.off('click', this.click, this);
            this.off('dblclick', this.dblclick, this);
        },

        select: function() {
            this.selectedColor = 0xf4f653;
            this.selectedAmbient = 0x333333;
            this.render();
        },

        deselect: function() {
            delete this.selectedColor;
            delete this.selectedAmbient;
            this.render();
        },

        highlight: function() {
            this.highlightAmbient = 0xffffff;
            this.render();
        },

        unhighlight: function() {
            delete this.highlightAmbient;
            this.render();
        },

        click: function() {
            if (event.shiftKey || event.ctrlKey || event.metaKey) {
                selection.addToSelection(this.model.vertex.id);
            } else {
                selection.selectOnly(this.model.vertex.id);
            }
        },

        dblclick: function() {
            this.model.editIfOnlySelection();
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

