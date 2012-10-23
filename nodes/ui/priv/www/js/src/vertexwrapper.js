define([
        'src/calculations',
        'src/geometrygraph', 
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

    });


    // ---------- Editing ----------

    var EditingModel = Model.extend({

        initialize: function(vertex) {
            Model.prototype.initialize.call(this, vertex);
            this.stage = 0;
            workplane.on('positionChanged', this.workplanePositionChanged, this);
            coordinator.on('keydown', this.keydown, this);
            coordinator.on('click', this.sceneClick, this);
            this.views = [];
            },

        destroy: function() {
            Model.prototype.destroy.call(this);
            workplane.off('positionChanged', this.workplanePositionChanged, this);
            coordinator.off('keydown', this.keydown, this);
            coordinator.off('click', this.sceneClick, this);
        },  

        keydown: function(event) {
            if ((event.keyCode === 13)  && this.canComplete()) {
                // Return when not in initial vertex placement
                this.ok();
                if (event.shiftKey && this.vertex.addAnotherFn) {
                    geometryGraph[this.vertex.addAnotherFn]();
                }
            } else if (event.keyCode === 27) {
                // Esc
                this.cancel();
            }

        },

        ok: function() {
            this.destroy();
            geometryGraph.removeVertex(this.vertex);
            geometryGraph.addVertex(this.vertex.cloneNonEditing());
        },

        cancel: function() {
            this.destroy();
            geometryGraph.removeVertex(this.vertex);
        },
    });

    var EditingDOMView = Backbone.View.extend({

        className: 'vertex editing',
        tagName: "tr",

        initialize: function() {
            this.render();
            $('#graph').prepend(this.$el);
            this.model.on('stageChanged', this.stageChanged, this);
            this.model.on('parametersChanged', this.updateParams, this);
        },

        remove: function() {
            Backbone.View.prototype.remove.call(this);
            this.model.off('stageChanged', this.stageChanged, this);
            this.model.off('parametersChanged', this.updateParams, this);
        },

        events: {
            'click .okcancel .ok' : 'ok',
            'click .okcancel .cancel' : 'cancel',
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
            this.model.on('stageChanged', this.render, this);
            this.model.on('parametersChanged', this.render, this);
        },

        remove: function() {
            SceneView.prototype.remove.call(this);
            this.model.off('parametersChanged', this.render, this);
            this.model.off('stageChanged', this.render, this);
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
            'click .vertex' : 'click'
        },

        click: function(event) {
            if (event.shiftKey || event.ctrlKey || event.metaKey) {
                selection.addToSelection(this.model.vertex.id);
            } else {
                selection.selectOnly(this.model.vertex.id);
            }
        },

        select: function() {
            this.$el.find('.vertex').addClass('selected');
        },

        deselect: function() {
            this.$el.find('.vertex').removeClass('selected');
        },

    });

    var DisplaySceneView = SceneView.extend({

        initialize: function() {
            this.color = this.unselectedColor;
            SceneView.prototype.initialize.call(this);
            this.model.on('selected', this.select, this);
            this.model.on('deselected', this.deselect, this);
            this.on('mouseEnter', this.highlight, this);
            this.on('mouseLeave', this.unhighlight, this);
            this.on('click', this.click, this);
        },

        remove: function() {
            SceneView.prototype.remove.call(this);
            this.model.off('selected', this.select, this);
            this.model.off('deselected', this.deselect, this);
            this.off('mouseEnter', this.highlight, this);
            this.off('mouseLeave', this.unhighlight, this);
            this.off('click', this.click, this);
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

