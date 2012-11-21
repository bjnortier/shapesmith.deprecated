define([
    'src/geometrygraphsingleton', 
    'src/interactioncoordinator',
    'src/scene',
    'src/scenevieweventgenerator',
    ], 
    function(geometryGraph, coordinator, sceneModel, sceneViewEventGenerator) {

    // ---------- Common ----------
    
    var Model = Backbone.Model.extend({

        initialize: function(vertex) {
            this.vertex = vertex;
            this.views = [];
            this.vertex.on('descendantChanged', this.descendantChanged, this);
        },

        destroy: function() {
            this.views.forEach(function(view) {
                view.remove();
            });
            this.views = [];
            this.vertex.off('descendantChanged', this.descendantChanged, this);

        },  

        descendantChanged: function(descendant) {
            this.vertex.trigger('change', this.vertex);
        },

    });

    var DOMView = Backbone.View.extend({

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

        okCreate: function() {
            geometryGraph.commitCreate(this.vertex);
        },

        okEdit: function() {
            geometryGraph.commitEdit();
        },

        cancel: function() {
            geometryGraph.cancel(this.vertex);
        },

    });

    var EditingDOMView = Backbone.View.extend({

        tagName: 'tr',
        className: 'vertex editing',

        initialize: function() {
            this.render();
            this.$el.addClass(this.model.vertex.id);
            this.model.vertex.on('change', this.update, this);
        },

        remove: function() {
            Backbone.View.prototype.remove.call(this);
            this.model.vertex.off('change', this.update, this);
        },

        events: {
            'focusin .field'  : 'fieldFocusIn',
            'focusout .field' : 'fieldFocusOut',
            'change .field'   : 'fieldChange',
            // 'keyup .field'    : 'fieldChange',
        },

        fieldFocusIn: function(event) {
            coordinator.setFieldFocus(true);
        },

        fieldFocusOut: function(event) {
            coordinator.setFieldFocus(false);
        },

        fieldChange: function(event) {
            this.updateFromDOM();
            this.model.vertex.trigger('change', this.model.vertex);
        },

    });


    // ---------- Display ---------

    var DisplayDOMView = Backbone.View.extend({

        tagName: "tr",
        className: 'vertex display',

        initialize: function() {
            this.render();
            this.$el.append('<td><div class="delete"></div></td>');
            this.$el.addClass(this.model.vertex.name);  
            this.model.on('updateSelection', this.updateSelection, this);
            this.model.vertex.on('change', this.update, this);
        },

        remove: function() {
            Backbone.View.prototype.remove.call(this);
            this.model.off('updateSelection', this.updateSelection, this);
            this.model.vertex.off('change', this.update, this);
        },

    });

    return {
        Model: Model,
        SceneView: SceneView,
        EditingModel: EditingModel,
        EditingDOMView: EditingDOMView,
        DisplayDOMView: DisplayDOMView,

    }
    

});

