define([
    'src/scene',
    'src/interactioncoordinator',
    'src/scenevieweventgenerator',
    'src/asyncAPI',
    ], function(
        sceneModel,
        coordinator,
        sceneViewEventGenerator,
        AsyncAPI
    ) {

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
            coordinator.on('keyup', this.cancel, this);
        },

        destroy: function() {
            Model.prototype.destroy.call(this);
            coordinator.off('keyup', this.cancel, this);

        },

        tryCommit: function(callback) {
            var that = this;
            if (this.vertex.proto) {
                AsyncAPI.tryCommitCreate([this.vertex], function(result) {
                    if (!result.error) {
                        that.destroy();
                        var newVertex = result.newVertices[0];
                        new that.displayModelConstructor(newVertex);
                        callback(true);
                    } else {
                        callback(false);
                    }
                });
            } else {
                AsyncAPI.tryCommitEdit([this.vertex], function(result) {
                    console.log('committed edit', result);
                    if (!result.error) {
                        callback(true);
                    } else {
                        callback(false);
                    }
                });
            }
        },

        tryDelete: function() {

        },

        cancel: function() {
            if (this.vertex.proto) {
                AsyncAPI.cancel(this.vertex);
            }
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
            'focusin'         : 'vertexFocusIn',
            'focusout'        : 'vertexFocusOut',
            'change .field'   : 'fieldChange',
            'keyup .field'    : 'fieldKeyUp',
            'click .delete'   : 'delete',
        },

        vertexFocusIn: function(event) {
            this.vertexFocusInTimestamp = new Date().getTime();
        },

        vertexFocusOut: function(event) {
            // If the focus is lost from the editin vertex within 100ms,
            // then the focus has been lost
            this.vertexFocusInTimestamp = 0;
            this.vertexFocusOutTimestamp = new Date().getTime();
            var that = this;
            var vertexFocusLostFn = function() {
                var diff = that.vertexFocusInTimestamp - that.vertexFocusOutTimestamp;
                if (diff < 0) {
                    that.tryCommit();
                }
            }
            setTimeout(vertexFocusLostFn, 100);
        },

        fieldChange: function(event) {
            event.stopPropagation();
            if (this.updateFromDOM) {
                this.updateFromDOM();
            }
        },

        fieldKeyUp: function(event) {
            // Return
            if (event.keyCode === 13) {
                this.model.tryCommit()
            }
            // Escape
            if (event.keyCode === 27) {
                this.model.cancel();
            } 
        },

        delete: function() {
            if (this.model.vertex.proto) {
                this.model.cancel();
            } else {
                this.model.tryDelete();
            }
        },

    });

    // ---------- Display ----------


    var DisplayDOMView = Backbone.View.extend({

        tagName: "tr",
        className: 'vertex display',

        initialize: function() {
            this.render();
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

    // ---------- Module ----------

    return {
        Model          : Model,
        SceneView      : SceneView,
        EditingModel   : EditingModel,
        EditingDOMView : EditingDOMView,
        DisplayDOMView : DisplayDOMView
    }

});