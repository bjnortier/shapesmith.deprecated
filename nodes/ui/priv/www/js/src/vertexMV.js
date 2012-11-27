define([
        'src/scene',
        'src/interactioncoordinator',
        'src/scenevieweventgenerator',
        'src/selection',
        'src/asyncAPI',
    ], function(
        sceneModel,
        coordinator,
        sceneViewEventGenerator,
        selection,
        AsyncAPI
    ) {

    // ---------- Common ----------

    var modelForVertex = {};

    var getModelForVertex  =function(vertex) {
        return modelForVertex[vertex.id];
    }

    var Model = Backbone.Model.extend({

        initialize: function(vertex) {
            this.vertex = vertex;
            this.views = [];
            this.selected = selection.isSelected(vertex.id);

            this.vertex.on('descendantChanged', this.descendantChanged, this);
            selection.on('selected', this.select, this);
            selection.on('deselected', this.deselect, this);

            modelForVertex[vertex.id] = this;
        },

        destroy: function() {
            this.views.forEach(function(view) {
                view.remove();
            });
            this.views = [];

            this.vertex.off('descendantChanged', this.descendantChanged, this);
            selection.off('selected', this.select, this);
            selection.off('deselected', this.deselected, this);

            delete modelForVertex[this.vertex.id];
        },  

        descendantChanged: function(descendant) {
            this.vertex.trigger('change', this.vertex);
        },

        select: function(ids, selection) {
            if (selection.indexOf(this.vertex.id) !== -1) {
                this.selected = true;
                this.trigger('updateSelection', selection);
            }
        },

        deselect: function(ids, selection) {
            if (selection.indexOf(this.vertex.id) !== -1) {
                this.selected = false;
                this.trigger('updateSelection', selection);
            }
        },

    });


    var SceneView = Backbone.View.extend({

        initialize: function() {
            this.scene = sceneModel.view.scene;
            this.rerenderOnCameraChange = false;
            this.cameraScale = new THREE.Vector3(1,1,1);
            this.updateCameraScale(sceneModel.view.camera.position);
            this.render();
            sceneModel.on('cameraChange', this.cameraChanged, this);
        },

        remove: function() {
            this.scene.remove(this.sceneObject);
            sceneViewEventGenerator.deregister(this);
            sceneModel.view.updateScene = true;

            sceneModel.off('cameraChange', this.cameraChanged, this);
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

        cameraChanged: function(cameraPosition) {
            if (this.rerenderOnCameraChange) {
                this.updateCameraScale(cameraPosition);
                this.render();
            }
        },

        updateCameraScale: function(cameraPosition) {
            var cameraDistance = cameraPosition.length();
            var newScale = cameraDistance/150;
            if (newScale !== this.cameraScale.x) {
                this.cameraScale = new THREE.Vector3(newScale, newScale, newScale);
                return true;
            } else {
                return false;
            }
        },


    });

    // ---------- Editing ----------

    var EditingModel = Model.extend({

        initialize: function(original, vertex) {
            if (!this.displayModelConstructor) {
                throw Error('no Display model constructor set');
            }
            this.originalVertex = original;
            Model.prototype.initialize.call(this, vertex);
        },

        destroy: function() {
            Model.prototype.destroy.call(this);

        },

        deselect: function(ids, selection) {
            Model.prototype.deselect.call(this, ids, selection);
            if ((selection.length > 0) && (ids.indexOf(this.vertex.id) !== -1)) {
                this.cancel();
            }
        },

        tryCommit: function(callback) {
            var that = this;
            if (this.vertex.proto) {
                AsyncAPI.tryCommitCreate([this.vertex], function(result) {
                    if (!result.error) {
                        that.destroy();
                        var newVertex = result.newVertices[0];
                        new that.displayModelConstructor(newVertex);
                        callback && callback(true);
                    } else {
                        callback && callback(false);
                    }
                });
            } else {
                AsyncAPI.tryCommitEdit([this.originalVertex], [this.vertex], function(result) {
                    if (!result.error) {
                        selection.deselectAll();
                        that.destroy();
                        new that.displayModelConstructor(that.vertex);
                        callback && callback(true);
                    } else {
                        throw Error('not implemented');
                        callback && callback(false);
                    }
                });
            }
        },

        tryDelete: function() {

        },

        cancel: function() {
            if (this.vertex.proto) {
                AsyncAPI.cancelCreate(this.vertex);
            } else {
                AsyncAPI.cancelEdit(this.originalVertex, this.vertex);
                this.destroy();
                new this.displayModelConstructor(this.originalVertex);
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
                    that.model.tryCommit();
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

    var DisplayModel = Model.extend({

        initialize: function(vertex) {
            if (!this.displayModelConstructor) {
                throw Error('no Display model constructor set');
            }
            if (!this.editingModelConstructor) {
                throw Error('no Editing model constructor set');
            }
            Model.prototype.initialize.call(this, vertex);
        },

        replaceDisplayVertex: function(original, replacement) {
            this.destroy();
            new this.displayModelConstructor(replacement);
        },

    });


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
        getModelForVertex : getModelForVertex,
        Model             : Model,
        SceneView         : SceneView,
        EditingModel      : EditingModel,
        EditingDOMView    : EditingDOMView,
        DisplayModel      : DisplayModel,
        DisplayDOMView    : DisplayDOMView
    }

});