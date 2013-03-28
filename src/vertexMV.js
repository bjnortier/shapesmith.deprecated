define([
        'jquery',
        'lib/jquery.autoGrowInput',
        'scene',
        'interactioncoordinator',
        'scenevieweventgenerator',
        'selection',
        'geometrygraphsingleton',
        'asyncAPI',
    ], function(
        $, __$,
        sceneModel,
        coordinator,
        sceneViewEventGenerator,
        selection,
        geometryGraph,
        AsyncAPI
    ) {

    // ---------- Common ----------

    var modelForVertex = {};
    
    var keyForVertex = function(vertex) {
        return vertex.editing ? 'editing_' + vertex.id : 'display_' + vertex.id;
    }

    var getModelForVertex = function(vertex) {
        return modelForVertex[keyForVertex(vertex)];
    }

    var getDisplayOrEditingModelForVertexWithId = function(id) {
        return modelForVertex['display_' + id] || modelForVertex['editing_' + id];
    }

    var cancelIfEditing = function() {
        _.values(modelForVertex).forEach(function(model) {
            if (model.cancel) {
                model.cancel();
            }
        });
    }

    var EventProxy = function() {
        _.extend(this, Backbone.Events);
    }

    var eventProxy = new EventProxy();

    // Replace vertex DOM view it it has one
    var replaceWithDisplay = function(original, replacement) {
        var modelToDestroy = getModelForVertex(original);
        var rowIndex = modelToDestroy.domView ? 
            modelToDestroy.domView.$el.closest('tr').prevAll().length : undefined;
        modelToDestroy.destroy();
        new modelToDestroy.displayModelConstructor({
            vertex: replacement,
            rowIndex: rowIndex,
        });
    }

    var Model = Backbone.Model.extend({

        initialize: function(options) {
            var vertex = options.vertex;

            // Add/remove must be symmetric
            if (modelForVertex[keyForVertex(vertex)]) {
                throw Error('Already a model for:', vertex.id);
            }

            this.vertex = vertex;
            this.views = [];
            this.selected = selection.isSelected(vertex.id);

            selection.on('selected', this.select, this);
            selection.on('deselected', this.deselect, this);

            modelForVertex[keyForVertex(vertex)] = this;
        },

        destroy: function() {
            // Add/remove must be symmetric
            if (!modelForVertex[keyForVertex(this.vertex)]) {
                throw Error('No model for:', keyForVertex(this.vertex));
            }

            this.views.forEach(function(view) {
                view.remove();
            });
            this.views = [];

            selection.off('selected', this.select, this);
            selection.off('deselected', this.deselect, this);

            delete modelForVertex[keyForVertex(this.vertex)];
        },  

        select: function(ids, selection) {
            if (ids.indexOf(this.vertex.id) !== -1) {
                this.selected = true;
            }
        },

        deselect: function(ids, selection) {
            if (ids.indexOf(this.vertex.id) !== -1) {
                this.selected = false;
            }
        },

        replaceWithDisplay: function(original, replacement) {
            replaceWithDisplay(original, replacement);
        },

        replaceWithEditing: function(original, replacement) {
            var modelToDestroy = getModelForVertex(original);
            var rowIndex = modelToDestroy.domView ? 
                modelToDestroy.domView.$el.closest('tr').prevAll().length : undefined;
            modelToDestroy.destroy();
            var newModel = new modelToDestroy.editingModelConstructor({
                original: original, 
                vertex:   replacement,
                rowIndex: rowIndex,
            });
        },

        replaceOrAppendInTable: function(view, tableSelector) {
            var rowIndex = view.model.attributes.rowIndex;
            if ((rowIndex !== undefined) && 
                (rowIndex < $(tableSelector + ' tr').length)) {
                $($(tableSelector + ' tr')[rowIndex]).before(view.$el);
            } else {
                $(tableSelector).append(view.$el);
            }
        },

    });


    var SceneView = Backbone.View.extend({

        initialize: function() {
            this.scene = sceneModel.view.scene;
            this.updateCameraScale();
            this.render();
            sceneModel.view.on('cameraMoveStarted', this.cameraMoveStarted, this);
            sceneModel.view.on('cameraMoved', this.cameraMoved, this);
            sceneModel.view.on('cameraMoveStopped', this.cameraMoveStopped, this);
        },

        remove: function() {
            this.scene.remove(this.sceneObject);
            sceneViewEventGenerator.deregister(this);
            this.removed = true;
            sceneModel.view.updateScene = true;
            sceneModel.view.off('cameraMoveStarted', this.cameraMoveStarted, this);
            sceneModel.view.off('cameraMoved', this.cameraMoved, this);
            sceneModel.view.off('cameraMoveStopped', this.cameraMoveStopped, this);
        },

        render: function() {
            this.clear();
        },

        clear: function() {
            // For async renders (e.g. when loading textures), don't render if the 
            // view has been removed
            if (this.removed) {
                return;
            }
            if (this.sceneObject) {
                this.scene.remove(this.sceneObject);
                sceneViewEventGenerator.deregister(this);
            }
            
            this.boundingBox = {
                min: new THREE.Vector3( Infinity, Infinity, Infinity),
                max: new THREE.Vector3( -Infinity, -Infinity, -Infinity),
            }
            var that = this;
            var updateBoundingBox = function(obj) {
                if (obj.geometry) {
                    obj.geometry.computeBoundingBox();
                    ['x', 'y', 'z'].forEach(function(dim) {
                        that.boundingBox.min[dim] = Math.min(
                            that.boundingBox.min[dim], 
                            obj.geometry.boundingBox.min[dim]);
                        that.boundingBox.max[dim] = Math.max(
                            that.boundingBox.max[dim], 
                            obj.geometry.boundingBox.max[dim]);
                        
                    })
                }
                if (obj.children && (obj.children.length > 0)) {
                    obj.children.map(updateBoundingBox);
                }
            }

            var patchedSceneObject = new THREE.Object3D();
            patchedSceneObject.add = function(child) {
                THREE.Object3D.prototype.add.call(this, child);
                updateBoundingBox(child);
                that.updateScreenBox && that.updateScreenBox(sceneModel.view.camera);
            }

            this.sceneObject = patchedSceneObject;

            // Each scene view has two objects, the one that is part of
            // the scene, and an object that is never added to the scene
            // but is only used for selections. E.g. an edge has cylinders 
            // that are used for selection
            this.hiddenSelectionObject = new THREE.Object3D();
            this.scene.add(this.sceneObject);

            sceneViewEventGenerator.register(this);
            sceneModel.view.updateScene = true;
        },

        isClickable: function() {
            return false;
        },

        isDraggable: function() {
            return false;
        },

        cameraMoveStarted: function() {
        },

        cameraMoved: function() {
            if (this.updateScaledObjects) {
                this.updateCameraScale();
                this.updateScaledObjects();
                sceneModel.view.updateScene = true;
            }
        },

        updateCameraScale: function() {
            var camera = sceneModel.view.camera;
            var cameraDistance = camera.position.length();
            var newScale = cameraDistance/150;
            this.cameraScale = new THREE.Vector3(newScale, newScale, newScale);
        },

    });

    // ---------- Editing ----------

    var EditingModel = Model.extend({

        initialize: function(options) {
            this.originalVertex = options.original;
            Model.prototype.initialize.call(this, options);
            coordinator.on('keyup', this.keyUp, this);
            coordinator.on('containerClick', this.containerClick, this);
        },

        destroy: function() {
            Model.prototype.destroy.call(this);
            coordinator.off('keyup', this.keyUp, this);
            coordinator.off('containerClick', this.containerClick, this);

        },

        // Selecting another vertex will cancel the editing
        // of this one
        deselect: function(ids, selection) {
            Model.prototype.deselect.call(this, ids, selection);
            if (ids.indexOf(this.vertex.id) !== -1) {
                this.cancel();
            }
        },

        tryCommit: function() {
            if (this.parentModel) {
                return this.parentModel.tryCommit();
            }

            var that = this;
            if (this.vertex.proto) {

                var originals = [this.vertex];

                var findImplicitChildren = function(parent) {
                    var uniqueImplicitChildren = _.uniq(geometryGraph.childrenOf(parent).filter(function(v) {
                        return v.implicit;
                    })); 
                    originals = originals.concat(uniqueImplicitChildren.concat());
                    uniqueImplicitChildren.forEach(findImplicitChildren);
                };
                findImplicitChildren(this.vertex);

                var result = AsyncAPI.tryCommitCreate(originals);
                if (!result.error) {
                    var committedVertices = result.newVertices;
                    originals.forEach(function(original, i) {
                        that.replaceWithDisplay(original, committedVertices[i]);
                    });
                    eventProxy.trigger('committedCreate', originals, committedVertices);
                    selection.deselectAll();

                }
            } else {
                var originals = [this.originalVertex];
                var editing = [this.vertex];

                if (this.originalImplicitChildren) {
                    originals = originals.concat(this.originalImplicitChildren);
                    editing = editing.concat(this.editingImplicitChildren);
                }

                var result = AsyncAPI.tryCommitEdit(originals, editing);
                if (!result.error) {
                    var committedVertices = result.newVertices;
                    editing.forEach(function(editingVertex, i) {
                        that.replaceWithDisplay(editingVertex, committedVertices[i]);
                    });
                    eventProxy.trigger('committedEdit', committedVertices);
                    selection.deselectAll();
                } 
            }
        },

        tryDelete: function() {
            // Cancel first as the editing state is unknown (i.e. parameter values may have
            // changed and SHA values are unknown
            this.cancel();
            var that = this;
            result = AsyncAPI.tryCommitDelete(this.originalVertex);
            eventProxy.trigger('committedDelete');
            selection.deselectAll();
        },

        cancel: function() {
            if (this.vertex.implicit) {
                // handled by parent
                return
            }

            if (this.vertex.proto) {
                var removeImplicitChildModels = function(parent) {
                    // Implicit hildren that aren't shared with other geometry
                    // I.e. has a parent other than the current parent
                    var uniqueImplicitChildrenWithOneParent = _.uniq(
                        geometryGraph.childrenOf(parent).filter(function(v) {
                            var parents = geometryGraph.parentsOf(v);
                            var hasOtherParent = _.find(parents, function(p) {
                                return p.id !== parent.id;
                            });
                            return v.implicit && (!hasOtherParent);
                        }));

                    uniqueImplicitChildrenWithOneParent.forEach(function(child) {
                        getModelForVertex(child).destroy();
                        removeImplicitChildModels(child);
                        AsyncAPI.cancelCreate(child);
                    });
                }
                removeImplicitChildModels(this.vertex);
                AsyncAPI.cancelCreate(this.vertex);
                eventProxy.trigger('cancelledCreate');

            } else {
                var originals = [this.originalVertex];
                var editing = [this.vertex];

                if (this.originalImplicitChildren) {
                    originals = originals.concat(this.originalImplicitChildren);
                    editing = editing.concat(this.editingImplicitChildren);
                }

                var that = this;
                AsyncAPI.cancelEdit(editing, originals);
                editing.forEach(function(editingVertex, i) {
                    that.replaceWithDisplay(editingVertex, originals[i]);
                });
                eventProxy.trigger('cancelledEdit');
            }

        },

        keyUp: function(event) {
            if (event.keyCode === 27) {
                this.cancel();
                selection.deselectAll();
            }
        },

    });

    var EditingDOMView = Backbone.View.extend({

        className: 'layer-leaf vertex editing',

        initialize: function() {
            this.model.domView = this;
            this.render();
            this.$el.addClass(this.model.vertex.id);
            this.model.vertex.on('change', this.update, this);
        },

        remove: function() {
            Backbone.View.prototype.remove.call(this);
            this.model.vertex.off('change', this.update, this);
        },

        events: {
            'click .field'    : 'fieldClick',
            'change .field'   : 'fieldChange',
            'keyup .field'    : 'fieldKeyUp',
            'click .delete'   : 'delete',
        },

        fieldClick: function(event) {
            event.stopPropagation();
            $('.field').autoGrowInput();
        },

        fieldChange: function(event) {
            event.stopPropagation();
            if (this.updateFromDOM) {
                this.updateFromDOM();
            }
        },

        fieldKeyUp: function(event) {
            event.stopPropagation();
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

        initialize: function(options) {
            Model.prototype.initialize.call(this, options);
        },

        tryDelete: function() {
            AsyncAPI.tryCommitDelete(this.vertex);
        },

    });

    var DisplayDOMView = Backbone.View.extend({

        initialize: function() {
            this.model.domView = this;
            this.render();
            this.$el.addClass(this.model.vertex.name);  
            this.model.vertex.on('change', this.update, this);
        },

        remove: function() {
            Backbone.View.prototype.remove.call(this);
            this.model.vertex.off('change', this.update, this);
        },

    });

    // ---------- Module ----------


    return {
        getModelForVertex      : getModelForVertex,
        getDisplayOrEditingModelForVertexWithId : getDisplayOrEditingModelForVertexWithId,
        cancelIfEditing        : cancelIfEditing,
        eventProxy             : eventProxy,
        replaceWithDisplay     : replaceWithDisplay,
        Model                  : Model,
        SceneView              : SceneView,
        EditingModel           : EditingModel,
        EditingDOMView         : EditingDOMView,
        DisplayModel           : DisplayModel,
        DisplayDOMView         : DisplayDOMView
    }

});