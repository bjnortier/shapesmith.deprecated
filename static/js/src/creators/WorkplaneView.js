var SS = SS || {};

SS.NodeDisplayModel = Backbone.Model.extend({
});

SS.NodeEditorModel = Backbone.Model.extend({
});

  
SS.NodeDisplayDOMView = Backbone.View.extend({

    initialize: function() {
        this.render();
        this.model.on('change', this.render, this);
    },

    remove: function() {
        Backbone.View.prototype.remove.call(this);
        this.model.off('change', this.render);
    },

    events: {
        "click .value": "edit"
    },

});

SS.WorkplaneDisplayModel = SS.NodeDisplayModel.extend({

    initialize: function(attributes) {
        this.node = new SS.WorkplaneNode();
        this.minExtents = 60;
        this.extents = 60;
        this.boundary = 50;
        this.domView = new SS.WorkplaneDisplayDOMView({model: this});
        this.views = [
            new SS.WorkplanePointerView({model: this}),
            new SS.WorkplanePointerDimensionText({model: this}),
            new SS.WorkplaneAxesSceneView({model: this}),
            new SS.WorkplaneMainGridSceneView({model: this}),
            new SS.WorkplaneFadingGridSceneView({model: this}),
            new SS.WorkplaneGlobalXYPlaneView({model: this}),
        ];
        this.rulers = this.addRulers();

        selectionManager.on('selected', this.selectionChanged, this);
        selectionManager.on('deselected', this.selectionChanged, this);
        geom_doc.on('add', this.updateExtents, this);
        geom_doc.on('remove', this.updateExtents, this);
        geom_doc.on('replace', this.updateExtents, this);
        geom_doc.on('replace', this.geomDocReplace, this);
    },

    destroy: function() {
        this.views.map(function(view) {
            view.remove();
        });
        this.rulers.map(function(ruler) {
            ruler.remove();
        });
        domView.remove();

        selectionManager.off('selected', this.selectionChanged, this);
        selectionManager.off('deselected', this.selectionChanged, this);
        geom_doc.off('add', this.updateExtents, this);
        geom_doc.off('remove', this.updateExtents, this);
        geom_doc.off('replace', this.updateExtents, this);
        geom_doc.off('replace', this.geomDocReplace, this);
    },

    addRulers: function() {
        var rulers = [];
        var spacing = 10;
        if (this.extents > 100) {
            spacing = 20;
        }
        for (var x = -this.extents/spacing; x <= this.extents/spacing; ++x) {
            rulers.push(new SS.WorkplaneRulerText({
                model: this,
                position: new THREE.Vector3(x*spacing,this.extents+5,0),
                label: x*spacing,
                axis: 'x'
            }));
        }
        for (var y = -this.extents/spacing; y <= this.extents/spacing; ++y) {
            rulers.push(new SS.WorkplaneRulerText({
                model: this,
                position: new THREE.Vector3(this.extents+5,y*spacing,0),
                label: y*spacing,
                axis: 'y'
            }));
        }
        return rulers;
    },

    persistentWorkplaneNode: undefined,

    pushNode: function(geomNode) {
        this.persistentWorkplaneNode = this.node;
        this.loadNode(geomNode.workplane);
    },

    popNode: function(geomNode) {
        if (this.persistentWorkplaneNode !== undefined) {
            this.loadNode(this.persistentWorkplaneNode);
            this.persistentWorkplaneNode = undefined;
        }
    },

    selectionChanged: function(selected) {
        if (this.persistentWorkplaneNode) {
            this.popNode();
        }
        var selected = selectionManager.getSelected();
        if (selected.length == 1) {
            this.pushNode(geom_doc.findById(selected[0]));
        } 
    },

    geomDocReplace: function(original, replacement) {
        if (replacement.isEditingOrTransformEditing()) {
            this.pushNode(replacement);
        } else {
            this.popNode();
        }
    },

    updateExtents: function() {
        var boundingBox = {min: new THREE.Vector3(), max: new THREE.Vector3()};
        geom_doc.rootNodes.map(function(rootNode) {
            var box = SS.normalizedBoundingBoxForGeomNode(rootNode);
                if (box) {
                boundingBox.min = new THREE.Vector3(
                    Math.min(boundingBox.min.x, box.min.x),
                    Math.min(boundingBox.min.y, box.min.y),
                    Math.min(boundingBox.min.z, box.min.z));
                boundingBox.max = new THREE.Vector3(
                    Math.max(boundingBox.max.x, box.max.x),
                    Math.max(boundingBox.max.y, box.max.y),
                    Math.max(boundingBox.max.z, box.max.z));
            }
        });
        var max = 0;
        max = Math.max(Math.abs(boundingBox.min.x), max)
        max = Math.max(Math.abs(boundingBox.max.x), max)
        max = Math.max(Math.abs(boundingBox.min.y), max)
        max = Math.max(Math.abs(boundingBox.max.y), max)
        var newExtents = this.minExtents;
        if (max > this.minExtents) {
            newExtents = Math.round(max/10)*10 + 10;
        } 
        if (newExtents !== this.extents) {
            this.extents = newExtents;
            this.rulers.map(function(ruler) {
                ruler.remove();
            });
            this.rulers = this.addRulers();
            this.trigger('changeExtents');
        }
    },

    setEditing: function() {
        this.domView.remove();
        this.domView = undefined;
        var editableWorkplaneNode = this.node.editableCopy();
        this.editingModel = new SS.WorkplaneEditorModel({editingNode: editableWorkplaneNode, extents: this.extents});
    },

    cancelEditing: function() {
        this.domView = new SS.WorkplaneDisplayDOMView({model: this});
        SS.UI_EDITING_STATE.editing = false;
    },

    setNewNode: function(newNode) {
        // TODO: Validation
        var validated = true;
        if (validated) {

            var oldNode = this.node;
            var model = this;
            var doFn = function() {
                model.node = newNode;
                model.trigger('change');
                if (model.domView) {
                    model.domView.remove();
                }2
                model.domView = new SS.WorkplaneDisplayDOMView({model: model});
                SS.UI_EDITING_STATE.editing = false;
                command_stack.commit()
            }

            var undoFn = function() {
                model.loadNode(oldNode);
                command_stack.success();
            }

            var redoFn = function() {
                model.loadNode(newNode);
                command_stack.success();
            }

            var cmd = new Command(doFn, undoFn, redoFn);
            command_stack.execute(cmd);

        }
        return validated;
    },

    loadNode: function(newNode) {
        if (!newNode.isEqual(this.node)) {
            this.node = newNode;
            this.trigger('change');
        }
    },

    updatePointer: function(position) {
        this.pointerPosition = position;
        this.trigger('pointerUpdated');
    },

});

SS.WorkplaneDisplayDOMView = SS.NodeDisplayDOMView.extend({

    render: function() {
        this.$el.html(SS.renderDisplayDOM('', SS.schemas.workplane, this.model.node));
        $('#workplane').append(this.$el);
    },

    edit: function() {
        if (!SS.UI_EDITING_STATE.isEditing()) {
            SS.UI_EDITING_STATE.editing = true;
            this.model.setEditing();
        }
    },

});

SS.WorkplaneDisplaySceneView = SS.SceneObjectView.extend({

    initialize: function() {
        SS.SceneObjectView.prototype.initialize.call(this);
        this.model.on('changeExtents', this.recreate, this);
        this.recreate();
    },

    remove: function() {
        SS.SceneObjectView.prototype.remove.call(this);
        this.model.off('changeExtents', this.recreate, this);
    },

    recreate: function() {
        this.create();
        this.render();
    },

    render: function() {
        var quaternion = new THREE.Quaternion();
        var axis = SS.objToVector(this.model.node.axis);
        var angle = this.model.node.angle/180*Math.PI;
        
        quaternion.setFromAxisAngle(axis, angle);
        this.sceneObject.useQuaternion = true;
        this.sceneObject.quaternion = quaternion;
        this.sceneObject.position = SS.objToVector(this.model.node.origin);
    },

    postRender: function() {
        SS.sceneView.scene.add(this.sceneObject);
        SS.sceneView.updateScene = true;
    },


});

SS.WorkplaneAxesSceneView = SS.WorkplaneDisplaySceneView.extend({

    create: function() {
        this.clear();

        var axes = [new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry(), 
                    new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry()];
        axes[0].vertices.push(new THREE.Vector3(0, 0, 0));
        axes[0].vertices.push(new THREE.Vector3(5000, 0, 0));

        axes[1].vertices.push(new THREE.Vector3(0, 0, 0));
        axes[1].vertices.push(new THREE.Vector3(0, 5000, 0));

        axes[2].vertices.push(new THREE.Vector3(0, 0, 0));
        axes[2].vertices.push(new THREE.Vector3(0, 0, 5000));

        axes[3].vertices.push(new THREE.Vector3(0, 0, 0));
        axes[3].vertices.push(new THREE.Vector3(-5000, 0, 0));

        axes[4].vertices.push(new THREE.Vector3(0, 0, 0));
        axes[4].vertices.push(new THREE.Vector3(0, -5000, 0));

        axes[5].vertices.push(new THREE.Vector3(0, 0, 0));
        axes[5].vertices.push(new THREE.Vector3(0, 0, -5000));

        this.sceneObject.add(new THREE.Line(axes[0], 
            new THREE.LineBasicMaterial({ color: SS.colors.xAxis, opacity: 0.5 }))); 
        this.sceneObject.add(new THREE.Line(axes[1], 
            new THREE.LineBasicMaterial({ color: SS.colors.yAxis, opacity: 0.5 })));
        this.sceneObject.add(new THREE.Line(axes[2], 
            new THREE.LineBasicMaterial({ color: SS.colors.zAxis, opacity: 0.5 })));
        this.sceneObject.add(new THREE.Line(axes[3], 
            new THREE.LineBasicMaterial({ color: SS.colors.xAxis, opacity: 0.2 })));
        this.sceneObject.add(new THREE.Line(axes[4], 
            new THREE.LineBasicMaterial({ color: SS.colors.yAxis, opacity: 0.2 })));
        this.sceneObject.add(new THREE.Line(axes[5], 
            new THREE.LineBasicMaterial({ color: SS.colors.zAxis, opacity: 0.2 })));
        this.postRender();
    },

});

SS.WorkplanePointerView = SS.WorkplaneDisplaySceneView.extend({

    initialize: function() {
        SS.WorkplaneDisplaySceneView.prototype.initialize.call(this);
        this.model.on('pointerUpdated', this.render, this);
    },

    remove: function() {
        SS.WorkplaneDisplaySceneView.prototype.remove.call(this);
        this.model.on('pointerUpdated', this.render, this);
    },

    create: function() {
        // do nothing
    },

    render: function() {
        this.clear();
        if (!SS.UI_EDITING_STATE.isEditing()) {
            var pointerMaterial = new THREE.MeshBasicMaterial( { color: 0xffff00, opacity: 0.7, wireframe: false } );
            var pointerGeometry = new THREE.CubeGeometry(0.5, 0.5, 0.5);
            this.pointer = new THREE.Mesh(pointerGeometry, pointerMaterial); 
            this.sceneObject.add(this.pointer);
            
            if (this.model.pointerPosition) {
                this.pointer.position = this.model.pointerPosition;
            }
            SS.WorkplaneDisplaySceneView.prototype.render.call(this);
        }
        this.postRender();
    },

});

SS.WorkplanePointerDimensionText = SS.DimensionText.extend({

    initialize: function() {
        SS.DimensionText.prototype.initialize.call(this);
        this.model.on('pointerUpdated', this.render, this);
        SS.UI_EDITING_STATE.on('change', this.render, this);
    },

    remove: function() {
        SS.DimensionText.prototype.remove.call(this);
        this.model.on('pointerUpdated', this.render, this);
        SS.UI_EDITING_STATE.off('change', this.render, this);
    },

    render: function() {
        this.clear();
        if (this.model.pointerPosition && !SS.UI_EDITING_STATE.isEditing()) {
            var origin = this.model.pointerPosition;

            if (origin.x || origin.y) {
                if (origin.z) {
                    this.$xyz = this.addElement($.mustache(
                        '<div class="dimension">(<span class="x">{{x}}</span>,' + 
                                                '<span class="y">{{y}}</span>,' +
                                                '<span class="z">{{z}}</span>)</div>',
                        origin));
                } else {
                    this.$xyz = this.addElement($.mustache(
                        '<div class="dimension">(<span class="x">{{x}}</span>,' + 
                                                '<span class="y">{{y}}</span>)</div>',
                        origin));
                }
            }
            this.update();
        } 
    },
    
    update: function() {
        if (this.$xyz) {
            if (this.model.pointerPosition) {
                var position = SS.worldPositionFromWorkplanePosition(
                    this.model.pointerPosition, this.model.node);

                this.moveToScreenCoordinates(
                    this.$xyz, 
                    position,
                    -30,
                    10);
            } 
            if (!this.model.pointerPosition || SS.UI_EDITING_STATE.isEditing()) {
                this.$xyz.hide();
            } 
        } 
    },

});


SS.WorkplaneMainGridSceneView = SS.WorkplaneDisplaySceneView.extend({

    create: function() {
        this.clear();

        for (var x = -this.model.extents; x <= this.model.extents; ++x) {
            if (x != 0) {
                var gridLineGeometry = new THREE.Geometry();
                gridLineGeometry.vertices.push(new THREE.Vector3(x, -this.model.extents, 0));
                gridLineGeometry.vertices.push(new THREE.Vector3(x, this.model.extents, 0));

                var material = (x % 10 == 0) ? SS.materials.gridMajor : SS.materials.gridMinor;
                var line = new THREE.Line(gridLineGeometry, material);
                this.sceneObject.add(line);
            }
        }

        for (var y = -this.model.extents; y <= this.model.extents; ++y) {
            if (y != 0) {
                var gridLineGeometry = new THREE.Geometry();
                gridLineGeometry.vertices.push(new THREE.Vector3(-this.model.extents, y, 0));
                gridLineGeometry.vertices.push(new THREE.Vector3(this.model.extents, y, 0));

                var material = (y % 10 == 0) ? SS.materials.gridMajor : SS.materials.gridMinor;
                var line = new THREE.Line(gridLineGeometry, material);
                this.sceneObject.add(line);
            }
        }        

        this.postRender();
    },

});

SS.WorkplaneFadingGridSceneView = SS.WorkplaneDisplaySceneView.extend({

    create: function() {
        this.clear();


        for(var x = -this.model.extents - this.model.boundary; x <= this.model.extents + this.model.boundary; ++x) {
            for(var y = -this.model.extents - this.model.boundary; y <= this.model.extents + this.model.boundary; ++y) {

                var inside = this.isInsideX(x) && this.isInsideY(y);
                if ((x % 10 == 0) && (y % 10 == 0)
                    && (x != 0) && (y != 0) 
                    && !inside) {
                    this.addFadingGridTile(x,y);
                }
            }
        }

        this.postRender();
    },

    isInsideX: function(x) {
        return ((x >= -this.model.extents) && (x <= this.model.extents));
    }
    ,
    isInsideY: function(y) {
        return ((y >= -this.model.extents) && (y <= this.model.extents));
    },

    addFadingGridTile:function(x,y) {
    
        var fadingGridLineGeometry = new THREE.Geometry();
        fadingGridLineGeometry.vertices.push(new THREE.Vector3(0, 0, 0));
        fadingGridLineGeometry.vertices.push(new THREE.Vector3(10, 0, 0));

        var dx = 0;
        if (x < -this.model.extents) {
            dx = -this.model.extents - x;
        } else if (x > this.model.extents) {
            dx = x - this.model.extents;
        }
        var dy = 0;
        if (y < -this.model.extents) {
            dy = -this.model.extents - y;
        } else if (y > this.model.extents) {
            dy = y - this.model.extents;
        }

        var r = Math.sqrt(dx*dx + dy*dy);
        var opacity = (r == 0) ? 1.0 : 1.0/(r*0.9);
        var material = new THREE.LineBasicMaterial({ color: SS.colors.fadingGrid, opacity: opacity });

        var line = new THREE.Line(fadingGridLineGeometry, material);
        line.position.x = x > 0 ? (x-10) : x;
        line.position.y = y;
        this.sceneObject.add(line);

        var line = new THREE.Line(fadingGridLineGeometry, material);
        line.position.x = x;
        line.position.y = y > 0 ? (y-10) : y;
        line.rotation.z = 90 * Math.PI / 180;
        this.sceneObject.add(line);
       
    },

});


SS.WorkplaneGlobalXYPlaneView = SS.SceneObjectView.extend({

    initialize: function() {
        SS.SceneObjectView.prototype.initialize.call(this);
        this.render();
    },

    remove: function() {
        SS.SceneObjectView.prototype.remove.call(this);
    },    

    render: function() {
        this.clear();
        if (!this.model.node.isGlobalXY()) {
            var origin = this.model.node.origin;
            var materials = [ SS.materials.wireframeMaterial ];
            var planeGeometry = new THREE.PlaneGeometry(this.model.extents*2, this.model.extents*2);
            var plane = THREE.SceneUtils.createMultiMaterialObject(planeGeometry, SS.materials.globalXYPlane);
            plane.rotation.x = Math.PI/2;
            this.sceneObject.add(plane);        
        }
        this.postRender();
    },

});

SS.WorkplaneRulerText = SS.DimensionText.extend({

    render: function() {
        this.clear();
        this.$xy = this.addElement($.mustache(
                '<div class="dimension ruler {{axis}}">{{label}}</div>',
                this.options));
        this.update();
    },

    updateCheckpoint: 0,
    hidden: false,

    delayedUpdate: function(triggerCheckpoint) {
        var view = this;
        setTimeout(function() {
            if (triggerCheckpoint === view.updateCheckpoint) {
                var origin = SS.objToVector(view.model.node.origin);
                var axis = SS.objToVector(view.model.node.axis);
                var rotatedPosition = SS.rotateAroundAxis(view.options.position, axis, view.model.node.angle);
                var finalPosition = new THREE.Vector3().add(rotatedPosition, origin);

                view.moveToScreenCoordinates(view.$xy, finalPosition);
                view.$xy.fadeIn(100);
                view.hidden = false;
            }
        }, 500);
    },
    
    update: function() {
        this.updateCheckpoint += 1;
        if (!this.hidden) {
            this.$xy.fadeOut(100);
        }
        var checkpoint = this.updateCheckpoint;
        this.delayedUpdate(checkpoint);
        
    },

});


