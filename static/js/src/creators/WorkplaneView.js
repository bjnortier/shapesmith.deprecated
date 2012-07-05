var SS = SS || {};

SS.NodeDisplayModel = Backbone.Model.extend({
});

SS.NodeEditorModel = Backbone.Model.extend({
});

  
SS.NodeDisplayDOMView = Backbone.View.extend({

    initialize: function() {
        this.render();
    },

    events: {
        "click .value": "edit"
    },

});

SS.WorkplaneDisplayModel = SS.NodeDisplayModel.extend({

    initialize: function(attributes) {
        this.node = new SS.WorkplaneNode();
        this.domView = new SS.WorkplaneDisplayDOMView({model: this});
        this.views = [
            new SS.WorkplaneAxesSceneView({model: this}),
            new SS.WorkplaneMainGridSceneView({model: this}),
            //new SS.WorkplaneFadingGridSceneView({model: this}),
            new SS.WorkplaneGlobalXYPlaneView({model: this}),
        ];
        this.views.concat(this.addRulers());
    },

    addRulers: function() {
        var rulers = [];
        for (var x = -this.node.extents.x/10; x <= this.node.extents.x/10; ++x) {
            rulers.push(new SS.WorkplaneRulerText({model: this,
                                                  position: new THREE.Vector3(x*10,this.node.extents.y+5,0),
                                                  label: x*10,
                                                  axis: 'x'}));
        }
        for (var y = -this.node.extents.y/10; y <= this.node.extents.y/10; ++y) {
            rulers.push(new SS.WorkplaneRulerText({model: this,
                                                  position: new THREE.Vector3(this.node.extents.x+5,y*10,0),
                                                  label: y*10,
                                                  axis: 'y'}));
        }
        return rulers;
    },

    destroy: function() {
        this.view.remove();
        this.permanentViews.concat(this.domView).map(function(view) {
            view.remove();
        });
    },

    setEditing: function() {
        this.domView.remove();
        this.domView = undefined;
        var editableWorkplaneNode = this.node.editableCopy();
        this.editingModel = new SS.WorkplaneEditorModel({editingNode: editableWorkplaneNode});
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
        this.node = newNode;
        this.trigger('change');
    },

});

SS.WorkplaneDisplayDOMView = SS.NodeDisplayDOMView.extend({

    render: function() {
        this.$el.html(SS.renderDisplayDOM('workplane', SS.schemas.workplane, this.model.node));
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
        this.model.on('change', this.update, this);
        this.update();
    },

    update:function() {
        this.extents = this.model.node.extents;
        this.minX = -this.extents.x;
        this.minY = -this.extents.y;
        this.maxX = this.extents.x;
        this.maxY = this.extents.y;
        this.boundary = this.model.node.boundary;
        this.render();
    },

    remove: function() {
        SS.SceneObjectView.prototype.remove.call(this);
        this.model.off('change', this.render);
    },

    postRender: function() {
        var quaternion = new THREE.Quaternion();
        var axis = new THREE.Vector3(this.model.node.axis.x, 
                                     this.model.node.axis.y,
                                     this.model.node.axis.z);
        var angle = this.model.node.angle/180*Math.PI;
        quaternion.setFromAxisAngle(axis, angle);

        this.sceneObject.useQuaternion = true;
        this.sceneObject.quaternion = quaternion;

        this.sceneObject.position = SS.objToVector(this.model.node.origin);

        SS.SceneObjectView.prototype.postRender.call(this);
    },


});

SS.WorkplaneAxesSceneView = SS.WorkplaneDisplaySceneView.extend({

    render: function() {
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

SS.WorkplaneMainGridSceneView = SS.WorkplaneDisplaySceneView.extend({

    render: function() {
        this.clear();

        for (var x = this.minX; x <= this.maxX; ++x) {
            if (x != 0) {
                var gridLineGeometry = new THREE.Geometry();
                gridLineGeometry.vertices.push(new THREE.Vector3(x, this.minY, 0));
                gridLineGeometry.vertices.push(new THREE.Vector3(x, this.maxY, 0));

                var material = (x % 10 == 0) ? SS.materials.gridMajor : SS.materials.gridMinor;
                var line = new THREE.Line(gridLineGeometry, material);
                this.sceneObject.add(line);
            }
        }

        for (var y = this.minY; y <= this.maxY; ++y) {
            if (y != 0) {
                var gridLineGeometry = new THREE.Geometry();
                gridLineGeometry.vertices.push(new THREE.Vector3(this.minX, y, 0));
                gridLineGeometry.vertices.push(new THREE.Vector3(this.maxX, y, 0));

                var material = (y % 10 == 0) ? SS.materials.gridMajor : SS.materials.gridMinor;
                var line = new THREE.Line(gridLineGeometry, material);
                this.sceneObject.add(line);
            }
        }        

        this.postRender();
    },

});

SS.WorkplaneFadingGridSceneView = SS.WorkplaneDisplaySceneView.extend({

    render: function() {
        this.clear();


        for(var x = this.minX - this.boundary; x <= this.maxX + this.boundary; ++x) {
            for(var y = this.minY - this.boundary; y <= this.maxY + this.boundary; ++y) {

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
        return ((x >= this.minX) && (x <= this.maxX));
    }
    ,
    isInsideY: function(y) {
        return ((y >= this.minY) && (y <= this.maxY));
    },

    addFadingGridTile:function(x,y) {
    
        var fadingGridLineGeometry = new THREE.Geometry();
        fadingGridLineGeometry.vertices.push(new THREE.Vector3(0, 0, 0));
        fadingGridLineGeometry.vertices.push(new THREE.Vector3(10, 0, 0));

        var dx = 0;
        if (x < this.minX) {
            dx = this.minX - x;
        } else if (x > this.maxX) {
            dx = x - this.maxX;
        }
        var dy = 0;
        if (y < this.minY) {
            dy = this.minY - y;
        } else if (y > this.maxY) {
            dy = y - this.maxY;
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
            var planeGeometry = new THREE.PlaneGeometry(this.model.node.extents.x*2, this.model.node.extents.y*2);
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


