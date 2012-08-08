var SS = SS || {};

SS.NodeModel = Backbone.Model.extend({

    setParameters: function(parameters) {
        for (var key in parameters) {
            this.node.parameters[key] = parameters[key];
        }

        this.trigger('beforeChange');
        if (this.editingNode.sceneObjects) {
            this.boundingBox = SS.boundingBoxForGeomNode(this.editingNode);
            this.normalizedBoundingBox = SS.normalizedBoundingBoxForGeomNode(this.editingNode);
            this.normalizedCenter = SS.centerOfGeom(this.normalizedBoundingBox);
        }

        this.trigger('change');
    },

});

SS.NodeEditorDOMView = Backbone.View.extend({

    initialize: function() {
        Backbone.View.prototype.initialize.call(this);
        this.render();
        this.update();
        this.model.on('change', this.update, this);
    },

    remove: function() {
        Backbone.View.prototype.remove.call(this);
        this.model.off('change', this.update);
    },

    events: {
        'click .ok' : 'ok',
        'click .cancel' : 'cancel',
        'change .field': 'fieldChanged',
        'keyup .field': 'fieldChanged',
        'click .field': 'fieldChanged',
    },

    preventRecursiveUpdate: false,

    traverseSchemaAndMatchInputs: function(rootSchema, targetNode, matchFunction) {

        var view = this;
        var updateFunction = function(ancestry, schema, targetNode) {
            if (!schema.properties) {
                return;
            }
            _.keys(schema.properties).map(function(key) {
                var ancestryCSS = '.' + ancestry.join('_');
                if (schema.properties[key].type !== 'array') {
                    var selector = ancestryCSS + ' .field.' + key;
                    var possibleInput = view.$el.find(selector);
                    if (possibleInput.length == 1) {
                        var targetObject = targetNode;
                        ancestry.map(function(ancestor) {
                            targetObject = targetObject[ancestor];
                        });
                        if (targetObject) {
                            // When remove a vertex from a polyline, the targetObject can be removed
                            matchFunction(schema.properties[key], possibleInput, targetObject, key);
                        }
                    }
                    updateFunction(ancestry.concat(key), schema.properties[key], targetNode);
                } else {
                    var i = 0;
                    while(true) {
                        var selector = ancestryCSS + '_' + key + '_' + i;  
                        var possibleInput = view.$el.find(selector);
                        if (possibleInput.length === 0) {
                            break;
                        } else {
                            updateFunction(ancestry.concat(key).concat('' + i), schema.properties[key]['items'], targetNode);
                        }
                        ++i;
                    }
                }
                
            });
         }

        updateFunction([this.model.node.type], rootSchema, targetNode);
    },

    update: function() {
        if (this.preventRecursiveUpdate) {
            return;
        }

        var matchFunction = function(schema, input, targetObject, key) {
            input.val(targetObject[key]);
        }
        rootObject = {};
        rootObject[this.model.node.type] = this.model.node;
        this.traverseSchemaAndMatchInputs(
            SS.schemas[this.model.node.type], 
            rootObject,
            matchFunction);
    }, 

    updateFromDOM: function() {
        var matchFunction = function(schema, input, targetObject, key) {
            var val =  input.val();
            switch(schema.type) {
            case "number":
                val = parseFloat(val);
                break;
            case "integer":
                val = parseInt(val);
                break;
            }
            targetObject[key] = val;
        }
        rootObject = {};
        rootObject[this.model.node.type] = this.model.node,
        this.traverseSchemaAndMatchInputs(
            SS.schemas[this.model.node.type], 
            rootObject,
            matchFunction);
        this.model.setParameters({});
    },

    fieldChanged: function(x) {
        this.preventRecursiveUpdate = true;
        this.updateFromDOM();
        this.preventRecursiveUpdate = false;
    },

});

SS.NodeDOMView = SS.NodeEditorDOMView.extend({

    render: function() {
        var schema = SS.schemas[this.model.node.type];
        this.$el.html(SS.renderEditingDOM(this.model.node.type, schema, this.model.node));
        $('#editing-area').append(this.$el);
        return this;
    },

    ok: function() {
        this.model.tryCommit();
    },

    cancel: function() {
        this.model.cancel();
    },
    
});


SS.SceneObjectView = Backbone.View.extend({
    
    initialize: function() {
        this.sceneObject = new THREE.Object3D(); 
        SS.sceneView.registerSceneObjectView(this);
        this.model.on('change', this.render, this);
    },

    remove: function() {
        SS.sceneView.deregisterSceneObjectView(this);
        SS.sceneView.scene.remove(this.sceneObject);
        this.model.off('change', this.render);
    },

    clear: function() {
        SS.sceneView.scene.remove(this.sceneObject);
        this.sceneObject = new THREE.Object3D(); 
    },

    postRender: function() {
        // Workplane of geometry node
        var workplane = ((this.model.node && this.model.node.workplane) || 
                         (this.model.originalNode && this.model.originalNode.workplane)); 

        
        if  (workplane && !this.dontApplyWorkplane) {
            this.sceneObject.useQuaternion = true;
            this.sceneObject.quaternion = new THREE.Quaternion();

            var quaternion = new THREE.Quaternion();
            var axis = SS.objToVector(workplane.axis);
            var angle = workplane.angle/180*Math.PI;
            quaternion.setFromAxisAngle(axis, angle);
            this.sceneObject.quaternion = quaternion;
            this.sceneObject.position = new THREE.Vector3().add(
                SS.rotateAroundAxis(
                    this.sceneObject.position, 
                    axis, 
                    workplane.angle),
                SS.objToVector(workplane.origin));

            // Rotation transformer
            if (this.model.node && this.model.node.type === 'rotate') {
                var transform = this.model.node;
                var quaternion = new THREE.Quaternion();
                var axis = SS.objToVector(transform.parameters);
                var angle = transform.parameters.angle/180*Math.PI;
                quaternion.setFromAxisAngle(axis, angle);

                this.sceneObject.quaternion = new THREE.Quaternion().multiply(
                    this.sceneObject.quaternion, quaternion);

            }
        }
        
        SS.sceneView.scene.add(this.sceneObject);
        SS.sceneView.updateScene = true;
    },

    priority: 0,

});


SS.InteractiveSceneView = SS.SceneObjectView.extend({

    initialize: function() {
        SS.SceneObjectView.prototype.initialize.call(this);
        this.on('mouseEnter', this.enterHighlight);
        this.on('mouseLeave', this.leaveHighlight);
        this.on('mouseDown', this.downHighlight);
        this.on('mouseUp', this.upHighlight);

        this.updateCameraScale();
        SS.sceneView.on('cameraChange', this.cameraChange, this);
    },

    remove: function() {
        SS.SceneObjectView.prototype.remove.call(this);
        SS.sceneView.off('cameraChange', this.cameraChange, this);
        this.off('mouseEnter', this.enterHighlight);
        this.off('mouseLeave', this.leaveHighlight);
        this.off('mouseDown', this.downHighlight);
        this.off('mouseUp', this.upHighlight);
    },

    priority: 1,
    active: true,
    cameraScale: 1,

    recursiveHighlightFn:  function(object, opacity) {
        var functor = function(object) {
            if (object.material) {
                object.material.opacity = opacity;
            }
            if (object.children) {
                object.children.map(functor);
            }
        }
        functor(object);
    },

    highlighted: false,
    mouseIsDown: false,
    opacity: 0.5,

    enterHighlight: function() {
        if (this.active) {
            this.recursiveHighlightFn(this.sceneObject, 1.0);
            this.highlighted = true;
            this.opacity = 1.0;
        }
    },

    leaveHighlight: function() {
        if (!this.mouseIsDown) {
            this.upHighlight();
        }
    },

    downHighlight: function() {
        this.mouseIsDown = true;
    },

    upHighlight:function() {
        if (this.highlighted) {
            this.recursiveHighlightFn(this.sceneObject, 0.5);
            this.highlighted = false;
            this.opacity = 0.5;
        }
        this.mouseIsDown = false;
    },

    updateCameraScale: function() {
        var cameraDistance = SS.sceneView.camera.position.length();
        var newScale = cameraDistance/150;
        if (newScale !== this.cameraScale) {
            this.cameraScale = newScale;
            return true;
        } else {
            return false;
        }
    },

    cameraChange: function() {
        if (this.updateCameraScale()) {
            this.render();
        }
    },
 
});

SS.OkCancelView = Backbone.View.extend({

    initialize: function() {
        this.render();
        this.model.on('change', this.update, this);
        SS.sceneView.on('cameraChange', this.update, this);
        this.update();
    },

    remove: function() {
        Backbone.View.prototype.remove.call(this);
        this.model.off('change', this.update);
        SS.sceneView.off('cameraChange', this.update);
    },

    render: function() {
        var table = '<table><tr><td><input class="ok" type="submit" value="Ok"/><input class="cancel" type="submit" value="Cancel"/></td></tr></table>';
        this.$el.html(table);
        $('#floating-dom-view .ok-cancel').append(this.$el);
    },

    update: function() {

        var pixelPosition = {
            x: window.innerWidth/2 + 10,
            y: window.innerHeight/2
        };
        var boundingBox = this.model.getBoundingBox();
        // Nested transforms don't have bounding boxes
        if (boundingBox) {
            if (this.model.node.workplane) {
                boundingBox.min = SS.worldPositionFromWorkplanePosition(
                    boundingBox.min, this.model.node.workplane);
                boundingBox.max = SS.worldPositionFromWorkplanePosition(
                    boundingBox.max, this.model.node.workplane);
            }


            var projScreenMat = new THREE.Matrix4();
            projScreenMat.multiply(SS.sceneView.camera.projectionMatrix, 
                                   SS.sceneView.camera.matrixWorldInverse);

            var xminmax = [boundingBox.min.x, boundingBox.max.x];
            var yminmax = [boundingBox.min.y, boundingBox.max.y];
            var zminmax = [boundingBox.min.z, boundingBox.max.z];
            var screenPositionMinMax = [new THREE.Vector3(2,2,0), new THREE.Vector3(-2,-2,0)];
            for (var i = 0; i < 2; i++) {
                for (var j = 0; j < 2; ++j) {
                    for (var k = 0; k < 2; ++k) {
                        var pos = new THREE.Vector3(xminmax[i], yminmax[j], zminmax[k]);
                        projScreenMat.multiplyVector3(pos);

                        screenPositionMinMax[0].x = Math.min(screenPositionMinMax[0].x, pos.x);
                        screenPositionMinMax[0].y = Math.min(screenPositionMinMax[0].y, pos.y);
                        screenPositionMinMax[1].x = Math.max(screenPositionMinMax[1].x, pos.x);
                        screenPositionMinMax[1].y = Math.max(screenPositionMinMax[1].y, pos.y);
                    }
                }
            }

            var centerYPosition = (screenPositionMinMax[0].y + screenPositionMinMax[1].y)/2;
            
            pixelPosition.x = window.innerWidth * ((screenPositionMinMax[1].x+1)/2) + 20;
            pixelPosition.y = window.innerHeight * ((-centerYPosition+1)/2) + 10;
            pixelPosition.x = Math.min(pixelPosition.x, window.innerWidth - 100);
            pixelPosition.y = Math.min(pixelPosition.y, window.innerHeight - 100);
            pixelPosition.x = Math.max(pixelPosition.x, 100);
            pixelPosition.y = Math.max(pixelPosition.y, 100);
        }
       
        $('#floating-dom-view').css('left', pixelPosition.x);
        $('#floating-dom-view').css('top', pixelPosition.y);
    },
    
    events: {
        'click .ok' : 'ok',
        'click .cancel' : 'cancel',
    },

    ok: function() {
        this.model.tryCommit();
    },

    cancel: function() {
        this.model.cancel();
    },

});
