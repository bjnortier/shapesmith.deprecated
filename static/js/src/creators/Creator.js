var SS = SS || {};

SS.Creator = SS.NodeModel.extend({

    initialize: function(attributes) {
        
        this.nodeDOMView = new SS.NodeDOMView({model: this});
        this.views = [
            this.nodeDOMView,
            new SS.OkCancelView({model: this}),
        ];
        if (!attributes.noOriginCorner) {
            this.views.push(new SS.DraggableOriginCorner({model: this}));
            this.views.push(new SS.OriginDimensionText({model: this}));
        }
        
        SS.geomDoc.on('replace', this.geomDocReplace, this);
        SS.geomDoc.on('remove', this.geomDocRemove, this);
    },

    destroy: function() {
        if (this.activeCornerView) {
            this.activeCornerView.remove();
        }
        this.views.map(function(view) {
            view.remove();
        });
        SS.geomDoc.off('replace', this.geomDocReplace);
        SS.geomDoc.off('remove', this.geomDocRemove);
    },

    updateFromDOMView: function() {
        SS.NodeModel.prototype.updateFromDOMView.call(this);
        this.trigger('change');
    },

    activateCorner: function(corner, constructor, args) {
        if (this.activeCorner === corner) {
            return;
        }
        this.activeCornerView && this.activeCornerView.remove();
        this.activeCorner = corner;
        if (constructor) {
            this.activeCornerView = new constructor(args || {model: this});
        }
    },

    mouseDownOnOrigin: function(corner) {
        this.activateCorner(corner, SS.OriginHeightCursoid);
    },
     
    geomDocReplace: function(original, replacement) {
        this.destroy();
    },

    geomDocRemove: function(node) {
        this.destroy();
    },

});

SS.PrimitiveCreator = SS.Creator.extend({

    initialize: function(attributes) {
        this.node = attributes.editingNode;
        this.editingNode = attributes.editingNode;
        this.originalNode = attributes.originalNode;
        SS.Creator.prototype.initialize.call(this, attributes);

        if (!this.originalNode) {
            this.setDefaultParamters();
            this.trigger('change');
        }
    },

    tryCommit: function() {
        if (this.originalNode) {
            var cmd = update_geom_command(this.originalNode, 
                                          this.editingNode,
                                          this.editingNode);
            SS.commandStack.execute(cmd);
        } else {
            var geometry = {type: this.node.type,
                            parameters: this.node.parameters};
            if (this.node.origin) {
                geometry.origin =  this.node.origin;
            }
            if(!(this.node.workplane).isGlobalXY()) {
                geometry.workplane = this.node.workplane.serializableSubset();
            }
            var cmd = create_geom_command(this.node, geometry);
            SS.commandStack.execute(cmd);
        }
    },

    cancel: function() {
        this.destroy();
        if (this.originalNode) {
            SS.geomDoc.replace(this.editingNode, this.originalNode);
        } else {
            SS.geomDoc.remove(this.editingNode); 
        }
    },

});

SS.ParentCreator = SS.Creator.extend({

    initialize: function(attributes) {
        this.node = attributes.editingNode;
        this.childNode = attributes.editingNode.children[0];
        this.editingNode = attributes.editingNode;
        this.originalNode = attributes.originalNode;
        SS.Creator.prototype.initialize.call(this, attributes);
    },

    tryCommit: function() {
        if (this.originalNode) {
            var cmd = update_geom_command(this.originalNode, 
                                          this.editingNode,
                                          this.editingNode);
            SS.commandStack.execute(cmd);
        } else {
            var cmd = update_geom_command(this.childNode, 
                                          this.editingNode,
                                          this.editingNode);
            SS.commandStack.execute(cmd);
        }
    },

    cancel: function() {
        this.destroy();
        if (this.originalNode) {
            SS.geomDoc.replace(this.editingNode, this.originalNode);
        } else {
            SS.geomDoc.replace(this.editingNode, this.childNode); 
        }
    },
});

SS.TransformCreator = SS.Creator.extend({

    initialize: function(attributes) {
        this.node = attributes.transform;
        this.originalNode = attributes.originalNode;
        this.editingNode = attributes.editingNode;
        SS.Creator.prototype.initialize.call(this, attributes);
    },

    tryCommit: function() {
        var cmd = update_geom_command(this.attributes.originalNode, 
                                      this.editingNode,
                                      this.editingNode);
        SS.commandStack.execute(cmd);
    },

    cancel: function() {
        this.destroy();
        SS.geomDoc.replace(this.editingNode, this.originalNode); 
    },
});


SS.PreviewWithOrigin = SS.SceneObjectView.extend({

    initialize: function() {
        SS.SceneObjectView.prototype.initialize.call(this);
    },
    
    render: function() {
        var origin = this.model.node.origin;
        this.sceneObject.position = new THREE.Vector3(origin.x, origin.y, origin.z);
    },

    priority: -1,

});

SS.DraggableCorner = SS.InteractiveSceneView.extend({

    initialize: function(options) {
        SS.InteractiveSceneView.prototype.initialize.call(this);
        this.on('mouseDown', this.mouseDown);
        this.on('mouseDrag', this.drag);
    },

    remove: function() {
        SS.InteractiveSceneView.prototype.remove.call(this);
        this.model.off('mouseDown', this.mouseDown);
        this.off('mouseDrag', this.drag);
    },

    render: function() {
        this.clear();

        var width = 1.0*this.cameraScale;
        var geometry = new THREE.CubeGeometry(width, width, width);
        var materials = [
            new THREE.MeshBasicMaterial({color: SS.materials.faceColor, opacity: this.opacity, wireframe: false } ),
            new THREE.MeshBasicMaterial({color: SS.materials.lineColor, wireframe: true})
        ];
        var cube = THREE.SceneUtils.createMultiMaterialObject(geometry, materials);
        
        var position = this.cornerPositionFromModel();

        cube.position.x = position.x;
        cube.position.y = position.y;
        cube.position.z = 0;
        this.sceneObject.add(cube);
        this.postRender();
        return this;
    },

    drag: function(event) {
        var workplanePosition = SS.sceneView.determinePositionOnWorkplane(event);
        var u = workplanePosition.x;
        var v = workplanePosition.y;

        this.updateModelFromCorner(workplanePosition);
        this.model.setParameters({});
   
    },

});

SS.DraggableOriginCorner = SS.DraggableCorner.extend({

    initialize: function(options) {
        SS.DraggableCorner.prototype.initialize.call(this, options);
        this.render();
    },

    mouseDown: function() {
        this.model.mouseDownOnOrigin(this);
    },

    cornerPositionFromModel: function() {
        var origin = this.model.node.origin;
        return {x: origin.x,
                y: origin.y,
                z: origin.z};
    },
    
    updateModelFromCorner: function(position) {
        var origin = this.model.node.origin;
        origin.x = position.x;
        origin.y = position.y;
    },

});

SS.HeightCursoid = SS.InteractiveSceneView.extend({

    initialize: function(options) {
        SS.InteractiveSceneView.prototype.initialize.call(this);
        this.on('mouseDrag', this.drag);
        this.render();
    },

    remove: function() {
        SS.InteractiveSceneView.prototype.remove.call(this);
        this.off('mouseDrag', this.drag);
    },

    render: function() {
        this.clear();

        var axis = new THREE.Geometry();
        axis.vertices.push(new THREE.Vector3(0,0,-1000));
        axis.vertices.push(new THREE.Vector3(0,0,1000));
        var line = new THREE.Line(axis, new THREE.LineBasicMaterial({ color: 0xcc6666, opacity: this.opacity }));  

        var geometry = new THREE.CylinderGeometry(0, 0.75*this.cameraScale, 1.5*this.cameraScale, 3);
        var materials = [
            new THREE.MeshBasicMaterial({color: 0x993333, opacity: this.opacity, wireframe: false } ),
            new THREE.MeshBasicMaterial({color: 0xcc6666, wireframe: true})
        ];
        var cursoid = THREE.SceneUtils.createMultiMaterialObject(geometry, materials);

        var position = this.cornerPositionFromModel();

        line.position.x = position.x;
        line.position.y = position.y;

        cursoid.position.x = position.x;
        cursoid.position.y = position.y;
        cursoid.position.z = position.z + 2.5*this.cameraScale;
        cursoid.rotation.x = Math.PI/2;
        
        this.sceneObject.add(line);
        this.sceneObject.add(cursoid);
        this.postRender();
        return this;
    },

    drag: function(event) {


        var workplaneOrigin = SS.objToVector(SS.workplaneModel.node.origin);
        var workplaneAxis = SS.objToVector(SS.workplaneModel.node.axis);   
        var workplaneAngle = SS.workplaneModel.node.angle;

        var cornerPosition = this.cornerPositionFromModel();
        var rayOrigin = 
            SS.rotateAroundAxis(new THREE.Vector3(cornerPosition.x, cornerPosition.y, 0), 
                                workplaneAxis, 
                                workplaneAngle)
            .addSelf(workplaneOrigin);


        var direction = new THREE.Vector3(0, 0, 1);
        var rotatedDirection = SS.rotateAroundAxis(direction, workplaneAxis, workplaneAngle);

        var ray = new THREE.Ray(rayOrigin, rotatedDirection);
        var positionOnRay = SS.sceneView.determinePositionOnRay(event, ray);
        if (positionOnRay) {

            positionOnRay.subSelf(rayOrigin);
            var normalizedPosition = SS.rotateAroundAxis(positionOnRay, workplaneAxis, -workplaneAngle);

            var newCornerPosition = new THREE.Vector3(cornerPosition.x, 
                                                      cornerPosition.y, 
                                                      normalizedPosition.z);

            if (!event.ctrlKey) {
                newCornerPosition.z = Math.round(newCornerPosition.z);
            }
            this.updateModelFromCorner(newCornerPosition);
            this.model.setParameters({});
        }
    },

});

SS.OriginHeightCursoid = SS.HeightCursoid.extend({

    cornerPositionFromModel: function() {
        return {x: this.model.node.origin.x,
                y: this.model.node.origin.y,
                z: this.model.node.origin.z};
    },    

    updateModelFromCorner: function(position) {
        this.model.node.origin.x = position.x;
        this.model.node.origin.y = position.y;
        this.model.node.origin.z = position.z;
    },
    
});

SS.DimensionText = Backbone.View.extend({

    initialize: function(options) {
        Backbone.View.prototype.initialize.call(this);
        SS.sceneView.on('cameraChange', this.update, this);
        this.model.on('change', this.render, this);
        this.elements = [];
        this.render();
    },
    
    remove: function() {
        Backbone.View.prototype.remove.call(this);
        SS.sceneView.off('cameraChange', this.update, this);
        this.model.off('change', this.render, this);
        this.clear();
    },
    
    clear: function() {
        this.elements.map(function(element) {
            element.remove();
        });
        this.elements = [];
    },

    addElement: function(html) {
        var element = $(html);
        element.css('position', 'absolute');
        $('body').append(element);
        this.elements.push(element);
        element.mousemove(SS.sceneView.onMouseMove);
        element.mousedown(SS.sceneView.onMouseDown);
        element.mouseup(SS.sceneView.onMouseUp);
        return element;
    },

    moveToScreenCoordinates: function(element, position, leftOffset, topOffset) {
        var workplane = ((this.model.node && this.model.node.workplane) ||
                         (this.model.originalNode && this.model.originalNode.workplane));
        if (workplane) { 
            position = SS.worldPositionFromWorkplanePosition(position, workplane);
        }
        leftOffset = leftOffset || 0;
        topOffset = topOffset || 0;
        var pixelPosition = SS.toScreenCoordinates(position.clone());
        element.css('left', pixelPosition.x - element.width()/2 + leftOffset);
        element.css('top', pixelPosition.y - element.height()/2 + topOffset);
    },
    
});
                                 
SS.OriginDimensionText = SS.DimensionText.extend({

    render: function() {
        this.clear();
        var origin = this.model.node.origin;

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
        } else if (origin.z) {
                this.$xyz = this.addElement($.mustache(
                    '<div class="dimension">(<span class="z">{{z}}</span>)</div>',
                    origin));
        }
        this.update();
    },
    
    update: function() {
        if (this.$xyz) {
            var origin = this.model.node.origin;
            this.moveToScreenCoordinates(
                this.$xyz, 
                new THREE.Vector3(origin.x, origin.y, origin.z),
                -30,
                10);
        }
    },

});
