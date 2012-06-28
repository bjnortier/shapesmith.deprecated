var SS = SS || {};



SS.WorkplaneEditorModel = SS.NodeEditorModel.extend({

    initialize: function(attributes) {
        this.node = attributes.editingNode;
        this.views = [
            new SS.DraggableOriginCorner({model: this}),
            new SS.OriginDimensionText({model: this}),
            new SS.WorkplaneEditorDOMView({model: this}),
            new SS.WorkplaneEditorOkCancelView({model: this}),
            new SS.WorkplanePreview({model: this}),
            new SS.WorkplaneURotationPreview({model: this}),
            new SS.WorkplaneVRotationPreview({model: this}),
            new SS.WorkplaneWRotationPreview({model: this}),
        ];
    },

    destroy: function() {
        if (this.activeCornerView) {
            this.activeCornerView.remove();
        }
        this.views.map(function(view) {
            view.remove();
        });
    },

    schema: SS.schemas.workplane,

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

    setParameters: function(parameters) {
        this.trigger('change');
    },
    
    mouseDownOnOrigin: function(corner) {
        this.activateCorner(corner, SS.OriginHeightCursoid);
    },

    ok: function() {
        if (SS.workplaneModel.setNewNode(this.node)) {
            this.destroy();
        }
    },

    cancel: function() {
        SS.workplaneModel.cancelEditing();
        this.destroy();
    },

    xy: function() {
        this.node = new SS.WorkplaneNode();
        this.trigger('change');
    },

    yz: function() {
        this.node = new SS.WorkplaneNode();
        this.node.axis.x = this.node.axis.y = this.node.axis.z = Math.tan(Math.PI/6).toFixed(4);
        this.node.angle = 120;
        this.trigger('change');
    },

    zx: function() {
        this.node = new SS.WorkplaneNode();
        this.node.axis.x = this.node.axis.y = this.node.axis.z = -Math.tan(Math.PI/6).toFixed(4);
        this.node.angle = 120;
        this.trigger('change');
    },


});

SS.WorkplaneEditorDOMView = SS.NodeEditorDOMView.extend({

    render: function() {
        this.$el.html(SS.renderEditingDOM('Workplane', SS.schemas.workplane, this.model.node));
        $('#workplane').append(this.$el);
    },

    ok: function() {
        this.model.ok();
    },

    cancel: function() {
        this.model.cancel();
    },

    

});

SS.WorkplaneEditorOkCancelView = Backbone.View.extend({

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
        var template = '<div>{{#planes}}<input class="{{value}}" type="submit" value="{{value}}"/>{{/planes}}</div>';
        var planes = $.mustache(template, {planes: [{value: 'XY'}, 
                                                    {value: 'YZ'}, 
                                                    {value: 'ZX'}]});
        var okCancel = $.mustache(template, {planes: [{value: 'Ok'}, 
                                                    {value: 'Cancel'}]});
        this.$el.html(planes + okCancel);
        $('#floating-dom-view .ok-cancel').append(this.$el);
    },

    update: function() {

        var boundingBox = {min: {x: -this.model.node.extents.x, y: -this.model.node.extents.y, z:0},
                           max: {x: this.model.node.extents.x, y: this.model.node.extents.y, z:0}};
        var screenPosition = SS.boundingBoxToScreenPosition(boundingBox);
        $('#floating-dom-view').css('left', screenPosition.x);
        $('#floating-dom-view').css('top', screenPosition.y);
    },
    
    events: {
        'click .Ok' : 'ok',
        'click .Cancel' : 'cancel',
        'click .XY' : 'xy',
        'click .YZ' : 'yz',
        'click .ZX' : 'zx',

    },

    ok: function() {
        this.model.ok();
    },

    cancel: function() {
        this.model.cancel();
    },

    xy: function() {
        this.model.xy();
    },

    yz: function() {
        this.model.yz();
    },

    zx: function() {
        this.model.zx();
    },

});


SS.WorkplanePreview = SS.PreviewWithOrigin.extend({

    initialize: function() {
        SS.PreviewWithOrigin.prototype.initialize.call(this);
        this.render();
        this.model.on('change', this.render, this);
    },

    remove: function() {
        SS.PreviewWithOrigin.prototype.remove.call(this);
        this.model.off('change', this.render);
    },    
    
    render: function() {
        this.clear();
        SS.PreviewWithOrigin.prototype.render.call(this);
        
        var origin = this.model.node.origin;
        var materials = [ SS.materials.faceMaterial, SS.materials.wireframeMaterial ];

        var planeGeometry = new THREE.PlaneGeometry(60, 60);
        var view = this;
        [-30, 30].map(function(xpos) {
            [-30, 30].map(function(ypos) {
                var topPlane = THREE.SceneUtils.createMultiMaterialObject(planeGeometry, materials);
                var bottomPlane = THREE.SceneUtils.createMultiMaterialObject(planeGeometry, materials);
                topPlane.position.x = xpos;
                topPlane.position.y = ypos;
                topPlane.rotation.x = Math.PI/2;
                bottomPlane.rotation.x = 3*Math.PI/2;
                bottomPlane.position.x = xpos;
                bottomPlane.position.y = ypos;

                view.sceneObject.add(topPlane);        
                view.sceneObject.add(bottomPlane);
            });
        });
        
        var quaternion = new THREE.Quaternion();
        var axis = new THREE.Vector3(this.model.node.axis.x, 
                                     this.model.node.axis.y,
                                     this.model.node.axis.z);
        var angle = this.model.node.angle/180*Math.PI;
        quaternion.setFromAxisAngle(axis, angle);

        this.sceneObject.useQuaternion = true;
        this.sceneObject.quaternion = quaternion;
        this.postRender();
    },

});

SS.WorkplaneRotationPreview = SS.InteractiveSceneView.extend({

    initialize: function() {
        SS.InteractiveSceneView.prototype.initialize.call(this);
        this.on('mouseDown', this.mouseDown, this);
        this.on('mouseUp', this.mouseUp, this);
        this.on('mouseDrag', this.drag);
        this.relativeAngle = 0;
        this.angleDimensionSubview
            = new SS.RelativeAngleDimensionText({
                model: this.model, 
                parentView: this,
                color: this.textColor});
        this.render();
    },

    remove: function() {
        SS.InteractiveSceneView.prototype.remove.call(this);
        this.off('mouseDown', this.mouseDown);
        this.off('mouseUp', this.mouseUp);
        this.off('mouseDrag', this.drag);
        this.angleDimensionSubview.remove();
    },
    
    render: function() {
        this.clear();
        SS.InteractiveSceneView.prototype.render.call(this);

        var circleGeom = new THREE.Geometry();
        for (var i = 0; i <= 360 - this.relativeAngle; ++i) {
            var angle = i/180*Math.PI;
            var x = (60)*Math.cos(angle);
            var y = (60)*Math.sin(angle);
            circleGeom.vertices.push(new THREE.Vector3(x,y,0));
        }
        var circleMaterial = new THREE.LineBasicMaterial({ 
            color: this.arrowLineColor, 
            wireframe : true, 
            linewidth: 1.0, 
            opacity: this.opacity });
        var circle = new THREE.Line(circleGeom, circleMaterial);


        var arcGeom = new THREE.Geometry();
        if (this.relativeAngle !== 0) {
            var arcStartAngle = Math.min(-this.relativeAngle, 0);
            var arcEndAngle = Math.max(-this.relativeAngle, 0);

            if (arcStartAngle == -this.relativeAngle) {
                arcGeom.vertices.push(new THREE.Vector3(0,0,0))
            }
            for (var i = arcStartAngle; i <= arcEndAngle; ++i) {
                var angle = i/180*Math.PI;
                var x = (60)*Math.cos(angle);
                var y = (60)*Math.sin(angle);
                arcGeom.vertices.push(new THREE.Vector3(x,y,0));
            }
            if (arcEndAngle == -this.relativeAngle) {
                arcGeom.vertices.push(new THREE.Vector3(0,0,0));
            }
        }
        var angleArc = new THREE.Line(arcGeom, circleMaterial);
                                    
        var arrowGeometry = new THREE.CylinderGeometry(0, 0.75*this.cameraScale, 2*this.cameraScale, 3);
        var arrowMaterials = [
            new THREE.MeshBasicMaterial({color: this.arrowLineColor, opacity: this.opacity, wireframe: false } ),
            new THREE.MeshBasicMaterial({color: this.arrowFaceColor, wireframe: true})
        ];
        var arrow = THREE.SceneUtils.createMultiMaterialObject(arrowGeometry, arrowMaterials);
        arrow.position.x = 60;

        this.circleAndArrow = new THREE.Object3D();
        this.circleAndArrow.add(arrow);
        this.circleAndArrow.add(circle);
        this.circleAndArrow.add(angleArc);
        this.sceneObject.add(this.circleAndArrow);
       
        var quaternion = new THREE.Quaternion();
        
        var axis = SS.objToVector(this.model.node.axis);
        var angle = this.model.node.angle/180*Math.PI;
        quaternion.setFromAxisAngle(axis, angle);

        this.sceneObject.useQuaternion = true;
        this.sceneObject.quaternion = quaternion;

        // Update the angle dimension
        var origin = SS.objToVector(this.model.node.origin);
        this.sceneObject.position = origin;

        if (this.showDimensionAngleText) {
            this.angleDimensionSubview.angle = this.relativeAngle; 
            this.angleDimensionSubview.position = this.relativeAnchorPosition;
            this.angleDimensionSubview.render();
        } else {
            this.angleDimensionSubview.clear();
            this.angleDimensionSubview.position = undefined;
        }

    },

    mouseDown: function() {
        this.startAxis = SS.objToVector(this.model.node.axis).normalize();
        this.startAngle = this.model.node.angle;
                
        this.anchorPosition = SS.rotateAroundAxis(this.relativeAnchorPosition, this.startAxis, this.startAngle);
        this.anchorPosition.addSelf(SS.objToVector(this.model.node.origin));        
        this.rotationAxis = SS.rotateAroundAxis(this.relativeRotationAxis, this.startAxis, this.startAngle);
        this.showDimensionAngleText = true;
    },

    mouseUp: function() {
        this.relativeAngle = 0;
        this.showDimensionAngleText = false;
        this.render();
    },

    drag: function(event) {
        
        var planeOrigin = SS.objToVector(this.model.node.origin);
        var positionOnRotationPlane = 
            SS.sceneView.determinePositionOnPlane2(event, planeOrigin, this.rotationAxis);
        if (!positionOnRotationPlane) {
            return;
        }

        var v1 = new THREE.Vector3().sub(positionOnRotationPlane, planeOrigin).normalize();
        var v2 = new THREE.Vector3().sub(this.anchorPosition, planeOrigin).normalize();
        var v2CrossV1 = new THREE.Vector3().cross(v2, v1);

        var angle = parseFloat((Math.acos(v1.dot(v2))/Math.PI*180).toFixed(0));
        if (this.rotationAxis.dot(v2CrossV1) < 0) {
            angle = -angle;
        }

        if (this.previousRelativeAngle == undefined) {
            this.previousRelativeAngle = 0;
        } else {
            this.previousRelativeAngle = this.relativeAngle;
        }

        var round = function(value, tolerance) {
            return Math.round(value/tolerance)*tolerance;
        }

        this.relativeAngle = angle;
        if (!event.ctrlKey) {
            this.relativeAngle = round(this.relativeAngle, 5);
            if (this.relativeAngle === 360) {
                this.relativeAngle = 0;
            }
        }

        if (this.relativeAngle !== this.previousRelativeAngle) {
            var quat1 = new THREE.Quaternion().setFromAxisAngle(this.startAxis, this.startAngle/180*Math.PI);
            var quat2 = new THREE.Quaternion().setFromAxisAngle(this.relativeRotationAxis, Math.PI*this.relativeAngle/180);
            var quat3 = new THREE.Quaternion().multiply(quat1, quat2);
            quat3.normalize();

            var quaternionToAxisAngle = function(q) {
                var angle = 2*Math.acos(q.w);
                var axis = new THREE.Vector3(q.x/Math.sqrt(1-q.w*q.w),
                                             q.y/Math.sqrt(1-q.w*q.w),
                                             q.z/Math.sqrt(1-q.w*q.w));
                return {angle: angle/Math.PI*180, axis: axis};
            }
            var axisAngle = quaternionToAxisAngle(quat3);

            this.model.node['axis'].x = axisAngle.axis.x.toFixed(3);
            this.model.node['axis'].y = axisAngle.axis.y.toFixed(3);
            this.model.node['axis'].z = axisAngle.axis.z.toFixed(3);
            this.model.node['angle']  = axisAngle.angle.toFixed(2);
        }
        this.model.trigger('change');
    },


});

SS.WorkplaneURotationPreview = SS.WorkplaneRotationPreview.extend({
    
    render: function() {
        SS.WorkplaneRotationPreview.prototype.render.call(this);

        this.circleAndArrow.rotation.y = Math.PI/2;
        this.circleAndArrow.rotation.z = Math.PI/2;

        this.postRender();
    },

    relativeAnchorPosition: new THREE.Vector3(0,60,0),
    relativeRotationAxis: new THREE.Vector3(1,0,0),

    arrowLineColor: 0x333399,
    arrowFaceColor: 0x6666cc,
    textColor: '#6666cc',
});

SS.WorkplaneVRotationPreview = SS.WorkplaneRotationPreview.extend({
    
    render: function() {

        SS.WorkplaneRotationPreview.prototype.render.call(this);

        this.circleAndArrow.rotation.x = -Math.PI/2;
        this.circleAndArrow.rotation.z = -Math.PI/2;
       
        this.postRender();
    },

    relativeAnchorPosition: new THREE.Vector3(0,0,60),
    relativeRotationAxis: new THREE.Vector3(0,1,0),

    arrowLineColor: 0x339933,
    arrowFaceColor: 0x66cc66,
    textColor: '#339933',
});

SS.WorkplaneWRotationPreview = SS.WorkplaneRotationPreview.extend({
    
    render: function() {
        SS.WorkplaneRotationPreview.prototype.render.call(this);
        this.postRender();
    },

    relativeAnchorPosition: new THREE.Vector3(60,0,0),
    relativeRotationAxis: new THREE.Vector3(0,0,1),

    arrowLineColor: 0x993333,
    arrowFaceColor: 0xcc6666,
    textColor: '#cc6666',
});

SS.RelativeAngleDimensionText = SS.DimensionText.extend({

    position: undefined,
    angle: 0,

    initialize: function(options) {
        this.color = options.color;
        this.parentView = options.parentView;
        SS.DimensionText.prototype.initialize.call(this);
    },

    render: function() {
        this.clear();
        var label = this.angle > 180 ? -360 + this.angle : this.angle;
        this.$angle = this.addElement('<div class="dimension">' + label + '&deg;</div>');
        this.$angle.css('color', this.color);
        this.update();
    },

    update: function() {
        if (this.parentView.showDimensionAngleText && this.position) {
            var origin = SS.objToVector(this.model.node.origin);
            var axis = SS.objToVector(this.model.node.axis);
            var finalPosition = SS.rotateAroundAxis(this.position, axis, this.model.node.angle);
            finalPosition.addSelf(origin);     

            this.moveToScreenCoordinates(this.$angle, finalPosition, 20, -20);
            this.$angle.show();
        } else {
            this.$angle.hide();
        }

    }

});