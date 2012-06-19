var SS = SS || {};

SS.NodeDisplayModel = Backbone.Model.extend({
});

SS.NodeEditorModel = Backbone.Model.extend({
});
   
SS.NodeDisplayView = Backbone.View.extend({

    initialize: function() {
        this.render();
    },

    events: {
        "click .value": "edit"
    },

});

SS.NodeEditorView = Backbone.View.extend({

    initialize: function() {
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

});

SS.WorkplaneDisplayModel = SS.NodeDisplayModel.extend({

    initialize: function(attributes) {
        this.node = SS.sceneView.workplane.node;
        this.view = new SS.WorkplaneDisplayView({model: this});
    },

    destroy: function() {
        this.view.remove();
    },

});

SS.WorkplaneEditorModel = SS.NodeEditorModel.extend({

    initialize: function(attributes) {
        this.node = new SS.WorkplaneNode();
        this.views = [
            new SS.DraggableOriginCorner({model: this}),
            new SS.OriginDimensionText({model: this}),
            new SS.WorkplaneEditorView({model: this}),
            new SS.WorkplanePreview({model: this}),
            new SS.WorkplaneURotationPreview({model: this}),
            //new SS.WorkplaneVRotationPreview({model: this}),
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

});

SS.WorkplaneDisplayView = SS.NodeDisplayView.extend({

    render: function() {
        this.$el.html(SS.renderDisplayDOM('Workplane', SS.schemas.workplane, this.model.node));
        $('#workplane').append(this.$el);
    },

    edit: function() {
        this.model.destroy();
        new SS.WorkplaneEditorModel();
    },

});

SS.WorkplaneEditorView = SS.NodeEditorView.extend({

    render: function() {
        this.$el.html(SS.renderEditingDOM('Workplane', SS.schemas.workplane, this.model.node));
        $('#workplane').append(this.$el);
    },

    ok: function() {
        console.log('ok');
    },

    cancel: function() {
        this.model.destroy();
        new SS.WorkplaneDisplayModel();
    },

    preventRecursiveUpdate: false,

    traverseSchemaAndMatchInputs: function(rootSchema, targetNode, matchFunction) {

        var view = this;
        var updateFunction = function(ancestry, schema, targetNode) {
            for (key in schema.properties) {
                var ancestryCSS = ancestry.reduce(function(acc, clazz) { return acc + ' .' + clazz; }, '');
                var possibleInput = view.$el.find(ancestryCSS + ' input.' + key);
                if (possibleInput.length == 1) {
                    var targetObject = targetNode;
                    ancestry.map(function(ancestor) {
                        targetObject = targetObject[ancestor];
                    });
                    matchFunction(schema.properties[key], possibleInput, targetObject, key);
                }
                updateFunction(ancestry.concat(key), schema.properties[key], targetNode);
            }
         }

        updateFunction([], rootSchema, targetNode);
    },

    update: function() {
        if (this.preventRecursiveUpdate) {
            return;
        }

        var matchFunction = function(schema, input, targetObject, key) {
            input.val(targetObject[key]);
        }
        this.traverseSchemaAndMatchInputs(SS.schemas.workplane, this.model.node, matchFunction);
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
        this.traverseSchemaAndMatchInputs(SS.schemas.workplane, this.model.node, matchFunction);
        this.model.trigger('change');
    },

    fieldChanged: function(x) {
        this.preventRecursiveUpdate = true;
        this.updateFromDOM();
        this.preventRecursiveUpdate = false;
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

        var planeGeometry = new THREE.PlaneGeometry(120, 120);
        var topPlane = THREE.SceneUtils.createMultiMaterialObject(planeGeometry, materials);
        var bottomPlane = THREE.SceneUtils.createMultiMaterialObject(planeGeometry, materials);
        bottomPlane.rotation.x = Math.PI;

        this.sceneObject.add(topPlane);        
        this.sceneObject.add(bottomPlane);

        var uAxis = this.model.node['u-axis'];
        var wAxis = this.model.node['w-axis'];
        this.sceneObject.rotation.z = Math.atan2(uAxis.y, uAxis.x);
        this.sceneObject.rotation.x = Math.atan2(wAxis.y, wAxis.z);
        
        //this.sceneObject.lookAt(new THREE.Vector3(0,0,1));

        this.postRender();
    },

});

SS.WorkplaneRotationPreview = SS.InteractiveSceneView.extend({

    initialize: function() {
        SS.InteractiveSceneView.prototype.initialize.call(this);
        this.on('mouseDown', this.mouseDown, this);
        this.on('mouseDrag', this.drag);

        this.render();
    },

    remove: function() {
        SS.InteractiveSceneView.prototype.remove.call(this);
        this.off('mouseDown', this.mouseDown);
        this.off('mouseDrag', this.drag);
        
    },
    
    render: function() {
        this.clear();
        SS.InteractiveSceneView.prototype.render.call(this);
        
        var origin = this.model.node.origin;

        var circleGeom = new THREE.Geometry();
        for (var i = 0; i <= 360; ++i) {
            var angle = i/180*Math.PI;
            var x = (60)*Math.cos(angle);
            var y = (60)*Math.sin(angle);
            circleGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(x,y,0)));
        }
        var circle = new THREE.Line(circleGeom, 
                                    new THREE.LineBasicMaterial({ color: SS.materials.lineColor, wireframe : true, linewidth: 1.0, opacity: 0.5 }));

        var arrowGeometry = new THREE.CylinderGeometry(0, 0.75*this.cameraScale, 1.5*this.cameraScale, 3);
        var arrowMaterials = [
            new THREE.MeshBasicMaterial({color: 0x993333, opacity: 0.5, wireframe: false } ),
            new THREE.MeshBasicMaterial({color: 0xcc6666, wireframe: true})
        ];
        var arrow = THREE.SceneUtils.createMultiMaterialObject(arrowGeometry, arrowMaterials);
        arrow.position.x = 60;

        this.circleAndArrow = new THREE.Object3D();
        this.circleAndArrow.add(arrow);
        this.circleAndArrow.add(circle);
        this.circleAndArrow.position = new THREE.Vector3(origin.x, origin.y, origin.z);
        this.sceneObject.add(this.circleAndArrow);
       
        this.rotationMesh = new THREE.Mesh(
            new THREE.PlaneGeometry(160, 160),
            new THREE.MeshBasicMaterial({ color: 0x333366, opacity: 0.5, transparent: true }));
        this.rotationPlane = new THREE.Object3D();
        this.rotationPlane.add(this.rotationMesh);


        var uAxis = this.model.node['u-axis'];
        var wAxis = this.model.node['w-axis'];
        [this.sceneObject, this.rotationPlane].map(function(obj) {
            obj.rotation.z = Math.atan2(uAxis.y, uAxis.x);
            obj.rotation.x = Math.atan2(wAxis.y, wAxis.z);
        });

        
    },

    mouseDown: function() {
        var uAxisParams = this.model.node['u-axis'];
        this.originalUAxis = new THREE.Vector3(uAxisParams.x, uAxisParams.y, uAxisParams.z);

        var wAxisParams = this.model.node['w-axis'];
        this.originalWAxis = new THREE.Vector3(wAxisParams.x, wAxisParams.y, wAxisParams.z);
    },

    drag: function(event) {
        var positionOnRotationPlane = SS.sceneView.determinePositionOnPlane(event, this.rotationPlane);
        if (!positionOnRotationPlane) {
            return;
        }

        var origin = new THREE.Vector3(this.model.node.origin.x,
                                       this.model.node.origin.y,
                                       this.model.node.origin.z);

        var v1 = new THREE.Vector3().sub(
            new THREE.Vector3(
                positionOnRotationPlane.x, 
                positionOnRotationPlane.y, 
                positionOnRotationPlane.z),
            origin).normalize();
        var v2 = new THREE.Vector3().sub(this.anchorPosition,
                                         origin).normalize();
        var v2CrossV1 = new THREE.Vector3().cross(v2, v1);

        var angle = parseFloat((Math.acos(v1.dot(v2))/Math.PI*180).toFixed(0));
        if (this.rotationAxis.dot(v2CrossV1) < 0) {
            angle = -angle;
        }
        if (!event.ctrlKey) {
            angle = Math.round(angle/5)*5;
        }
        if (this.swapAngle) {
            angle = -angle;
        }

        var that = this;
        var rotationFn = function(position) {
            var un = that.rotationAxis.x, vn = that.rotationAxis.y, wn = that.rotationAxis.z;
            var a = (un*position.x + vn*position.y + wn*position.z);
            var x2 = a*un + (position.x - a*un)*Math.cos(angle/180*Math.PI) 
                + (vn*position.z - wn*position.y)*Math.sin(angle/180*Math.PI);
            var y2 = a*vn + (position.y - a*vn)*Math.cos(angle/180*Math.PI) 
                + (wn*position.x - un*position.z)*Math.sin(angle/180*Math.PI);
            var z2 = a*wn + (position.z - a*wn)*Math.cos(angle/180*Math.PI) 
                + (un*position.y - vn*position.x)*Math.sin(angle/180*Math.PI);
            var newPosition = new THREE.Vector3(x2, y2, z2);
            return newPosition;
        }

        var newUAxis = rotationFn(this.originalUAxis);
        var newWAxis = rotationFn(this.originalWAxis);

        this.model.node['u-axis'].x = newUAxis.x;
        this.model.node['u-axis'].y = newUAxis.y;
        this.model.node['u-axis'].z = newUAxis.z;

        this.model.node['w-axis'].x = newWAxis.x;
        this.model.node['w-axis'].y = newWAxis.y;
        this.model.node['w-axis'].z = newWAxis.z;

        this.model.trigger('change');
    },


});

SS.WorkplaneURotationPreview = SS.WorkplaneRotationPreview.extend({
    
    render: function() {
        SS.WorkplaneRotationPreview.prototype.render.call(this);

        this.circleAndArrow.rotation.y = Math.PI/2;
        this.circleAndArrow.rotation.z = Math.PI/2;
       
        this.rotationMesh.rotation.z = Math.PI/2;
        this.rotationMesh.rotation.y = Math.PI/2;        

        this.postRender();
    },

    anchorPosition: new THREE.Vector3(0,60,0),
    rotationAxis: new THREE.Vector3(1,0,0),
    swapAngle: true,


});

// SS.WorkplaneVRotationPreview = SS.WorkplaneRotationPreview.extend({
    
//     render: function() {
//         SS.WorkplaneRotationPreview.prototype.render.call(this);
//         this.sceneObject.rotation.x = -Math.PI/2;
//         this.sceneObject.rotation.z = -Math.PI/2;


//         //this.rotationPlane.rotation.y = Math.PI/2;

//         //this.sceneObject.add(this.rotationPlane);
        

//         this.postRender();

//     },

//     //anchorPosition: new THREE.Vector3(0,60,0),
//     //axis: new THREE.Vector3(1,0,0),

// });

SS.WorkplaneWRotationPreview = SS.WorkplaneRotationPreview.extend({
    
    render: function() {
        SS.WorkplaneRotationPreview.prototype.render.call(this);

        //this.rotationMesh.rotation.z = Math.PI/2;

        this.postRender();

    },

    anchorPosition: new THREE.Vector3(60,0,0),
    rotationAxis: new THREE.Vector3(0,0,1),

});