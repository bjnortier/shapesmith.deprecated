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
        this.sceneObject.rotation.z = Math.atan2(uAxis.y, uAxis.x, 0);
        
        //this.sceneObject.lookAt(new THREE.Vector3(0,0,1));

        this.postRender();
    },

});

SS.WorkplaneRotationPreview = SS.InteractiveSceneView.extend({

    initialize: function() {
        SS.InteractiveSceneView.prototype.initialize.call(this);
        this.render();
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

        this.sceneObject.add(arrow);
        this.sceneObject.add(circle);
        this.sceneObject.position = new THREE.Vector3(origin.x, origin.y, origin.z);
    },

});

SS.WorkplaneURotationPreview = SS.WorkplaneRotationPreview.extend({
    
    render: function() {
        SS.WorkplaneRotationPreview.prototype.render.call(this);
        this.sceneObject.rotation.x = -Math.PI/2;
        this.sceneObject.rotation.z = -Math.PI/2;
        this.postRender();
    },

});

SS.WorkplaneVRotationPreview = SS.WorkplaneRotationPreview.extend({
    
    render: function() {
        SS.WorkplaneRotationPreview.prototype.render.call(this);
        this.sceneObject.rotation.z = Math.PI/2;
        this.sceneObject.rotation.y = Math.PI/2;
        this.postRender();
    },

});

SS.WorkplaneWRotationPreview = SS.WorkplaneRotationPreview.extend({
    
    render: function() {
        SS.WorkplaneRotationPreview.prototype.render.call(this);
        this.postRender();
    },

});