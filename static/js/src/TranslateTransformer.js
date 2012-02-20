var SS = SS || {};


SS.TranslateTransformerInitiator = SS.TransformerInitiator.extend({

    initialize: function(attributes) { 
        SS.TransformerInitiator.prototype.initialize.call(this, attributes);
        this.geomNode = attributes.geomNode;
        this.translateView = new SS.TranslateTransformerView({model: this});
        this.views.push(this.translateView);
    },

    mouseDownOnTranslate: function(translateView) {
        
        var geomNode = this.geomNode;
        var editingNode = geomNode.editableCopy();
        var transform = new Transform({
            type: 'translate',
            editing: true,
	    origin: {x: Math.round(this.center.x), 
                     y: Math.round(this.center.y), 
                     z: 0},
            parameters: {u: 0.0,
                         v: 0.0,
                         w: 0.0,
                         n: 0}
        });

        editingNode.transforms.push(transform);
        geomNode.originalSceneObjects = geomNode.sceneObjects;

        selectionManager.deselectID(geomNode.id);
        geom_doc.replace(geomNode, editingNode);

        new SS.TranslateTransformer({originalNode: geomNode,
                                     editingNode: editingNode, 
                                     transform: transform});
    },

});

SS.TranslateTransformer = SS.Transformer.extend({

    initialize: function(attributes) { 
        SS.Transformer.prototype.initialize.call(this, attributes);

        if (!attributes.editingExisting) {
            this.translateView = new SS.TranslateTransformerView({model: this});
            SS.sceneView.addToMouseOverAndMouseDown(this.translateView);
            
            this.views = this.views.concat([
                this.translateView,
                new SS.TranslateGeomNodeView({model: this}),
                new SS.ScaleBoxView({model: this}),
                new SS.ScaleFootprintView({model: this}),
            ]);
            
        }
    },

});

SS.TranslateTransformerView = SS.InteractiveSceneView.extend({
    
    initialize: function() {
	SS.InteractiveSceneView.prototype.initialize.call(this);
        this.on('mouseDown', this.mouseDown, this);
        this.on('mouseDrag', this.drag);
        this.render();
        console.log('Translate cid: ' + this.cid);
    },

    mouseDown: function() {
        this.model.mouseDownOnTranslate && this.model.mouseDownOnTranslate(this);
    },

    remove: function() {
        SS.InteractiveSceneView.prototype.remove.call(this);
        this.model.off('mouseDown', this.mouseDown);
        this.off('mouseDrag', this.drag);
    },

    render: function() {
        this.clear();

        var geometry = new THREE.CubeGeometry(0.5, 0.5, 0.5);
        var materials = [
            new THREE.MeshBasicMaterial({color: SS.constructors.faceColor, opacity: 0.5, wireframe: false } ),
            new THREE.MeshBasicMaterial({color: SS.constructors.lineColor, wireframe: true})
        ];
        var cube = THREE.SceneUtils.createMultiMaterialObject(geometry, materials);
        
	cube.position.x = this.model.center.x;
	cube.position.y = this.model.center.y;
	cube.position.z = 0;
	this.sceneObject.add(cube);

        this.postRender();
        return this;
    },

    drag: function() {
        var workplanePosition = SS.sceneView.determinePositionOnWorkplane(event);
        var u = workplanePosition.x - this.model.transform.origin.x;
        var v = workplanePosition.y - this.model.transform.origin.y;

        var parameters = {
            u: parseFloat(u.toFixed(3)),
            v: parseFloat(v.toFixed(3))
        };
        if (!event.ctrlKey) {
            parameters.u = 
                Math.round(parameters.u*10)/10;    
            parameters.v =
                Math.round(parameters.v*10)/10;
        }

        this.model.setParameters(parameters);
    },

});

SS.TranslateGeomNodeView = Backbone.View.extend({

    initialize: function() {
        this.render();
        this.model.on('change:model', this.render, this);
    },

    render: function() {
        if (this.model.originalNode.originalSceneObjects) {
            var transform = this.model.transform;
            var position = new THREE.Vector3(transform.parameters.u,
                                             transform.parameters.v,
                                             transform.parameters.w);
            
            // TODO: Replace with model for geom node
            SS.translateGeomNodeRendering(this.model.originalNode, 
                                          this.model.editingNode, 
                                          position);
        }
    },

});
