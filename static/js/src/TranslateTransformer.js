var SS = SS || {};


SS.TranslateTransformerInitiator = SS.TransformerInitiator.extend({

    initialize: function() { 
        SS.TransformerInitiator.prototype.initialize.call(this);
        this.translateView = new SS.TranslateTransformerView({model: this});
        this.views.push(this.translateView);
    },

    mouseDownOnTranslate: function(translateView) {
        
	var geomNode = this.attributes.geomNode;
        var editingNode = geomNode.editableCopy();
        var transform = new Transform({
            type: 'translate',
            editing: true,
	    origin: {x: parseFloat((this.center.x).toFixed(3)), 
                     y: parseFloat((this.center.y).toFixed(3)), 
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
                                     transform: transform,
                                     translateView: this.translateView});
        this.destroy();
    },

});

SS.TranslateTransformer = SS.Transformer.extend({

    initialize: function() { 
        SS.Transformer.prototype.initialize.call(this);

        this.translateView = new SS.TranslateTransformerView({model: this});
        this.views.push(this.translateView);
        SS.sceneView.replaceSceneObjectViewInMouseState(this.attributes.translateView,  this.translateView);

        this.views.push(new SS.TransformDOMView({model: this}));
        this.views.push(new SS.TranslateGeomNodeView({model: this}));

        this.translateView.on('mouseDrag', this.drag, this);
    },

    drag: function(event) {
        
        var workplanePosition = SS.sceneView.determinePositionOnWorkplane(event);
        var u = workplanePosition.x - this.attributes.transform.origin.x;
        var v = workplanePosition.y - this.attributes.transform.origin.y;

        this.attributes.transform.parameters.u = parseFloat(u.toFixed(3));        
        this.attributes.transform.parameters.v = parseFloat(v.toFixed(3));
        if (!event.ctrlKey) {
            this.attributes.transform.parameters.u = 
                Math.round(this.attributes.transform.parameters.u*10)/10;    
            this.attributes.transform.parameters.v = 
                Math.round(this.attributes.transform.parameters.v*10)/10;
        }

        this.trigger('change:model');
        this.boundingBox = SS.boundingBoxForGeomNode(this.attributes.editingNode);
        this.center = SS.transformers.centerOfGeom(this.boundingBox);

        this.trigger('change');

    },

});

SS.TranslateTransformerView = SS.ActiveTransformerView.extend({
    
    initialize: function() {
	SS.ActiveTransformerView.prototype.initialize.call(this);
        this.on('mouseDown', this.mouseDown, this);
        this.render();
    },

    mouseDown: function() {
        this.model.mouseDownOnTranslate && this.model.mouseDownOnTranslate(this);
    },

    remove: function() {
        SS.ActiveTransformerView.prototype.remove.call(this);
        this.model.off('mouseDown', this.mouseDownOnArrow);
    },

    render: function() {
        this.clear();

        var geometry = new THREE.CubeGeometry(1, 1, 1);
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

});

SS.TranslateGeomNodeView = Backbone.View.extend({

    initialize: function() {
        this.render();
        this.model.on('change:model', this.render, this);
    },

    render: function() {
        var transform = this.model.attributes.transform;
        var position = new THREE.Vector3(transform.parameters.u,
                                         transform.parameters.v,
                                         transform.parameters.w);

        // TODO: Replace with model for geom node
        SS.translateGeomNodeRendering(this.model.attributes.originalNode, 
                                      this.model.attributes.editingNode, 
                                      position);
    },

});
