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
            origin: {x: Math.round(this.normalizedCenter.x), 
                     y: Math.round(this.normalizedCenter.y), 
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
                new SS.TranslateHeightCursoid({model: this}),
                new SS.TranslateGeomNodeView({model: this}),
                new SS.TranslateDimensionArrows({model: this}),
                new SS.TranslateDimensionText({model: this}),
            ]);
            
        }
    },

});

SS.TranslateTransformerView = SS.InteractiveSceneView.extend({
    
    initialize: function() {
        SS.InteractiveSceneView.prototype.initialize.call(this);
        this.on('mouseDown', this.mouseDown, this);
        this.on('mouseDrag', this.drag);
        this.model.on('change', this.render, this);
        this.render();
    },

    remove: function() {
        SS.InteractiveSceneView.prototype.remove.call(this);
        this.model.off('mouseDown', this.mouseDown);
        this.off('mouseDrag', this.drag);
        this.model.off('change', this.render, this);
    },

    mouseDown: function() {
        this.model.mouseDownOnTranslate && this.model.mouseDownOnTranslate(this);
    },


    render: function() {
        this.clear();

        var width = this.cameraScale*1.0;
        var geometry = new THREE.CubeGeometry(width, width, width);
        var materials = [
            new THREE.MeshBasicMaterial({color: SS.materials.faceColor, opacity: 0.5, wireframe: false } ),
            new THREE.MeshBasicMaterial({color: SS.materials.lineColor, wireframe: true})
        ];
        var cube = THREE.SceneUtils.createMultiMaterialObject(geometry, materials);
        
        cube.position.x = this.model.normalizedCenter.x; 
        cube.position.y = this.model.normalizedCenter.y;
        cube.position.z = 0;
        this.sceneObject.add(cube);

        this.postRender();
        return this;
    },

    drag: function(event) {
        var workplanePosition = SS.sceneView.determinePositionOnWorkplane(event);
        var u = workplanePosition.x - this.model.node.origin.x;
        var v = workplanePosition.y - this.model.node.origin.y;

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
        Backbone.View.prototype.initialize.call(this);
        this.render();
        this.model.on('beforeChange', this.render, this);
    },

    remove: function() {
        Backbone.View.prototype.remove.call(this);
        this.model.off('beforeChange', this.render, this);
    },

    render: function() {
        if (this.model.originalNode.originalSceneObjects) {
            var transform = this.model.node;
            var position = SS.objToVector(transform.parameters);
            SS.translateGeomNodeRendering(this.model.originalNode, 
                                          this.model.editingNode, 
                                          position);
        }
    },

});


SS.TranslateHeightCursoid = SS.HeightCursoid.extend({

    initialize: function(options) {
        SS.HeightCursoid.prototype.initialize.call(this);
        this.render();
    },
    
    cornerPositionFromModel: function() {
        return {x: this.model.normalizedCenter.x,
                y: this.model.normalizedCenter.y,
                z: this.model.node.parameters.w};
    },    

    updateModelFromCorner: function(position) {
        this.model.node.parameters.w = position.z - this.model.node.origin.z;
    },

});

SS.TranslateDimensionArrows = SS.DimensionArrowsView.extend({

    render: function() {
        this.clear();

        var origin = this.model.node.origin;
        var u = this.model.node.parameters.u;
        var v = this.model.node.parameters.v;
        var w = this.model.node.parameters.w;

        if (u) {
            var uDim = this.createDimArrow(u, new THREE.Vector3(u,0,0));
            this.sceneObject.add(uDim);
        }
        if (v) {
            var vDim = this.createDimArrow(v, new THREE.Vector3(0,v,0));
            vDim.position = new THREE.Vector3(u,0,0);
            this.sceneObject.add(vDim);
        }
        if (w) {
            var wDim = this.createDimArrow(v, new THREE.Vector3(0,0,w));
            wDim.position = new THREE.Vector3(u,v,0);
            this.sceneObject.add(wDim);
        }

        this.sceneObject.position = new THREE.Vector3(origin.x, origin.y, origin.z);
        this.postRender();
    },

});

SS.TranslateDimensionText = SS.DimensionText.extend({

    render: function() {
        this.clear();

        var origin = this.model.node.origin;
        var u = this.model.node.parameters.u;
        var v = this.model.node.parameters.v;
        var w = this.model.node.parameters.w;

        if (u) {
            this.$u = this.addElement('<div class="dimension">' + u + '</div>');
        }
        if (v) {
            this.$v = this.addElement('<div class="dimension">' + v + '</div>');
        }
        if (w) {
            this.$w = this.addElement('<div class="dimension">' + w + '</div>');
        }

        this.update();
    },

    update: function() {
        var origin = this.model.node.origin;
        var u = this.model.node.parameters.u;
        var v = this.model.node.parameters.v;
        var w = this.model.node.parameters.w;
      
        var positions = {u: new THREE.Vector3(origin.x + u/2, origin.y, origin.z),
                         v: new THREE.Vector3(origin.x + u, origin.y + v/2, origin.z),
                         w: new THREE.Vector3(origin.x + u, origin.y + v, origin.z + w/2)};
        for (key in positions) {
            if (this['$' + key]) {
                this.moveToScreenCoordinates(this['$' + key], positions[key]);
            }
        }
    },

});
