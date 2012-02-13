var SS = SS || {};

SS.ScaleTransformer = SS.Transformer.extend({

    initialize: function() { 
        SS.Transformer.prototype.initialize.call(this);

        this.anchorPosition = this.attributes.anchorFunction(this.boundingBox);

        var arrowViews = [
	    new SS.ScaleArrowViewMaxXMaxY({model: this}),
            new SS.ScaleArrowViewMaxXMinY({model: this}),
            new SS.ScaleArrowViewMinXMinY({model: this}),
            new SS.ScaleArrowViewMinXMaxY({model: this}),
	];
        for (var i = 0; i < 4; ++i) {
            SS.sceneView.replaceSceneObjectViewInMouseState(this.attributes.arrowViews[i], arrowViews[i]);
        }

        var that = this;
        arrowViews.map(function(arrowView) {
            arrowView.on('mouseDrag', that.drag, that);
        });

        var newViews = [
            new SS.ScaleGeomNodeView({model: this}),
            new SS.ScaleFactorView({model: this}),
            new SS.TransformDOMView({model: this}),
            new SS.ScaleBoxView({model: this}),
            new SS.ScaleFootprintView({model: this}),
        ];

        this.views = this.views.concat(newViews);
        this.views = this.views.concat(arrowViews);
    },

    mouseDown: function(arrowView, event) {
        this.anchorPosition = arrowView.anchorFunction(this.boundingBox);
    },

    drag: function(event) {

        var dxFrom = this.anchorPosition.x - this.attributes.transform.origin.x;
        var dyFrom = this.anchorPosition.y - this.attributes.transform.origin.y;

        var r1 = Math.sqrt(dxFrom*dxFrom + dyFrom*dyFrom);

        var workplanePosition = SS.sceneView.determinePositionOnWorkplane(event);
        var dxTo = workplanePosition.x - this.attributes.transform.origin.x;
        var dyTo = workplanePosition.y - this.attributes.transform.origin.y;
        var r2 = Math.sqrt(dxTo*dxTo + dyTo*dyTo);

        var factor = parseFloat((r2/r1).toFixed(3));
        if (!event.ctrlKey) {
            factor = Math.round(factor*10)/10;
        }

        this.attributes.transform.parameters.factor = factor;

        this.trigger('change:model');
        this.boundingBox = SS.boundingBoxForGeomNode(this.attributes.editingNode);
        this.center = SS.transformers.centerOfGeom(this.boundingBox);

        this.trigger('change');
    },

});

SS.ScaleGeomNodeView = Backbone.View.extend({

    initialize: function() {
        this.render();
        this.model.on('change:model', this.render, this);
    },

    render: function() {
        var transform = this.model.attributes.transform;
        var scalePoint = new THREE.Vector3(transform.origin.x,
                                           transform.origin.y,
                                           transform.origin.z);

        // TODO: Replace with model for geom node
        SS.scaleGeomNodeRendering(this.model.attributes.originalNode, 
                                  this.model.attributes.editingNode, 
                                  scalePoint, 
                                  this.model.attributes.transform.parameters.factor);
    },

});

SS.ScaleFactorView = SS.SceneObjectView.extend({
    
    initialize: function() {
	SS.SceneObjectView.prototype.initialize.call(this);
    },

    render: function() {
        this.clear();
        
        var factor = this.model.attributes.transform.parameters.factor;
        if (factor && (factor > 0)) {
            var textGeo = new THREE.TextGeometry('' + factor, {
	        size: 2, height: 0.01, curveSegments: 6,
	        font: 'helvetiker', weight: 'normal', style: 'normal',
	        bezelEnabled: false});
            var text = new THREE.Mesh(textGeo, 
                                      new THREE.MeshBasicMaterial({color: 0xffffff, 
                                                                   opacity: 0.8}));
            
            var boundingBox = SS.boundingBoxForGeomNode(this.model.attributes.editingNode);
            var center = SS.transformers.centerOfGeom(boundingBox);
            text.position.y = center.y - text.boundRadius/2;
            text.position.x = boundingBox.max.x + 5;
            text.rotation.z = Math.PI/2; 
            this.sceneObject.add(text);
        }

        this.postRender();
    },

    

});

