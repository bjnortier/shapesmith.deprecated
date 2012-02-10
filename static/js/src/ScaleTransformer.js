var SS = SS || {};

SS.ScaleTransformerModel = Backbone.Model.extend({

    initialize: function(attributes) { 
        var that = this;

        this.boundingBox = SS.boundingBoxForGeomNode(this.attributes.editingNode);
        this.center = SS.transformers.centerOfGeom(this.boundingBox);

        this.attributes.arrowViews.map(function(arrowView) {
            arrowView.attach(that);
            arrowView.on('mouseDrag', function(event) {
                that.drag(event);
            });
            arrowView.render();
        });

        new SS.ScaleFactorView({model: this});

    },

    drag: function(event) {
        var r1 = Math.sqrt(this.center.x*this.center.x + this.center.y*this.center.y);

        var workplanePosition = SS.sceneView.determinePositionOnWorkplane(event);
        var dxTo = workplanePosition.x - this.center.x;
        var dyTo = workplanePosition.y - this.center.y;
        var r2 = Math.sqrt(dxTo*dxTo + dyTo*dyTo);

        var factor = parseFloat((r2/r1).toFixed(3));
        if (event.shiftKey) {
            factor = Math.round(factor*10)/10;
        }
                
        var scalePoint = this.center.clone();
        scalePoint.z = 0;

        // TODO: Replace with model for geom node
        SS.scaleGeomNodeRendering(this.attributes.originalNode, 
                                  this.attributes.editingNode, 
                                  scalePoint, 
                                  factor);

        this.boundingBox = SS.boundingBoxForGeomNode(this.attributes.editingNode);

        this.set({transform: {parameters: {factor: factor}}});

    },


});

SS.ScaleFactorView = SS.SceneObjectView.extend({
    
    initialize: function() {
	SS.SceneObjectView.prototype.initialize.call(this);
        this.model.on("change", this.render, this);
    },

    render: function() {
        this.clear();
        
        var factor = this.model.attributes.transform.parameters.factor;
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
        this.postRender();
    },

    

});

