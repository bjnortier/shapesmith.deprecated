var SS = SS || {};
SS.constructors = {};


SS.constructors.createEllipse1d = function(spec) {
    spec.scene = sceneView.scene;
    spec.cursoid = sceneView.cursoid;

    SS.constructors.active = new SS.constructors.Ellipse1D(spec);
}

SS.constructors.disposeActive = function() {
    SS.constructors.active = undefined;
}

SS.constructors.Base = function(spec) {
    var that = this;
    if (spec) {
        this.geomNode = spec.geomNode;
        this.scene    = spec.scene;
        this.cursoid  = spec.cursoid;
        
        spec.cursoid.on('cursoidUpdated', function(event) {
            that.updatePreview();
        });
    }
};

SS.constructors.Ellipse1D = function(spec) {
    SS.constructors.Base.call(this, spec);
};

SS.constructors.Ellipse1D.prototype = new SS.constructors.Base;
SS.constructors.Ellipse1D.prototype.constructor = SS.constructors.Ellipse1D;

SS.constructors.Ellipse1D.prototype.updatePreview = function() {

    for (key in this.geomNode.origin) {
        this.geomNode.origin[key] = parseFloat($('#' + key).val());
    }
    for (key in this.geomNode.parameters) {
        this.geomNode.parameters[key] = parseFloat($('#' + key).val());
    }

    var preview = new THREE.Object3D();

    var r1 = this.geomNode.parameters.r1;
    var r2 = this.geomNode.parameters.r2;
    if (r1 && r2) {

	var ellipseGeom = new THREE.Geometry();
	for(var i = 0; i <= 50; ++i) {
	    var theta = Math.PI*2*i/50;
	    var dx = r1*Math.cos(theta);
	    var dy = r2*Math.sin(theta);
	    ellipseGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
	}
	var ellipse = new THREE.Line(ellipseGeom, SS.constructors.lineMaterial);
        preview.addChild(ellipse);
    }
    
    preview.position.x = this.geomNode.origin.x;
    preview.position.y = this.geomNode.origin.y;
    preview.position.z = this.geomNode.origin.z;
    
    this.scene.addObject(preview);
};
