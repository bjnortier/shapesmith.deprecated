var SS = SS || {};
SS.constructors = {};
SS.preview = {};
SS.modifier = {};

SS.GeomNodeUpdater = function(geomNode) {
    var geomNode = geomNode, that = this;

    evented(this);

    this.setOrigin = function(origin) {
        geomNode.origin = origin;
        that.fire({type: 'updated'});
    }

}

SS.modifier.Origin = function(spec) {
    var geomNode = spec.geomNode, updater = spec.updater, cursoid = spec.cursoid;
    
    cursoid.on('cursoidUpdated', function(event) {
        updater.setOrigin({x: event.position.x,
                           y: event.position.y,
                           z: event.position.z});
    });

    cursoid.setPosition(geomNode.origin);
}

SS.preview.Ellipse1d = function(spec) {
    var that = this, scene = spec.scene, geomNode = spec.geomNode, updater = spec.updater;
    var cursoid = spec.cursoid;
    var sceneObject;

    var updatePreview = function() {
        if(sceneObject) {
            scene.removeObject(sceneObject);
        }
        sceneObject = new THREE.Object3D();

        var r1 = geomNode.parameters.r1;
        var r2 = geomNode.parameters.r2;

        if (r1 && r2) {

	    var ellipseGeom = new THREE.Geometry();
	    for(var i = 0; i <= 50; ++i) {
	        var theta = Math.PI*2*i/50;
	        var dx = r1*Math.cos(theta);
	        var dy = r2*Math.sin(theta);
	        ellipseGeom.vertices.push(new THREE.Vertex(new THREE.Vector3(dx, dy, 0)));
	    }
	    var ellipse = new THREE.Line(ellipseGeom, SS.constructors.lineMaterial);
            sceneObject.addChild(ellipse);
        }
    
        sceneObject.position.x = geomNode.origin.x;
        sceneObject.position.y = geomNode.origin.y;
        sceneObject.position.z = geomNode.origin.z;
    
        scene.addObject(sceneObject);
    }
    
    spec.updater.on('updated', function(event) {
        updatePreview();
    });

    updatePreview();
}

SS.preview.createEllipse1d = function(spec) {
    
    var updater = new SS.GeomNodeUpdater(spec.geomNode);
    SS.constructors.active = new SS.preview.Ellipse1d({geomNode : spec.geomNode,
                                                       scene    : sceneView.scene,
                                                       cursoid  : sceneView.cursoid,
                                                       updater  : updater});
    SS.modifier.active = new SS.modifier.Origin({geomNode : spec.geomNode,
                                                 cursoid  : sceneView.cursoid,
                                                 updater  : updater});
    
}

SS.constructors.disposeActive = function() {
    SS.constructors.active = undefined;
}
