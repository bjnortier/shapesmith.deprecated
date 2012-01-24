SS.constructors = SS.constructors || {};


SS.constructors.editTransform = function(geomNode, transform, specialisationClazz) {
    var updater = new SS.NodeUpdater(transform);
    SS.constructors.active = new SS.constructors.Constructor({geomNode : geomNode,
                                                              transform: transform,
                                                              scene    : sceneView.scene,
                                                              cursoid  : sceneView.cursoid,
                                                              specialisation : new specialisationClazz(),
                                                              updater  : updater});
}

SS.constructors.editTranslate = function(geomNode, transform) {
    SS.constructors.editTransform(geomNode, transform, SS.constructors.Translate);
}

SS.constructors.createTranslate = function(spec) {
    spec.transform.origin.x = 0;
    spec.transform.origin.y = 0;
    spec.transform.origin.z = 0;
    spec.transform.parameters.u = 10;
    spec.transform.parameters.v = 10;
    spec.transform.parameters.w = 0;
    spec.transform.parameters.n = 0;

    SS.constructors.editTranslate(spec.geomNode, spec.transform);
}

SS.constructors.editRotate = function(geomNode, transform) {
    SS.constructors.editTransform(geomNode, transform, SS.constructors.Rotate);
}

SS.constructors.createRotate = function(spec) {
    spec.transform.origin.x = 0;
    spec.transform.origin.y = 0;
    spec.transform.origin.z = 0;
    spec.transform.parameters.u = 0;
    spec.transform.parameters.v = 0;
    spec.transform.parameters.w = 10;
    spec.transform.parameters.angle = 45;
    spec.transform.parameters.n = 0;

    SS.constructors.editRotate(spec.geomNode, spec.transform);
}

SS.constructors.editScale = function(geomNode, transform) {
    SS.constructors.editTransform(geomNode, transform, SS.constructors.Scale);
}

SS.constructors.createScale = function(spec) {
    spec.transform.origin.x = 0;
    spec.transform.origin.y = 0;
    spec.transform.origin.z = 0;
    spec.transform.parameters.factor = 2.0;
    SS.constructors.editScale(spec.geomNode, spec.transform);
}

SS.constructors.editMirror = function(geomNode, transform) {
    SS.constructors.editTransform(geomNode, transform, SS.constructors.AxisMirror);
}

SS.constructors.createMirror = function(spec) {
    spec.transform.origin.x = 0;
    spec.transform.origin.y = 0;
    spec.transform.origin.z = 0;
    spec.transform.parameters.u = 0;
    spec.transform.parameters.v = 0;
    spec.transform.parameters.w = 10;
    spec.transform.parameters.n = 0;

    SS.constructors.editMirror(spec.geomNode, spec.transform);
}


SS.constructors.Translate = function() {

    this.createSceneObject = function(geomNode, transform, activeAnchor) {
        
        var sceneObjects = {};
        
        var x = transform.origin.x, y = transform.origin.y, z = transform.origin.z;
        var u = transform.parameters.u, v = transform.parameters.v, w = transform.parameters.w;

	var translatedObj = new THREE.Object3D();
        var r = parseFloat((Math.sqrt(u*u + v*v + w*w)).toFixed(3));

        if (r > 0) {
            var rDim = SS.preview.createDimArrow2(r, u, v, w);
            sceneObjects.rDim = rDim;
        }

        if (r > 0) {

            var geometries = sceneView.createGeometry(geomNode.mesh);
	    var meshes = [];
            if (geometries['3d'].vertices.length > 0) {
                meshes.push(new THREE.Mesh(geometries['3d'], SS.constructors.faceMaterial));
            }
            if (geometries['1d'].vertices.length > 0) {
                meshes.push(new THREE.Line(geometries['1d'], SS.constructors.lineMaterial));
            }

            meshes.map(function(mesh) {
                mesh.position.x = u - x;
	        mesh.position.y = v - y;
	        mesh.position.z = w - z;
	        mesh.doubleSided = true;
	        translatedObj.add(mesh);
            });
        }

        if (activeAnchor !== 'uvw') {
            var uvwAnchor = new THREE.Mesh(SS.constructors.anchorGeometry, SS.constructors.anchorMaterial);
            uvwAnchor.position.x = u;
            uvwAnchor.position.y = v;
            uvwAnchor.position.w = w;
            uvwAnchor.name = {anchor: 'uvw'};
            translatedObj.add(uvwAnchor);
	}

        sceneObjects.translated = translatedObj;
        return sceneObjects;
    }

    this.anchorPriorities = ['uvw', 'origin'];

    this.getModifierForAnchor = function(spec) {
        if (spec.anchorName === 'uvw') {
	    return new SS.modifier.UVW(spec);
	}
        return null;
    }

}

SS.constructors.Rotate = function() {

    this.createSceneObject = function(geomNode, transform, activeAnchor) {
        
        var sceneObjects = {};
        
        var x = transform.origin.x, y = transform.origin.y, z = transform.origin.z;
        var u = transform.parameters.u, v = transform.parameters.v, w = transform.parameters.w;
        var angle = transform.parameters.angle;

	var rotateObj = new THREE.Object3D();
        var r = parseFloat((Math.sqrt(u*u + v*v + w*w)).toFixed(3));

        if (r > 0) {
            var rDim = SS.preview.createDimArrow2('', u, v, w);
            sceneObjects.rDim = rDim;
        }

        if (r > 0) {

            var geometries = sceneView.createGeometry(geomNode.mesh);

            for (key in geometries) {
                var geometry = geometries[key];
                if (geometry.vertices.length > 0) {

                    var un, vn, wn;
                    var normUVW = new THREE.Vector3(u, v, w).normalize();
                    un = normUVW.x;
                    vn = normUVW.y;
                    wn = normUVW.z;

	            geometry.vertices = geometry.vertices.map(function(vertex) {
	                var position = vertex.position.clone();
                        position.x = position.x - x;
                        position.y = position.y - y;
                        position.z = position.z - z;
                        
                        // http://blog.client9.com/2007/09/rotating-point-around-vector.html
                        var a = (un*position.x + vn*position.y + wn*position.z);
                        var x2 = a*un + (position.x - a*un)*Math.cos(angle/180*Math.PI) 
                            + (vn*position.z - wn*position.y)*Math.sin(angle/180*Math.PI);
                        var y2 = a*vn + (position.y - a*vn)*Math.cos(angle/180*Math.PI) 
                            + (wn*position.x - un*position.z)*Math.sin(angle/180*Math.PI);
                        var z2 = a*wn + (position.z - a*wn)*Math.cos(angle/180*Math.PI) 
                            + (un*position.y - vn*position.x)*Math.sin(angle/180*Math.PI);
                        
	                var newPosition = new THREE.Vector3(x2, y2, z2);
	                return new THREE.Vertex(newPosition);
	            });
	            geometry.computeCentroids();
	            geometry.computeFaceNormals();
                }
            }

            var rotatedMeshObj = new THREE.Object3D();
            if (geometries['3d'].vertices.length > 0) {
                var mesh3d = new THREE.Mesh(geometries['3d'], SS.constructors.faceMaterial)
                mesh3d.doubleSided = true;
                rotatedMeshObj.add(mesh3d);
            }
            if (geometries['1d'].vertices.length > 0) {
                rotatedMeshObj.add(new THREE.Line(geometries['1d'], SS.constructors.lineMaterial));
            }
            sceneObjects.rotatedMeshObj = rotatedMeshObj;
        }

        if (angle) {
            rotateObj.add(SS.preview.createAngleArrow(angle, x,y,z, u,v,w, angle));
        }

        if (activeAnchor !== 'uvw') {
            var uvwAnchor = new THREE.Mesh(SS.constructors.anchorGeometry, SS.constructors.anchorMaterial);
            uvwAnchor.position.x = u;
            uvwAnchor.position.y = v;
            uvwAnchor.position.z = w;
            uvwAnchor.name = {anchor: 'uvw'};
            rotateObj.add(uvwAnchor);
	}

        if (activeAnchor !== 'angle') {
            var angleAnchor = new THREE.Mesh(SS.constructors.anchorGeometry, SS.constructors.anchorMaterial);
            angleAnchor.position.x = 20*Math.cos(angle/180*Math.PI);
            angleAnchor.position.y = 20*Math.sin(angle/180*Math.PI);
            angleAnchor.position.z = 0;
            angleAnchor.name = {anchor: 'angle'};
            rotateObj.add(angleAnchor);
        }

        sceneObjects.rotate = rotateObj;
        return sceneObjects;
    }

    this.anchorPriorities = ['angle', 'uvw', 'origin'];

    this.getModifierForAnchor = function(spec) {
        if (spec.anchorName === 'uvw') {
	    return new SS.modifier.UVW(spec);
	}
        if (spec.anchorName === 'angle') {
	    return new SS.modifier.Angle(spec);
	}
        return null;
    }

}

SS.constructors.Scale = function() {

    this.createSceneObject = function(geomNode, transform, activeAnchor) {
        
        var sceneObjects = {};
        
        var x = transform.origin.x, y = transform.origin.y, z = transform.origin.z;
        var factor = transform.parameters.factor;

	var scaledObj = new THREE.Object3D();
        if (factor) {

            var geometries = sceneView.createGeometry(geomNode.mesh);
            for (key in geometries) {
                var geometry = geometries[key];
                if (geometry.vertices.length > 0) {

	            geometry.vertices = geometry.vertices.map(function(vertex) {
	                var position = vertex.position.clone();
	                position.x = (position.x - x)*factor;
	                position.y = (position.y - y)*factor;
	                position.z = (position.z - z)*factor;
	                return new THREE.Vertex(position);
	            });
	            geometry.computeCentroids();
	            geometry.computeFaceNormals();
                }
            }

            if (geometries['3d'].vertices.length > 0) {
                var mesh3d = new THREE.Mesh(geometries['3d'], SS.constructors.faceMaterial)
                mesh3d.doubleSided = true;
                scaledObj.add(mesh3d);
            }
            if (geometries['1d'].vertices.length > 0) {
                scaledObj.add(new THREE.Line(geometries['1d'], SS.constructors.lineMaterial));
            }
        }

        sceneObjects.scaled = scaledObj;
        return sceneObjects;
    }

    this.anchorPriorities = ['origin'];

    this.getModifierForAnchor = function(spec) {
        return null;
    }

}


SS.constructors.AxisMirror = function() {

    this.createSceneObject = function(geomNode, transform, activeAnchor) {
        
        var sceneObjects = {};
        
        var x = transform.origin.x, y = transform.origin.y, z = transform.origin.z;
        var u = transform.parameters.u, v = transform.parameters.v, w = transform.parameters.w;
        var angle = transform.parameters.angle;

	var mirrorObj = new THREE.Object3D();
        var r = parseFloat((Math.sqrt(u*u + v*v + w*w)).toFixed(3));

        if (r > 0) {
            var rDim = SS.preview.createDimArrow2('', u, v, w);
            sceneObjects.rDim = rDim;
        }

        if (r > 0) {

            var geometries = sceneView.createGeometry(geomNode.mesh);
            for (key in geometries) {
                var geometry = geometries[key];
                if (geometry.vertices.length > 0) {

                    var un, vn, wn;
                    var normUVW = new THREE.Vector3(u, v, w).normalize();
                    un = normUVW.x;
                    vn = normUVW.y;
                    wn = normUVW.z;

	            geometry.vertices = geometry.vertices.map(function(vertex) {
	                var position = vertex.position.clone();
                        position.x = position.x - x;
                        position.y = position.y - y;
                        position.z = position.z - z;
                        
                        var a = (un*position.x + vn*position.y + wn*position.z);
                        var x2 = 2*a*un - position.x;
                        var y2 = 2*a*vn - position.y;
                        var z2 = 2*a*wn - position.z;
                        
	                var newPosition = new THREE.Vector3(x2, y2, z2);
	                return new THREE.Vertex(newPosition);
	            });
	            geometry.computeCentroids();
	            geometry.computeFaceNormals();                    
                }
                
            }

            var mirrorMeshObj = new THREE.Object3D();
            if (geometries['3d'].vertices.length > 0) {
                var mesh3d = new THREE.Mesh(geometries['3d'], SS.constructors.faceMaterial)
                mesh3d.doubleSided = true;
                mirrorMeshObj.add(mesh3d);
            }
            if (geometries['1d'].vertices.length > 0) {
                mirrorMeshObj.add(new THREE.Line(geometries['1d'], SS.constructors.lineMaterial));
            }
            sceneObjects.mirrorMeshObj = mirrorMeshObj;
        }

        if (activeAnchor !== 'uvw') {
            var uvwAnchor = new THREE.Mesh(SS.constructors.anchorGeometry, SS.constructors.anchorMaterial);
            uvwAnchor.position.x = u;
            uvwAnchor.position.y = v;
            uvwAnchor.position.z = w;
            uvwAnchor.name = {anchor: 'uvw'};
            mirrorObj.add(uvwAnchor);
	}

        sceneObjects.rotate = mirrorObj;
        return sceneObjects;
    }

    this.anchorPriorities = ['angle', 'uvw', 'origin'];

    this.getModifierForAnchor = function(spec) {
        if (spec.anchorName === 'uvw') {
	    return new SS.modifier.UVW(spec);
	}
        return null;
    }

}

