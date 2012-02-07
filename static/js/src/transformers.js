var SS = SS || {};
SS.transformers = {};

SS.transformers.centerOfGeom = function(boundingBox) {
    return new THREE.Vector3().add(boundingBox.min, 
                                   new THREE.Vector3().sub(boundingBox.max, boundingBox.min).divideScalar(2));
}

SS.transformers.TranslateElement = function(geomNode) {
    
    var boundingBox = SS.boundingBoxForGeomNode(geomNode);
    var width = boundingBox.max.x - boundingBox.min.x;
    var depth = boundingBox.max.y - boundingBox.min.y;
    var height = boundingBox.max.z - boundingBox.min.z;

    var planeGeometry = new THREE.PlaneGeometry(width, depth); 
    var planeMesh = new THREE.Mesh(planeGeometry, 
                                   new THREE.MeshBasicMaterial({color: SS.constructors.faceColor, 
                                                                transparent: true, 
                                                                opacity: 0.5}));
    var rotationPlane;
    var rotationIndicator;

    planeMesh.doubleSided = true;
    planeMesh.position.x = boundingBox.min.x + width/2;
    planeMesh.position.y = boundingBox.min.y + depth/2;
    planeMesh.position.z = -0.05;
    planeMesh.name = {transformerElement: 'translate-XY'};
    
    this.sceneObject = planeMesh;

    this.highlight = function() {
        this.sceneObject.material.opacity = 1.0;
    }

    this.unHighlight = function() {
        this.sceneObject.material.opacity = 0.5;
    }

    this.cursor = 'move';
}

SS.transformers.ScaleElement = function(geomNode, elementName) {
    
    var boundingBox = SS.boundingBoxForGeomNode(geomNode);

    var arrowGeometry = new THREE.Geometry();
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(2, -1.5, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(2, -0.5, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(3, -0.5, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(3, -1.5, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(5, 0, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(3, 1.5, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(3, 0.5, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(2, 0.5, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(2, 1.5, 0)));
    arrowGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0, 0, 0)));
    
    arrowGeometry.faces.push(new THREE.Face4(2,3,7,8));
    arrowGeometry.faces.push(new THREE.Face3(0,1,9));
    arrowGeometry.faces.push(new THREE.Face3(4,5,6));
    arrowGeometry.computeCentroids();
    arrowGeometry.computeFaceNormals();

    var arrowMesh = new THREE.Mesh(arrowGeometry, 
                                   new THREE.MeshBasicMaterial({color: SS.constructors.faceColor, 
                                                                transparent: true, 
                                                                opacity: 0.5}));
    arrowMesh.name = {transformerElement: elementName};
    
    var lineGeom = new THREE.Geometry();
    lineGeom.vertices = arrowGeometry.vertices;
    var line = new THREE.Line(lineGeom, 
                              new THREE.LineBasicMaterial({color: SS.constructors.lineColor, 
                                                           wireframe : true, 
                                                           linewidth: 2.0, 
                                                           transparent: true, 
                                                           opacity: 0.5 }));
    line.name = {transformerElement: elementName};
    
    this.sceneObject = new THREE.Object3D();
    this.sceneObject.add(arrowMesh);
    this.sceneObject.add(line);
    
    this.highlight = function() {
        this.sceneObject.children.map(function(child) {
            child.material.opacity = 1.0;
        });
    }

    this.unHighlight = function() {
        this.sceneObject.children.map(function(child) {
            child.material.opacity = 0.5;
        });
    }

    this.cursor = 'move';
}

SS.transformers.RotateElement = function(geomNode, elementName) {
    
    var boundingBox = SS.boundingBoxForGeomNode(geomNode);
    var center = SS.transformers.centerOfGeom(boundingBox);

    var width = 3;
    var r = 30;
    var dr = 0.5;
    var maxAngle = 7;

    var outerCurveGeometry = new THREE.Geometry();
    for (var i = -maxAngle; i <= 0; ++i) {
        var angle = i/180*Math.PI;
        var z = (r+dr)*Math.cos(angle);
        var x = (r+dr)*Math.sin(angle);
        outerCurveGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(x,0,z)));
    }

    var innerCurveGeometry = new THREE.Geometry();
    for (var i = 0; i >= -maxAngle; --i) {
        var angle = i/180*Math.PI;
        var z = (r-dr)*Math.cos(angle);
        var x = (r-dr)*Math.sin(angle);
        innerCurveGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(x,0,z)));
    }

    var arrowHeadGeometry = new THREE.Geometry();
    arrowHeadGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0,0,r+dr)));
    arrowHeadGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0,0,r+dr+1)));
    arrowHeadGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(3,0,r)));
    arrowHeadGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0,0,r-dr-1)));
    arrowHeadGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0,0,r-dr)));

    var arrowLineGeometry = new THREE.Geometry();
    arrowLineGeometry.vertices.push(innerCurveGeometry.vertices[maxAngle]);
    arrowLineGeometry.vertices.push(outerCurveGeometry.vertices[0]);
    arrowLineGeometry.vertices = arrowLineGeometry.vertices.concat(outerCurveGeometry.vertices);
    arrowLineGeometry.vertices = arrowLineGeometry.vertices.concat(arrowHeadGeometry.vertices);
    arrowLineGeometry.vertices = arrowLineGeometry.vertices.concat(innerCurveGeometry.vertices);

    var lineMaterial = new THREE.LineBasicMaterial({color: SS.constructors.lineColor, 
                                                    linewidth: 2.0, 
                                                    transparent: true, 
                                                    opacity: 0.5 });
    
    var arrowLine = new THREE.Line(arrowLineGeometry, lineMaterial);
    arrowLine.name = {transformerElement: elementName};
    
    var arrowFaceGeometry = new THREE.Geometry();
    arrowFaceGeometry.vertices = outerCurveGeometry.vertices;
    arrowFaceGeometry.vertices = arrowFaceGeometry.vertices.concat(innerCurveGeometry.vertices.reverse());
    for (var i = 0; i < maxAngle; ++i) {
        arrowFaceGeometry.faces.push(new THREE.Face4(i, i+1, 
                                                     outerCurveGeometry.vertices.length + i + 1,
                                                     outerCurveGeometry.vertices.length + i));
    }
    arrowFaceGeometry.vertices.push(arrowHeadGeometry.vertices[1]);
    arrowFaceGeometry.vertices.push(arrowHeadGeometry.vertices[2]);
    arrowFaceGeometry.vertices.push(arrowHeadGeometry.vertices[3]);
    arrowFaceGeometry.faces.push(new THREE.Face3(arrowFaceGeometry.vertices.length-3,
                                                 arrowFaceGeometry.vertices.length-2,
                                                 arrowFaceGeometry.vertices.length-1));
    arrowFaceGeometry.computeCentroids();
    arrowFaceGeometry.computeFaceNormals();

    var arrowFace = new THREE.Mesh(arrowFaceGeometry,
                                   new THREE.MeshBasicMaterial({color: SS.constructors.faceColor, 
                                                                transparent: true, 
                                                                opacity: 0.5}));
    arrowFace.doubleSided = true;
    arrowFace.name = {transformerElement: elementName};

    this.sceneObject = new THREE.Object3D();
    this.sceneObject.add(arrowFace);
    this.sceneObject.add(arrowLine);
    arrowFace.position.z = -r;
    arrowLine.position.z = -r;
    arrowFace.rotation.y = maxAngle/180*Math.PI;
    arrowLine.rotation.y = maxAngle/180*Math.PI;


    this.highlight = function() {
        this.sceneObject.children.map(function(child) {
            if (child.material) {
                child.material.opacity = 1.0;
            }
        });
    }

    this.unHighlight = function() {
        this.sceneObject.children.map(function(child) {
            if (child.material) {
                child.material.opacity = 0.5;
            }
        });
    }

    var rotationAngle = new THREE.Object3D();

    var positionForAngle;
    this.setPositionForAngle = function(position) {
        positionForAngle = position;
    }

    this.drawAngle = function(angle, params) {
        this.sceneObject.remove(rotationAngle);
        rotationAngle = new THREE.Object3D();

        var angleGeometry = new THREE.Geometry();
        angleGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0,0,0)));

        var r = new THREE.Vector3().sub(positionForAngle, center).length()
        for (var i = 0; 
             angle > 0 ? i <= Math.round(angle) : i > Math.round(angle); 
             angle > 0 ? ++i : --i) {
            var theta = i/180*Math.PI;
            var z = r*Math.cos(theta);
            var x = r*Math.sin(theta);
            angleGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(x,0,z)));
        }
        angleGeometry.vertices.push(new THREE.Vertex(new THREE.Vector3(0,0,0)));
        
        var lineMaterial = new THREE.LineBasicMaterial({color: 0xeeeeee, 
                                                        linewidth: 2.0, 
                                                        transparent: true, 
                                                        opacity: 1.0 });
    
        var line = new THREE.Line(angleGeometry, lineMaterial);
        line.position.z = -r;
        rotationAngle.add(line);

        var textGeo = new THREE.TextGeometry('' + angle, {
	    size: 2, height: 0.01, curveSegments: 6,
	    font: 'helvetiker', weight: 'normal', style: 'normal',
	    bezelEnabled: false});
        var text = new THREE.Mesh(textGeo, 
                                  new THREE.MeshBasicMaterial({color: 0xffffff, 
                                                               opacity: 0.8}));

        text.position.z = (r+3)*Math.cos(angle/180*Math.PI) - r;
        text.position.x = (r+3)*Math.sin(angle/180*Math.PI);
        if (params.v > 0) {
            text.rotation.z = Math.PI;
        }
        if (params.u > 0) {
            text.rotation.z = -Math.PI/2;
        }
        text.rotation.x = -Math.PI/2;
        rotationAngle.add(text);

        this.sceneObject.add(rotationAngle);

    }

    this.cursor = 'move';
}



SS.transformers.Manager = function() {

    var that = this;
    var transformerUI = new THREE.Object3D();
    var UISTATE = {DEACTIVATED: 0, SCALE: 1, ROTATE: 2};
    var uiState = UISTATE.DEACTIVATED;
    var transformingState = undefined;
    var lastWorkplanePosition = undefined;

    var cube;
    var uiElements = [];

    var addBoundingBox = function(geomNode) {
        if (cube) {
            transformerUI.remove(cube);
        }
        
        var boundingBox = SS.boundingBoxForGeomNode(geomNode);
        var center = SS.transformers.centerOfGeom(boundingBox);

        var width = boundingBox.max.x - boundingBox.min.x;
        var depth = boundingBox.max.y - boundingBox.min.y;
        var height = boundingBox.max.z - boundingBox.min.z;

        var geometry = new THREE.CubeGeometry(width, depth, height);
	cube = new THREE.Mesh(geometry, new THREE.MeshBasicMaterial({color: SS.constructors.lineColor, 
                                                                     wireframe: true}));
        
	cube.position.x = boundingBox.min.x + width/2;
	cube.position.y = boundingBox.min.y + depth/2;
	cube.position.z = boundingBox.min.z + height/2;
	transformerUI.add(cube);
    }

    var activateScale = function(geomNode) {
        uiElements = [];
        SS.sceneView.scene.remove(transformerUI);

        transformerUI = new THREE.Object3D();
        addBoundingBox(geomNode);
        var boundingBox = SS.boundingBoxForGeomNode(geomNode);

        var translateElement = new SS.transformers.TranslateElement(geomNode);
        uiElements.push(translateElement);
        transformerUI.add(translateElement.sceneObject);

        var scaleElementSpec = {'scale1' : {x: boundingBox.max.x + 1,
                                           y: boundingBox.max.y + 1,
                                           zRotation: 1/4*Math.PI},
                                'scale2' : {x: boundingBox.min.x - 1,
                                           y: boundingBox.max.y + 1,
                                           zRotation: 3/4*Math.PI},
                                'scale3' : {x: boundingBox.min.x - 1,
                                           y: boundingBox.min.y - 1,
                                           zRotation: 5/4*Math.PI},
                                'scale4' : {x: boundingBox.max.x + 1,
                                           y: boundingBox.min.y - 1,
                                           zRotation: 7/4*Math.PI}};
                                
        for (name in scaleElementSpec) {
            var element = new SS.transformers.ScaleElement(geomNode, name);
            uiElements.push(element);
            element.sceneObject.position.x = scaleElementSpec[name].x;
            element.sceneObject.position.y = scaleElementSpec[name].y;
            element.sceneObject.rotation.z = scaleElementSpec[name].zRotation;
            transformerUI.add(element.sceneObject);
        }

        SS.sceneView.scene.add(transformerUI);
    }

    var activateRotate = function(geomNode) {
        uiElements = [];
        SS.sceneView.scene.remove(transformerUI);
        
        transformerUI = new THREE.Object3D();
        var boundingBox = SS.boundingBoxForGeomNode(geomNode);
        
        var width = boundingBox.max.x - boundingBox.min.x;
        var depth = boundingBox.max.y - boundingBox.min.y;
        var height = boundingBox.max.z - boundingBox.min.z;

        var center = SS.transformers.centerOfGeom(boundingBox);

        var jutOut = 10;
        ['x', 'y', 'z'].map(function(key) {
            var lineGeom = new THREE.Geometry();
            var from = center.clone(), to = center.clone();
            from[key] = boundingBox.min[key] - jutOut;
            to[key]   = boundingBox.max[key] + jutOut;

	    lineGeom.vertices.push(new THREE.Vertex(from), new THREE.Vertex(to));
	    var line = new THREE.Line(lineGeom, SS.constructors.lineMaterial);
	    transformerUI.add(line);
        });

        var rotateElementSpec = {
            'rotateY' : {
                x: center.x,
                y: center.y,
                z: boundingBox.max.z + jutOut
            },
            'rotateZ' : {
                x : boundingBox.max.x + jutOut,
                y: center.y,
                z: center.z,
                rotationZ: Math.PI/2,
                rotationY: Math.PI/2
            },
            'rotateX' : {
                x : center.x,
                y: boundingBox.max.y + jutOut,
                z: center.z,
                rotationZ: -Math.PI/2,
                rotationX: -Math.PI/2
            }
        };
        
        for (name in rotateElementSpec) {
            var element = new SS.transformers.RotateElement(geomNode, name);

            uiElements.push(element);

            element.sceneObject.position.x = rotateElementSpec[name].x || 0;
            element.sceneObject.position.y = rotateElementSpec[name].y || 0;
            element.sceneObject.position.z = rotateElementSpec[name].z || 0;

            element.setPositionForAngle(element.sceneObject.position);
            
            element.sceneObject.rotation.z = rotateElementSpec[name].rotationZ || 0;
            element.sceneObject.rotation.y = rotateElementSpec[name].rotationY || 0;
            element.sceneObject.rotation.x = rotateElementSpec[name].rotationX || 0;

            transformerUI.add(element.sceneObject);
        }


        SS.sceneView.scene.add(transformerUI);
    }
    
    var activate = function(geomNode) {
        if (uiState === UISTATE.SCALE) {
            activateRotate(geomNode);
            uiState = UISTATE.ROTATE;
        } else {
            activateScale(geomNode);
            uiState = UISTATE.SCALE;
        }
    }

    var deactivate = function() {
        uiElements = [];
        SS.sceneView.scene.remove(transformerUI);
        uiState = UISTATE.DEACTIVATED;
    }

    var highlightElement = function(event) {
        var found = SS.selectInScene(SS.sceneView.scene, SS.sceneView.camera, event.originalEvent);
        var foundUIElement;
        for (var i = 0; (i < found.length) && !foundUIElement; ++i) {
            for (var j = 0; (j < uiElements.length) && !foundUIElement; ++j) {
                if ((found[i].object === uiElements[j].sceneObject) || 
                    (found[i].object.parent && (found[i].object.parent === uiElements[j].sceneObject))) {
                    foundUIElement = uiElements[j];
                }
            }
        }

        uiElements.map(function(element) {
            if (element !== foundUIElement) {
                element.unHighlight();
            }
        });
        if (foundUIElement) {
            foundUIElement.highlight();
            that.cursor = foundUIElement.cursor;
        } else {
            that.cursor = undefined;
        }
    }

    SS.sceneView.workplane.on('workplaneXYCursorUpdated', function(event) {
        lastWorkplanePosition = {x: event.x, y: event.y};
        
        if (transformingState) {

            if (transformingState.transform.type === 'translate') {
                
                transformingState.transform.parameters.u = event.x - transformingState.from.x
                transformingState.transform.parameters.v = event.y - transformingState.from.y

                transformerUI.position.x = transformingState.transform.parameters.u;
                transformerUI.position.y = transformingState.transform.parameters.v;
                transformerUI.position.z = transformingState.transform.parameters.w;
                
                SS.moveGeomNodeRendering(transformingState.editingNode, transformerUI.position);

            } else if (transformingState.transform.type === 'scale') {
                var dxFrom =  transformingState.from.x - transformingState.center.x;
                var dyFrom =  transformingState.from.y - transformingState.center.y;
                var r1 = Math.sqrt(dxFrom*dxFrom + dyFrom*dyFrom);


                var dxTo = event.x - transformingState.center.x;
                var dyTo = event.y - transformingState.center.y;
                var r2 = Math.sqrt(dxTo*dxTo + dyTo*dyTo);

                var factor = parseFloat((r2/r1).toFixed(3));
                if (event.originalEvent.shiftKey) {
                    factor = Math.round(factor*10)/10;
                }
                
                var scalePoint = transformingState.center.clone();
                scalePoint.z = 0;

                SS.scaleGeomNodeRendering(transformingState.originalNode, 
                                          transformingState.editingNode, 
                                          scalePoint, 
                                          factor);

                transformingState.transform.parameters.factor = factor;
                activateScale(transformingState.editingNode);

                var textGeo = new THREE.TextGeometry('' + factor, {
	            size: 2, height: 0.01, curveSegments: 6,
	            font: 'helvetiker', weight: 'normal', style: 'normal',
	            bezelEnabled: false});
                var text = new THREE.Mesh(textGeo, 
                                          new THREE.MeshBasicMaterial({color: 0xffffff, 
                                                                       opacity: 0.8}));

                var boundingBox = SS.boundingBoxForGeomNode(transformingState.editingNode);
                var center = SS.transformers.centerOfGeom(boundingBox);
                text.position.y = center.y - text.boundRadius/2;
                text.position.x = boundingBox.max.x + 5;
                text.rotation.z = Math.PI/2; 

                transformerUI.add(text);

            } else if (transformingState.transform.type === 'rotate') {
                
                var positionOnRotationPlane = SS.sceneView.determinePositionOnPlane(event.originalEvent, rotationPlane);
                if (!positionOnRotationPlane) {
                    return;
                }

                var initial = transformingState.initialPositionOnPlane;
                var v1 = new THREE.Vector3().sub(
                    new THREE.Vector3(
                        positionOnRotationPlane.x, 
                        positionOnRotationPlane.y, 
                        positionOnRotationPlane.z),
                    transformingState.center).normalize();
                var v2;
                if (transformingState.transform.parameters.u > 0) {
                    v2 = new THREE.Vector3(0,1,0);
                } else if (transformingState.transform.parameters.v > 0) {
                    v2 = new THREE.Vector3(0,0,1);
                } else if (transformingState.transform.parameters.w > 0) {
                    v2 = new THREE.Vector3(1,0,0);
                } 

                
                var v2CrossV1 = new THREE.Vector3().cross(v2, v1);
                var rotationVector = new THREE.Vector3(
                    transformingState.transform.parameters.u, 
                    transformingState.transform.parameters.v, 
                    transformingState.transform.parameters.w);

                var angle = parseFloat((Math.acos(v1.dot(v2))/Math.PI*180).toFixed(0));
                if (rotationVector.dot(v2CrossV1) < 0) {
                    angle = -angle;
                }
                if (event.originalEvent.shiftKey) {
                    angle = Math.round(angle/15)*15;
                }
                transformingState.transform.parameters.angle = angle;

                SS.rotateGeomNodeRendering(transformingState.originalNode, 
                                           transformingState.editingNode, 
                                           transformingState.center,
                                           transformingState.transform.parameters.u, 
                                           transformingState.transform.parameters.v, 
                                           transformingState.transform.parameters.w, 
                                           angle);

                transformingState.element.drawAngle(angle, transformingState.transform.parameters);


            }
        }
        highlightElement(event);

    });

    SS.sceneView.workplane.on('workplaneZCursorUpdated', function(event) {
    });

    SS.sceneView.workplane.on('workplaneClicked', function(event) {
        if (transformingState) {
            var doTranslate = 
                ((transformingState.transform.type === 'translate') &&
                 ((transformingState.transform.parameters.u !== 0) ||
                  (transformingState.transform.parameters.v !== 0) ||
                  (transformingState.transform.parameters.w !== 0)));
            var doScale = ((transformingState.transform.type === 'scale') &&
                           (transformingState.transform.parameters.factor !== 1.0));
            var doRotate = ((transformingState.transform.type === 'rotate') &&
                            (transformingState.transform.parameters.angle !== 0.0));

            if (doTranslate || doScale || doRotate) {
                var cmd = update_geom_command(transformingState.originalNode, 
                                              transformingState.editingNode, 
                                              transformingState.editingNode);
                command_stack.execute(cmd);
            } else {
                SS.restoreGeomNodeRendering(transformingState.originalNode, 
                                            transformingState.originalGeometrySnapshot);
            }
            transformingState = undefined;
            SS.sceneView.scene.remove(transformerUI);
        }
        
    });

    this.selectionUpdated = function(event) {
        if (event.deselected) {
            if (event.deselected.length === 1) {
                var node =  geom_doc.findById(event.deselected[0]);
                if (!transformingState || (node !== transformingState.originalNode)) {
                    deactivate();
                }
            } else {
                deactivate();
            }
        } else if (event.selected) {
            if (selectionManager.size() === 1) {
                var node = geom_doc.findById(selectionManager.getSelected()[0]);
                if (!transformingState || (node !== transformingState.editingNode)) {
                    activate(node);
                }
            } else {
                deactivate();
            }
        }
    }

    var initiateTransform = function(event, geomNode, type, parameters) {
        var boundingBox = SS.boundingBoxForGeomNode(geomNode);
        var center = SS.transformers.centerOfGeom(boundingBox);
        
        var editingNode = geomNode.editableCopy();
        var transform = new Transform({
            type: type,
            editing: true,
	    origin: {x: parseFloat((center.x).toFixed(3)), 
                     y: parseFloat((center.y).toFixed(3)), 
                     z: 0},
            parameters: parameters
        });
        editingNode.transforms.push(transform);

        transformingState = {from: {x: lastWorkplanePosition.x, y: lastWorkplanePosition.y},
                             center: center,
                             originalNode: geomNode,
                             editingNode:  editingNode,
                             transform: transform};

        selectionManager.deselectID(geomNode.id);
        geomNode.originalSceneObjects = geomNode.sceneObjects;
        geom_doc.replace(geomNode, editingNode);
        selectionManager.selectID(editingNode.id);
    }

    this.initiateTranslate = function(event, geomNode) {
        initiateTransform(event, geomNode, 'translate', {u: 0, v: 0, w: 0, n:0});
    }

    this.initiateScale = function(event, geomNode) {
        initiateTransform(event, geomNode, 'scale', {factor: 1.0});
    }

    this.initiateRotate = function(event, geomNode, axis) {
        var boundingBox = SS.boundingBoxForGeomNode(geomNode);
        var center = SS.transformers.centerOfGeom(boundingBox);

        var parameters = {u:0, 
                          v:0, 
                          w:0, 
                          angle:0.0, 
                          n:0};
        if (axis === 'X') {
            parameters.u = 1.0;
        } else if (axis === 'Y') {
            parameters.v = 1.0;
        } else if (axis === 'Z') {
            parameters.w = 1.0;
        }


        rotationPlane = new THREE.Mesh(
            new THREE.PlaneGeometry(1000, 1000),
            new THREE.MeshBasicMaterial({ color: 0x333366, opacity: 0.0, transparent: true }));
        rotationPlane.position = center;
        if (parameters.u > 0) {
            rotationPlane.rotation.y = Math.PI/2;
        }
        if (parameters.v > 0) {
            rotationPlane.rotation.x = Math.PI/2;
        }
        rotationPlane.doubleSided = true;
        transformerUI.add(rotationPlane);

        initiateTransform(event, geomNode, 'rotate', parameters);
        transformingState.transform.origin.x = parseFloat((center.x).toFixed(3));
        transformingState.transform.origin.y = parseFloat((center.y).toFixed(3));
        transformingState.transform.origin.z = parseFloat((center.z).toFixed(3));
        
        // Animate() is required for the ray casting below to work correctly
        SS.sceneView.animate();
        transformingState.initialPositionOnPlane =
            SS.sceneView.determinePositionOnPlane(event, rotationPlane);
    }

    this.isMouseOverTransformerElement = function(scene, camera, event) {

        var found = SS.selectInScene(scene, camera, event);
        var foundTransformerUIElements = found.filter(function(obj) {
            return obj.object.name.transformerElement;
        });
   
        return foundTransformerUIElements.length > 0;
    }

    this.initiateTransformer = function(scene, camera, event) {

        var found = SS.selectInScene(scene, camera, event);
        var foundTransformerUIElements = found.filter(function(obj) {
            return obj.object.name.transformerElement;
        });

        if (foundTransformerUIElements.length > 0) {
            var element = foundTransformerUIElements[0].object.name.transformerElement;
            var geomNode = geom_doc.findById(selectionManager.getSelected()[0]);
            if (element === 'translate-XY') {
                SS.transformerManager.initiateTranslate(event, geomNode);
            }
            if (element.match(/scale.*/)) {
                SS.transformerManager.initiateScale(event, geomNode);
            }
            if (element.match(/rotate.*/)) {
                var axis = element.match("^rotate(X|Y|Z)$")[1];
                SS.transformerManager.initiateRotate(event, geomNode, axis);
 
                for (var j = 0; (j < uiElements.length); ++j) {
                    if ((foundTransformerUIElements[0].object === uiElements[j].sceneObject) || 
                        (foundTransformerUIElements[0].object.parent && 
                         (foundTransformerUIElements[0].object.parent === uiElements[j].sceneObject))) {
                        
                        transformingState.element = uiElements[j];
                    }
                }
                    
            }
        }
        
        return foundTransformerUIElements.length > 0;

    }

    
}

