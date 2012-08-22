var SS = SS || {};

(function() {

    var unselectedColor = 0x00dd00, selectedColor = 0xdddd00;

    var create1DGeometries = function(mesh) {

        var precisionPoints = 4; 
        var precision = Math.pow( 10, precisionPoints );
        var keyForVertex = function(vertex) {
            return [Math.round( vertex.x * precision ), 
            Math.round( vertex.y * precision ), 
            Math.round( vertex.z * precision )].join( '_' );
        }

        var segmentsByStartPosition = {};
        var segmentsByEndPosition = {};
        var allSegments = [];

        mesh.segments.map(function(segment) {

            var vertices = [];
            for (var i = segment.start; i < segment.end; i += 3) {
                var vertex = new THREE.Vector3(mesh.positions[i], 
                    mesh.positions[i + 1], 
                    mesh.positions[i + 2]);
                vertices.push(vertex);
            }

            var startVertex = vertices[0];
            var endVertex = vertices[vertices.length-1];
            var startVertexKey = keyForVertex(startVertex);
            var endVertexKey = keyForVertex(endVertex);

            allSegments.push(vertices);
            segmentsByStartPosition[startVertexKey] = segmentsByStartPosition[startVertexKey] || [];
            segmentsByStartPosition[startVertexKey].push(vertices);

            segmentsByEndPosition[endVertexKey] = segmentsByEndPosition[endVertexKey] || [];
            segmentsByEndPosition[endVertexKey].push(vertices);
        });

    // Compression
    do {

        var didACompression = false;
        for (vertexKey in segmentsByStartPosition)  {

            if (!didACompression) {
            // If the is a segment with the same end position key as this 
            // start position, compress the segments

                if ((segmentsByStartPosition[vertexKey].length > 0) &&
                    segmentsByEndPosition[vertexKey] &&
                    (segmentsByEndPosition[vertexKey].length > 0)) {

                    var firstSegment = segmentsByEndPosition[vertexKey][0];
                    var secondSegment = segmentsByStartPosition[vertexKey][0];

                    // Circular segments
                    if (!(firstSegment === secondSegment)) {

                        var newSegment = [].concat(firstSegment).concat(secondSegment);

                        newSegmentStartKey = keyForVertex(newSegment[0]);
                        newSegmentEndKey   = keyForVertex(newSegment[newSegment.length - 1]);

                        allSegments.splice(allSegments.indexOf(firstSegment), 1);
                        allSegments.splice(allSegments.indexOf(secondSegment), 1);
                        allSegments.push(newSegment);

                        segmentsByStartPosition[vertexKey].splice(
                            segmentsByStartPosition[vertexKey].indexOf(secondSegment), 1);
                        segmentsByStartPosition[newSegmentStartKey].splice(
                            segmentsByStartPosition[newSegmentStartKey].indexOf(firstSegment), 1);

                        segmentsByEndPosition[vertexKey].splice(
                            segmentsByEndPosition[vertexKey].indexOf(firstSegment), 1);
                        segmentsByEndPosition[newSegmentEndKey].splice(
                            segmentsByEndPosition[newSegmentEndKey].indexOf(secondSegment), 1);


                        segmentsByStartPosition[newSegmentStartKey].push(newSegment);
                        segmentsByEndPosition[newSegmentEndKey].push(newSegment);

                        didACompression = true;
                    }
                }
            }

        }


    } while(didACompression);

    return allSegments.map(function(segment) {
       var geometry =  new THREE.Geometry();
       geometry.vertices = segment;
       geometry.dynamic = true;
       return geometry;
   });

}

var createGeometry = function(meshes) {
    return {
            'faces' : create3DGeometries(meshes['faces']),
            'edges' : create1DGeometries(meshes['edges'])
    };
}

var create1DSelectionGeometries = function(mesh) {
    return mesh.segments.map(function(segment) {
       var positions = [];
       for (var i = segment.start; i < segment.end; i+=3) {
            positions.push(new THREE.Vector3(
                mesh.positions[i], 
                mesh.positions[i + 1], 
                mesh.positions[i + 2]));
        }
        return new THREE.PipeGeometry(3, positions);
    });
}

var createVertexGeometries = function(mesh) {
    var precisionPoints = 4; 
    var precision = Math.pow( 10, precisionPoints );
    var keyForPosition = function(position) {
        return [ Math.round( position.x * precision ), Math.round( position.y * precision ), Math.round( position.z * precision ) ].join( '_' );
    }

    var positions = {};
    var vertexMaterial = new THREE.MeshBasicMaterial({color: 0xffffff, opacity: 0.8});
    mesh.segments.map(function(segment) {
        var startPosition = new THREE.Vector3(mesh.positions[segment.start], 
          mesh.positions[segment.start + 1], 
          mesh.positions[segment.start + 2]);
        var endPosition = new THREE.Vector3(mesh.positions[segment.end - 2], 
          mesh.positions[segment.end - 1], 
          mesh.positions[segment.end]);
        [startPosition, endPosition].map(function(position) {
            var key = keyForPosition(position);
            if (!positions[key]) {
                positions[key] = new THREE.Mesh(new THREE.CubeGeometry(0.5, 0.5, 0.5), vertexMaterial);
                positions[key].position = position;
            }
        });
    });

    var cubes = [];
    for (var key in positions) {
        cubes.push(positions[key]);
    }
    return cubes;
}

var create3DGeometries = function(mesh) {
    if (mesh.positions.length === 0) {
        return [];
    }
    var geometry = new THREE.Geometry();

    for (var i = 0; i  < mesh.positions.length/3; ++i) {
        var vertex = new THREE.Vector3(mesh.positions[i * 3], 
           mesh.positions[i * 3 + 1], 
           mesh.positions[i * 3 + 2]);
        geometry.vertices.push(vertex);
    }
    
    for (var i = 0; i < mesh.indices.length/3; ++i) {
        var a = mesh.indices[i * 3],
        b = mesh.indices[i * 3 + 1],
        c = mesh.indices[i * 3 + 2];

        var face = new THREE.Face3(a,b,c);
        face.vertexNormals = [new THREE.Vector3(mesh.normals[a*3], 
            mesh.normals[a*3+1], 
            mesh.normals[a*3+2]),
        new THREE.Vector3(mesh.normals[b*3], 
            mesh.normals[b*3+1], mesh.normals[b*3+2]),
        new THREE.Vector3(mesh.normals[c*3], 
            mesh.normals[c*3+1], mesh.normals[c*3+2])];
        geometry.faces.push(face);
    }
    geometry.computeCentroids();
    geometry.computeFaceNormals();
    geometry.mergeVertices();
    geometry.dynamic = true;
    return [geometry];
}

SS.createGeometry = function(geomNode) {

    if (!geomNode.mesh) {
        return;
    }

    var color = unselectedColor, opacity = 1.0;
    var isEditing = geomNode.editing;
    for (index in geomNode.transforms) {
        if (geomNode.transforms[index].editing) {
            isEditing = true;
        }
    }
    if (isEditing) {
        color = 0xdddd00;
        opacity = 0.5;
    }
    var objectName = {geomNodeId: geomNode.id};

    var triangleGeometries = create3DGeometries(geomNode.mesh['faces']);
    var triangleMaterial = new THREE.MeshPhongMaterial( { ambient: color, color: color, opacity: opacity,  specular: color, shininess: 50, shading: THREE.SmoothShading, transparent:true } );
    var triangles = new THREE.Object3D();
    triangleGeometries.map(function(triangleGeometry) {
        var mesh = new THREE.Mesh(triangleGeometry, triangleMaterial);
        mesh.doubleSided = true; 
        mesh.name = objectName
        triangles.add(mesh);
    });

    triangles.name = objectName;

    var lineGeometries = create1DGeometries(geomNode.mesh['edges']);
    var lineMaterial = new THREE.LineBasicMaterial({ color: color, opacity: 1.0, linewidth: 2, transparent:true });
    var lines = new THREE.Object3D();
    lineGeometries.map(function(lineGeometry) {
        var line = new THREE.Line(lineGeometry, lineMaterial);
        line.name = objectName;
        lines.add(line);
    });
    lines.name = objectName;

    // Only render line selections when there are no triangles
    var selectionMeshes = new THREE.Object3D();
    if (triangles.children.length === 0) {
        var selectionGeometriesForLines = create1DSelectionGeometries(geomNode.mesh['edges']);
        selectionGeometriesForLines.map(function(selectionGeometriesForLine) {
            var mesh = new THREE.Mesh(
                selectionGeometriesForLine, 
                new THREE.MeshBasicMaterial({ color: 0x000000, opacity: 0, transparent: true})); 
            mesh.name = objectName
            mesh.doubleSided = true;
            selectionMeshes.add(mesh);
        });
        selectionMeshes.name = objectName; 
    }                      
    
    return {
        'faces': triangles, 
        //'vertices' : vertices,
        'edges': lines,
        'selectionForEdges' : selectionMeshes
    };
}

SS.renderGeometry = function(geomNode) {
    var geometries = SS.createGeometry(geomNode);
    if (geometries) {
        SS.sceneView.scene.add(geometries.faces);
        SS.sceneView.scene.add(geometries.edges);
        SS.sceneView.selectionOnlyMeshes.push(geometries.selectionForEdges);
        geomNode.sceneObjects = geometries;
    }
}

SS.hideGeometry = function(geomNode) {
    if (geomNode.sceneObjects) {
        for (key in geomNode.sceneObjects) {
            if (key === 'selectionForEdges') {
                var index = SS.sceneView.selectionOnlyMeshes.indexOf(geomNode.sceneObjects[key]);
                SS.sceneView.selectionOnlyMeshes.splice(index, 1);
            } else {
                SS.sceneView.scene.remove(geomNode.sceneObjects[key]);
            }
        }
        geomNode.sceneObjects = {};
    }
}

SS.highlightGeometry = function(geomNode) {
    for (key in geomNode.sceneObjects) {
        var object = geomNode.sceneObjects[key];
        if (key !== 'selectionForEdges') {
            SS.sceneView.scene.remove(object);
            SS.sceneView.scene.add(object);
        }
    };
    geomNode.sceneObjects['faces'].children.map(function(child) {
        child.material.color.setHex(selectedColor);
        child.material.ambient.setHex(selectedColor);
        child.material.specular.setHex(selectedColor);
        child.material.opacity = 0.7;
    });
    geomNode.sceneObjects['edges'].children.map(function(child) {
        child.material.color.setHex(selectedColor);
        child.material.opacity = 0.7;
    });
}

SS.unhighlightGeometry = function(geomNode) {
    SS.restoreOpacity();
    geomNode.sceneObjects['faces'].children.map(function(child) {
        child.material.color.setHex(unselectedColor);
        child.material.ambient.setHex(unselectedColor);
        child.material.specular.setHex(unselectedColor);
        child.material.opacity = 1.0;
    });
    geomNode.sceneObjects['edges'].children.map(function(child) {
        child.material.color.setHex(unselectedColor);
        child.material.opacity = 1.0;
    });
}


SS.setTransparent = function(geomNode) {
    for (key in geomNode.sceneObjects) {
        geomNode.sceneObjects[key].children.map(function(child) {
            if (child.material.opacity === 1.0) {
                child.material.opacity = 0.5;
            }
        });
    }
}

SS.restoreOpacity = function() {
    geom_doc.rootNodes.map(function(geomNode) {
        if (geomNode.sceneObjects) {
            for (key in geomNode.sceneObjects) {
                geomNode.sceneObjects[key].children.map(function(child) {
                    if ((key !== 'selectionForEdges') && 
                        (child.material.opacity !== 1.0)) {
                        child.material.opacity = 1.0;
                }
            });
            }
        }
    });
}

SS.translateGeomNodeRendering = function(originalNode, editingNode,  translation) {
    for (key in editingNode.sceneObjects) {
        for (var i = 0; i < editingNode.sceneObjects[key].children.length; ++i) {

            var originalGeometry = originalNode.originalSceneObjects[key].children[i].geometry;
            var editingGeometry = editingNode.sceneObjects[key].children[i].geometry;

            var axis = SS.objToVector(originalNode.workplane.axis);
            var angle = originalNode.workplane.angle;

            var translationVec = SS.objToVector(translation);
            editingGeometry.vertices = originalGeometry.vertices.map(function(vertex) {
                var position = vertex.clone();
                position.addSelf(SS.rotateAroundAxis(translationVec, axis, angle));
                return position;
            });

            editingGeometry.computeCentroids();
            editingGeometry.computeFaceNormals();
            editingGeometry.verticesNeedUpdate = true;
        }
    }
}

SS.scaleGeomNodeRendering = function(originalNode, editingNode, scalePoint, factor) {
    var axis = SS.objToVector(originalNode.workplane.axis);
    var angle = originalNode.workplane.angle;
    var globalScalePoint = SS.rotateAroundAxis(scalePoint, axis, angle);
    globalScalePoint.addSelf(SS.objToVector(originalNode.workplane.origin));

    for (key in editingNode.sceneObjects) {
        for (var i = 0; i < editingNode.sceneObjects[key].children.length; ++i) {

            var originalGeometry = originalNode.originalSceneObjects[key].children[i].geometry;
            var editingGeometry = editingNode.sceneObjects[key].children[i].geometry;


            editingGeometry.vertices = originalGeometry.vertices.map(function(vertex) {
                var position = vertex.clone();
                return new THREE.Vector3().add(
                    globalScalePoint,
                    new THREE.Vector3().sub(position, globalScalePoint).multiplyScalar(factor));
            });

            editingGeometry.computeCentroids();
            editingGeometry.computeFaceNormals();
            editingGeometry.verticesNeedUpdate = true;
        }
    }
}

SS.rotateGeomNodeRendering = function(originalNode, editingNode, transform) {

    var workplaneOrigin = SS.objToVector(originalNode.workplane.origin);
    var workplaneAxis   = SS.objToVector(originalNode.workplane.axis);
    var workplaneAngle  = originalNode.workplane.angle;

    var transformOrigin = SS.objToVector(transform.origin);
    var localRotationAxis = SS.objToVector(transform.parameters);
    var rotationAxis = SS.rotateAroundAxis(localRotationAxis, workplaneAxis, workplaneAngle);

    var angle = transform.parameters.angle;

    var globalTransformOrigin = SS.rotateAroundAxis(transformOrigin, workplaneAxis, workplaneAngle);

    for (key in editingNode.sceneObjects) {

        for (var i = 0; i < editingNode.sceneObjects[key].children.length; ++i) {

            var originalGeometry = originalNode.originalSceneObjects[key].children[i].geometry;
            var editingGeometry = editingNode.sceneObjects[key].children[i].geometry;

            editingGeometry.vertices = originalGeometry.vertices.map(function(vertex) {
                var position = vertex.clone();

                position.subSelf(workplaneOrigin);
                position.subSelf(globalTransformOrigin);
                position = SS.rotateAroundAxis(position, rotationAxis, angle);
                position.addSelf(globalTransformOrigin);
                position.addSelf(workplaneOrigin);                               

                return position;
            });
            editingGeometry.computeCentroids();
            editingGeometry.computeFaceNormals();
            editingGeometry.verticesNeedUpdate = true;
        }
    }
}

SS.axisMirrorGeomNodeRendering = function(originalNode, editingNode, transform) {

    var workplaneOrigin = SS.objToVector(originalNode.workplane.origin);
    var workplaneAxis   = SS.objToVector(originalNode.workplane.axis);
    var workplaneAngle  = originalNode.workplane.angle;

    var transformOrigin = SS.objToVector(transform.origin);
    var localRotationAxis = SS.objToVector(transform.parameters);
    var rotationAxis = SS.rotateAroundAxis(localRotationAxis, workplaneAxis, workplaneAngle);

    var globalTransformOrigin = SS.rotateAroundAxis(transformOrigin, workplaneAxis, workplaneAngle);
    
    for (key in editingNode.sceneObjects) {

        for (var i = 0; i < editingNode.sceneObjects[key].children.length; ++i) {

            var originalGeometry = originalNode.originalSceneObjects[key].children[i].geometry;
            var editingGeometry = editingNode.sceneObjects[key].children[i].geometry;

            editingGeometry.vertices = originalGeometry.vertices.map(function(vertex) {
                var position = vertex.clone();

                position.subSelf(workplaneOrigin);
                position.subSelf(globalTransformOrigin);
                position = SS.rotateAroundAxis(position, rotationAxis, 180);
                position.addSelf(globalTransformOrigin);
                position.addSelf(workplaneOrigin);                               

                return position;
            });
            editingGeometry.computeCentroids();
            editingGeometry.computeFaceNormals();
            editingGeometry.verticesNeedUpdate = true;
        }
    }            
}

SS.planeMirrorGeomNodeRendering = function(originalNode, editingNode, transform) {

    var workplaneOrigin = SS.objToVector(originalNode.workplane.origin);
    var localNormal = SS.objToVector(transform.parameters).normalize().negate();
    var axis   = SS.objToVector(originalNode.workplane.axis);
    var angle  = originalNode.workplane.angle;
    var normal = SS.rotateAroundAxis(localNormal, axis, angle);

    var transformOrigin = SS.objToVector(transform.origin);
    var globalTransformOrigin = SS.rotateAroundAxis(transformOrigin, axis, angle);

    for (key in editingNode.sceneObjects) {

        for (var i = 0; i < editingNode.sceneObjects[key].children.length; ++i) {

            var originalGeometry = originalNode.originalSceneObjects[key].children[i].geometry;
            var editingGeometry = editingNode.sceneObjects[key].children[i].geometry;

            editingGeometry.vertices = originalGeometry.vertices.map(function(vertex) {
                var position = vertex.clone().subSelf(workplaneOrigin);
                var dot = new THREE.Vector3().sub(position, globalTransformOrigin).dot(normal);
                var dPos = normal.clone().multiplyScalar(-2*dot);
                var newPosition = new THREE.Vector3().add(position, dPos)
                newPosition.addSelf(workplaneOrigin);
                return newPosition;
            });
            editingGeometry.computeCentroids();
            editingGeometry.computeFaceNormals();
            editingGeometry.verticesNeedUpdate = true;
        }
    }            
}

SS.boundingBoxForSceneObject = function(sceneObject) {
    var boundingBoxes = [];
    var addRecursively = function(child) {
        if (child.geometry) {
            child.geometry.computeBoundingBox();
            child.geometry.boundingBox && boundingBoxes.push(child.geometry.boundingBox);
        }
        for (i in child.children) {
            addRecursively(child.children[i]);
        }
    };
    addRecursively(sceneObject);

    if (boundingBoxes.length == 0) {
        return {min: new THREE.Vector3(0,0,0), max: new THREE.Vector3(0,0,0)};
    }

    var min = boundingBoxes[0].min;
    var max = boundingBoxes[0].max;
    for (var i = 1; i < boundingBoxes.length; ++i) {
        var box = boundingBoxes[i];
        min.x = Math.min(min.x, box.min.x);
        min.y = Math.min(min.y, box.min.y);
        min.z = Math.min(min.z, box.min.z);
        max.x = Math.max(max.x, box.max.x);
        max.y = Math.max(max.y, box.max.y);
        max.z = Math.max(max.z, box.max.z);
    }

    return {min: min, max: max};
}

var computeBox = function(workplane, geomNode) {
    var boundingBoxes = [];
    var addFunction = function(child) {
        var childBox = SS.computeNormalizedBoundingBox(workplane, child.geometry);
        if (childBox) {
            boundingBoxes.push(childBox);
        }
    };

    // Preview geometry have no scene objects, and hidden
    // geometry have ampty scene objects
    geomNode.sceneObjects && geomNode.sceneObjects.edges && geomNode.sceneObjects.edges.children.map(addFunction);
    geomNode.sceneObjects && geomNode.sceneObjects.faces && geomNode.sceneObjects.faces.children.map(addFunction);

    if (boundingBoxes.length == 0) {
        return undefined;
    }

    var min = boundingBoxes[0].min;
    var max = boundingBoxes[0].max;
    for (var i = 1; i < boundingBoxes.length; ++i) {
        var box = boundingBoxes[i];
        min.x = Math.min(min.x, box.min.x);
        min.y = Math.min(min.y, box.min.y);
        min.z = Math.min(min.z, box.min.z);
        max.x = Math.max(max.x, box.max.x);
        max.y = Math.max(max.y, box.max.y);
        max.z = Math.max(max.z, box.max.z);
    }
    return {min: min, max: max};
}

SS.boundingBoxForGeomNode = function(geomNode) {
    return computeBox(new SS.WorkplaneNode(), geomNode);
}

SS.normalizedBoundingBoxForGeomNode = function(geomNode) {
    return computeBox(geomNode.workplane, geomNode);
}

SS.computeNormalizedBoundingBox = function(workplane, geometry) {

    var boundingBox = { min: new THREE.Vector3(), max: new THREE.Vector3() };
    var axis = SS.objToVector(workplane.axis);
    var angle = workplane.angle;
    var origin = SS.objToVector(workplane.origin);

    var normalize = function(position) {
        return SS.rotateAroundAxis(position.clone().subSelf(origin), axis, -angle);
    }

    if (geometry.vertices.length > 0) {

        var position = normalize(geometry.vertices[0]);

        boundingBox.min.copy(position);
        boundingBox.max.copy(position);

        var min = boundingBox.min,
            max = boundingBox.max;

        for (var v = 1, vl = geometry.vertices.length; v < vl; ++v) {
            position = normalize(geometry.vertices[v]);
            if (position.x < min.x) {
                min.x = position.x;
            } else if ( position.x > max.x ) {
                max.x = position.x;
            }

            if (position.y < min.y) {
                min.y = position.y;
            } else if (position.y > max.y) {
                max.y = position.y;
            }

            if (position.z < min.z) {
                min.z = position.z;
            } else if ( position.z > max.z ) {
                max.z = position.z;
            }
        }
        return boundingBox;
    } else {
        return undefined;
    }
}

    
SS.centerOfGeom = function(boundingBox) {
    return new THREE.Vector3().add(boundingBox.min, 
     new THREE.Vector3().sub(boundingBox.max, boundingBox.min).divideScalar(2));
}

SS.normalizedBoundingRadius = function(boundingBox) {
    return new THREE.Vector3().sub(boundingBox.max, boundingBox.min).length()/2;
}

})();
