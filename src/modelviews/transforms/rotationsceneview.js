define([
    'calculations',
    'scene',
    'geometrygraphsingleton',
    'asyncAPI',
    'modelviews/modelgraph',
    './sceneview',
  ], function(calc, sceneModel, geometryGraph, AsyncAPI, modelGraph, TransformSceneView) {

  var RotationSceneView = TransformSceneView.extend({

    initialize: function() {

      this.relativeAngle = 0;
      // this.updateAxis();

      TransformSceneView.prototype.initialize.call(this);
    },

    render: function() {
      var i, angle, x, y;

      TransformSceneView.prototype.render.call(this);

      var circleGeom = new THREE.Geometry();
      for (i = 0; i <= 360 - this.relativeAngle; ++i) {
        angle = i/180*Math.PI;
        x = (this.radius)*Math.cos(angle);
        y = (this.radius)*Math.sin(angle);
        circleGeom.vertices.push(new THREE.Vector3(x,y,0));
      }
      var circleMaterial = new THREE.LineBasicMaterial({ 
        color: this.arrowLineColor, 
        linewidth: 1.0});
      var circle = new THREE.Line(circleGeom, circleMaterial);

      var arcGeom = new THREE.Geometry();
      if (this.relativeAngle !== 0) {
          var arcStartAngle = Math.min(-this.relativeAngle, 0);
          var arcEndAngle = Math.max(-this.relativeAngle, 0);

          if (arcStartAngle === -this.relativeAngle) {
              arcGeom.vertices.push(new THREE.Vector3(0,0,0));
          }
          for (i = arcStartAngle; i <= arcEndAngle; ++i) {
              angle = i/180*Math.PI;
              x = (this.radius)*Math.cos(angle);
              y = (this.radius)*Math.sin(angle);
              arcGeom.vertices.push(new THREE.Vector3(x,y,0));
          }
          if (arcEndAngle === -this.relativeAngle) {
              arcGeom.vertices.push(new THREE.Vector3(0,0,0));
          }
      }
      var angleArc = new THREE.Line(arcGeom, circleMaterial);
                                  
      var arrowGeometry = new THREE.CylinderGeometry(0, 0.75, 2, 3);
      var arrowMaterials = [
          new THREE.MeshBasicMaterial({color: this.arrowLineColor, wireframe: false}),
          new THREE.MeshBasicMaterial({color: this.arrowFaceColor, wireframe: true})
      ];
      this.arrow = THREE.SceneUtils.createMultiMaterialObject(arrowGeometry, arrowMaterials);
      this.arrow.position = this.arrowStartPosition;
      this.arrow.rotation.z = Math.PI/2;
      this.arrow.scale = this.cameraScale;

      this.circleAndArrow = new THREE.Object3D();
      this.circleAndArrow.add(this.arrow);
      this.circleAndArrow.add(circle);
      this.circleAndArrow.add(angleArc);
      this.sceneObject.add(this.circleAndArrow);
     
      var quaternion = new THREE.Quaternion();
      
      angle = this.getAngle()/180*Math.PI;
      quaternion.setFromAxisAngle(this.getAxis(), angle);

      this.sceneObject.useQuaternion = true;
      this.sceneObject.quaternion = quaternion;

      this.sceneObject.position = this.center;

    },

    updateCameraScale: function() {
      TransformSceneView.prototype.updateCameraScale.call(this);

      if (this.arrow) {
        this.arrow.scale = this.cameraScale;
      }
    },

    dragStarted: function() {
      this.originalVertex = this.model.vertex;
      this.originalVertex.transforming = true;
      this.editingVertex = AsyncAPI.edit(this.model.vertex);
      this.editingModel = modelGraph.get(this.editingVertex.id);
      this.dragging = true;
    },

    drag: function(position, intersections, event) {

      var planeOrigin = this.center;
      var planeAxis = this.relativeRotationAxis;//calc.objToVector(this.rotation.axis, geometryGraph, THREE.Vector3);

      var camera = sceneModel.view.camera;
      var positionOnRotationPlane = calc.positionOnPlane($('#scene'), event, planeOrigin, planeAxis, camera);

      if (!positionOnRotationPlane) {
        return;
      }

      var v1 = new THREE.Vector3().subVectors(positionOnRotationPlane, planeOrigin).normalize();
      var v2 = this.arrowStartPosition.clone().normalize();
      var v2CrossV1 = new THREE.Vector3().crossVectors(v2, v1);

      var angle = parseFloat((Math.acos(v1.dot(v2))/Math.PI*180).toFixed(0));
      if (planeAxis.dot(v2CrossV1) < 0) {
        angle = -angle;
      }

      if (this.previousRelativeAngle === undefined) {
        this.previousRelativeAngle = 0;
      } else {
        this.previousRelativeAngle = this.relativeAngle;
      }

      var round = function(value, tolerance) {
        return Math.round(value/tolerance)*tolerance;
      };

      this.relativeAngle = angle;
      if (!event.ctrlKey) {
        this.relativeAngle = round(this.relativeAngle, 5);
        if (this.relativeAngle === 360) {
          this.relativeAngle = 0;
        }
      }

      if (this.relativeAngle !== this.previousRelativeAngle) {
        // var quat1 = new THREE.Quaternion().setFromAxisAngle(this.startAxis, this.startAngle/180*Math.PI);
      //   var quat2 = new THREE.Quaternion().setFromAxisAngle(this.relativeRotationAxis, Math.PI*this.relativeAngle/180);
      //   var quat3 = new THREE.Quaternion().multiply(quat1, quat2);
      //   quat3.normalize();
        
        var quat2 = new THREE.Quaternion().setFromAxisAngle(planeAxis, Math.PI*this.relativeAngle/180);
        var quat3 = quat2.normalize();

        var quaternionToAxisAngle = function(q) {
          var angle = 2*Math.acos(q.w);
          var axis = new THREE.Vector3(q.x/Math.sqrt(1-q.w*q.w),
                                       q.y/Math.sqrt(1-q.w*q.w),
                                       q.z/Math.sqrt(1-q.w*q.w));
          return {angle: angle/Math.PI*180, axis: axis};
        };

        var axisAngle = quaternionToAxisAngle(quat3);

        this.editingVertex.transforms.rotation.axis.x = parseFloat(axisAngle.axis.x.toFixed(3));
        this.editingVertex.transforms.rotation.axis.y = parseFloat(axisAngle.axis.y.toFixed(3));
        this.editingVertex.transforms.rotation.axis.z = parseFloat(axisAngle.axis.z.toFixed(3));
        this.editingVertex.transforms.rotation.angle  = parseFloat(axisAngle.angle.toFixed(2));

        this.sceneObject.quaternion = quat3;

        this.editingVertex.trigger('change', this.editingVertex);
      }

    },

    dragEnded: function() {
      this.sceneObject.remove(this.axis);
      this.dragging = false;
      this.editingVertex.transforming = false;
      this.editingModel.tryCommit();
    },

    getAngle: function() {
      return geometryGraph.evaluate(this.rotation.angle);
    },

    getAxis: function() {
      return calc.objToVector(this.rotation.axis, geometryGraph, THREE.Vector3);
    },

  });

  return RotationSceneView;

});


// SS.WorkplaneRotationPreview = SS.InteractiveSceneView.extend({

//     initialize: function() {
//         SS.InteractiveSceneView.prototype.initialize.call(this);
//         this.on('mouseDown', this.mouseDown, this);
//         this.on('mouseUp', this.mouseUp, this);
//         this.on('mouseDrag', this.drag);
//         this.relativeAngle = 0;
//         this.angleDimensionSubview = new SS.RelativeAngleDimensionText({
//             model: this.model, 
//             parentView: this,
//             color: this.textColor});
//         this.updateAxis();
//         this.render();
//     },

//     remove: function() {
//         SS.InteractiveSceneView.prototype.remove.call(this);
//         this.off('mouseDown', this.mouseDown);
//         this.off('mouseUp', this.mouseUp);
//         this.off('mouseDrag', this.drag);
//         this.angleDimensionSubview.remove();
//     },
    
//     render: function() {
//         this.clear();
//         SS.InteractiveSceneView.prototype.render.call(this);
//         var radius = this.options.radius;
//         var circleGeom = new THREE.Geometry();
//         for (var i = 0; i <= 360 - this.relativeAngle; ++i) {
//             var angle = i/180*Math.PI;
//             var x = (radius)*Math.cos(angle);
//             var y = (radius)*Math.sin(angle);
//             circleGeom.vertices.push(new THREE.Vector3(x,y,0));
//         }
//         var circleMaterial = new THREE.LineBasicMaterial({ 
//             color: this.arrowLineColor, 
//             wireframe : true, 
//             linewidth: 1.0, 
//             opacity: this.opacity });
//         var circle = new THREE.Line(circleGeom, circleMaterial);


//         var arcGeom = new THREE.Geometry();
//         if (this.relativeAngle !== 0) {
//             var arcStartAngle = Math.min(-this.relativeAngle, 0);
//             var arcEndAngle = Math.max(-this.relativeAngle, 0);

//             if (arcStartAngle == -this.relativeAngle) {
//                 arcGeom.vertices.push(new THREE.Vector3(0,0,0))
//             }
//             for (var i = arcStartAngle; i <= arcEndAngle; ++i) {
//                 var angle = i/180*Math.PI;
//                 var x = (radius)*Math.cos(angle);
//                 var y = (radius)*Math.sin(angle);
//                 arcGeom.vertices.push(new THREE.Vector3(x,y,0));
//             }
//             if (arcEndAngle == -this.relativeAngle) {
//                 arcGeom.vertices.push(new THREE.Vector3(0,0,0));
//             }
//         }
//         var angleArc = new THREE.Line(arcGeom, circleMaterial);
                                    
//         var arrowGeometry = new THREE.CylinderGeometry(0, 0.75*this.cameraScale, 2*this.cameraScale, 3);
//         var arrowMaterials = [
//             new THREE.MeshBasicMaterial({color: this.arrowLineColor, opacity: this.opacity, wireframe: false } ),
//             new THREE.MeshBasicMaterial({color: this.arrowFaceColor, wireframe: true})
//         ];
//         var arrow = THREE.SceneUtils.createMultiMaterialObject(arrowGeometry, arrowMaterials);
//         arrow.position.x = this.options.radius;

//         this.circleAndArrow = new THREE.Object3D();
//         this.circleAndArrow.add(arrow);
//         this.circleAndArrow.add(circle);
//         this.circleAndArrow.add(angleArc);
//         this.sceneObject.add(this.circleAndArrow);
       
//         var quaternion = new THREE.Quaternion();
        
//         var angle = this.getAngle()/180*Math.PI;
//         quaternion.setFromAxisAngle(this.getAxis(), angle);

//         this.sceneObject.useQuaternion = true;
//         this.sceneObject.quaternion = quaternion;

//         // Update the angle dimension
//         var origin = SS.objToVector(this.model.node.origin);
//         this.sceneObject.position = origin;

//         if (this.showDimensionAngleText) {
//             this.angleDimensionSubview.angle = this.relativeAngle; 
//             this.angleDimensionSubview.position = this.relativeAnchorPosition;
//             this.angleDimensionSubview.render();
//         } else {
//             this.angleDimensionSubview.clear();
//             this.angleDimensionSubview.position = undefined;
//         }

//     },

//     getAngle: function() {
//         return this.model.node.hasOwnProperty('angle') ?
//             this.model.node.angle : this.model.node.parameters.angle;
//     },

//     getAxis: function() {
//         return this.model.node.hasOwnProperty('axis') ?
//             SS.objToVector(this.model.node.axis) : SS.objToVector(this.model.node.parameters);
//     },

//     updateAxis: function() {
//         // Workplane rotation
//         this.startAxis = this.getAxis().normalize();
//         if (this.model.node.hasOwnProperty('angle')) {
//             this.startAngle = this.model.node.angle;
//         } else {
//             this.startAngle = this.model.node.parameters.angle;
//         }
                
//         if (this.model.originalNode) {
//             // Rotation transform
//             this.arrowStartPosition = SS.rotateAroundAxis(this.relativeAnchorPosition, this.startAxis, this.startAngle);
//             this.arrowStartPosition.addSelf(SS.objToVector(this.model.node.origin));

//             var workplaneAxis = SS.objToVector(this.model.originalNode.workplane.axis);
//             var workplaneOrigin = SS.objToVector(this.model.originalNode.workplane.origin);
//             var workplaneAngle = this.model.originalNode.workplane.angle;

//             this.arrowStartPosition = SS.rotateAroundAxis(this.arrowStartPosition, workplaneAxis, workplaneAngle);
//             this.arrowStartPosition.addSelf(workplaneOrigin);
//         } else {
//             this.arrowStartPosition = SS.rotateAroundAxis(this.relativeAnchorPosition, this.startAxis, this.startAngle);
//             this.arrowStartPosition.addSelf(SS.objToVector(this.model.node.origin));
//         }

//         this.rotationAxis = SS.rotateAroundAxis(this.relativeRotationAxis, this.startAxis, this.startAngle);

//     },

//     mouseDown: function() {
//         this.updateAxis();
//         this.showDimensionAngleText = true;
//         if (this.model.mouseDownOnArrow) {
//             // For rotation transformer
//             this.model.mouseDownOnArrow(this.rotationAxis, this.options.index);
//         } 
//     },

//     mouseUp: function() {
//         this.relativeAngle = 0;
//         this.showDimensionAngleText = false;
//         this.render();
//     },

//     drag: function(event) {
        

//         if (this.model.originalNode) {
//             // Rotation transform
//             var transformOrigin = SS.objToVector(this.model.node.origin);
//             var transformAxis = this.rotationAxis;

//             var workplaneAxis = SS.objToVector(this.model.originalNode.workplane.axis);
//             var workplaneOrigin = SS.objToVector(this.model.originalNode.workplane.origin);
//             var workplaneAngle = this.model.originalNode.workplane.angle;
            
//             var planeOrigin = SS.rotateAroundAxis(transformOrigin, workplaneAxis, workplaneAngle).addSelf(workplaneOrigin);
//             var planeAxis = SS.rotateAroundAxis(this.rotationAxis, workplaneAxis, workplaneAngle);


//         } else {
//             // Editing workplane
//             var planeOrigin = SS.objToVector(this.model.node.origin);
//             var planeAxis = this.rotationAxis;

//         }

//         var positionOnRotationPlane = 
//             SS.sceneView.determinePositionOnPlane2(event, planeOrigin, planeAxis);
//         if (!positionOnRotationPlane) {
//             return;
//         }

//         var v1 = new THREE.Vector3().sub(positionOnRotationPlane, planeOrigin).normalize();
//         var v2 = new THREE.Vector3().sub(this.arrowStartPosition, planeOrigin).normalize();
//         var v2CrossV1 = new THREE.Vector3().cross(v2, v1);

//         var angle = parseFloat((Math.acos(v1.dot(v2))/Math.PI*180).toFixed(0));
//         if (planeAxis.dot(v2CrossV1) < 0) {
//             angle = -angle;
//         }

//         if (this.previousRelativeAngle == undefined) {
//             this.previousRelativeAngle = 0;
//         } else {
//             this.previousRelativeAngle = this.relativeAngle;
//         }

//         var round = function(value, tolerance) {
//             return Math.round(value/tolerance)*tolerance;
//         }

//         this.relativeAngle = angle;
//         if (!event.ctrlKey) {
//             this.relativeAngle = round(this.relativeAngle, 5);
//             if (this.relativeAngle === 360) {
//                 this.relativeAngle = 0;
//             }
//         }

//         if (this.relativeAngle !== this.previousRelativeAngle) {
//             var quat1 = new THREE.Quaternion().setFromAxisAngle(this.startAxis, this.startAngle/180*Math.PI);
//             var quat2 = new THREE.Quaternion().setFromAxisAngle(this.relativeRotationAxis, Math.PI*this.relativeAngle/180);
//             var quat3 = new THREE.Quaternion().multiply(quat1, quat2);
//             quat3.normalize();

//             var quaternionToAxisAngle = function(q) {
//                 var angle = 2*Math.acos(q.w);
//                 var axis = new THREE.Vector3(q.x/Math.sqrt(1-q.w*q.w),
//                                              q.y/Math.sqrt(1-q.w*q.w),
//                                              q.z/Math.sqrt(1-q.w*q.w));
//                 return {angle: angle/Math.PI*180, axis: axis};
//             }
//             var axisAngle = quaternionToAxisAngle(quat3);

//             if (this.model.node.hasOwnProperty('angle')) {
//                 // Workplane
//                 this.model.node.axis.x = parseFloat(axisAngle.axis.x.toFixed(3));
//                 this.model.node.axis.y = parseFloat(axisAngle.axis.y.toFixed(3));
//                 this.model.node.axis.z = parseFloat(axisAngle.axis.z.toFixed(3));
//                 this.model.node.angle  = parseFloat(axisAngle.angle.toFixed(2));
//             } else {
//                 // Rotation transform
//                 this.model.node.parameters.u = parseFloat(axisAngle.axis.x.toFixed(3));
//                 this.model.node.parameters.v = parseFloat(axisAngle.axis.y.toFixed(3));
//                 this.model.node.parameters.w = parseFloat(axisAngle.axis.z.toFixed(3));
//                 this.model.node.parameters.angle  = parseFloat(axisAngle.angle.toFixed(2));
   
//             }
//         }
//         this.model.trigger('beforeChange');
//         this.model.trigger('change');
//     },


// });