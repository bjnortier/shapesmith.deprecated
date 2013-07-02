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
      this.arrow.position = new THREE.Vector3(this.radius, 0, 0);
      this.arrow.scale = this.cameraScale;

      this.circleAndArrow = new THREE.Object3D();
      this.circleAndArrow.add(this.arrow);
      this.circleAndArrow.add(circle);
      this.circleAndArrow.add(angleArc);
      this.sceneObject.add(this.circleAndArrow);
     
      var quaternion = new THREE.Quaternion();
      
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

      this.originalAxis = calc.objToVector(this.editingVertex.transforms.rotation.axis, geometryGraph, THREE.Vector3);
      this.originalAngle = geometryGraph.evaluate(this.editingVertex.transforms.rotation.angle);
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
        var quat1 = new THREE.Quaternion().setFromAxisAngle(planeAxis, Math.PI*this.relativeAngle/180);
        var quat2 = new THREE.Quaternion().setFromAxisAngle(this.originalAxis, Math.PI*this.originalAngle/180);
        var quat3 = new THREE.Quaternion().multiplyQuaternions(quat1, quat2);
        quat3.normalize();

        var quaternionToAxisAngle = function(q) {
          var angle = 2*Math.acos(q.w);
          var axis = new THREE.Vector3(q.x/Math.sqrt(1-q.w*q.w),
                                       q.y/Math.sqrt(1-q.w*q.w),
                                       q.z/Math.sqrt(1-q.w*q.w));
          return {angle: angle/Math.PI*180, axis: axis};
        };

        var axisAngle = quaternionToAxisAngle(quat3);

        this.editingVertex.transforms.rotation.origin.x = this.center.x;
        this.editingVertex.transforms.rotation.origin.y = this.center.y;
        this.editingVertex.transforms.rotation.origin.z = this.center.z;
        this.editingVertex.transforms.rotation.axis.x = parseFloat(axisAngle.axis.x.toFixed(3));
        this.editingVertex.transforms.rotation.axis.y = parseFloat(axisAngle.axis.y.toFixed(3));
        this.editingVertex.transforms.rotation.axis.z = parseFloat(axisAngle.axis.z.toFixed(3));
        this.editingVertex.transforms.rotation.angle  = parseFloat(axisAngle.angle.toFixed(2));

        this.sceneObject.quaternion = quat1;

        this.editingVertex.trigger('change', this.editingVertex);
      }

    },

    dragEnded: function() {
      this.sceneObject.remove(this.axis);
      this.dragging = false;
      this.editingVertex.transforming = false;
      this.editingModel.tryCommit();
    },

  });

  return RotationSceneView;

});

