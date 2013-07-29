define([
  'calculations',
  'scene',
  'geometrygraphsingleton',
  'settings',
  'modelviews/workplaneMV',
  'modelviews/geomvertexMV',
  ], function(calc, sceneModel, geometryGraph, settings, WorkplaneMV, GeomVertexMV) {

  var ZAnchor = GeomVertexMV.EditingSceneView.extend({

    initialize: function(options) {
      this.vertex = options.vertex;
      this.origin = options.origin;

      this.rayOrigin = calc.objToVector(
        this.origin, 
        geometryGraph, 
        THREE.Vector3);

      GeomVertexMV.EditingSceneView.prototype.initialize.call(this);
      this.render();
      this.on('dragStarted', this.dragStarted, this);
      this.on('drag', this.drag, this);
      this.on('dragEnded', this.dragEnded, this);
    },

    remove: function() {
      GeomVertexMV.EditingSceneView.prototype.remove.call(this);
      this.off('dragStarted', this.dragStarted, this);
      this.off('drag', this.drag, this);
      this.off('dragEnded', this.dragEnded, this);
    },

    render: function() {
      GeomVertexMV.EditingSceneView.prototype.render.call(this);
      var color = this.highlightColor || this.selectedColor || this.color || 0x00dd00;

      this.pointSceneObject = THREE.SceneUtils.createMultiMaterialObject(
        new THREE.CylinderGeometry(0, 0.75, 1.5, 3), 
        [
          new THREE.MeshBasicMaterial({color: 0x993333, opacity: 0.5, wireframe: false } ),
          new THREE.MeshBasicMaterial({color: 0xcc6666, wireframe: true})
        ]);
      var pointPosition = calc.objToVector(this.origin, geometryGraph, THREE.Vector3);

      var zOffset = 1.5*this.cameraScale.x;

      // this.pointSceneObject.position = pointPosition;
      this.pointSceneObject.position.z = zOffset;
      this.pointSceneObject.scale = this.cameraScale;
      this.pointSceneObject.rotation.x = Math.PI/2;
      this.sceneObject.add(this.pointSceneObject);

      if (this.showHeightLine) {
        var lineGeometry = new THREE.Geometry();
        lineGeometry.vertices.push(pointPosition.clone().setZ(-1000));
        lineGeometry.vertices.push(pointPosition.clone().setZ(1000));
        var line = new THREE.Line(lineGeometry, new THREE.LineBasicMaterial({color: 0xff6666}));
        this.sceneObject.add(line);
      }

    },

    updateScaledObjects: function() {
      this.pointSceneObject.scale = this.cameraScale;
    },

    isDraggable: function() {
      return true;
    },

    dragStarted: function() {
      this.showHeightLine = true;
    },

    dragEnded: function() {
      this.showHeightLine = false;
      this.model.vertex.trigger('change', this.model.vertex);
    },

    drag: function(workplanePosition, intersection, event) {
      this.dragging = true;

      var sceneElement = $('#scene');
      var camera = sceneModel.view.camera;
      var mouseRay = calc.mouseRayForEvent(sceneElement, camera, event);

      if (true) {
        var rayDirection = new THREE.Vector3(0,0,1);
        var ray = new THREE.Ray(this.rayOrigin, rayDirection);
        var absolutePositionOnNormal = calc.positionOnRay(mouseRay, ray);
        var h = absolutePositionOnNormal.z - this.rayOrigin.z;
      }

      // Apply Workplane
      // var workplaneOrigin = calc.objToVector(
      //   this.vertex.workplane.origin, 
      //   geometryGraph, 
      //   THREE.Vector3);
      // var workplaneAxis = calc.objToVector(
      //   this.vertex.workplane.axis, 
      //   geometryGraph, 
      //   THREE.Vector3);
      // var workplaneAngle = geometryGraph.evaluate(this.vertex.workplane.angle);

      // var rayOriginUsingWorkplane = calc.rotateAroundAxis(rayOrigin, workplaneAxis, workplaneAngle);
      // rayOriginUsingWorkplane.add(workplaneOrigin);

      // var rayDirection = new THREE.Vector3(0,0,1);
      // var rayDirectionUsingWorkplane = calc.rotateAroundAxis(rayDirection, workplaneAxis, workplaneAngle);

      // var ray = new THREE.Ray(rayOriginUsingWorkplane, rayDirectionUsingWorkplane);
      // var absolutePositionOnNormal = calc.positionOnRay(mouseRay, ray);

      // // Back into local coordinates
      // var positionOnNormalInLocalCoords = 
      //   calc.rotateAroundAxis(absolutePositionOnNormal, workplaneAxis, -workplaneAngle);

      // var h = positionOnNormalInLocalCoords.z - rayOrigin.z;

      var grid = settings.get('gridsize');
      this.origin.z = Math.round(parseFloat(h/grid))*grid;

      this.vertex.trigger('change', this.vertex);
    },

  });
  
  return ZAnchor;

});
