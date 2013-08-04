define([
    'calculations',
    'settings',
    'asyncAPI',
    'scene',
    'geometrygraphsingleton',
    './sceneview',
    'modelviews/modelgraph',
  ],
  function(
    calc,
    settings, 
    AsyncAPI, 
    sceneModel,
    geometryGraph,
    TransformSceneView,
    modelGraph) {

  var TranslateSceneView = TransformSceneView.extend({

    greyLineColor: 0xcc9999,
    greyFaceColor: 0xcc9999,
    highlightFaceColor: 0xcc3333,
    highlightLineColor: 0xcc3333,

    initialize: function() {
      var extents = this.model.selectedModel.getExtents();
      this.center = extents.center;
      TransformSceneView.prototype.initialize.call(this);
    },

    render: function() {
      TransformSceneView.prototype.render.call(this);

      var extents = this.model.selectedModel.getExtents();

      var boundaryGeometry = new THREE.Geometry();
      var buffer = 5;
      boundaryGeometry.vertices.push(new THREE.Vector3(
        + extents.dx, 
        + extents.dy, 
        0).add(new THREE.Vector3(extents.center.x, extents.center.y, 0)));
      boundaryGeometry.vertices.push(new THREE.Vector3(
        - extents.dx, 
        + extents.dy, 
        0).add(new THREE.Vector3(extents.center.x, extents.center.y, 0)));
      boundaryGeometry.vertices.push(new THREE.Vector3(
        - extents.dx, 
        - extents.dy, 
        0).add(new THREE.Vector3(extents.center.x, extents.center.y, 0)));
      boundaryGeometry.vertices.push(new THREE.Vector3(
        + extents.dx, 
        - extents.dy, 
        0).add(new THREE.Vector3(extents.center.x, extents.center.y, 0)));
      boundaryGeometry.vertices.push(boundaryGeometry.vertices[0]);
      this.boundary = new THREE.Line(boundaryGeometry, 
        new THREE.LineBasicMaterial({ color: this.greyLineColor }));

      this.sceneObject.add(this.boundary);

      var cornerGeometry = new THREE.Geometry();
      cornerGeometry.vertices.push(new THREE.Vector3(0,0,0));
      cornerGeometry.vertices.push(new THREE.Vector3(-buffer/2, 0, 0));
      cornerGeometry.vertices.push(new THREE.Vector3(-buffer/2, 1, 0));
      cornerGeometry.vertices.push(new THREE.Vector3(1, 1, 0));
      cornerGeometry.vertices.push(new THREE.Vector3(0, -buffer/2, 0));
      cornerGeometry.vertices.push(new THREE.Vector3(1, -buffer/2, 0));
      cornerGeometry.vertices.push(new THREE.Vector3(1, 1, 0));
      cornerGeometry.faces.push(new THREE.Face4(0,1,2,3));
      cornerGeometry.faces.push(new THREE.Face4(0,6,5,4));
      cornerGeometry.computeFaceNormals();

      var that = this;
      this.corners = [
        {
          rotation: 0,
          offset: new THREE.Vector3(extents.dx, extents.dy, 0)
        },
        {
          rotation: Math.PI/2,
          offset: new THREE.Vector3(-extents.dx, extents.dy, 0)
        },
        {
          rotation: Math.PI,
          offset: new THREE.Vector3(-extents.dx, -extents.dy, 0)
        },
        {
          rotation: 3*Math.PI/2,
          offset: new THREE.Vector3(extents.dx, -extents.dy, 0)
        },

      ].map(function(rotationAndOffset) {
        var corner = THREE.SceneUtils.createMultiMaterialObject(
          cornerGeometry,
          [
            new THREE.MeshBasicMaterial({color: that.greyFaceColor, transparent: true, opacity: 0.5, side: THREE.DoubleSide } ),
          ]);
        corner.position = new THREE.Vector3(extents.center.x, extents.center.y, 0)
          .add(rotationAndOffset.offset);
        corner.rotation.z = rotationAndOffset.rotation;
        that.sceneObject.add(corner);
        corner.scale = that.cameraScale;
        return corner;
        
      });
    },

    updateCameraScale: function() {
      TransformSceneView.prototype.updateCameraScale.call(this);

      if (this.corners) {
        var that = this;
        this.corners.forEach(function(corner) {
          corner.scale = that.cameraScale;
        });
      }

    },

    dragStarted: function() {
      this.model.hideOtherViews(this);

      this.originalVertex = this.model.vertex;
      this.originalVertex.transforming = true;
      this.editingVertex = AsyncAPI.edit(this.model.vertex);
      this.editingModel = modelGraph.get(this.editingVertex.id);
      this.dragging = true;
    },

    drag: function(position) {
      var extents = this.model.selectedModel.getExtents();
      if (!this.initialPosition) {
        this.initialScale = this.model.vertex.transforms.scale.factor;
        this.initialPosition = position;
        this.centerOnWorkplane = new THREE.Vector3(extents.center.x, extents.center.y, 0);
        this.initialDistance = new THREE.Vector3().subVectors(
          this.initialPosition, this.centerOnWorkplane).length();
        console.log(this.initialDistance);
      } else {

        var distance = new THREE.Vector3().subVectors(
          position, this.centerOnWorkplane).length();
        var scale = Math.round(distance/this.initialDistance*10)/10;

        var that = this;
        [
          new THREE.Vector3(extents.center.x, extents.center.y, 0).add(
            new THREE.Vector3(extents.dx*scale, extents.dy*scale, 0)),
          new THREE.Vector3(extents.center.x, extents.center.y, 0).add(
            new THREE.Vector3(-extents.dx*scale, extents.dy*scale, 0)),
          new THREE.Vector3(extents.center.x, extents.center.y, 0).add(
            new THREE.Vector3(-extents.dx*scale, -extents.dy*scale, 0)),
          new THREE.Vector3(extents.center.x, extents.center.y, 0).add(
            new THREE.Vector3(extents.dx*scale, -extents.dy*scale, 0)),
        ].map(function(p, i) {
          that.corners[i].position = p;
          that.boundary.geometry.vertices[i] = p;
        });
        this.boundary.geometry.vertices[4] = this.boundary.geometry.vertices[0]; 
        this.boundary.geometry.verticesNeedUpdate = true;
        sceneModel.view.updateScene = true;

        // The scale factor parameter is relative to the previous scale
        this.editingModel.scale(
          this.center,
          Math.round(this.initialScale*scale*10)/10);
      }
    
    },

    dragEnded: function() {
      this.dragging = false;
      this.editingVertex.transforming = false;
      this.editingModel.tryCommit();
    },

    mouseenter: function() {
      this.sceneObject.add(this.axis);
    },

    mouseleave: function() {
      this.sceneObject.remove(this.axis);
    },

  });

  return TranslateSceneView;

});
