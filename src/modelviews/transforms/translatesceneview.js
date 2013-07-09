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
      var axisGeom = new THREE.Geometry();
      axisGeom.vertices.push(new THREE.Vector3(0, 0, 5000));
      axisGeom.vertices.push(new THREE.Vector3(0, 0, -5000));

      this.axis = new THREE.Line(axisGeom, 
        new THREE.LineBasicMaterial({ color: this.greyLineColor }));

      var extents = this.model.selectedModel.getExtents();
      this.axis.position = extents.center;

      TransformSceneView.prototype.initialize.call(this);
    },

    render: function() {
      TransformSceneView.prototype.render.call(this);

      this.arrow = new THREE.Object3D();
      this.arrow.add(new THREE.Mesh(
        new THREE.CylinderGeometry(0, 0.75, 1.5, 3), 
        new THREE.MeshBasicMaterial({color: this.greyFaceColor, transparent: true, opacity: 0.5})));

      this.arrow.scale = this.cameraScale;
      this.sceneObject.add(this.arrow);

      var extents = this.model.selectedModel.getExtents();
      this.arrow.position = extents.center.clone().add(
        new THREE.Vector3(0, 0, extents.dz));
      this.arrow.children[0].position.z = 2;
      this.arrow.children[0].rotation.x = Math.PI/2; 
    },

    updateCameraScale: function() {
      TransformSceneView.prototype.updateCameraScale.call(this);

      if (this.arrow) {
        this.arrow.scale = this.cameraScale;
      }

      if (!this.dragging) {
        var extents = this.model.selectedModel.getExtents();
        this.initialPosition = extents.center.clone().add(
          new THREE.Vector3(0, 0, extents.dz + 2*this.cameraScale.z));
      }
    },

    highlight: function() {
      TransformSceneView.prototype.highlight.call(this);
      this.sceneObject.add(this.axis);
    },

    unhighlight: function() {
      TransformSceneView.prototype.unhighlight.call(this);
      this.sceneObject.remove(this.axis); 
    },

    dragStarted: function() {
      this.model.hideOtherViews(this);

      this.originalVertex = this.model.vertex;
      this.originalVertex.transforming = true;
      this.editingVertex = AsyncAPI.edit(this.model.vertex);
      this.editingModel = modelGraph.get(this.editingVertex.id);
      this.dragging = true;
      this.sceneObject.add(this.axis);
    },

    drag: function(position, intersection, event) {
      var sceneElement = $('#scene');
      var camera = sceneModel.view.camera;
      var mouseRay = calc.mouseRayForEvent(sceneElement, camera, event);

      var extents = this.model.selectedModel.getExtents();
      var rayOrigin = calc.objToVector(extents.center, geometryGraph, THREE.Vector3);
      var rayDirection = new THREE.Vector3(0,0,1);
      var ray = new THREE.Ray(rayOrigin, rayDirection);

      var positionOnNormal = calc.positionOnRay(mouseRay, ray);

      this.arrow.position = positionOnNormal;
      var diff = new THREE.Vector3().subVectors(positionOnNormal, this.initialPosition);
      var grid = settings.get('gridsize');
      var translation = new THREE.Vector3(Math.round(diff.x/grid) * grid,
                                          Math.round(diff.y/grid) * grid,
                                          Math.round(diff.z/grid) * grid).add(this.initialTranslation);
      
      this.editingModel.translate(translation);
    },

    dragEnded: function() {
      this.sceneObject.remove(this.axis);
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
