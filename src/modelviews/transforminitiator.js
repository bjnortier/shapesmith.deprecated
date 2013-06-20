define([
    'backbone',
    'selection',
    'scene',
    'scenevieweventgenerator',
    'geometrygraphsingleton',
    'asyncAPI',
    'modelviews/modelgraph',
    'calculations',
    'settings',
  ], 
  function(
    Backbone, 
    selection, 
    sceneModel,
    sceneViewEventGenerator,
    geometryGraph,
    AsyncAPI,
    modelGraph,
    calc,
    settings) {

  var Model = Backbone.Model.extend({

    initialize: function() {
      this.sceneViews = [];
      selection.on('selected', this.selected, this);
      selection.on('deselected', this.deselected, this);
    },

    deselected: function() {
      this.sceneViews.forEach(function(view) {
        view.remove();
      })
      this.sceneViews = [];
    },

    selected: function(_, selected) {
      if (selected.length === 1) {
        this.vertex = geometryGraph.vertexById(selected[0]);
        this.selectedModel = modelGraph.get(selected[0]);

        this.selectedModel.sceneView.on('dragStarted', this.dragStarted, this);
        this.selectedModel.sceneView.on('dragEnded', this.dragEnded, this);
        this.selectedModel.sceneView.on('drag', this.drag, this);

        this.sceneViews = [
          new ZPlusSceneView({model: this}),
          // new ZMinSceneView({model: this}),
        ]
      } else {
        this.sceneViews.forEach(function(view) {
          view.remove();
        })
        this.sceneViews = [];

        this.selectedModel.sceneView.on('dragStarted', this.dragStarted, this);
        this.selectedModel.sceneView.on('dragEnded', this.dragEnded, this);
        this.selectedModel.sceneView.on('drag', this.drag, this);
        this.selectedModel = undefined;
      }
    },

    dragStarted: function() {
      this.initialTranslation = calc.objToVector(
        this.vertex.transforms.translate || {x:0,y:0,z:0}, 
        geometryGraph, 
        THREE.Vector3);
      console.log('initialTranslation', this.initialTranslation);
      this.originalVertex = this.vertex;
      this.originalVertex.transforming = true;
      this.editingVertex = AsyncAPI.edit(this.vertex);
      this.editingModel = modelGraph.get(this.editingVertex.id);
    },

    drag: function(position) {
      this.sceneViews.forEach(function(view) {
        view.remove();
      })
      this.sceneViews = [];

      if (!this.initialPosition) {
        this.initialPosition = position;
      }
      var diff = new THREE.Vector3().subVectors(position, this.initialPosition);
      var grid = settings.get('gridsize');
      var translation = new THREE.Vector3(Math.round(diff.x/grid) * grid,
                                          Math.round(diff.y/grid) * grid,
                                          Math.round(diff.z/grid) * grid).add(this.initialTranslation);

      this.editingModel.translate(translation);
    },

    dragEnded: function() {
      this.initialPosition = undefined;
      this.dragging = false;
      this.editingVertex.transforming = false;
      this.editingModel.tryCommit();
    },

  })

  var SceneView = Backbone.View.extend({

    initialize: function() {
      this.scene = sceneModel.view.scene;
      this.initialTranslation = calc.objToVector(this.model.vertex.transforms.translate || {x:0,y:0,z:0}, geometryGraph, THREE.Vector3);
      this.updateCameraScale();
      this.render();
      sceneModel.view.on('cameraMoved', this.cameraMoved, this);
      sceneModel.view.on('cameraMoveStopped', this.cameraMoveStopped, this);
      this.on('dragStarted', this.dragStarted, this);
      this.on('dragEnded', this.dragEnded, this);
      this.on('drag', this.drag, this);
    },

    remove: function() {
      this.scene.remove(this.sceneObject);
      sceneViewEventGenerator.deregister(this);
      sceneModel.view.updateScene = true;
      sceneModel.view.off('cameraMoved', this.cameraMoved, this);
      sceneModel.view.off('cameraMoveStopped', this.cameraMoveStopped, this);
      this.off('dragStarted', this.dragStarted, this);
      this.off('dragEnded', this.dragEnded, this);
      this.off('drag', this.drag, this);
    },

    render: function() {
      this.clear();

      this.arrow = new THREE.Object3D();
      this.arrow.add(THREE.SceneUtils.createMultiMaterialObject(
        new THREE.CylinderGeometry(0, 0.75, 1.5, 3), 
        [
          new THREE.MeshBasicMaterial({color: this.faceColor, opacity: 0.5, wireframe: false } ),
          new THREE.MeshBasicMaterial({color: this.edgeColor, wireframe: true})
        ]));

      this.arrow.scale = this.cameraScale;
      this.sceneObject.add(this.arrow);
      
    },

    clear: function() {
      if (this.sceneObject) {
        this.scene.remove(this.sceneObject);
        sceneViewEventGenerator.deregister(this);
      }

      this.sceneObject = new THREE.Object3D();
      this.hiddenSelectionObject = new THREE.Object3D();
      this.scene.add(this.sceneObject);
      sceneViewEventGenerator.register(this);
      sceneModel.view.updateScene = true;
    },

    isClickable: function() {
      return false;
    },

    isDraggable: function() {
      return !this.dragging;
    },

    cameraMoved: function() {
      this.updateCameraScale();
      sceneModel.view.updateScene = true;
    },

    updateCameraScale: function() {
      var camera = sceneModel.view.camera;
      var cameraDistance = camera.position.length();
      var newScale = cameraDistance/150;
      this.cameraScale = new THREE.Vector3(newScale, newScale, newScale);

      this.arrow && (this.arrow.scale = this.cameraScale);
    },

    dragStarted: function() {
      this.originalVertex = this.model.vertex;
      this.originalVertex.transforming = true;
      this.editingVertex = AsyncAPI.edit(this.model.vertex);
      this.editingVertex.transforms.translate = {
        x: 0, y:0, z: 0,
      }
      this.editingModel = modelGraph.get(this.editingVertex.id);
      this.dragging = true;
    },

    drag: function(position) {
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
      this.dragging = false;
      this.editingVertex.transforming = false;
      this.editingModel.tryCommit();
    },

  });

  var ZPlusSceneView = SceneView.extend({

    faceColor: 0x993333,
    edgeColor: 0xcc6666,

    render: function() {
      SceneView.prototype.render.call(this);

      var extents = this.model.selectedModel.getExtents();
      this.arrow.position = extents.center.clone().add(
        new THREE.Vector3(0, 0, extents.dz));
      this.arrow.children[0].position.z = 2;
      this.arrow.children[0].rotation.x = Math.PI/2; 
    },

    updateCameraScale: function() {
      SceneView.prototype.updateCameraScale.call(this);
      if (!this.dragging) {
        var extents = this.model.selectedModel.getExtents();
        this.initialPosition = extents.center.clone().add(
          new THREE.Vector3(0, 0, extents.dz + 2*this.cameraScale.z));
      }
    },
  });

  var ZMinSceneView = SceneView.extend({

    faceColor: 0x993333,
    edgeColor: 0xcc6666,

    render: function() {
      SceneView.prototype.render.call(this);
      
      var extents = this.model.selectedModel.getExtents();
      this.arrow.position = extents.center.clone().add(
        new THREE.Vector3(0, 0, 0));
      this.arrow.children[0].position.z = -2;
      this.arrow.children[0].rotation.x = -Math.PI/2; 
    },

    updateCameraScale: function() {
      SceneView.prototype.updateCameraScale.call(this);
      if (!this.dragging) {
        var extents = this.model.selectedModel.getExtents();
        this.initialPosition = extents.center.clone().add(
          new THREE.Vector3(0, 0, - 2*this.cameraScale.z));
      }
    },
  });

  return new Model();


});