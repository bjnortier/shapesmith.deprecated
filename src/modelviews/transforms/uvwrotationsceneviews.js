define([
    './rotationsceneview'
  ], function(RotationSceneView) {

  var U = RotationSceneView.extend({

    arrowLineColor: 0x333399,
    arrowFaceColor: 0x6666cc,
    textColor: '#6666cc',

    initialize: function(options) {
      var extents = this.model.selectedModel.getExtents();
      this.center = extents.center;
      this.radius = Math.sqrt(extents.dx*extents.dx + extents.dy*extents.dy + extents.dz*extents.dz);
      this.rotation = this.model.vertex.transforms.rotation;
      this.arrowStartPosition = new THREE.Vector3(0, this.radius, 0);

      RotationSceneView.prototype.initialize.call(this, options);
    },
    
    render: function() {
      RotationSceneView.prototype.render.call(this);
      this.circleAndArrow.rotation.y = Math.PI/2;
      this.circleAndArrow.rotation.x = Math.PI/2;
    },

    relativeRotationAxis: new THREE.Vector3(1,0,0),

  });

  var V = RotationSceneView.extend({

    arrowLineColor: 0x339933,
    arrowFaceColor: 0x66cc66,
    textColor: '#66cc66',

    initialize: function(options) {
      var extents = this.model.selectedModel.getExtents();
      this.center = extents.center;
      this.radius = Math.sqrt(extents.dx*extents.dx + extents.dy*extents.dy + extents.dz*extents.dz);
      this.rotation = this.model.vertex.transforms.rotation;
      this.arrowStartPosition = new THREE.Vector3(0, 0, this.radius);

      RotationSceneView.prototype.initialize.call(this, options);
    },
    
    render: function() {
      RotationSceneView.prototype.render.call(this);
      this.circleAndArrow.rotation.x = -Math.PI/2;
      this.circleAndArrow.rotation.z = -Math.PI/2;
    },

    relativeRotationAxis: new THREE.Vector3(0,1,0),

  });

  var W = RotationSceneView.extend({

    arrowLineColor: 0x993333,
    arrowFaceColor: 0xcc6666,
    textColor: '#cc6666',

    initialize: function(options) {
      var extents = this.model.selectedModel.getExtents();
      this.center = extents.center;
      this.radius = Math.sqrt(extents.dx*extents.dx + extents.dy*extents.dy + extents.dz*extents.dz);
      this.rotation = this.model.vertex.transforms.rotation;
      this.arrowStartPosition = new THREE.Vector3(this.radius, 0, 0);

      RotationSceneView.prototype.initialize.call(this, options);
    },
    
    render: function() {
      RotationSceneView.prototype.render.call(this);
    },

    relativeRotationAxis: new THREE.Vector3(0,0,1),

  });

  return {
    U: U,
    V: V,
    W: W,
  };

});