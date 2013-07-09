define([
    './rotationsceneview'
  ], function(RotationSceneView) {

  var U = RotationSceneView.extend({

    greyLineColor: 0x3333cc,
    greyFaceColor: 0x6666cc,
    highlightFaceColor: 0x3333cc,
    highlightLineColor: 0x0000ff,

    initialize: function(options) {
      var extents = this.model.selectedModel.getExtents();
      this.center = extents.center;
      this.radius = Math.sqrt(extents.dx*extents.dx + extents.dy*extents.dy + extents.dz*extents.dz) + 5;
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

    greyLineColor: 0x33cc33,
    greyFaceColor: 0x66cc66,
    highlightFaceColor: 0x33cc33,
    highlightLineColor: 0x00ff00,

    initialize: function(options) {
      var extents = this.model.selectedModel.getExtents();
      this.center = extents.center;
      this.radius = Math.sqrt(extents.dx*extents.dx + extents.dy*extents.dy + extents.dz*extents.dz) + 5;
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

    greyLineColor: 0xcc3333,
    greyFaceColor: 0xcc6666,
    highlightFaceColor: 0xcc3333,
    highlightLineColor: 0xff0000,

    initialize: function(options) {
      var extents = this.model.selectedModel.getExtents();
      this.center = extents.center;
      this.radius = Math.sqrt(extents.dx*extents.dx + extents.dy*extents.dy + extents.dz*extents.dz) + 5;
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