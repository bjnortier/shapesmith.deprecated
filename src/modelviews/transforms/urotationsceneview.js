define([
    './rotationsceneview'
  ], function(RotationSceneView) {


  var URotationSceneView = RotationSceneView.extend({

    arrowLineColor: 0x333399,
    arrowFaceColor: 0x6666cc,
    textColor: '#6666cc',

    initialize: function(options) {
      // this.relativeAnchorPosition = new THREE.Vector3(0,this.options.radius,0);
      if (!this.model.vertex.transforms.rotation) {
        this.model.vertex.transforms.rotation = {
          axis: {
            x: 1, y: 0, z:0,
          },
          angle: 0
        };
      }
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
    },

    relativeRotationAxis: new THREE.Vector3(1,0,0),

  });

  return URotationSceneView;

});