define([
    'jquery',
    'lib/jquery.mustache',
    'calculations',
    'worldcursor',
    'scene',
    'selection',
    'geometrygraphsingleton',
    'modelviews/geomvertexMV', 
    'modelviews/pointMV', 
    'modelviews/heightanchorview',
    'modelviews/heightOnWidthDepthAnchorView',
    'modelviews/widthdepthcornerview',
    'asyncAPI',
    'latheapi/normalize',
    
  ], 
  function(
    $, __$,
    calc,
    worldCursor,
    sceneModel,
    selection,
    geometryGraph,
    GeomVertexMV,
    PointMV,
    OriginHeightAnchor,
    CornerEditingHeightAnchor,
    WidthDepthCornerView,
    AsyncAPI,
    Normalize) {

  // ---------- Common ----------

  var SceneViewMixin = {

    render: function() {
      GeomVertexMV.SceneView.prototype.render.call(this);

      var points = geometryGraph.childrenOf(this.model.vertex).filter(function(v) {
        return v.type === 'point'
      });
      if (points.length !== 1) {
        return;
      }

      var materials;
      if (this.model.vertex.editing) {
        materials = [
          this.materials.editing.face, 
          this.materials.editing.wire
        ]
      } else {
        materials = [
          this.materials.normal.face, 
          this.materials.normal.wire
        ]
      }

      var dimensions = Normalize.normalizeVertex(this.model.vertex);
      var position = new THREE.Vector3(dimensions.x, dimensions.y, dimensions.z);

      var cube = THREE.SceneUtils.createMultiMaterialObject(
        new THREE.CubeGeometry(dimensions.w, dimensions.d, dimensions.h),
        materials);
      cube.position = position.add(new THREE.Vector3(
        dimensions.w/2, dimensions.d/2, dimensions.h/2));
      cube.position = position.add(new THREE.Vector3(
        dimensions.dx, dimensions.dy, dimensions.dz));
      this.sceneObject.add(cube);
    },

  }

  // ---------- Editing ----------

  var EditingModel = GeomVertexMV.EditingModel.extend({

    initialize: function(options) {
      this.DOMView = EditingDOMView;
      this.SceneView = EditingSceneView;
      GeomVertexMV.EditingModel.prototype.initialize.call(this, options);

      this.origin = geometryGraph.childrenOf(this.vertex).filter(function(v) {
        return v.type === 'point'
      })[0];

      // Create the child models
      var that = this;
      if (this.vertex.proto) {
        this.stage = 0;
        this.updateHint();
      } else {
        this.originalImplicitChildren = [this.origin];
        this.origin = AsyncAPI.edit(this.origin);
        this.editingImplicitChildren = [this.origin];

        if (!this.vertex.transforming) {
          that.views.push(new OriginHeightAnchor({
            model: that, 
            heightKey: 'height',
            pointVertex: this.origin,
          }));

          that.views.push(new CornerEditingHeightAnchor({
            model: this, 
            heightKey: 'height',
            origin: this.origin,
            vertex: this.vertex,
          }));
          that.views.push(new WidthDepthCornerView({
            model: this,
          }))
        }
      }

    },

    translate: function(translation) {
      if (!this.startOrigin) {
        this.startOrigin = {
          x: this.origin.parameters.coordinate.x,
          y: this.origin.parameters.coordinate.y,
          z: this.origin.parameters.coordinate.z,
        }
      }
      this.origin.parameters.coordinate.x = this.startOrigin.x + translation.x;
      this.origin.parameters.coordinate.y = this.startOrigin.y + translation.y;
      this.origin.parameters.coordinate.z = this.startOrigin.z + translation.z;
      this.origin.trigger('change', this.origin);
    },

    workplanePositionChanged: function(position, event) {
      if (this.vertex.proto) {
        if (this.stage === 0) {
          this.origin.parameters.coordinate.x = position.x;
          this.origin.parameters.coordinate.y = position.y;
          this.origin.parameters.coordinate.z = position.z;
          this.origin.trigger('change', this.origin);
        } else if (this.stage === 1) {  
          this.vertex.parameters.width = position.x - this.origin.parameters.coordinate.x;
          this.vertex.parameters.depth = position.y - this.origin.parameters.coordinate.y;
          this.vertex.trigger('change', this.vertex);
        } else if (this.stage === 2) {
          this.heightAnchor.drag(position, undefined, event);
        }
      }
    },

    sceneViewClick: function(viewAndEvent) {
      if (this.vertex.proto) {
        this.workplaneClick(worldCursor.lastPosition);
      }
    },

    workplaneClick: function(position) {
      if (this.vertex.proto) {
        if (this.stage === 0) {
          ++this.stage;

          this.widthDepthCornerView = new WidthDepthCornerView({
            model: this, 
          });
          this.widthDepthCornerView.dragStarted();
          this.widthDepthCornerView.isDraggable = function() {
            return false;
          };
          this.views.push(this.widthDepthCornerView);

          this.updateHint();

        } else if (this.stage === 1) {
          ++this.stage;

          this.heightAnchor = new CornerEditingHeightAnchor({
            model: this, 
            heightKey: 'height',
            origin: this.origin,
            vertex: this.vertex,
          });
          this.heightAnchor.dragStarted();
          this.heightAnchor.isDraggable = function() {
            return false;
          };
          this.views.push(this.heightAnchor);
          delete this.activePoint;
          this.updateHint();

        } else if (this.stage === 2) {
          this.tryCommit();
        }
      } else {
        this.tryCommit();
      }
    },

    addPoint: function(position) {
      var point = geometryGraph.addPointToParent(this.vertex);
      this.activePoint = point;
      this.activePoint.active = true;
      this.workplanePositionChanged(position);
    },

    updateHint: function() {
      if (this.vertex.proto) {
        switch(this.stage) {
          case 0: 
            this.hintView.set('Click to add a corner.');
            break;
          case 1:
            this.hintView.set('Click to add a corner diagonally opposite.');
            break;
          case 2:
            this.hintView.set('Click to set the height.');
            break;
        }
      }
    },

  });

  var EditingDOMView = GeomVertexMV.EditingDOMView.extend({

    render: function() {
      GeomVertexMV.EditingDOMView.prototype.render.call(this);
      var template = 
        this.beforeTemplate +
        '<div>width  <input class="field width" type="text" value="{{width}}"></input></div>' +
        '<div>depth  <input class="field depth" type="text" value="{{depth}}"></input></div>' +
        '<div>height <input class="field height" type="text" value="{{height}}"></input></div>' +
        this.afterTemplate;
        
      var translate = this.model.vertex.transforms.translate || {x:0, y:0, z:0};
      var view = _.extend(this.baseView, {
        width  : this.model.vertex.parameters.width,
        depth  : this.model.vertex.parameters.depth,
        height : this.model.vertex.parameters.height,
      });
      this.$el.html($.mustache(template, view));
      return this;
    },

    update: function() {
      var that = this;
      ['width', 'depth', 'height'].forEach(function(key) {
        that.$el.find('.field.' + key).val(that.model.vertex.parameters[key]);
      });
      var translate = this.model.vertex.transforms.translate || {x:0, y:0, z:0};
      ['x', 'y', 'z'].forEach(function(key) {
        that.$el.find('.field.d' + key).val(translate[key]);
      });
    },

    updateFromDOM: function() {
      var that = this;
      ['width', 'depth', 'height'].forEach(function(key) {
        try {
          var expression = that.$el.find('.field.' + key).val();
          that.model.vertex.parameters[key] = expression;
        } catch(e) {
          console.error(e);
        }
      });
      this.model.vertex.trigger('change', this.model.vertex);
    }

  }); 


  var EditingSceneView = GeomVertexMV.EditingSceneView.extend(SceneViewMixin);

  // ---------- Display ----------

  var DisplayModel = GeomVertexMV.DisplayModel.extend({

    initialize: function(options) {
      this.SceneView = DisplaySceneView;
      GeomVertexMV.DisplayModel.prototype.initialize.call(this, options);
    },

    destroy: function() {
      GeomVertexMV.DisplayModel.prototype.destroy.call(this);
    },

    getExtents: function() {
      var origin = geometryGraph.childrenOf(this.vertex).filter(function(v) {
        return v.type === 'point'
      })[0];

      var width = geometryGraph.evaluate(this.vertex.parameters.width);
      var depth = geometryGraph.evaluate(this.vertex.parameters.depth);
      var height = geometryGraph.evaluate(this.vertex.parameters.height);

      var translate = this.vertex.transforms.translate || {x:0, y:0, z:0};
      var dx = geometryGraph.evaluate(translate.x);
      var dy = geometryGraph.evaluate(translate.y);
      var dz = geometryGraph.evaluate(translate.z);

      var p1 = calc.objToVector(origin.parameters.coordinate, geometryGraph, THREE.Vector3);
      var p2 = new THREE.Vector3().addVectors(p1, new THREE.Vector3(width, depth, 0));
      var center = new THREE.Vector3().addVectors(p1, p2).multiplyScalar(0.5).add(
        new THREE.Vector3(dx, dy, dz));

      return {
        center: center,
        dx: Math.abs(width/2),
        dy: Math.abs(depth/2),
        dz: height,
      }
    },

  });

  var DisplaySceneView = GeomVertexMV.DisplaySceneView.extend(SceneViewMixin).extend({

    render: function() {
      GeomVertexMV.DisplaySceneView.prototype.render.call(this);
      var that = this;
      this.createMesh(function(result) {
        that.renderMesh(result);
      });
    },

    dragStarted: function() {
      console.log('dragStarted');
    },

    drag: function(position) {
      console.log('drag', position);
    },

  })

  // ---------- Module ----------

  return {
    EditingModel: EditingModel,
    DisplayModel: DisplayModel,
  }

});