define([
    'jquery',
    'lib/jquery.mustache',
    'calculations',
    'worldcursor',
    'scene',
    'geometrygraphsingleton',
    'modelviews/geomvertexMV', 
    'modelviews/pointMV', 
    'modelviews/heightOnWidthDepthAnchorView',
    'modelviews/widthDepthCornerView',
    'asyncAPI',
    'latheapi/normalize',
    
  ], 
  function(
    $, __$,
    calc,
    worldCursor,
    sceneModel,
    geometryGraph,
    GeomVertexMV,
    PointMV,
    EditingHeightAnchor,
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
      this.sceneObject.add(cube);
    },

  }

  // ---------- Editing ----------

  var EditingModel = GeomVertexMV.EditingModel.extend({

    initialize: function(options) {
      this.DOMView = EditingDOMView;
      this.SceneView = EditingSceneView;
      GeomVertexMV.EditingModel.prototype.initialize.call(this, options);

      this.corner = geometryGraph.childrenOf(this.vertex).filter(function(v) {
        return v.type === 'point'
      })[0];

      // Create the child models
      var that = this;
      if (this.vertex.proto) {
        this.stage = 0;
        this.updateHint();
      } else {
        this.originalImplicitChildren = geometryGraph.childrenOf(this.vertex).filter(function(v) {
          return v.implicit;
        })
        this.editingImplicitChildren = [];
        this.editingImplicitChildren = this.originalImplicitChildren.map(function(child, i) {
          return AsyncAPI.edit(child);
        })

        that.views.push(new EditingHeightAnchor({
          model: this, 
          heightKey: 'height',
          corner: this.corner,
          vertex: this.vertex,
        }));

        that.views.push(new WidthDepthCornerView({
          model: this,
        }))
      }

    },

    workplanePositionChanged: function(position, event) {
      if (this.vertex.proto) {
        if (this.stage === 0) {
          this.corner.parameters.coordinate.x = position.x;
          this.corner.parameters.coordinate.y = position.y;
          this.corner.parameters.coordinate.z = position.z;
          this.corner.trigger('change', this.corner);
        } else if (this.stage === 1) {  
          this.vertex.parameters.width = position.x - this.corner.parameters.coordinate.x;
          this.vertex.parameters.depth = position.y - this.corner.parameters.coordinate.y;
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

          this.heightAnchor = new EditingHeightAnchor({
            model: this, 
            heightKey: 'height',
            corner: this.corner,
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
        '<div>height <input class="field height" type="text" value="{{height}}"></input></div>';
        this.afterTemplate;
        
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
        that.$el.find('.field.' + key).val(
          that.model.vertex.parameters[key]);
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

  });

  var DisplaySceneView = GeomVertexMV.DisplaySceneView.extend(SceneViewMixin).extend({

    render: function() {
      GeomVertexMV.DisplaySceneView.prototype.render.call(this);
      var that = this;
      this.createMesh(function(result) {
        that.renderMesh(result);
      });
    },

  })



  // ---------- Module ----------

  return {
    EditingModel: EditingModel,
    DisplayModel: DisplayModel,
  }

});