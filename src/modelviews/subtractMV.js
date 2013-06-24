define([
    'jquery',
    'lib/jquery.mustache',
    'calculations',
    'worldcursor',
    'scene',
    'geometrygraphsingleton',
    'modelviews/geomvertexMV', 
    'asyncAPI',
  ], 
  function(
    $, __$,
    calc,
    worldCursor,
    sceneModel,
    geometryGraph,
    GeomVertexMV,
    AsyncAPI) {

  // ---------- Editing ----------

  var EditingModel = GeomVertexMV.EditingModel.extend({

    initialize: function(options) {
      this.DOMView = EditingDOMView;
      this.SceneView = EditingSceneView;
      GeomVertexMV.EditingModel.prototype.initialize.call(this, options);
    },

    workplaneClick: function(position) {
      if (!this.vertex.proto) {
        this.tryCommit();
      }
    },

  });

  var EditingDOMView = GeomVertexMV.EditingDOMView.extend({

    render: function() {
      GeomVertexMV.EditingDOMView.prototype.render.call(this);
      var template = 
        this.beforeTemplate +
        this.afterTemplate;
      var view = this.baseView;
      this.$el.html($.mustache(template, view));
      return this;
    },

  });

  var EditingSceneView = GeomVertexMV.EditingSceneView.extend({

    render: function() {
      this.renderMesh();
    },

  });

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

  var DisplaySceneView = GeomVertexMV.DisplaySceneView.extend({

    render: function() {
      GeomVertexMV.DisplaySceneView.prototype.render.call(this);
      var that = this;
      this.createMesh(function(result) {
        that.renderMesh(result);
      });
    },

  });

  // ---------- Module ----------

  return {
    DisplayModel: DisplayModel,
    EditingModel: EditingModel,
  } 

});
