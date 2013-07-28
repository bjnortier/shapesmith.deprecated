define([
    'jquery',
    'lib/jquery.mustache',
    'colors',
    'calculations', 
    'interactioncoordinator', 
    'scene',
    'settings',
    'geomnode',
    'geometrygraphsingleton',
    'modelviews/vertexMV',
    'selection',
    'asyncAPI',
    'icons',
  ], function(
    $, _$,
    colors,
    calc, 
    coordinator, 
    sceneModel,
    settings,
    geomNode,
    geometryGraph,
    VertexMV,
    selection,
    AsyncAPI,
    icons) {

  var currentDisplayModel = undefined;

  var EditingModel = VertexMV.EditingModel.extend({

    initialize: function(options) {
      this.SceneView = GridView;
      VertexMV.EditingModel.prototype.initialize.call(this, options);
      this.views.push(new EditingDOMView({model: this}));
      coordinator.on('sceneClick', this.tryCommit, this);
    },

    destroy: function() {
      VertexMV.EditingModel.prototype.destroy.call(this);
      coordinator.off('sceneClick', this.tryCommit, this);
    },
    
  });

  var DisplayModel = VertexMV.DisplayModel.extend({

    initialize: function(options) {
      this.displayModelConstructor = DisplayModel;
      this.SceneView = GridView;
      VertexMV.DisplayModel.prototype.initialize.call(this, options);

      currentDisplayModel = this;

      settings.on('change:gridsize', this.gridSizeChanged, this);
      coordinator.on('mousemove', this.mousemove, this);
      coordinator.on('sceneClick', this.workplaneClick, this);
      coordinator.on('sceneDblClick', this.workplaneDblClick, this);
      
      geometryGraph.on('vertexAdded', this.vertexAdded, this);
      geometryGraph.on('vertexRemoved', this.vertexRemoved, this);
      geometryGraph.on('vertexReplaced', this.vertexReplaced, this);


      this.views.push(new DisplayDOMView({model: this}));
    },

    destroy: function() {
      VertexMV.DisplayModel.prototype.destroy.call(this);
      settings.off('change:gridsize', this.gridSizeChanged, this);
      coordinator.off('mousemove', this.mousemove, this);
      coordinator.off('sceneClick', this.workplaneClick, this);
      coordinator.off('sceneDblClick', this.workplaneDblClick, this);
      geometryGraph.off('vertexAdded', this.vertexAdded, this);
      geometryGraph.off('vertexRemoved', this.vertexRemoved, this);
      geometryGraph.off('vertexReplaced', this.vertexReplaced, this);

    },

    workplaneClick: function(event) {
      VertexMV.eventProxy.trigger('workplaneClick');
    },

    workplaneDblClick: function(eventst) {
      VertexMV.eventProxy.trigger('workplaneDblClick');
    },

    mousemove: function(event) {
      VertexMV.eventProxy.trigger('workplanePositionChanged', event);
    },

    edit: function() {
      geometryGraph.edit(this.vertex);
    },

    pushVertex: function(vertex) {
      this.savedWorkplane = this.vertex.workplane;
      this.vertex.workplane = vertex.workplane;
    },

    popVertex: function(vertex) {
      this.vertex.workplane = this.savedWorkplane;
      this.savedWorkplane = undefined;
    },

    vertexAdded: function(vertex) {
      this.trigger('change');
    },

    vertexRemoved: function(vertex) {
      this.trigger('change');
    },

    // Push and pop the editing node's workplane
    // when it's being editied
    vertexReplaced: function(original, replacement) {
      if (replacement.category !== 'geometry') {
        this.trigger('change');
        return;
      }
      if (!replacement.proto && replacement.editing && !replacement.implicit) {
        this.pushVertex(replacement);
      } else if(!original.proto && original.editing && !original.implicit) {
        this.popVertex(replacement);
      }
      this.trigger('change');
    },

    gridSizeChanged: function() {
      this.trigger('change');
    },

  });

  var DisplayDOMView = VertexMV.DisplayDOMView.extend({

    render: function() {
      this.$el.html($.mustache(
        '<div class="icon32">{{{icon}}}</div>',
        {
          icon: icons.workplane,
        }));
      $('#workplane-settings').append(this.$el);
    },

    events: {
      'click' : 'click',
    },

    click: function(event) {
      event.stopPropagation();
      if (!geometryGraph.isEditing()) {
        AsyncAPI.edit(this.model.vertex);
      }
    },

  });

  var EditingDOMView = VertexMV.EditingDOMView.extend({

    render: function() {
      var template = 
        '<div>origin</div>' +   
        '<div>' +
          'x <input class="field originx" type="text" value="{{originx}}"></input>' +
          'y <input class="field originy" type="text" value="{{originy}}"></input>' +
          'z <input class="field originz" type="text" value="{{originz}}"></input>' +
        '</div>' +
        '<div>axis</div>' +   
        '<div>' +
          'x <input class="field axisx" type="text" value="{{axisx}}"></input>' +
          'y <input class="field axisy" type="text" value="{{axisy}}"></input>' +
          'z <input class="field axisz" type="text" value="{{axisz}}"></input>' +
        '</div>' +
        '<div>angle <input class="field angle" type="text" value="{{angle}}"></input></div>';
      var parameters = this.model.vertex.workplane;
      var view = {
        originx : parameters.origin.x,
        originy : parameters.origin.y,
        originz : parameters.origin.z,
        axisx : parameters.axis.x,
        axisy : parameters.axis.y,
        axisz : parameters.axis.z,
        angle: parameters.angle,
      };
      this.$el.html($.mustache(template, view));
      $('#workplane-settings').append(this.$el);
      return this;
    },

    update: function() {
      var parameters = this.model.vertex.workplane;
      ['x', 'y', 'z'].forEach(function(key) {
        this.$el.find('.origin' + key).val(parameters.origin[key]);
        this.$el.find('.axis' + key).val(parameters.axis[key]);
      }, this);
      this.$el.find('.angle').val(parameters.angle);
    },

    updateFromDOM: function() {
      var parameters = this.model.vertex.workplane;
      ['x', 'y', 'z'].forEach(function(key) {
        var expression;
        try {
          expression = this.$el.find('.field.origin' + key).val();
          parameters.origin[key] = expression;

          expression = this.$el.find('.field.axis' + key).val();
          parameters.axis[key] = expression;

        } catch(e) {
          console.error(e);
        }
      }, this);

      try {
        var expression = this.$el.find('.field.angle').val();
        parameters.angle = expression;
      } catch(e) {
        console.error(e);
      }
      this.model.vertex.trigger('change', this.model.vertex);

    },

  })

  var GridView = VertexMV.SceneView.extend({

    initialize: function() {
      VertexMV.SceneView.prototype.initialize.call(this);
      this.model.on('change', this.render, this);
    },

    remove: function() {
      VertexMV.SceneView.prototype.remove.call(this);
      this.model.off('change', this.render, this);
    },
    
    render: function() {
      VertexMV.SceneView.prototype.render.call(this);

      var grid = settings.get('gridsize');
      var boundary = Math.floor(grid*10)*10;

      var majorGridLineGeometry = new THREE.Geometry();
      var majorMaterialInside = new THREE.LineBasicMaterial({
        opacity: 0.5, 
        color: colors.workplane.majorGridLine});
      majorGridLineGeometry.vertices.push(new THREE.Vector3(-Math.floor(boundary/grid)*grid, 0, 0));
      majorGridLineGeometry.vertices.push(new THREE.Vector3(Math.ceil(boundary/grid)*grid, 0, 0));

      for (var x = -Math.floor(boundary/grid); x <= Math.ceil(boundary/grid); ++x) {
        if ((x % 10 === 0) && (x !== 0)) {
          var line = new THREE.Line(majorGridLineGeometry, majorMaterialInside);
          line.position.x = x*grid;
          line.rotation.z = 90 * Math.PI / 180;
          this.sceneObject.add(line);
        }
      }

      for (var y = -Math.floor(boundary/grid); y <= Math.ceil(boundary/grid); ++y) {
        if ((y % 10 === 0) && (y !== 0)) {
          var line = new THREE.Line(majorGridLineGeometry, majorMaterialInside);
          line.position.y = y*grid;
          this.sceneObject.add(line);
        }
      }
      
      if (geometryGraph.isEditing()) {
        var minorGridLineGeometry = new THREE.Geometry();
        var minorMaterialInside = new THREE.LineBasicMaterial({ 
          color: colors.workplane.minorGridLine, 
          opacity: 0.1, 
          transparent: true });

        minorGridLineGeometry.vertices.push(new THREE.Vector3(-Math.floor(boundary/grid)*grid, 0, 0));
        minorGridLineGeometry.vertices.push(new THREE.Vector3(Math.ceil(boundary/grid)*grid, 0, 0));

        for (var x = -Math.floor(boundary/grid); x <= Math.ceil(boundary/grid); ++x) {
          if (x % 10 !== 0) {
            var line = new THREE.Line(minorGridLineGeometry, minorMaterialInside);
            line.position.x = x*grid;
            line.rotation.z = 90 * Math.PI / 180;
            this.sceneObject.add(line);
          }
        }

        for (var y = -Math.floor(boundary/grid); y <= Math.ceil(boundary/grid); ++y) {
          if (y % 10 !== 0) {
            var line = new THREE.Line(minorGridLineGeometry, minorMaterialInside);
            line.position.y = y*grid;
            this.sceneObject.add(line);
          }
        }
      }

      this.addAxes();

      this.sceneObject.position.add(
        calc.objToVector(
          this.model.vertex.workplane.origin, 
          geometryGraph, 
          THREE.Vector3));

    },

    addAxes: function() {

      var axes = [new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry(), 
                  new THREE.Geometry(), new THREE.Geometry(), new THREE.Geometry()];
      axes[0].vertices.push(new THREE.Vector3(0, 0, 0));
      axes[0].vertices.push(new THREE.Vector3(5000, 0, 0));

      axes[1].vertices.push(new THREE.Vector3(0, 0, 0));
      axes[1].vertices.push(new THREE.Vector3(0, 5000, 0));

      axes[2].vertices.push(new THREE.Vector3(0, 0, 0));
      axes[2].vertices.push(new THREE.Vector3(0, 0, 5000));

      axes[3].vertices.push(new THREE.Vector3(0, 0, 0));
      axes[3].vertices.push(new THREE.Vector3(-5000, 0, 0));

      axes[4].vertices.push(new THREE.Vector3(0, 0, 0));
      axes[4].vertices.push(new THREE.Vector3(0, -5000, 0));

      axes[5].vertices.push(new THREE.Vector3(0, 0, 0));
      axes[5].vertices.push(new THREE.Vector3(0, 0, -5000));

      this.sceneObject.add(new THREE.Line(axes[0], 
          new THREE.LineBasicMaterial({ color: 0x0000ff }))); 
      this.sceneObject.add(new THREE.Line(axes[1], 
          new THREE.LineBasicMaterial({ color: 0x00ff00 })));
      this.sceneObject.add(new THREE.Line(axes[2], 
          new THREE.LineBasicMaterial({ color: 0xff0000 })));
      this.sceneObject.add(new THREE.Line(axes[3], 
          new THREE.LineBasicMaterial({ color: 0x6666cc })));
      this.sceneObject.add(new THREE.Line(axes[4], 
          new THREE.LineBasicMaterial({ color: 0x66cc66 })));
      this.sceneObject.add(new THREE.Line(axes[5], 
          new THREE.LineBasicMaterial({ color: 0xcc6666 })));

    },

  });

  return {
    EditingModel: EditingModel,
    DisplayModel: DisplayModel,
    getCurrent  : function() { return currentDisplayModel; },
  }

});