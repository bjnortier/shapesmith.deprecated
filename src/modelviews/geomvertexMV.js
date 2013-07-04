  define([
    'backbone',
    'jquery',
    'lib/jquery.mustache',
    'colors',
    'scene',
    'interactioncoordinator',
    'scenevieweventgenerator',
    'worldcursor',
    'calculations',
    'modelviews/vertexMV',
    'hintview',
    'selection',
    'geometrygraphsingleton',
    'asyncAPI',
    'icons',
    'latheapi/adapter',
  ], function(
    Backbone,
    $, __$,
    colors, 
    sceneModel,
    coordinator,
    sceneViewEventGenerator, 
    worldCursor, 
    calc,
    VertexMV,
    hintView,
    selection,
    geometryGraph,
    AsyncAPI,
    icons,
    latheAdapter) {

  // ---------- Common ----------

  var SceneView = VertexMV.SceneView.extend({

    initialize: function() {
      var normalColor = 0x6cbe32;
      var highlightColor = normalColor;
      var vertexMaterial = this.model.vertex.parameters.material;
      if (vertexMaterial && vertexMaterial.color) {
        normalColor = vertexMaterial.color
        highlightColor = normalColor;
      }
      this.materials = {
        normal: {
          face: new THREE.MeshLambertMaterial({
            color: normalColor,
            ambient: normalColor,
            name: 'normal.face'
          }),
          wire: new THREE.MeshBasicMaterial({
            color: normalColor,
            wireframe: true,
            linewidth: 1,
            name: 'normal.wire'
          }),
          edge: new THREE.LineBasicMaterial({
            color: normalColor,
            linewidth: 2,
            name: 'normal.edge'
          }),
        },
        implicit: {
          face: new THREE.MeshLambertMaterial({
            color: 0xff0000,
            transparent: true,
            opacity: 0,
            name: 'implicit.face'
          }),
          wire: new THREE.MeshBasicMaterial({
            color: 0x000000,
            wireframe: true,
            transparent: true,
            opacity: 0,
            name: 'implicit.wire'
          }),
          edge: new THREE.LineBasicMaterial({
            color: 0x000000,
            transparent: true,
            opacity: 0,
            linewidth: 1,
            name: 'implicit.edge'
          }),
        },
        highlight: {
          face: new THREE.MeshLambertMaterial({
            color: highlightColor,
            ambient: normalColor,
            // transparent: true,
            // opacity: 0.5,
            name: 'highlight.face'
          }),
          wire: new THREE.MeshBasicMaterial({
            color: highlightColor,
            wireframe: true,
            linewidth: 1,
            name: 'highlight.wire'
          }),
          edge: new THREE.LineBasicMaterial({
            color: highlightColor,
            linewidth: 2,
            name: 'highlight.edge'
          }),
          },
        selected: {
          face: new THREE.MeshLambertMaterial({
            color: 0x999933,
            ambient: 0xffff99,
            // transparent: true,
            // opacity: 0.5,
            name: 'selected.face'}),
          wire: new THREE.MeshBasicMaterial({
            color: 0x999933,
            wireframe: true,
            linewidth: 1,
            name: 'selected.wire'
          }),
          edge: new THREE.LineBasicMaterial({
            color: 0x999933,
            linewidth: 2,
            name: 'selected.edge'
          }),
        },
        editing: {
          face: new THREE.MeshLambertMaterial({
            color: 0x0099cc,
            transparent: true,
            opacity: 0.5,
            name: 'editing.face'
          }),
          wire: new THREE.MeshBasicMaterial({
            color: 0x007088,
            wireframe: true,
            linewidth: 1,
            name: 'editing.wire'
            }),
          edge: new THREE.LineBasicMaterial({
            color: 0x007088,
            linewidth: 2,
            name: 'editing.edge'
          }),
        },

      }
      
      VertexMV.SceneView.prototype.initialize.call(this);
      sceneModel.view.on('cameraMoveStopped', this.updateScreenBox, this);
    },

    remove: function() {
      VertexMV.SceneView.prototype.remove.call(this);
      sceneModel.view.off('cameraMoveStopped', this.updateScreenBox, this);
    },

    findObjects: function(sceneObjects) {
      var lines = [], meshes = [];
      var searchFn = function(obj) {
        if (obj.children.length) {
          obj.children.map(searchFn);
        }
        if (obj instanceof THREE.Mesh) {
          meshes.push(obj);
        } else if (obj instanceof THREE.Line) {
          lines.push(obj);
        }
      }
      sceneObjects.forEach(function(obj) {
        searchFn(obj);
      });
      return {lines: lines, meshes: meshes};
    },

    updateMaterials: function(key) {
      var objects = this.findObjects([this.sceneObject]);
      var that = this;
      objects.lines.forEach(function(line) {
        line.material = that.materials[key].edge;
      });
      objects.meshes.forEach(function(mesh) {
        if (mesh.material) {
          if (mesh.material.name.endsWith('face')) {
            mesh.material = that.materials[key].face;
          } else if (mesh.material.name.endsWith('wire')) {
            mesh.material = that.materials[key].wire;
          }
        }
      });
      sceneModel.view.updateScene = true;
    },

    calculateScreenBox: function(boundingBox, sceneWidth, sceneHeight, camera) {
      var boundMin = boundingBox.min;
      var boundMax = boundingBox.max;
      var corners = [
        new THREE.Vector3(boundMin.x, boundMin.y, boundMin.z), // 000 
        new THREE.Vector3(boundMin.x, boundMin.y, boundMax.z), // 001
        new THREE.Vector3(boundMin.x, boundMax.y, boundMin.z), // 010
        new THREE.Vector3(boundMin.x, boundMax.y, boundMax.z), // 011 
        new THREE.Vector3(boundMax.x, boundMin.y, boundMin.z), // 100 
        new THREE.Vector3(boundMax.x, boundMin.y, boundMax.z), // 101 
        new THREE.Vector3(boundMax.x, boundMax.y, boundMin.z), // 110 
        new THREE.Vector3(boundMax.x, boundMax.y, boundMax.z), // 111 
      ]

      var screenBox = new THREE.Box2();

      var that = this;
      corners.forEach(function(corner) {
        var screenPos = calc.toScreenCoordinates(sceneWidth, sceneHeight, camera, corner);
        screenBox.expandByPoint(new THREE.Vector2(
          Math.min(screenBox.min.x, screenPos.x - 5),
          Math.min(screenBox.min.y, screenPos.y - 5)));
        screenBox.expandByPoint(new THREE.Vector2(
          Math.max(screenBox.max.x, screenPos.x + 5),
          Math.max(screenBox.max.y, screenPos.y + 5)));
      })

      return screenBox;
    },

    updateScreenBox: function(camera) {
      if (this.preventScreenBoxUpdate) {
        return;
      }

      var sceneWidth = $('#scene').innerWidth();
      var sceneHeight = $('#scene').innerHeight();

      var that = this;
      var updateScreenBoxForObj = function(obj) {
        if (obj.geometry) {
          obj.screenBox = that.calculateScreenBox(
            obj.geometry.boundingBox, sceneWidth, sceneHeight, camera);
        }
        if (obj.children && (obj.children.length > 0)) {
          obj.children.map(updateScreenBoxForObj);
        }
      }
      updateScreenBoxForObj(this.sceneObject);
    },

    polygonsToMesh: function(polygons) {
      var geometry = new THREE.Geometry();
      var indices = [];
      var box3 = new THREE.Box3();
      polygons.forEach(function(coordinates, i) {

        // Remove degenerates
        var coordinates = coordinates.reduce(function(acc, c, i) {
          if (i === 0) {
            return acc.concat(c);
          } else if (new THREE.Vector3().subVectors(c,acc[acc.length-1]).length() > 0.000000001) {
            return acc.concat(c);
          } else {
            return acc;
          }
        }, []);

        var polygonIndices = coordinates.map(function(coordinate) {
          var vertex = new THREE.Vector3(coordinate.x, coordinate.y, coordinate.z);
          box3.expandByPoint(vertex);
          return geometry.vertices.push(vertex) - 1;
        });
        if (coordinates.length < 3) {
          // Ignore degenerates
          // throw Error('invalid polygon');
        } else if (coordinates.length === 3) {
          geometry.faces.push(new THREE.Face3(polygonIndices[0],polygonIndices[1],polygonIndices[2]));
        } else if (coordinates.length === 4) {
          geometry.faces.push(new THREE.Face4(polygonIndices[0],polygonIndices[1],polygonIndices[2],polygonIndices[3]));
        } else {
          // Only support cnvex polygons
          geometry.faces.push(new THREE.Face3(polygonIndices[0],polygonIndices[1],polygonIndices[2]));
          for (var i = 2; i < coordinates.length -1; ++i) {
            geometry.faces.push(new THREE.Face3(polygonIndices[0], polygonIndices[0]+i,polygonIndices[0]+i+1));
          }
        }
        
        indices.push(polygonIndices);

      })

      geometry.computeFaceNormals();
      return {
        geometry: geometry, 
        indices: indices,
        box3: box3,
      };
    },

    createMesh: function(callback) {
      var that = this;
      latheAdapter.generate(
        that.model.vertex,
        function(err, result) {

        if (err) {
          console.error('no mesh', that.model.vertex.id);
          return;
        } else if(callback) {
          callback(result);
        }
        
      });
    },

    renderMesh: function(result) {
      if (this.model.inContext) {
        this.clear();
        var toMesh = this.polygonsToMesh(result.polygons);

        this.extents = {
          center: toMesh.box3.center(),
          dx: toMesh.box3.max.x - toMesh.box3.center().x,
          dy: toMesh.box3.max.y - toMesh.box3.center().y,
          dz: toMesh.box3.max.z - toMesh.box3.center().z,
        };

        var faceGeometry = toMesh.geometry;
        var faceMaterial;
        if (this.model.vertex.editing) {
          if (this.model.vertex.transforming) {
            faceMaterial = this.materials.selected.face;
          } else {
            faceMaterial = this.materials.editing.face;
          }
        } else {
          faceMaterial = this.materials.normal.face;
        }
        var meshObject = new THREE.Mesh(faceGeometry, faceMaterial);
        this.sceneObject.add(meshObject);
        sceneModel.view.updateScene = true;
      }
    },

  });


  // ---------- Editing ----------

  var EditingModel = VertexMV.EditingModel.extend({

    initialize: function(options) {
      this.parentModel = options.parentModel;
      VertexMV.EditingModel.prototype.initialize.call(this, options);
      this.hintView = hintView;
      

      worldCursor.on('positionChanged', this.workplanePositionChanged, this);
      worldCursor.on('click', this.workplaneClick, this);
      worldCursor.on('dblclick', this.workplaneDblClick, this);
      coordinator.on('keyup', this.keyup, this);
      sceneViewEventGenerator.on('sceneViewClick', this.sceneViewClick, this);
      sceneViewEventGenerator.on('sceneViewDblClick', this.sceneViewDblClick, this);
      geometryGraph.on('vertexReplaced', this.vertexReplaced, this);
    },

    destroy: function() {
      VertexMV.EditingModel.prototype.destroy.call(this);
      this.hintView.clear();
      worldCursor.off('positionChanged', this.workplanePositionChanged, this);
      worldCursor.off('click', this.workplaneClick, this);
      worldCursor.off('dblclick', this.workplaneDblClick, this);
      coordinator.off('keyup', this.keyup, this);
      sceneViewEventGenerator.off('sceneViewClick', this.sceneViewClick, this);
      sceneViewEventGenerator.off('sceneViewDblClick', this.sceneViewDblClick, this);
      geometryGraph.off('vertexReplaced', this.vertexReplaced, this);
    },  

    keyup: function(event) {
      // Delete
      if (!this.vertex.implicit && (event.keyCode === 46)) {
        this.tryDelete();
      }
    },

    // Update the workplane is it changes during editing
    vertexReplaced: function(original, replacement) {
      if (replacement.type === 'workplane') {
        this.vertex.workplane = calc.copyObj(replacement.workplane);
      }
    },

  })

  var EditingDOMView = VertexMV.EditingDOMView.extend({

    initialize: function(options) {
      VertexMV.EditingDOMView.prototype.initialize.call(this, options);
      if (options.appendDomElement) {
        options.appendDomElement.append(this.$el);
      } else if (options.replaceDomElement) {
        options.replaceDomElement.replaceWith(this.$el);
      }
      
    },

    render: function() {
      this.beforeTemplate = 
        '{{^implicit}}' +
        '<div class="title">' + 
        '<div class="icon24">{{{icon}}}</div>' +
        '<div class="name">{{name}}</div>' + 
        '<div class="actions">' +
          '{{#isTopLevel}}' +
          // '<i class="showhide icon-eye-open"></i>' +
          '<i class="delete icon-remove"></i>' +
          '{{/isTopLevel}}' +
        '</div>' +
        '{{/implicit}}' +
        '</div>' + 
        '<div class="children {{id}}"></div>';
      this.afterTemplate = this.model.vertex.implicit ? '' : 
        '<div>axisx <input class="field axisx" type="text" value="{{axisx}}"></input></div>' +
        '<div>axisy <input class="field axisy" type="text" value="{{axisy}}"></input></div>' +
        '<div>axisz <input class="field axisz" type="text" value="{{axisz}}"></input></div>' + 
        '<div>angle <input class="field angle" type="text" value="{{angle}}"></input></div>';

      var rotation = this.model.vertex.transforms.rotation;
      this.baseView = {
        id: this.model.vertex.id,
        name : this.model.vertex.name,
        implicit: this.model.vertex.implicit,
        icon: icons[this.model.vertex.type],
        isTopLevel: !geometryGraph.parentsOf(this.model.vertex).length,
        axisx: rotation.axis.x,
        axisy: rotation.axis.y,
        axisz: rotation.axis.z,
        angle: rotation.angle,
      }
    },

    remove: function() {
      VertexMV.EditingDOMView.prototype.remove.call(this);
    },

    update: function() {
      var that = this;
      var rotation = this.model.vertex.transforms.rotation;
      ['x', 'y', 'z'].forEach(function(key) {
        that.$el.find('.field.axis' + key).val(rotation.axis[key]);
      });
      this.$el.find('.field.angle').val(rotation.angle);
    },

    updateFromDOM: function() {
      var that = this;
      ['x', 'y', 'z'].forEach(function(key) {
        try {
          var expression = that.$el.find('.field.axis' + key).val();
          that.model.vertex.transforms.rotation.axis[key] = expression;
        } catch(e) {
          console.error(e);
        }
      });
      try {
        var expression = that.$el.find('.field.angle').val();
        that.model.vertex.transforms.rotation.angle = expression;
      } catch(e) {
        console.error(e);
      }
      this.model.vertex.trigger('change', this.model.vertex);
    }

  });
  

  var EditingSceneView = SceneView.extend({

    initialize: function() {
      SceneView.prototype.initialize.call(this);
      this.model.vertex.on('change', this.renderIfInContext, this);
    },

    remove: function() {
      SceneView.prototype.remove.call(this);
      this.model.vertex.off('change', this.renderIfInContext, this);
    },

  });

  // ---------- Display ----------

  var DisplayModel = VertexMV.DisplayModel.extend({ 

    initialize: function(options) {
      this.DOMView = DisplayDOMView;
      VertexMV.DisplayModel.prototype.initialize.call(this, options);
      coordinator.on('keyup', this.keyup, this);
      worldCursor.on('click', this.workplaneClick, this);
    },

    destroy: function() {
      VertexMV.DisplayModel.prototype.destroy.call(this);
      coordinator.off('keyup', this.keyup, this);
      worldCursor.off('click', this.workplaneClick, this);
    },


    canSelect: function() {
      return !this.vertex.implicit && this.inContext && !this.selected;
    },

    keyup: function(event) {
      if (!this.vertex.implicit && (event.keyCode === 46)) {
        if (this.get('selected')) {
          this.tryDelete();
        }
      }
    },

    workplaneClick: function(event) {
      selection.deselectAll();
    },

    getExtents: function() {
      return this.sceneView.extents;
    },

  })

  var DisplayDOMView = VertexMV.DisplayDOMView.extend({

    className: 'vertex display',

    initialize: function(options) {
      VertexMV.DisplayDOMView.prototype.initialize.call(this, options);
      this.$el.addClass(this.model.vertex.name);  
      if (options.appendDomElement) {
        options.appendDomElement.append(this.$el);
      } else if (options.replaceDomElement) {
        options.replaceDomElement.replaceWith(this.$el);
      }
    },

    render: function() {
      if (!this.model.vertex.implicit) {
        var parameters = this.model.vertex.parameters;
        var color = (parameters.material && parameters.material.color) || '#6cbe32';
        var hasExplicitChildren = !!_.find(
          geometryGraph.childrenOf(this.model.vertex),
          function(child) {
            return !child.implicit && !(child.type === 'variable');
          });
        var view = {
          id:   this.model.vertex.id,
          name: this.model.vertex.name,
          type: this.model.vertex.type,
          color: color,
          icon: icons[this.model.vertex.type],
          isTopLevel: !geometryGraph.parentsOf(this.model.vertex).length,
          hasExplicitChildren: hasExplicitChildren,
        }
        var template = 
          '<div class="title">' + 
            '{{#hasExplicitChildren}}' +
            '<i class="dive icon-chevron-sign-down"></i>' +
            '<i class="ascend icon-chevron-sign-up"></i>' +
            '{{/hasExplicitChildren}}' +  
            '<div class="icon24" style="fill: {{color}}; stroke: {{color}};">{{{icon}}}</div>' +
            '<div class="name">{{name}}</div>' + 
            '<div class="actions">' +
              '{{#isTopLevel}}' +
              // '<i class="showhide icon-eye-open"></i>' +
              '<i class="delete icon-remove"></i>' +
              '{{/isTopLevel}}' +
            '</div>' +
          '</div>' +
          '<div class="children {{id}}"></div>';
        this.$el.html($.mustache(template, view));
        return this;
      } else {
        this.$el.html();
      }
    },    

    events: {
      'click > .title .icon24'   : 'clickTitle',
      'click > .title .name'   : 'clickTitle',
      'dblclick > .title .name': 'dblclickTitle',
      'dblclick > .title .icon24': 'dblclickTitle',
      'click > .title > .actions > .delete'  : 'delete',
      'click > .title > .dive'  : 'clickDive',
      'click > .title > .ascend'  : 'clickAscend',
    },

    clickDive: function(event) {
      event.stopPropagation();
      selection.deselectAll();
      this.trigger('dive');
      this.dive();
    },

    clickAscend: function(event) {
      event.stopPropagation();
      selection.deselectAll();
      this.trigger('ascend');
      this.ascend();
    },

    ascend: function() {
      this.$el.removeClass('dived');
      var parameters = this.model.vertex.parameters;
      var color = (parameters.material && parameters.material.color) || '#6cbe32';
      this.$el.find('> .title > .icon24').attr(
        "style", 
        $.mustache("fill: {{color}}; stroke: {{color}}", {color: color}));
    },

    dive: function() {
      this.$el.addClass('dived');
      var color = "#bbb";
      this.$el.find('> .title > .icon24').attr(
        "style", 
        $.mustache("fill: {{color}}; stroke: {{color}}", {color: color}));
    },

    clickTitle: function(event) {
      event.stopPropagation();
      if (this.model.canSelect()) {
        if (event.shiftKey) {
          selection.addToSelection(this.model.vertex.id);
        } else {
          selection.selectOnly(this.model.vertex.id);
        }
      }
    },

    dblclickTitle: function(event) {
      event.stopPropagation();
      if (!geometryGraph.isEditing() && this.model.inContext &&  !this.model.vertex.implicit) {
        selection.deselectAll();
        AsyncAPI.edit(this.model.vertex);
      }
    },

    delete: function(event) {
      event.stopPropagation();
      this.model.tryDelete();
    },

  });

  var DisplaySceneView = SceneView.extend({

    initialize: function() {
      SceneView.prototype.initialize.call(this);
      this.on('mouseenterfirst', this.highlight, this);
      this.on('mouseleavefirst', this.unhighlight, this);
      this.on('click', this.click, this);
      this.on('dblclick', this.dblclick, this);
      this.model.vertex.on('change', this.renderIfInContext, this);
      this.model.on('change:selected', this.updateSelected, this);
    },

    remove: function() {
      SceneView.prototype.remove.call(this);
      this.off('mouseenterfirst', this.highlight, this);
      this.off('mouseleavefirst', this.unhighlight, this);
      this.off('click', this.click, this);
      this.off('dblclick', this.dblclick, this);
      this.model.vertex.off('change', this.renderIfInContext, this);
      this.model.off('change:selected', this.updateSelected, this);
    },

    isClickable: function() {
      return true;
    },

    updateSelected: function() {
      if (this.model.get('selected')) {
        this.updateMaterials('selected');
      } else {
        this.updateMaterials('normal');
      }
    },

    highlight: function() {
      this.highlighted = true;
      if (!geometryGraph.isEditing()) {
        if (this.model.vertex.implicit) {
          // this.updateMaterials('normal');
        } else {
          this.updateMaterials('highlight');
        }
      }
      // var implicitViews = this.findImplicitDescendantSceneviews(this.model.vertex);
      // implicitViews.forEach(function(view) {
      //   view.highlight && view.highlight();
      // })
    },

    unhighlight: function() {
      delete this.highlighted;
      if (!geometryGraph.isEditing()) {
        if (this.model.vertex.implicit) {
          // this.updateMaterials('implicit');
        } else {
          this.updateSelected();
        }
      }
      // var implicitViews = this.findImplicitDescendantSceneviews(this.model.vertex);
      // implicitViews.forEach(function(view) {
      //   view.unhighlight();
      // })
    },

    click: function(event) {
      var vertexToSelect, parents;
      if (this.model.canSelect()) {
        vertexToSelect = this.model.vertex;
      } else if (this.model.vertex.implicit) {
        // Select the parent that is in context
        // var findNonImplicitContextParent = function(vertex) {
        //   var parents = _.uniq(geometryGraph.parentsOf(vertex));
        //   if (parents.length === 1) {
        //     if (parents[0].implicit) {
        //       return findNonImplicitContextParent(parents[0]);
        //     } else {
        //       return parents[0];
        //     }
        //   } else {
        //     return undefined;
        //   }
        // }
        // vertexToSelect = findNonImplicitContextParent(this.model.vertex);
      } 
      if (vertexToSelect) {
        if (event.shiftKey) {
          selection.addToSelection(vertexToSelect.id);
        } else {
          selection.selectOnly(vertexToSelect.id);
        }
      }
    },

    dblclick: function(event) {
      if (!geometryGraph.isEditing() && !this.model.vertex.implicit) {
        selection.deselectAll();
        AsyncAPI.edit(this.model.vertex);
      }
    },

    isDraggable: function() {
      // Can translate if this is the only selected object
      var selected = selection.getSelected();
      var draggable = ((selected.length === 1) && (selected[0] === this.model.vertex.id));
      return draggable;
    },

  });

  // ---------- Module ----------

  return {
    SceneView    : SceneView,
    EditingModel   : EditingModel,
    EditingDOMView   : EditingDOMView,
    EditingSceneView : EditingSceneView,
    DisplayModel   : DisplayModel, 
    DisplayDOMView   : DisplayDOMView,
    DisplaySceneView : DisplaySceneView,
  }

});
