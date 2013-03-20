define([
        'jquery',
        'lib/jquery.mustache',
        'src/calculations',
        'src/worldcursor',
        'src/scene',
        'src/geometrygraphsingleton',
        'src/modelviews/geomvertexMV', 
        'src/asyncAPI',
        'src/lathe/adapter',
        'requirejsplugins/text!/ui/images/icons/subtract.svg',
    ], 
    function(
        $, __$,
        calc,
        worldCursor,
        sceneModel,
        geometryGraph,
        GeomVertexMV,
        AsyncAPI,
        latheAdapter,
        icon) {

    // ---------- Editing ----------

     var EditingModel = GeomVertexMV.EditingModel.extend({

        initialize: function(options) {
            this.displayModelConstructor = DisplayModel;
            GeomVertexMV.EditingModel.prototype.initialize.call(this, options);
        },

        addTreeView: function() {
            var domView = new EditingDOMView({model: this});
            this.views.push(domView);
            return domView;
        },

    });

    var EditingDOMView = GeomVertexMV.EditingDOMView.extend({

        render: function() {
            var template = 
                '<table><tr>' +
                '<td class="title">' + 
                '<div class="icon24">' + icon + '</div>' +
                '<div class="name">{{name}}</div>' + 
                '<div class="delete"></div>' + 
                '</td></tr><tr><td>' +
                '</div>' + 
                '<div class="children"></div>' +
                '</td></tr></table>';
            var view = {
                name      : this.model.vertex.name,
                height    : this.model.vertex.parameters.height,
            };
            this.$el.html($.mustache(template, view));
            return this;
        },

    });


    // ---------- Display ----------

    var DisplayModel = GeomVertexMV.DisplayModel.extend({

        initialize: function(options) {
            this.displayModelConstructor = DisplayModel;
            this.editingModelConstructor = EditingModel;
            GeomVertexMV.DisplayModel.prototype.initialize.call(this, options);
            this.sceneView = new DisplaySceneView({model: this});
            this.views.push(this.sceneView);
            this.vertex.on('change', this.updateCumulativeArea, this);
        },

        destroy: function() {
            GeomVertexMV.DisplayModel.prototype.destroy.call(this);
            this.vertex.off('change', this.updateCumulativeArea, this);
        },

        icon: icon,

    });

    var DisplaySceneView = GeomVertexMV.DisplaySceneView.extend({

        initialize: function() {
            GeomVertexMV.DisplaySceneView.prototype.initialize.call(this);
        },

        render: function() {
            GeomVertexMV.DisplaySceneView.prototype.render.call(this);

            var that = this;
            latheAdapter.generate(
                that.model.vertex,
                function(err, result) {

                if (err) {
                    console.error('no mesh', that.model.vertex.id);
                    return;
                }

                that.clear();
                var toMesh = that.polygonsToMesh(result.polygons);
                var faceGeometry = toMesh.geometry;
                var meshObject = THREE.SceneUtils.createMultiMaterialObject(faceGeometry, [
                    that.materials.normal.face, 
                ]);
                that.sceneObject.add(meshObject);
                sceneModel.view.updateScene = true;
            });
        },

        remove: function() {
            GeomVertexMV.DisplaySceneView.prototype.remove.call(this);
        },

    })



    // ---------- Module ----------

    return {
        DisplayModel: DisplayModel,
        EditingModel: EditingModel,
    } 

});
