define([
        'jquery',
        'lib/jquery.mustache',
        'src/calculations',
        'src/worldcursor',
        'src/scene',
        'src/geometrygraphsingleton',
        'src/vertexMV',
        'src/geomvertexMV', 
        'src/pointMV', 
        'src/heightanchorview',
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
        VertexMV,
        GeomVertexMV,
        PointMV,
        EditingHeightAnchor,
        AsyncAPI,
        latheAdapter,
        icon) {

    // ---------- Editing ----------

     var EditingModel = GeomVertexMV.EditingModel.extend({

        initialize: function(options) {
            this.displayModelConstructor = DisplayModel;
            GeomVertexMV.EditingModel.prototype.initialize.call(this, options);
            if (this.vertex.proto) {
                this.tryCommit();
            }
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
            this.views.push(new GeomVertexMV.DisplayDOMView({model: this}));
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
