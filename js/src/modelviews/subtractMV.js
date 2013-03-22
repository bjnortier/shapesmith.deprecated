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
    ], 
    function(
        $, __$,
        calc,
        worldCursor,
        sceneModel,
        geometryGraph,
        GeomVertexMV,
        AsyncAPI,
        latheAdapter) {

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
            GeomVertexMV.EditingDOMView.prototype.render.call(this);
            var template = 
                this.beforeTemplate +
                '<div>' + 
                'height <input class="field height" type="text" value="{{height}}"></input>' +
                '</div>' +
                this.afterTemplate;
            var view = this.baseView;
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
        },

        destroy: function() {
            GeomVertexMV.DisplayModel.prototype.destroy.call(this);
        },

        addSceneView: function() {
            this.sceneView = new DisplaySceneView({model: this});
            this.views.push(this.sceneView);
            return this.sceneView;
        },

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
