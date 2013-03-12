define([
        'jquery',
        'lib/jquery.mustache',
        'src/calculations',
        'src/worldcursor',
        'src/geometrygraphsingleton',
        'src/vertexMV',
        'src/geomvertexMV', 
        'src/pointMV', 
        'src/heightanchorview',
        'src/asyncAPI',
        'src/lathe/pool',
    ], 
    function(
        $, __$,
        calc,
        worldCursor,
        geometryGraph,
        VertexMV,
        GeomVertexMV,
        PointMV,
        EditingHeightAnchor,
        AsyncAPI,
        Lathe) {


    // ---------- Display ----------

    var DisplayModel = GeomVertexMV.DisplayModel.extend({

        initialize: function(options) {
            this.displayModelConstructor = DisplayModel;
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

    });

    var DisplaySceneView = GeomVertexMV.DisplaySceneView.extend({

        initialize: function() {
            GeomVertexMV.DisplaySceneView.prototype.initialize.call(this);
        },

        render: function() {
            GeomVertexMV.DisplaySceneView.prototype.render.call(this);

            var children = geometryGraph.childrenOf(this.model.vertex);
            var a = children[0].bsp;
            var b = children[1].bsp;

            var subtract = LatheModels.subtract(a, b);
            var polygons = LatheModels.toBrep(subtract);

            var toMesh = this.polygonsToMesh(polygons);
            var faceGeometry = toMesh.geometry;
            var meshObject = THREE.SceneUtils.createMultiMaterialObject(faceGeometry, [
                this.materials.normal.face, 
            ]);
            this.sceneObject.add(meshObject);

            geometryGraph.remove(children[0]);
            geometryGraph.remove(children[1]);
        },

        remove: function() {
            GeomVertexMV.DisplaySceneView.prototype.remove.call(this);
        },

    })

    // ---------- Module ----------

    return {
        DisplayModel: DisplayModel,
    } 

});
