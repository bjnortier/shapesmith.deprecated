define([
        'jquery',
        'lib/jquery.mustache',
        'src/calculations',
        'src/worldcursor',
        'src/scene',
        'src/geometrygraphsingleton',
        'src/modelviews/geomvertexMV', 
        'src/asyncAPI',
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
            this.displayModelConstructor = DisplayModel;
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
                '<div>' + 
                'height <input class="field height" type="text" value="{{height}}"></input>' +
                '</div>' +
                this.afterTemplate;
            var view = this.baseView;
            this.$el.html($.mustache(template, view));
            return this;
        },

    });

    var EditingSceneView = GeomVertexMV.EditingSceneView.extend({

        render: function() {
            GeomVertexMV.EditingSceneView.prototype.render.call(this);
            this.renderMesh();
        },

    });

    // ---------- Display ----------

    var DisplayModel = GeomVertexMV.DisplayModel.extend({

        initialize: function(options) {
            this.displayModelConstructor = DisplayModel;
            this.editingModelConstructor = EditingModel;
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
            this.renderMesh();
        },

    });



    // ---------- Module ----------

    return {
        DisplayModel: DisplayModel,
        EditingModel: EditingModel,
    } 

});
