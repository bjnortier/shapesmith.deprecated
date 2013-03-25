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
            GeomVertexMV.EditingModel.prototype.initialize.call(this, options);
        },

        addTreeView: function() {
            var domView = new EditingDOMView({model: this});
            this.views.push(domView);
            return domView;
        },

        addSceneView: function() {
            this.sceneView = new EditingSceneView({model: this});
            this.views.push(this.sceneView);
            return this.sceneView;
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
