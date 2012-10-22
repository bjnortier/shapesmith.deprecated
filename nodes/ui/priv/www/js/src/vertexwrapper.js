define([
        'src/calculations',
        'src/geometrygraph', 
        'src/interactioncoordinator', 
        'src/selection',
        'src/scene',
        'src/workplane',
    ], 
    function(calc, geometryGraph, coordinator, selection, sceneModel, workplane) {

    var Model = Backbone.Model.extend({

        initialize: function(vertex) {
            this.vertex = vertex;
        },

        destroy: function() {
            this.views.forEach(function(view) {
                view.remove();
            });
            this.views = [];
        },  

    });

    var DisplayModel = Model.extend({

        initialize: function(vertex) {
            Model.prototype.initialize.call(this, vertex);
            this.selected = false;
            selection.on('selected', this.select, this);
            selection.on('deselected', this.deselect, this);
            this.views = [
                new DisplayDOMView({model: this}),
                new DisplayVertexSceneView({model: this}),
            ];
        },

        destroy: function() {
            Model.prototype.destroy.call(this);
            selection.off('selected', this.selected, this);
            selection.off('deselected', this.deselected, this);
        },  

        select: function(ids) {
            if (ids.indexOf(this.vertex.id) !== -1) {
                this.selected = true;
                this.trigger('selected');
            }
        },

        deselect: function(ids) {
            if (ids.indexOf(this.vertex.id) !== -1) {
                this.selected = false;
                this.trigger('deselected');
            }
        },

    });

    var EditingModel = Model.extend({

        initialize: function(vertex) {
            Model.prototype.initialize.call(this, vertex);
            this.stage = 0;
            this.views = [
                new EditingDOMView({model: this}),
                new EditingVertexSceneView({model: this}),
            ];
            workplane.on('positionChanged', this.workplanePositionChanged, this);
            coordinator.on('keydown', this.keydown, this);
            coordinator.on('click', this.click, this);
        },

        destroy: function() {
            Model.prototype.destroy.call(this);
            workplane.off('positionChanged', this.workplanePositionChanged, this);
            coordinator.off('keydown', this.keydown, this);
            coordinator.off('click', this.click, this);
        },  

        workplanePositionChanged: function(position) {
            if (this.stage === 0) {
                this.vertex.parameters.x = position.x;
                this.vertex.parameters.y = position.y;
                this.vertex.parameters.z = position.z;
                this.trigger('parametersChanged');
            }
        },

        keydown: function(event) {
            if ((event.keyCode === 13)  && (this.stage !== 0)) {
                // Return when not in initial vertex placement
                this.ok();
                if (event.shiftKey && this.vertex.addAnotherFn) {
                    geometryGraph[this.vertex.addAnotherFn]();
                }
            } else if (event.keyCode === 27) {
                // Esx
                this.cancel();
            }

        },

        click: function() {
            this.stage = 1;
            this.trigger('stageChanged', this.stage);
        },

        ok: function() {
            this.destroy();
            geometryGraph.removeVertex(this.vertex);
            geometryGraph.addVertex(this.vertex.cloneNonEditing());
        },

        cancel: function() {
            this.destroy();
            geometryGraph.removeVertex(this.vertex);
        },
    });


    var EditingDOMView = Backbone.View.extend({

        className: 'vertex editing',
        tagName: "tr",

        initialize: function() {
            this.render();
            $('#graph').prepend(this.$el);
            this.model.on('stageChanged', this.stageChanged, this);
            this.model.on('parametersChanged', this.updateParams, this);
        },

        remove: function() {
            Backbone.View.prototype.remove.call(this);
            this.model.off('stageChanged', this.stageChanged, this);
            this.model.off('parametersChanged', this.updateParams, this);
        },

        events: {
            'click .okcancel .ok' : 'ok',
            'click .okcancel .cancel' : 'cancel',
        },

        render: function() {
            var template = 
                '<td>' +
                '<div class="title"><img src="/ui/images/icons/point32x32.png"/>' +
                '<div class="name">Vertex</div>' + 
                '<span class="okcancel">' + 
                '<span class="ok button disabled"><img src="/ui/images/icons/ok24x24.png"/></span>' +
                '<span class="cancel button"><img src="/ui/images/icons/cancel24x24.png"/></span>' +
                '</span>' + 
                '</div>' + 
                '<div class="coordinate">' +
                '<span class="x">{{x}}</span><span class="y">{{y}}</span><span class="z">{{z}}</span>' +
                '</div>' +
                '</td>';
            var view = {
                x: this.model.vertex.parameters.x,
                y: this.model.vertex.parameters.y,
                z: this.model.vertex.parameters.z,
            }
            this.$el.html($.mustache(template, view));
            return this;
        },

        stageChanged: function(stage) {
            if (stage === 1) {
                this.$el.find('.ok').removeClass('disabled')
            }
        },

        updateParams: function() {
            this.$el.find('.coordinate').find('.x').text(this.model.vertex.parameters.x);
            this.$el.find('.coordinate').find('.y').text(this.model.vertex.parameters.y);
            this.$el.find('.coordinate').find('.z').text(this.model.vertex.parameters.z);
        },

        ok: function() {
            if (this.model.stage === 1) {
                this.model.ok();
            }
        },

        cancel: function() {
            this.model.cancel();
        },

    });

    var DisplayDOMView = Backbone.View.extend({

        tagName: "tr",

        initialize: function() {
            this.render();
            $('#graph').append(this.$el);
            this.model.on('selected', this.select, this);
            this.model.on('deselected', this.deselect, this);
        },

        remove: function() {
            Backbone.View.prototype.remove.call(this);
            this.model.off('selected', this.select, this);
            this.model.off('deselected', this.deselect, this);
        },

        render: function() {
            var template = '<td class="vertex display"><img src="/ui/images/icons/point32x32.png"/></td>';
            var view = {
                x: this.model.vertex.parameters.x,
                y: this.model.vertex.parameters.y,
                z: this.model.vertex.parameters.z,
            }
            this.$el.html($.mustache(template, view));
            return this;
        },

        events: {
            'click .vertex' : 'click'
        },

        click: function(event) {
            if (event.shiftKey || event.ctrlKey || event.metaKey) {
                selection.addToSelection(this.model.vertex.id);
            } else {
                selection.selectOnly(this.model.vertex.id);
            }
        },

        select: function() {
            this.$el.find('.vertex').addClass('selected');
        },

        deselect: function() {
            this.$el.find('.vertex').removeClass('selected');
        },

    });


    var VertexSceneView = Backbone.View.extend({

        initialize: function() {
            this.scene = sceneModel.view.scene;
            this.sceneObject = new THREE.Object3D();
            this.render();
        },

        remove: function() {
            this.scene.remove(this.sceneObject);
            sceneModel.view.updateScene = true;
        },

        render: function() {
            this.scene.remove(this.sceneObject);
            this.sceneObject = new THREE.Object3D();

            var color = 0x0b5fa5;
            var point = THREE.SceneUtils.createMultiMaterialObject(
                new THREE.SphereGeometry(0.5, 10, 10), 
                [
                    new THREE.MeshLambertMaterial( { ambient: this.ambientColor || 0x333333,  side: THREE.DoubleSide} ),
                    new THREE.MeshBasicMaterial( { color: this.color, wireframe: false, transparent: true, opacity: 0.5, side: THREE.DoubleSide } ),
                ]);
            point.position = calc.objToVector(this.model.vertex.parameters);
            this.sceneObject.add(point);

            this.scene.add(this.sceneObject);
            sceneModel.view.updateScene = true;
        },
    });

    var DisplayVertexSceneView = VertexSceneView.extend({

        unselectedColor: 0x00dd00,

        initialize: function() {
            this.color = this.unselectedColor;
            VertexSceneView.prototype.initialize.call(this);
            this.model.on('selected', this.select, this);
            this.model.on('deselected', this.deselect, this);
        },

        remove: function() {
            VertexSceneView.prototype.remove.call(this);
            this.model.off('selected', this.select, this);
            this.model.off('deselected', this.deselect, this);
        },

        select: function() {
            this.color = 0xf4f653;
            this.ambientColor = 0xffffff;
            this.render();
        },

        deselect: function() {
            this.color = 0x00dd00;
            this.ambientColor = undefined;
            this.render();
        },

    });

    var EditingVertexSceneView = VertexSceneView.extend({

        initialize: function() {
            this.color = 0x94dcfc;
            VertexSceneView.prototype.initialize.call(this);
            this.model.on('stageChanged', this.render, this);
            this.model.on('parametersChanged', this.render, this);
        },

        remove: function() {
            VertexSceneView.prototype.remove.call(this);
            this.model.off('parametersChanged', this.render, this);
            this.model.off('stageChanged', this.render, this);
        },

    });

    geometryGraph.on('vertexAdded', function(vertex) {
        if (vertex.editing) {
            new EditingModel(vertex);
        } else {
            new DisplayModel(vertex);
        }
    });
    geometryGraph.on('vertexRemoved', function(vertex) {
    }, this);

});

