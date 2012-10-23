define(['src/calculations', 'src/geometrygraph', 'src/vertexwrapper'], function(calc, geometryGraph, vertexWrapper) {

    // ---------- Common ----------

    var SceneView = {

        render: function() {
            vertexWrapper.EditingSceneView.prototype.render.call(this);
            var point = THREE.SceneUtils.createMultiMaterialObject(
                new THREE.SphereGeometry(0.5, 10, 10), 
                [
                    new THREE.MeshLambertMaterial( { ambient: this.ambientColor || 0x333333,  side: THREE.DoubleSide} ),
                    new THREE.MeshBasicMaterial( { color: this.color, wireframe: false, transparent: true, opacity: 0.5, side: THREE.DoubleSide } ),
                ]);
            point.position = calc.objToVector(this.model.vertex.parameters);
            this.sceneObject.add(point);
        },

    };

    // ---------- Editing ----------

    var EditingModel = vertexWrapper.EditingModel.extend({

        initialize: function(vertex) {
            vertexWrapper.EditingModel.prototype.initialize.call(this, vertex);
            this.views = this.views.concat([
                new EditingDOMView({model: this}),
                new EditingSceneView({model: this}),
            ]);
        },

        workplanePositionChanged: function(position) {
            if (this.stage === 0) {
                this.vertex.parameters.x = position.x;
                this.vertex.parameters.y = position.y;
                this.vertex.parameters.z = position.z;
                this.trigger('parametersChanged');
            }
        },

        windowClick: function() {
            this.stage = 1;
            this.trigger('stageChanged', this.stage);
        },

        canComplete: function() {
            return this.stage !== 0;
        },

    });

    var EditingDOMView = vertexWrapper.EditingDOMView.extend({

        render: function() {
            var template = 
                '<td>' +
                '<div class="title"><img src="/ui/images/icons/point32x32.png"/>' +
                '<div class="id">{{id}}</div>' + 
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
                id: this.model.vertex.id,
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
    });

    var EditingSceneView = vertexWrapper.EditingSceneView.extend(SceneView, {

    });


    // ---------- Display ----------

    var DisplayModel = vertexWrapper.DisplayModel.extend({

        initialize: function(vertex) {
            vertexWrapper.DisplayModel.prototype.initialize.call(this, vertex);
            this.views = this.views.concat([
                new DisplaySceneView({model: this}),
                new DisplayDOMView({model: this}),
            ]);
        },

    });

    var DisplaySceneView = vertexWrapper.DisplaySceneView.extend(SceneView);

    var DisplayDOMView = vertexWrapper.DisplayDOMView.extend({

        render: function() {
            this.$el.html('<td class="vertex display"><img src="/ui/images/icons/point32x32.png"/></td>');
        },

    })

    // ---------- Init ----------

    geometryGraph.on('vertexAdded', function(vertex) {
        if (vertex.type === 'point') {
            if (vertex.editing) {
                new EditingModel(vertex);
            } else {
                new DisplayModel(vertex);
            }
        }
    });

});