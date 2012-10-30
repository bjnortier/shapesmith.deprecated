define(['src/calculations', 'src/geometrygraphsingleton', 'src/vertexwrapper'], 
    function(calc, geometryGraph, vertexWrapper) {

    // ---------- Common ----------

    var SceneView = {

        render: function() {
            vertexWrapper.EditingSceneView.prototype.render.call(this);
            var ambient = this.highlightAmbient || this.selectedAmbient || this.ambient || 0x333333;
            var color = this.highlightColor || this.selectedColor || this.color || 0x00dd00;
            var point = THREE.SceneUtils.createMultiMaterialObject(
                new THREE.SphereGeometry(0.5, 10, 10), 
                [
                    new THREE.MeshLambertMaterial({ambient: ambient, side: THREE.DoubleSide}),
                    new THREE.MeshBasicMaterial({color: color, wireframe: false, transparent: true, opacity: 0.5, side: THREE.DoubleSide}),
                ]);
            point.position = calc.objToVector(this.model.vertex.parameters.coordinate);
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
                this.vertex.parameters.coordinate.x = position.x;
                this.vertex.parameters.coordinate.y = position.y;
                this.vertex.parameters.coordinate.z = position.z;
                this.trigger('parametersChanged');
            }
        },

        workplaneClick: function() {
            this.stage = 1;
            this.trigger('stageChanged', this.stage);
        },

        keydown: function(event) {
            if ((event.keyCode === 13)  && this.canComplete()) {
                // Return when not in initial vertex placement
                this.ok();
                if (event.shiftKey && this.vertex.addAnotherFn) {
                    geometryGraph[this.vertex.addAnotherFn]();
                }
            } else if (event.keyCode === 27) {
                // Esc
                this.cancel();
            }
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
                '<div class="name">{{name}}</div>' + 
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
                name: this.model.vertex.name,
                x: this.model.vertex.parameters.coordinate.x,
                y: this.model.vertex.parameters.coordinate.y,
                z: this.model.vertex.parameters.coordinate.z,
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
            this.$el.find('.coordinate').find('.x').text(this.model.vertex.parameters.coordinate.x);
            this.$el.find('.coordinate').find('.y').text(this.model.vertex.parameters.coordinate.y);
            this.$el.find('.coordinate').find('.z').text(this.model.vertex.parameters.coordinate.z);
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

    return {
        DisplayModel: DisplayModel,
        EditingModel: EditingModel,
    }

});