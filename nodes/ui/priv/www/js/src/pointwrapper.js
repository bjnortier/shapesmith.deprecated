define(['src/calculations', 'src/geometrygraphsingleton', 'src/vertexwrapper', 'src/scene', 'src/workplane'], 
    function(calc, geometryGraph, vertexWrapper, sceneModel, workplaneModel) {

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
            this.stage = undefined;
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
            return this.stage === undefined;
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

        updateParams: function() {
            var that = this;
            ['x', 'y', 'z'].forEach(function(key) {
                that.$el.find('.coordinate').find('.' + key).text(that.model.vertex.parameters.coordinate[key]);
            });
        },
    });

    var EditingSceneView = vertexWrapper.EditingSceneView.extend({

        initialize: function(options) {
            this.point = this.model.vertex;
            this.draggable = true; 
            vertexWrapper.EditingSceneView.prototype.initialize.call(this);
            this.on('drag', this.drag, this);
            this.on('dragEnded', this.dragEnded, this);
        },

        remove: function() {
            vertexWrapper.EditingSceneView.prototype.remove.call(this);
            this.off('drag', this.drag, this);
            this.off('dragEnded', this.dragEnded, this);
        },

        render: function() {
            vertexWrapper.EditingSceneView.prototype.render.call(this);
            var ambient = this.highlightAmbient || this.selectedAmbient || this.ambient || 0x333333;
            var color = this.highlightColor || this.selectedColor || this.color || 0x00dd00;
            var point = THREE.SceneUtils.createMultiMaterialObject(
                new THREE.CubeGeometry(1, 1, 1, 1, 1, 1), 
                [
                    new THREE.MeshBasicMaterial({color: color, wireframe: false, transparent: true, opacity: 0.5, side: THREE.DoubleSide}),
                    new THREE.MeshBasicMaterial({color: color, wireframe: true, transparent: true, opacity: 0.5, side: THREE.DoubleSide}),
                ]);
            point.position = calc.objToVector(this.point.parameters.coordinate);
            this.sceneObject.add(point);
        },

        drag: function(event) {
            this.dragging = true;
            this.model.stage = 0;
            var positionOnWorkplane = calc.positionOnWorkplane(
                event, workplaneModel.node, sceneModel.view.camera);
            this.point.parameters.coordinate = {
                x: positionOnWorkplane.x,
                y: positionOnWorkplane.y,
                z: positionOnWorkplane.z,
            }
            this.model.trigger('parametersChanged');
        },

        dragEnded: function() {
            if (this.dragging) {
                this.model.stage = undefined;
                this.dragging = false;
                this.model.trigger('stageChanged', this.model.stage);
            }
        },

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

    var DisplaySceneView = vertexWrapper.DisplaySceneView.extend({

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

    });

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