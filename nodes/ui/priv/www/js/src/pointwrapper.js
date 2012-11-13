define([
        'src/calculations', 
        'src/geometrygraphsingleton', 
        'src/vertexwrapper', 
        'src/scene', 
        'src/scenevieweventgenerator',
        'src/workplane'
    ], 
    function(calc, geometryGraph, vertexWrapper, sceneModel, sceneViewEventGenerator, workplaneModel) {

    // ---------- Editing ----------

    var EditingModel = vertexWrapper.EditingModel.extend({

        initialize: function(vertex) {
            vertexWrapper.EditingModel.prototype.initialize.call(this, vertex);
            if (workplaneModel.lastPosition) {
                this.workplanePositionChanged(workplaneModel.lastPosition);
            }
            this.views = this.views.concat([
                new EditingDOMView({model: this}),
                new EditingSceneView({model: this}),
            ]);
        },

        workplanePositionChanged: function(position) {
            if (this.vertex.proto) {
                this.vertex.parameters.coordinate.x = position.x;
                this.vertex.parameters.coordinate.y = position.y;
                this.vertex.parameters.coordinate.z = position.z;
                this.vertex.trigger('change', this.vertex);            
            }
        },

        workplaneClick: function() {
            var that = this;
            if (this.vertex.proto) {
                if (this.vertex.implicit) {
                    geometryGraph.parentsOf(this.vertex).map(function(parent) {
                        parent.trigger('beforeImplicitChildCommit', that.vertex);
                    });
                }
                this.okCreate();
            }
        },

    });

    var EditingDOMView = vertexWrapper.EditingDOMView.extend({

        render: function() {
            var template = 
                '<td class="vertex {{name}} editing">' + 
                '<div class="title"><img src="/ui/images/icons/point32x32.png"/>' +
                '<div class="name">{{name}}</div>' + 
                '</div>' + 
                '<div class="coordinate">' +
                '<span class="x">{{x}}</span><span class="y">{{y}}</span><span class="z">{{z}}</span>' +
                '</div>' +
                '</td>';
            var view = {
                id: this.model.vertex.id,
                name: this.model.vertex.name,
            };
            var that = this;
            ['x', 'y', 'z'].forEach(function(key) {
                view[key] = that.model.vertex.parameters.coordinate[key];
            });
            this.$el.html($.mustache(template, view));
            return this;
        },

        update: function() {
            var that = this;
            ['x', 'y', 'z'].forEach(function(key) {
                that.$el.find('.coordinate').find('.' + key).text(that.model.vertex.parameters.coordinate[key]);
            });
        },
    });

    var EditingSceneView = vertexWrapper.EditingSceneView.extend({

        initialize: function(options) {
            this.point = this.model.vertex;
            vertexWrapper.EditingSceneView.prototype.initialize.call(this);
            this.on('dragStarted', this.dragStarted, this);
            this.on('drag', this.drag, this);
            this.on('dragEnded', this.dragEnded, this);
        },

        remove: function() {
            vertexWrapper.EditingSceneView.prototype.remove.call(this);
            this.off('dragStarted', this.dragStarted, this);
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

        // This view is not draggable, but the display scene view can transfer 
        // the dragging to this view (e.g. when a point is dragged), but only for
        // points that are not prototypes any more
        isDraggable: function() {
            return !this.model.vertex.proto;
        },

        dragStarted: function() {
            this.dragStartedWhilstEditing = true;
        },

        drag: function(event) {
            this.dragging = true;
            var positionOnWorkplane = calc.positionOnWorkplane(
                event, workplaneModel.node, sceneModel.view.camera);
            this.point.parameters.coordinate = {
                x: positionOnWorkplane.x,
                y: positionOnWorkplane.y,
                z: positionOnWorkplane.z,
            }
            this.model.vertex.trigger('change', this.model.vertex);
        },

        dragEnded: function() {
            if (this.dragging && !this.dragStartedWhilstEditing) {
                this.model.okEdit();
                this.dragging = false;
            }
        },

    });

    // ---------- Display ----------

    var DisplayModel = vertexWrapper.DisplayModel.extend({

        initialize: function(vertex) {
            vertexWrapper.DisplayModel.prototype.initialize.call(this, vertex);
            this.views = this.views.concat([
                new DisplaySceneView({model: this}),
            ]);
            if (!vertex.implicit) {
                this.views.push(new DisplayDOMView({model: this}));
            }
        },

        canSelect: function() {
            return !this.vertex.implicit;
        },

        selectParentOnClick: function() {
            return this.vertex.implicit;
        },

    });

    var DisplaySceneView = vertexWrapper.DisplaySceneView.extend({

        initialize: function(vertex) {
            vertexWrapper.DisplaySceneView.prototype.initialize.call(this, vertex);
            this.on('dragStarted', this.dragStarted, this);
            this.on('drag', this.drag, this);
            this.on('dragEnded', this.dragEnded, this);
        },

        remove: function() {
            vertexWrapper.DisplaySceneView.prototype.remove.call(this);
            this.off('dragStarted', this.dragStarted, this);
            this.off('drag', this.drag, this);
            this.off('dragEnded', this.dragEnded, this);
        },

        render: function() {
            vertexWrapper.EditingSceneView.prototype.render.call(this);
            var ambient = this.highlightAmbient || this.selectedAmbient || this.ambient || 0x333333;
            var color = this.highlightColor || this.selectedColor || this.color || 0x00dd00;
            var radius = this.model.vertex.implicit ? 0.35 : 0.5;
            var point = THREE.SceneUtils.createMultiMaterialObject(
                new THREE.SphereGeometry(radius, 10, 10), 
                [
                    new THREE.MeshLambertMaterial({ambient: ambient, side: THREE.DoubleSide}),
                    new THREE.MeshBasicMaterial({color: color, wireframe: false, transparent: true, opacity: 0.5, side: THREE.DoubleSide}),
                ]);
            point.position = calc.objToVector(this.model.vertex.parameters.coordinate);
            this.sceneObject.add(point);

            if (this.model.vertex.implicit) {
                var selectionPoint = new THREE.Mesh(
                    new THREE.SphereGeometry(0.5, 10, 10), 
                    new THREE.MeshBasicMaterial({ color: color, transparent: true, opacity: 0, side: THREE.DoubleSide }));
                selectionPoint.position = calc.objToVector(this.model.vertex.parameters.coordinate);
                // For some reason selection & highlighting doesn't work with a hidden 
                // scene object... 
                this.sceneObject.add(selectionPoint);
            }
        },

        isDraggable: function() {
            return !geometryGraph.isEditing();
        },

        dragStarted: function(event) {
            this.dragging = true;
            geometryGraph.edit(this.model.vertex);
            sceneViewEventGenerator.replaceInDraggable(this, this.model.vertex.id);
        },

    });

    var DisplayDOMView = vertexWrapper.DisplayDOMView.extend({

        render: function() {
            var view = {
                name: this.model.vertex.name,
            }
            var template = 
                '<td class="vertex {{name}} display">' + 
                '<img src="/ui/images/icons/point32x32.png"/>' + 
                '<div class="name">{{name}}</div>' + 
                '</td>';
            this.$el.html($.mustache(template, view));
            return this;
        },

    })


    return {
        DisplayModel: DisplayModel,
        EditingModel: EditingModel,
    }


});