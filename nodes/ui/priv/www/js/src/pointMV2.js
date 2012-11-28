define([
        'src/calculations', 
        'src/colors',
        'src/scene', 
        'src/scenevieweventgenerator',
        'src/geometrygraphsingleton',
        'src/geomvertexMV', 
        'src/asyncAPI',
    ], 
    function(
        calc, 
        colors, 
        sceneModel, 
        sceneViewEventGenerator,
        geometryGraph,
        GeomVertexMV,
        AsyncAPI) {

    // ---------- Editing ----------

    var EditingModel = GeomVertexMV.EditingModel.extend({

        initialize: function(original, vertex) {
            this.displayModelConstructor = DisplayModel;
            GeomVertexMV.EditingModel.prototype.initialize.call(this, original, vertex);

            this.views.push(new EditingSceneView({model: this}));
            this.views.push(new EditingDOMView({model: this}));
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
            this.tryCommit()
        },

    });

    var EditingDOMView = GeomVertexMV.EditingDOMView.extend({

        render: function() {
            var template = 
                '<td>' +
                '<table><tr>' +
                '<td class="title">' + 
                '<img src="/ui/images/icons/point32x32.png"/>' +
                '<div class="name">{{name}}</div>' + 
                '<div class="delete"></div>' + 
                '</td></tr><tr><td>' +
                '<div class="coordinate">' +
                '<input class="field x" type="text" value="{{x}}"></input>' +
                '<input class="field y" type="text" value="{{y}}"></input>' +
                '<input class="field z" type="text" value="{{z}}"></input>' +
                '</div>' +
                '</td></tr></table>' +
                '</td>'
            var view = {
                id: this.model.vertex.id,
                name: this.model.vertex.name,
            };
            this.$el.html($.mustache(template, view));
            this.update();
            return this;
        },

        update: function() {
            var that = this;
            ['x', 'y', 'z'].forEach(function(key) {
                that.$el.find('.coordinate').find('.' + key).val(
                    that.model.vertex.parameters.coordinate[key]);
            });
        },

        updateFromDOM: function() {
            var that = this;
            ['x', 'y', 'z'].forEach(function(key) {
                try {
                    var expression = that.$el.find('.field.' + key).val();
                    that.model.vertex.parameters.coordinate[key] = expression;
                } catch(e) {
                    console.error(e);
                }
            });
            this.model.vertex.trigger('change', this.model.vertex);
        }
    });
 
    var EditingSceneView = GeomVertexMV.EditingSceneView.extend({

        initialize: function(options) {
            GeomVertexMV.EditingSceneView.prototype.initialize.call(this);
            this.on('dragStarted', this.dragStarted, this);
            this.on('drag', this.drag, this);
            this.on('dragEnded', this.dragEnded, this);
            this.rerenderOnCameraChange = true;
        },

        remove: function() {
            GeomVertexMV.EditingSceneView.prototype.remove.call(this);
            this.off('dragStarted', this.dragStarted, this);
            this.off('drag', this.drag, this);
            this.off('dragEnded', this.dragEnded, this);
        },

        render: function() {
            GeomVertexMV.EditingSceneView.prototype.render.call(this);
            var ambient = this.highlightAmbient || this.selectedAmbient || this.ambient || colors.geometry.defaultAmbient;
            var color = this.highlightColor || this.selectedColor || this.color || colors.geometry.default;
            var point = THREE.SceneUtils.createMultiMaterialObject(
                new THREE.CubeGeometry(1, 1, 1, 1, 1, 1), 
                [
                    new THREE.MeshBasicMaterial({color: color, wireframe: false, transparent: true, opacity: 0.5, side: THREE.DoubleSide}),
                    new THREE.MeshBasicMaterial({color: color, wireframe: true, transparent: true, opacity: 0.5, side: THREE.DoubleSide}),
                ]);
            point.position = calc.objToVector(this.model.vertex.parameters.coordinate, geometryGraph);
            point.scale = this.cameraScale;
            this.sceneObject.add(point);
        },

        // This view is not draggable, but the display scene view can transfer 
        // the dragging to this view (e.g. when a point is dragged), but only for
        // points that are not prototypes any more
        isDraggable: function() {
            return !this.model.vertex.proto;
        },

        dragStarted: function() {
            // The drag was started when the point was being edited, as
            // opposed to starting from a display node
            this.dragStartedInEditingMode = true;
        },

        drag: function(event) {
            this.dragging = true;
            var positionOnWorkplane = calc.positionOnWorkplane(
                event, this.model.currentWorkplaneModel.vertex, sceneModel.view.camera);
            var workplaneOrigin = this.model.currentWorkplaneModel.vertex.workplane.origin;
            this.model.vertex.parameters.coordinate = {
                x: positionOnWorkplane.x + workplaneOrigin.x,
                y: positionOnWorkplane.y + workplaneOrigin.y,
                z: positionOnWorkplane.z + workplaneOrigin.z,
            }
            this.model.vertex.trigger('change', this.model.vertex);
        },

        dragEnded: function() {
            if (!this.dragStartedInEditingMode) {
                this.model.tryCommit();
            }
        },

    });

    // ---------- Display ----------

    var DisplayModel = GeomVertexMV.DisplayModel.extend({

        initialize: function(vertex, possibleEditingParentModel) {
            this.editingModelConstructor = EditingModel;
            this.displayModelConstructor = DisplayModel;
            GeomVertexMV.DisplayModel.prototype.initialize.call(this, vertex);

            this.views = this.views.concat([
                new DisplaySceneView({model: this}),
                new GeomVertexMV.DisplayDOMView({model: this}),
            ]);
        },

    });    

    var DisplaySceneView = GeomVertexMV.DisplaySceneView.extend({

        initialize: function(vertex) {
            GeomVertexMV.DisplaySceneView.prototype.initialize.call(this, vertex);
            this.on('dragStarted', this.dragStarted, this);
            this.on('drag', this.drag, this);
            this.on('dragEnded', this.dragEnded, this);
            this.rerenderOnCameraChange = true;
        },

        remove: function() {
            GeomVertexMV.DisplaySceneView.prototype.remove.call(this);
            this.off('dragStarted', this.dragStarted, this);
            this.off('drag', this.drag, this);
            this.off('dragEnded', this.dragEnded, this);
        },

        render: function() {
            GeomVertexMV.EditingSceneView.prototype.render.call(this);
            var ambient = this.highlightAmbient || this.selectedAmbient || this.ambient || 0x333333;
            var color = this.highlightColor || this.selectedColor || this.color || 0x00dd00;
            var radius = this.model.vertex.implicit ? 0.35 : 0.5;
            var point = THREE.SceneUtils.createMultiMaterialObject(
                new THREE.SphereGeometry(radius, 10, 10), 
                [
                    new THREE.MeshLambertMaterial({ambient: ambient, side: THREE.DoubleSide}),
                    new THREE.MeshBasicMaterial({color: color, wireframe: false, transparent: true, opacity: 0.5, side: THREE.DoubleSide}),
                ]);
            point.position = calc.objToVector(
                this.model.vertex.parameters.coordinate,
                geometryGraph);
            point.scale = this.cameraScale;
            this.sceneObject.add(point);

            if (this.model.vertex.implicit) {
                var selectionPoint = new THREE.Mesh(
                    new THREE.SphereGeometry(0.5, 10, 10), 
                    new THREE.MeshBasicMaterial({ color: color, transparent: true, opacity: 0, side: THREE.DoubleSide }));
                selectionPoint.position = calc.objToVector(
                    this.model.vertex.parameters.coordinate,
                    geometryGraph);
                selectionPoint.scale = this.cameraScale;
                this.hiddenSelectionObject.add(selectionPoint);
            }
        },

        isDraggable: function() {
            return !geometryGraph.isEditing();
        },

        dragStarted: function(event) {
            this.model.destroy();
            var editingVertex = AsyncAPI.edit(this.model.vertex);
            new this.model.editingModelConstructor(this.model.vertex, editingVertex);
            sceneViewEventGenerator.replaceInDraggable(this, this.model.vertex.id);
        },

    });


    // ---------- Module ----------

    return {
        EditingModel: EditingModel,
        DisplayModel: DisplayModel,
    }


});

