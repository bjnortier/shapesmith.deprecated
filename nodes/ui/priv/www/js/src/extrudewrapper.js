define([
        'src/calculations',
        'src/geometrygraphsingleton',
        'src/geomvertexwrapper',
        'src/scene',
        'src/workplane'
    ], 
    function(calc, geometryGraph, geomVertexWrapper, sceneModel, workplaneModel) {

    // ---------- Common ----------

    var RenderExtrudeFacesMixin = {

        render: function() {
            geomVertexWrapper.EditingSceneView.prototype.render.call(this);
            var polyline = _.first(geometryGraph.childrenOf(this.model.vertex));
            var points = geometryGraph.childrenOf(polyline);
            for (var i = 1; i < points.length; ++i) {
                this.renderPlaneForPoints(points[i], points[i-1]);
            }
        },

        renderPlaneForPoints: function(pointA, pointB) {
            var geometry = new THREE.Geometry();
            var h = geometryGraph.evaluate(this.model.vertex.parameters.h);
            var a  = calc.objToVector(pointA.parameters.coordinate, geometryGraph);
            var ah = a.clone().setZ(h);
            var b  = calc.objToVector(pointB.parameters.coordinate, geometryGraph);
            var bh = b.clone().setZ(h);

            geometry.vertices.push(a);
            geometry.vertices.push(b);
            geometry.vertices.push(bh);
            geometry.vertices.push(ah);

            geometry.faces.push(new THREE.Face4(0,1,2,3));
            geometry.computeCentroids();
            geometry.computeFaceNormals();

            var ambient = this.highlightAmbient || this.selectedAmbient || this.ambient || 0x333333;
            var color = this.highlightColor || this.selectedColor || this.color || 0x00dd00;
            var face = THREE.SceneUtils.createMultiMaterialObject(
                geometry,
                [
                    new THREE.MeshLambertMaterial({ambient: ambient, side: THREE.DoubleSide}),
                    new THREE.MeshBasicMaterial({color: color, wireframe: false, transparent: true, opacity: 0.5, side: THREE.DoubleSide}),
                ]);
            this.sceneObject.add(face);
        },
    }

    // ---------- Editing ----------

    var EditingModel = geomVertexWrapper.EditingModel.extend({

        initialize: function(vertex) {
            geomVertexWrapper.EditingModel.prototype.initialize.call(this, vertex);
            this.views = this.views.concat([
                new EditingDOMView({model: this}),
                new EditingFacesSceneView({model: this}),
            ]);
            var polyline = _.first(geometryGraph.childrenOf(this.vertex));
            var points = geometryGraph.childrenOf(polyline);
            for (var i = 0; i < points.length; ++i) {
                this.views.push(new EditingHeightAnchor({model: this, pointVertex: points[i]}));
            }
            this.vertex.on('descendantChanged', this.descendantChanged, this);
        },

        destroy: function() {
            geomVertexWrapper.EditingModel.prototype.destroy.call(this);
            this.vertex.off('descendantChanged', this.descendantChanged, this);
        },

        // Trigger a 'change' event if any of the child polylines changed
        descendantChanged: function(descendant) {
            this.vertex.trigger('change', this.vertex);
        },

        workplaneClick: function() {
            if (this.vertex.proto) {
                this.okCreate();
            } 
            // Edit is handled by listening to selection
        },

    });

    var EditingDOMView = geomVertexWrapper.EditingDOMView.extend({

        render: function() {
            var template = 
                '<td>' + 
                '<div class="title"><img src="/ui/images/icons/line32x32.png"/>' +
                '<div class="name">{{name}}</div>' + 
                '</div>' + 
                '<div class="coordinate">' + 
                'h <input class="field h" type="text" value="{{h}}"></input>' +
                '</div>' +
                '</td>';
            var view = {
                name: this.model.vertex.name,
                h   : this.model.vertex.parameters.h,
            };
            this.$el.html($.mustache(template, view));
            this.update();
            return this;
        },

        update: function() {
            this.$el.find('.field.h').val(    
                this.model.vertex.parameters.h);
        },

        updateFromDOM: function() {
            var expression = this.$el.find('.field.h').val();
            this.model.vertex.parameters.h = expression;
        }

    }); 

    var EditingHeightAnchor = geomVertexWrapper.EditingSceneView.extend({

        initialize: function(options) {
            this.pointVertex = options.pointVertex;
            geomVertexWrapper.EditingSceneView.prototype.initialize.call(this);
            this.on('dragStarted', this.dragStarted, this);
            this.on('drag', this.drag, this);
            this.on('dragEnded', this.dragEnded, this);
        },

        remove: function() {
            geomVertexWrapper.EditingSceneView.prototype.remove.call(this);
            this.off('dragStarted', this.dragStarted, this);
            this.off('drag', this.drag, this);
            this.off('dragEnded', this.dragEnded, this);
        },

        render: function() {
            geomVertexWrapper.EditingSceneView.prototype.render.call(this);
            var ambient = this.highlightAmbient || this.selectedAmbient || this.ambient || 0x333333;
            var color = this.highlightColor || this.selectedColor || this.color || 0x00dd00;

            var pointSceneObject = THREE.SceneUtils.createMultiMaterialObject(
                new THREE.CubeGeometry(1, 1, 1, 1, 1, 1), 
                [
                    new THREE.MeshBasicMaterial({color: color, wireframe: false, transparent: true, opacity: 0.5, side: THREE.DoubleSide}),
                    new THREE.MeshBasicMaterial({color: color, wireframe: true, transparent: true, opacity: 0.5, side: THREE.DoubleSide}),
                ]);
            var pointPosition = calc.objToVector(this.pointVertex.parameters.coordinate, geometryGraph);
            pointSceneObject.position = pointPosition;
            pointSceneObject.position.z = geometryGraph.evaluate(this.model.vertex.parameters.h);
            this.sceneObject.add(pointSceneObject);

            if (this.showHeightLine) {
                var lineGeometry = new THREE.Geometry();
                lineGeometry.vertices.push(pointPosition.clone().setZ(-1000));
                lineGeometry.vertices.push(pointPosition.clone().setZ(1000));
                var line = new THREE.Line(lineGeometry, new THREE.LineBasicMaterial({color: 0xff6666}));
                this.sceneObject.add(line);
            }

        },

        isDraggable: function() {
            return true;
        },

        dragStarted: function() {
            this.showHeightLine = true;
        },

        dragEnded: function() {
            this.showHeightLine = false;
            this.model.vertex.trigger('change', this.model.vertex);
        },

        drag: function(event) {
            this.dragging = true;

            var rayOrigin = calc.objToVector(this.pointVertex.parameters.coordinate, geometryGraph);
            var rayDirection = new THREE.Vector3(0,0,1);
            var ray = new THREE.Ray(rayOrigin, rayDirection);

            var positionOnNormal = calc.positionOnRay(event, ray, sceneModel.view.camera);
            this.model.vertex.parameters.h = parseFloat(positionOnNormal.z.toFixed(0))
            this.model.vertex.trigger('change', this.model.vertex);
        },

    });

    var EditingFacesSceneView = geomVertexWrapper.EditingSceneView.extend(RenderExtrudeFacesMixin);

    // ---------- Display ----------

    var DisplayModel = geomVertexWrapper.DisplayModel.extend({

        initialize: function(vertex) {
            geomVertexWrapper.DisplayModel.prototype.initialize.call(this, vertex);
            this.views = this.views.concat([
                new DisplayFacesSceneView({model: this}),
                new DisplayDOMView({model: this}),
            ]);

        },

    });

    var DisplayDOMView = geomVertexWrapper.DisplayDOMView.extend({

        render: function() {
            var view = {
                name: this.model.vertex.name,
            }
            var template = 
                '<td>' + 
                '<img src="/ui/images/icons/extrude32x32.png"/>' + 
                '<div class="name">{{name}}</div>' + 
                '</td>';
            this.$el.html($.mustache(template, view));
            return this;
        },

    })

    var DisplayFacesSceneView = geomVertexWrapper.DisplaySceneView.extend(RenderExtrudeFacesMixin);


    return {
        EditingModel: EditingModel,
        DisplayModel: DisplayModel,
    }

});