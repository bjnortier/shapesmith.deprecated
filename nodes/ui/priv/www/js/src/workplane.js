define(['src/calculations', 'src/interactioncoordinator'], function(calc, coordinator) {

    var Model = Backbone.Model.extend({

        initialize: function(attributes) {
            _.extend(this, Backbone.Events);
            coordinator.on('mousemove', this.mousemove, this);
            coordinator.on('toolActivated', this.toolActivated, this);
            this.sceneView = attributes.sceneView;
            this.scene = attributes.sceneView.scene;
            this.camera = attributes.sceneView.camera;
            this.gridView = new GridView({model: this});
            this.cursorViews = [];
            this.node = {
                origin: new THREE.Vector3(),
                axis: new THREE.Vector3(0,0,1),
                angle: 0
            }
            this.coordinatesOffset = {
                x: -20,
                y: -20,
            }

        },

        mousemove: function(event) {
            var positionOnWorkplane = calc.positionOnWorkplane(event, this.node, this.camera);
            if (!this.lastPosition || !positionOnWorkplane.equals(this.lastPosition)) {
                this.lastPosition = positionOnWorkplane;
                this.attributes.sceneView.updateScene = true;
                this.trigger('lastPositionChanged', this.lastPosition);
            }

        },

        toolActivated: function(name) {
            if (name === undefined) {
                this.cursorViews.map(function(cursorView) {
                    cursorView.remove();
                });
                this.cursorViews = [];
            } else if (this.cursorViews.length === 0) {
                this.cursorViews = [
                    new CursorView({model: this}),
                    new CursorCoordinateView({model: this}),
                ];
            }
        },

    });

    var GridView = Backbone.View.extend({

        initialize: function() {
            this.render();
        },

        render: function() {
            var scene = this.model.scene;

            var majorGridLineGeometry = new THREE.Geometry();
            var minorGridLineGeometry = new THREE.Geometry();
            var majorMaterialInside = new THREE.LineBasicMaterial({ color: 0xffffff, opacity: 0.2, transparent: true });
            var minorMaterialInside = new THREE.LineBasicMaterial({ color: 0xffffff, opacity: 0.02, transparent: true });

            majorGridLineGeometry.vertices.push(new THREE.Vector3(-100, 0, 0));
            majorGridLineGeometry.vertices.push(new THREE.Vector3(100, 0, 0));
            minorGridLineGeometry.vertices.push(new THREE.Vector3(-100, 0, 0));
            minorGridLineGeometry.vertices.push(new THREE.Vector3(100, 0, 0));

            for (var x = -100; x <= 100; ++x) {
                var material = (x % 10 == 0) ? majorMaterialInside : minorMaterialInside;
                var geometry = (x % 10 == 0) ? majorGridLineGeometry : minorGridLineGeometry;
                var line = new THREE.Line(geometry, material);
                line.position.x = x;
                line.position.z = (x % 10 == 0) ? line.position.z : line.position.z - 0.1;
                line.rotation.z = 90 * Math.PI / 180;
                scene.add(line);
            }

            for (var y = -100; y <= 100; ++y) {
                var material = (y % 10 == 0) ? majorMaterialInside : minorMaterialInside;
                var geometry = (y % 10 == 0) ? majorGridLineGeometry : minorGridLineGeometry;
                var line = new THREE.Line(geometry, material);
                line.position.y = y;
                line.position.z = (y % 10 == 0) ? line.position.z : line.position.z - 0.1;
                scene.add(line);
            }
        },

    });

    var CursorView = Backbone.View.extend({

        initialize: function() {
            this.sceneObject = new THREE.Object3D();
            this.model.on('lastPositionChanged', this.render, this);
        },

        remove: function() {
            this.model.scene.remove(this.sceneObject);
            this.model.off('lastPositionChanged', this.render, this);
            this.model.sceneView.updateScene = true;
        },

        render: function() {
            var scene = this.model.scene;
            scene.remove(this.sceneObject);
            this.sceneObject = new THREE.Object3D();

            if (this.model.lastPosition && coordinator.hasActiveTool()) {
                var cubeCursor = new THREE.Mesh(
                    new THREE.CubeGeometry(0.5, 0.5, 0.5, 1, 1, 1), 
                    new THREE.MeshBasicMaterial({color: 0xffffff}));
                cubeCursor.position = calc.objToVector(this.model.lastPosition);
                this.sceneObject.add(cubeCursor);

                var cursorScreenCoordinates = calc.toScreenCoordinates(
                    this.model.camera, 
                    this.model.lastPosition.clone());

                var fakeEvent = {
                    clientX: cursorScreenCoordinates.x + this.model.coordinatesOffset.x,
                    clientY: cursorScreenCoordinates.y + this.model.coordinatesOffset.y,
                };
                var normal = this.model.camera.position.clone().negate();
                var endLineWorldPos = calc.positionOnPlane(
                    fakeEvent, 
                    this.model.lastPosition.clone(), 
                    normal, 
                    this.model.camera);
                
                var lineGeometry = new THREE.Geometry();
                lineGeometry.vertices.push(this.model.lastPosition);
                lineGeometry.vertices.push(endLineWorldPos);
                var line = new THREE.Line(lineGeometry, 
                    new THREE.LineBasicMaterial({ color: 0xffffff }));
                this.sceneObject.add(line);

                scene.add(this.sceneObject);
            }
        },
    });

    var CursorCoordinateView = Backbone.View.extend({

        className: 'coordinate',

        initialize: function() {
            $('body').append(this.$el);
            this.render();
            this.model.on('lastPositionChanged', this.render, this);
        },

        remove: function() {
            Backbone.View.prototype.remove.call(this);
            this.model.off('lastPositionChanged', this.render, this);
        },

        render: function() {
            if (this.model.lastPosition) {
                var template = '<span class="x">{{x}}</span><span class="y">{{y}}</span><span class="z">{{z}}</span>';
                this.$el.html($.mustache(template, this.model.lastPosition));

                var cursorScreenCoordinates = calc.toScreenCoordinates(
                    this.model.camera, 
                    this.model.lastPosition.clone());
                this.$el.css('right', (window.innerWidth - cursorScreenCoordinates.x - this.model.coordinatesOffset.x) + 'px');
                this.$el.css('bottom', (window.innerHeight - cursorScreenCoordinates.y - this.model.coordinatesOffset.y)+ 'px');
            }
            return this;
        },


    });

    return {
        Model: Model
    }

});