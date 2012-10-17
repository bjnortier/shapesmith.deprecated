define(['src/calculations'], function(calc) {

    var Model = Backbone.Model.extend({

        initialize: function(attributes) {
            _.extend(this, Backbone.Events);
            attributes.sceneView.on('mousemove', this.mousemove, this);
            this.scene = attributes.sceneView.scene;
            this.camera = attributes.sceneView.camera;
            this.views = [
                new GridView({model: this}),
                new CursorView({model: this}),
            ];

            this.node = {
                origin: new THREE.Vector3(),
                axis: new THREE.Vector3(0,0,1),
                angle: 0
            }

        },

        mousemove: function(event) {
            var positionOnWorkplane = calc.posOnWorkplane(event, this.node, this.camera);
            if (!this.lastPosition || !positionOnWorkplane.equals(this.lastPosition)) {
                this.lastPosition = positionOnWorkplane;
                this.attributes.sceneView.updateScene = true;
                this.trigger('lastPositionChanged', this.lastPosition);
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

        render: function() {
            var scene = this.model.scene;
            scene.remove(this.sceneObject);
            this.sceneObject = new THREE.Object3D();

            if (this.model.lastPosition && SS.interactionCoordinator.hasActiveTool()) {
                var materials = [
                    new THREE.MeshBasicMaterial( { color: 0xffff00, opacity: 0.7, wireframe: false } ),
                    // new THREE.MeshBasicMaterial( { color: 0x00bb00, wireframe: true, transparent: false, side: THREE.DoubleSide } ),
                    // new THREE.MeshBasicMaterial( { color: 0x00bb00, wireframe: false, transparent: false, side: THREE.DoubleSide } ),
                ];
                var cubeCursor = THREE.SceneUtils.createMultiMaterialObject( new THREE.CubeGeometry(1, 1, 1, 1, 1, 1), materials );
                cubeCursor.position = calc.objToVector(this.model.lastPosition);
                this.sceneObject.add(cubeCursor);

                scene.add(this.sceneObject);

            }
        },
    });

    return {
        Model: Model
    }

});