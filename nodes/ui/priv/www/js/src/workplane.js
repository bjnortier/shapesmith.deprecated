define([
    'src/colors',
    'src/calculations', 
    'src/interactioncoordinator', 
    'src/scene',
    ], function(
        colors,
        calc, 
        coordinator, 
        sceneModel) {

    var Model = Backbone.Model.extend({

        initialize: function(attributes) {
            _.extend(this, Backbone.Events);
            coordinator.on('mousemove', this.mousemove, this);
            coordinator.on('sceneClick', this.workplaneClick, this);
            coordinator.on('sceneDblClick', this.workplaneDblClick, this);
            coordinator.on('toolInitiated', this.toolInitiated, this);
            this.sceneView = attributes.sceneView;
            this.scene = attributes.sceneView.scene;
            this.camera = attributes.sceneView.camera;
            this.node = {
                origin: new THREE.Vector3(),
                axis: new THREE.Vector3(0,0,1),
                angle: 0
            }
            this.gridView = new GridView({model: this});
        },

        workplaneClick: function(event) {
            this.trigger('click', this.lastPosition);
        },

        workplaneDblClick: function(event) {
            this.trigger('dblclick', this.lastPosition);
        },

        mousemove: function(event) {
            var positionOnWorkplane = calc.positionOnWorkplane(event, this.node, this.camera);
            if (!this.lastPosition || !positionOnWorkplane.equals(this.lastPosition)) {
                this.lastPosition = positionOnWorkplane.addSelf(calc.objToVector(this.node.origin));
                this.attributes.sceneView.updateScene = true;
                this.trigger('positionChanged', this.lastPosition);
            }
        },

        toolInitiated: function(name) {
            if (name === undefined) {
                this.cursorViews.map(function(cursorView) {
                    cursorView.remove();
                });
                this.cursorViews = [];
            } else if (this.cursorViews.length === 0) {
                this.cursorViews = [
                    new CursorView({model: this}),
                    new CursorCoordinateConnectorView({model: this}),
                    new CursorCoordinateView({model: this}),
                    new CursorCoordinateAxisProjectionView({model: this}),
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

            if (this.sceneObject) {
                scene.remove(this.sceneObject);
            }
            this.sceneObject = new THREE.Object3D();


            var majorGridLineGeometry = new THREE.Geometry();
            var minorGridLineGeometry = new THREE.Geometry();
            var majorMaterialInside = new THREE.LineBasicMaterial({ 
                color: colors.workplane.majorGridLine, 
                opacity: 0.5, 
                transparent: false });
            var minorMaterialInside = new THREE.LineBasicMaterial({ 
                color: colors.workplane.minorGridLine, 
                opacity: 0.1, 
                transparent: true });

            majorGridLineGeometry.vertices.push(new THREE.Vector3(-100, 0, 0));
            majorGridLineGeometry.vertices.push(new THREE.Vector3(100, 0, 0));
            minorGridLineGeometry.vertices.push(new THREE.Vector3(-100, 0, 0));
            minorGridLineGeometry.vertices.push(new THREE.Vector3(100, 0, 0));

            for (var x = -100; x <= 100; ++x) {
                var material = (x % 10 == 0) ? majorMaterialInside : minorMaterialInside;
                var geometry = (x % 10 == 0) ? majorGridLineGeometry : minorGridLineGeometry;
                var line = new THREE.Line(geometry, material);
                line.position.x = x;
                line.position.z = this.model.node.origin.z;
                line.rotation.z = 90 * Math.PI / 180;
                this.sceneObject.add(line);
            }

            for (var y = -100; y <= 100; ++y) {
                var material = (y % 10 == 0) ? majorMaterialInside : minorMaterialInside;
                var geometry = (y % 10 == 0) ? majorGridLineGeometry : minorGridLineGeometry;
                var line = new THREE.Line(geometry, material);
                line.position.y = y;
                line.position.z = this.model.node.origin.z;
                this.sceneObject.add(line);
            }

            scene.add(this.sceneObject);
            sceneModel.view.updateScene = true;
        },

    });

    return new Model({sceneView: sceneModel.view});

});