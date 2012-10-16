define(function() {

    var Model = Backbone.Model.extend({

        initialize: function(attributes) {
            this.view = new View({model: this});
        },

    });

    var View = Backbone.View.extend({

        initialize: function() {
            $('body').append(this.$el);

            this.width = this.$el.width();
            this.height = this.$el.height();

            this.scene = new THREE.Scene();

            this.camera = new THREE.PerspectiveCamera(30, this.width/this.height, 1, 10000);
            this.camera.up = new THREE.Vector3(0,0,1);
            this.camera.position = new THREE.Vector3(1000,0,0);
            this.camera.lookAt(new THREE.Vector3(0,0,0));
            this.scene.add(this.camera);


            this.renderer = new THREE.WebGLRenderer({antialias: true});
            this.renderer.autoClear = true;
            this.renderer.setClearColorHex(0x080808, 0.0);
            this.renderer.setSize(this.width, this.height);
            this.renderer.sortObjects = false;

            // Lights
            this.scene.add(new THREE.AmbientLight(0x404040));
            var pointLight = new THREE.PointLight( 0x999999 );
            pointLight.position.set(0, 0, 100);
            this.scene.add(pointLight);
            pointLight = new THREE.PointLight( 0x999999 );
            pointLight.position.set(0, 0, -100);
            this.scene.add(pointLight);

            var materials = [
                new THREE.MeshLambertMaterial( { ambient: 0xbbbbbb,  side: THREE.DoubleSide } ),
                new THREE.MeshBasicMaterial( { color: 0xffff00, wireframe: false, transparent: true, opacity: 0.5, side: THREE.DoubleSide } )
            ];
            var object = THREE.SceneUtils.createMultiMaterialObject( new THREE.CubeGeometry(100, 100, 100, 4, 4, 4), materials );
            this.scene.add( object );

            this.el.appendChild(this.renderer.domElement);

            this.render();
        },

        render: function() {
            // var azimuth = -1.373, elevation = 1.08;
            // var target = { azimuth: -1.373, elevation: 1.08 };
            // var distance = 1000, distanceTarget = 300;
            // var targetScenePosition = new THREE.Vector3(0,0,0);

            // azimuth += (target.azimuth - azimuth) * 0.2;
            // elevation += (target.elevation - elevation) * 0.3;

            // var dDistance = (distanceTarget - distance) * 0.3;

            // if (distance + dDistance > 1000) {
            //     distanceTarget = 1000;
            //     distance = 1000;
            // } else if (distance + dDistance < 3) {
            //     distanceTarget = 3;
            //     distance = 3;
            // } else {
            //     distance += dDistance;
            // }

            // var lastCameraPosition = this.camera.position.clone();

            // this.camera.position.x = distance * Math.sin(elevation) * Math.cos(azimuth);
            // this.camera.position.y = distance * Math.sin(elevation) * Math.sin(azimuth);
            // this.camera.position.z = distance * Math.cos(elevation);

            this.renderer.render(this.scene, this.camera);
        },

        id: 'scene',

        events: {
            'mousedown' : 'mousedown',
            'mouseover' : 'mouseover',
            'mouseup'   : 'mouseup',
        },

        mouseup: function() {
            console.log('mouseup');
        },        

        mousedown: function() {
            console.log('mousedown');
        },        

        mouseover: function() {
            console.log('mouseover');
        },

    });
    
    return {
        Model: Model
    };

});