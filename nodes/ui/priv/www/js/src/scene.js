define(['lib/backbone-require'], function(Backbone) {

    var Model = Backbone.Model.extend({

        initialize: function(attributes) {
            this.view = new View({model: this, el: document.getElementById('scene')});
        },

    });

    var View = Backbone.View.extend({

        initialize: function() {
            
            _.extend(this, Backbone.Events);

            var that = this;
            window.addEventListener('resize', function(event) {
                that.resize(event);
            }, false);

            var width = this.$el.width();
            var height = this.$el.height();
            this.scene = new THREE.Scene();

            // Camera
            this.camera = new THREE.PerspectiveCamera(30, width/height, 1, 10000);
            this.camera.up = new THREE.Vector3(0,0,1);
            this.camera.position = new THREE.Vector3(0,0,1000);
            this.camera.lookAt(new THREE.Vector3(0,0,0));
            this.scene.add(this.camera);

            // Renderer
            this.renderer = new THREE.WebGLRenderer({antialias: true});
            this.renderer.autoClear = true;
            this.renderer.setClearColorHex(0x080808, 0.0);
            this.renderer.setSize(width, height);
            this.renderer.sortObjects = false;

            // Lights
            this.scene.add(new THREE.AmbientLight(0x404040));
            this.pointLight1 = new THREE.PointLight( 0x888888 );
            this.pointLight1.position.set(1000, 1000, 1000);
            this.scene.add(this.pointLight1);

            this.el.appendChild(this.renderer.domElement);
            this.updateScene = true;
            this.animate();
        },

        animate: function() {
            var that = this;
            var animateFn = function() {
                requestAnimationFrame(animateFn);
                that.render();
            }
            animateFn();
        },

        render: function() {
            // Pre-rendered state
            var lastCameraPosition = this.camera.position.clone();

            this.trigger('prerender');

            var dCameraPosition = new THREE.Vector3().sub(this.camera.position, lastCameraPosition);
            if ((dCameraPosition.length() > 0.1) || (this.updateScene)) {
                this.model.trigger('cameraChange', this.camera.position);
                this.pointLight1.position = this.camera.position;
                this.renderer.render(this.scene, this.camera);
                this.updateScene = false;
            }
        },

        id: 'scene',

        resize: function(event) {
            this.camera.aspect = window.innerWidth / window.innerHeight;
            this.camera.updateProjectionMatrix();
            this.renderer.setSize(window.innerWidth, window.innerHeight);
            this.model.trigger('cameraChange', this.camera.position);
            this.updateScene = true;
        },

    });
    
    return new Model();

});