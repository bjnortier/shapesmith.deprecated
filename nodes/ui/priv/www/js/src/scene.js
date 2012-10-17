define(['src/trackball'], function(trackball) {

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

            // Camera
            this.camera = new THREE.PerspectiveCamera(30, this.width/this.height, 1, 1000);
            this.camera.up = new THREE.Vector3(0,0,1);
            this.camera.position = new THREE.Vector3(0,0,1000);
            this.camera.lookAt(new THREE.Vector3(0,0,0));
            this.scene.add(this.camera);

            // Renderer
            this.renderer = new THREE.WebGLRenderer({antialias: true});
            this.renderer.autoClear = true;
            this.renderer.setClearColorHex(0x080808, 0.0);
            this.renderer.setSize(this.width, this.height);
            this.renderer.sortObjects = false;

            // Lights
            this.scene.add(new THREE.AmbientLight(0x404040));
            this.pointLight1 = new THREE.PointLight( 0x999999 );
            this.pointLight1.position.set(1000, 1000, 1000);
            this.scene.add(this.pointLight1);

            var materials = [
                new THREE.MeshLambertMaterial( { ambient: 0xbbbbbb,  side: THREE.DoubleSide } ),
                new THREE.MeshBasicMaterial( { color: 0xffff00, wireframe: false, transparent: true, opacity: 0.5, side: THREE.DoubleSide } )
            ];
            var object = THREE.SceneUtils.createMultiMaterialObject( new THREE.CubeGeometry(50, 50, 50, 4, 4, 4), materials );
            this.scene.add( object );

            this.el.appendChild(this.renderer.domElement);
            this.trackball = new trackball.Trackball();
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
            this.trackball.updateCamera(this.camera);
            this.pointLight1.position = this.camera.position;
            this.renderer.render(this.scene, this.camera);
        },

        id: 'scene',

        events: {
            'mousedown' : 'mousedown',
            'mousemove' : 'mousemove',
            'mouseup'   : 'mouseup',
        },

        mouseup: function(event) {
            this.trackball.mouseup(event);
        },        

        mousedown: function() {
            this.trackball.mousedown(event);
        },        

        mousemove: function() {
            this.trackball.mousemove(event);
        },

    });
    
    return {
        Model: Model
    };

});