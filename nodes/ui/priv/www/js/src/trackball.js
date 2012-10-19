define(['src/scene',  'src/interactioncoordinator'], function(sceneModel, coordinator) {

    var Trackball = function(scene, camera) {
        this.camera = camera;
        this.scene  = scene;

        this.minDistance = 3;
        this.maxDistance = 1000;


        this.position = { azimuth: -1.373, elevation: 1.08, distance: 1000 };
        this.target = { azimuth: -1.373, elevation: 1.08, distance: 300, scenePosition: new THREE.Vector3() };

        coordinator.on('drag', this.drag, this);
        coordinator.on('mousedown', this.mousedown, this);
        coordinator.on('mouseup', this.mouseup, this);
        coordinator.on('mousewheel', this.mousewheel, this);
        coordinator.on('keydown', this.keydown, this);
        sceneModel.view.on('prerender', this.updateCamera, this);
    };

    Trackball.prototype.mousedown = function(event) {
        this.targetOnDown = { 
            azimuth:  this.target.azimuth, 
            elevation: this.target.elevation,
        };
    };    

    Trackball.prototype.mouseup = function(event) {
        this.state = undefined;
    };

    Trackball.prototype.drag = function(event) {
        if (this.lastMousePosition) {
            var eventPosition = eventToPosition(event);
            var mouseDownPosition = eventToPosition(event.mouseDownEvent);

            var dMouseFromDown = {
                x: eventPosition.x - mouseDownPosition.x,
                y: eventPosition.y - mouseDownPosition.y,
            }
            var dMouseFromLast = {
                x: eventPosition.x - this.lastMousePosition.x,
                y: eventPosition.y - this.lastMousePosition.y,
            }

            if (this.state === undefined) {
                if (!event.shiftKey && (event.button === 0)) {
                    this.state = 'rotating';
                } else if ((event.button === 1) ||
                    ((event.button === 0) && event.shiftKey)) {
                    this.state = 'panning';
                }
            }

            if (this.state === 'rotating') {
                var zoomDamp = 0.0005 * Math.sqrt(this.position.distance);
                this.target.azimuth = this.targetOnDown.azimuth - dMouseFromDown.x * zoomDamp;
                this.target.elevation = this.targetOnDown.elevation - dMouseFromDown.y * zoomDamp;
                this.target.elevation = this.target.elevation > Math.PI ? Math.PI : this.target.elevation;
                this.target.elevation = this.target.elevation < 0 ? 0 : this.target.elevation;
            }

            if (this.state === 'panning') {

                var camVec = this.camera.position.clone().negate().normalize();
                var upVec = new THREE.Vector3(0,0,1);
                var mouseLeftVec = new THREE.Vector3().cross(upVec, camVec);
                var mouseUpVec = new THREE.Vector3().cross(camVec, mouseLeftVec);
                
                var dPos = mouseLeftVec.clone().multiplyScalar(dMouseFromLast.x).addSelf(
                    mouseUpVec.clone().multiplyScalar(dMouseFromLast.y));
                dPos.multiplyScalar(Math.sqrt(this.position.distance)/50);

                this.target.scenePosition.addSelf(dPos);   
            }

        }
        this.lastMousePosition = eventToPosition(event);
    };

    Trackball.prototype.mousewheel = function(event) {
        var factor = 0.01;
        event.preventDefault();
        event.stopPropagation();
        if (event.originalEvent.wheelDelta) {
            this.zoom(event.originalEvent.wheelDelta * Math.sqrt(this.position.distance)*factor);
        }
        // For Firefox
        if (event.originalEvent.detail) {
            this.zoom(-event.originalEvent.detail*60 * Math.sqrt(this.position.distance)*factor);
        }
    };

    Trackball.prototype.keydown = function(event) {
        var factor = 10;
        switch (event.keyCode) {
        case 187:
        case 107:
            this.zoom(Math.sqrt(this.position.distance)*factor);
            event.preventDefault();
            break;
        case 189:
        case 109:
            this.zoom(-Math.sqrt(this.position.distance)*factor);
            event.preventDefault();
            break;
        }
    }

    Trackball.prototype.zoom = function(delta) {
        this.target.distance -= delta;
    }

    Trackball.prototype.updateCamera = function() {
        var damping = 0.25;
        this.position.azimuth += (this.target.azimuth - this.position.azimuth) * damping;
        this.position.elevation += (this.target.elevation - this.position.elevation) * damping;

        var dDistance = (this.target.distance - this.position.distance) * damping;
        var newDistance = this.position.distance + dDistance;
        if (newDistance > this.maxDistance) {
            this.target.distance = this.maxDistance;
            this.position.distance = this.maxDistance;
        } else if (newDistance < this.minDistance) {
            this.target.distance = this.minDistance;
            this.position.distance = this.minDistance;
        } else {
            this.position.distance = newDistance;
        }

        this.camera.position.x = this.position.distance * Math.sin(this.position.elevation) * Math.cos(this.position.azimuth);
        this.camera.position.y = this.position.distance * Math.sin(this.position.elevation) * Math.sin(this.position.azimuth);
        this.camera.position.z = this.position.distance * Math.cos(this.position.elevation);
        this.camera.position.addSelf(this.scene.position);

        var dScenePosition = new THREE.Vector3().sub(this.target.scenePosition, this.scene.position).multiplyScalar(0.2);
        this.scene.position.addSelf(dScenePosition);
        this.camera.lookAt(this.scene.position);


    };

    var eventToPosition = function(event) {
        return {
            x: event.clientX,
            y: event.clientY,
        };
    }

    return new Trackball(sceneModel.view.scene, sceneModel.view.camera);


});