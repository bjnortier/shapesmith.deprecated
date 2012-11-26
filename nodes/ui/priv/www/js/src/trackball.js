define(['src/scene',  'src/interactioncoordinator'], function(sceneModel, coordinator) {

    var Trackball = function(scene, camera) {

        var minDistance = 3;
        var maxDistance = 1000;
        var position = { azimuth: 0.373, elevation: 1.08, distance: 1000 };
        var target = { azimuth: 0.373, elevation: 1.08, distance: 300, scenePosition: new THREE.Vector3() };
        var lastMousePosition = undefined;
        var targetOnDown = undefined;
        var state = undefined;
        var damping = 0.25;

        if ($.getQueryParam("noFadein")) {
            damping = 1.0;
            target.distance = 200;
        }

        this.mousedown = function(event) {
            targetOnDown = { 
                azimuth:  target.azimuth, 
                elevation: target.elevation,
            };
        };    

        this.mouseup = function(event) {
            state = undefined;
            lastMousePosition = undefined;
        };

        this.drag = function(event) {
            if (lastMousePosition) {
                var eventPosition = eventToPosition(event);
                var mouseDownPosition = eventToPosition(event.mouseDownEvent);

                var dMouseFromDown = {
                    x: eventPosition.x - mouseDownPosition.x,
                    y: eventPosition.y - mouseDownPosition.y,
                }
                var dMouseFromLast = {
                    x: eventPosition.x - lastMousePosition.x,
                    y: eventPosition.y - lastMousePosition.y,
                }

                if (state === undefined) {
                    if (!event.shiftKey && (event.button === 0)) {
                        state = 'rotating';
                    } else if ((event.button === 1) ||
                        ((event.button === 0) && event.shiftKey)) {
                        state = 'panning';
                    }
                }

                if (state === 'rotating') {
                    var zoomDamp = 0.0005 * Math.sqrt(position.distance);
                    target.azimuth = targetOnDown.azimuth - dMouseFromDown.x * zoomDamp;
                    target.elevation = targetOnDown.elevation - dMouseFromDown.y * zoomDamp;
                    target.elevation = target.elevation > Math.PI ? Math.PI : target.elevation;
                    target.elevation = target.elevation < 0 ? 0 : target.elevation;
                }

                if (state === 'panning') {

                    var camVec = camera.position.clone().negate().normalize();
                    var upVec = new THREE.Vector3(0,0,1);
                    var mouseLeftVec = new THREE.Vector3().cross(upVec, camVec);
                    var mouseUpVec = new THREE.Vector3().cross(camVec, mouseLeftVec);
                    
                    var dPos = mouseLeftVec.clone().multiplyScalar(dMouseFromLast.x).addSelf(
                        mouseUpVec.clone().multiplyScalar(dMouseFromLast.y));
                    dPos.multiplyScalar(Math.sqrt(position.distance)/50);

                    target.scenePosition.addSelf(dPos);   
                }

            }
            lastMousePosition = eventToPosition(event);
        };

        this.mousewheel = function(event) {
            var factor = 0.01;
            event.preventDefault();
            event.stopPropagation();
            if (event.originalEvent.wheelDelta) {
                this.zoom(event.originalEvent.wheelDelta * Math.sqrt(position.distance)*factor);
            }
            // For Firefox
            if (event.originalEvent.detail) {
                this.zoom(-event.originalEvent.detail*60 * Math.sqrt(position.distance)*factor);
            }
        };

        this.keyup = function(event) {
            var factor = 10;
            switch (event.keyCode) {
            case 187:
            case 107:
                this.zoom(Math.sqrt(position.distance)*factor);
                event.preventDefault();
                break;
            case 189:
            case 109:
                this.zoom(-Math.sqrt(position.distance)*factor);
                event.preventDefault();
                break;
            }
        }

        this.zoom = function(delta) {
            target.distance -= delta;
        }

        this.updateCamera = function() {

            position.azimuth += (target.azimuth - position.azimuth) * damping;
            position.elevation += (target.elevation - position.elevation) * damping;

            var dDistance = (target.distance - position.distance) * damping;
            var newDistance = position.distance + dDistance;
            if (newDistance > maxDistance) {
                target.distance = maxDistance;
                position.distance = maxDistance;
            } else if (newDistance < minDistance) {
                target.distance = minDistance;
                position.distance = minDistance;
            } else {
                position.distance = newDistance;
            }

            camera.position.x = position.distance * Math.sin(position.elevation) * Math.cos(position.azimuth);
            camera.position.y = position.distance * Math.sin(position.elevation) * Math.sin(position.azimuth);
            camera.position.z = position.distance * Math.cos(position.elevation);
            camera.position.addSelf(scene.position);

            var dScenePosition = new THREE.Vector3().sub(target.scenePosition, scene.position).multiplyScalar(0.2);
            scene.position.addSelf(dScenePosition);
            camera.lookAt(scene.position);

        }

        coordinator.on('drag', this.drag, this);
        coordinator.on('mousedown', this.mousedown, this);
        coordinator.on('mouseup', this.mouseup, this);
        coordinator.on('mousewheel', this.mousewheel, this);
        coordinator.on('keyup', this.keyup, this);
        sceneModel.view.on('prerender', this.updateCamera, this);

    } 

    var eventToPosition = function(event) {
        return {
            x: event.clientX,
            y: event.clientY,
        };
    }

    return new Trackball(sceneModel.view.scene, sceneModel.view.camera);


});