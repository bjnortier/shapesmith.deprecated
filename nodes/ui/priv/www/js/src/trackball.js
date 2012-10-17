define(function() {

    var Trackball = function() {

        this.minDistance = 3;
        this.maxDistance = 1000;
        this.panOrRotateThreshold = 10;

        this.position = { azimuth: -1.373, elevation: 1.08, distance: 1000, scenePosition: new THREE.Vector3() };
        this.target = { azimuth: -1.373, elevation: 1.08, distance: 300, scenePosition: new THREE.Vector3() };

    };

    Trackball.prototype.mousedown = function(event) {
        this.mouseDownPosition = eventToPosition(event);
        this.targetOnDown = { 
            azimuth:  this.target.azimuth, 
            elevation: this.target.elevation,
        };
    };    

    Trackball.prototype.mouseup = function(event) {
        this.mouseDownPosition = undefined;
        this.state = undefined;
    };

    Trackball.prototype.mousemove = function(event) {

        if (this.mouseDownPosition) {
            var eventPosition = eventToPosition(event);

            if (this.state === undefined) {
                if (this.overThreshold(eventPosition)) {
                    this.state = 'rotating';
                }
            }

            if (this.state === 'rotating') {
                var zoomDamp = 0.0005 * Math.sqrt(this.position.distance);
                this.target.azimuth = this.targetOnDown.azimuth - (eventPosition.x - this.mouseDownPosition.x) * zoomDamp;
                this.target.elevation = this.targetOnDown.elevation - (eventPosition.y - this.mouseDownPosition.y) * zoomDamp;
                this.target.elevation = this.target.elevation > Math.PI ? Math.PI : this.target.elevation;
                this.target.elevation = this.target.elevation < 0 ? 0 : this.target.elevation;
            }

        }
    };

    Trackball.prototype.updateCamera = function(camera) {
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

        camera.position.x = this.position.distance * Math.sin(this.position.elevation) * Math.cos(this.position.azimuth);
        camera.position.y = this.position.distance * Math.sin(this.position.elevation) * Math.sin(this.position.azimuth);
        camera.position.z = this.position.distance * Math.cos(this.position.elevation);

        camera.lookAt(new THREE.Vector3(0,0,0));

    };

    Trackball.prototype.overThreshold = function(pos2) {
        if (!this.mouseDownPosition) {
            return false;
        }
        var pos1 = this.mouseDownPosition;
        var dx = Math.abs(pos1.x - pos2.x);
        var dy = Math.abs(pos1.y - pos2.y);
        return Math.sqrt(dx*dx + dy*dy) > this.panOrRotateThreshold;
    };

    var eventToPosition = function(event) {
        return {
            x: event.clientX,
            y: event.clientY,
        };
    }



    return {
        Trackball: Trackball
    }

});