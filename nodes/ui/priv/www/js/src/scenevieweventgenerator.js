define(['src/interactioncoordinator', 'src/scene'], function(coordinator, sceneModel) {


    var EventGenerator = function() {
        _.extend(this, Backbone.Events);

        this.sceneViews = [];
        this.mouseOverViews = [];

        coordinator.on('mousemove', this.mousemove, this);
    }

    EventGenerator.prototype.register = function(sceneView) {
        var index = this.sceneViews.indexOf(sceneView);
        if (index !== -1) {
            throw Error('Scene view already in event generator');
        }
        this.sceneViews.push(sceneView);
    }

    EventGenerator.prototype.deregister= function(sceneView) {
        var index = this.sceneViews.indexOf(sceneView);
        if (index === -1) {
            throw Error('Scene view not found in event generator');
        }
        this.sceneViews.splice(index, 1);
    }

    EventGenerator.prototype.mousemove = function(event) {

        var previousOverObjects = this.mouseOverViews.slice(0);
        var leaveObjects = this.mouseOverViews.slice(0);

        this.mouseOverViews = [];
        var that = this;
        this.findViewsForEvent(event).map(function(view) {
            if (previousOverObjects.indexOf(view) === -1) {
                view.trigger('mouseEnter', event);
            }
            that.mouseOverViews.push(view);
            var index = leaveObjects.indexOf(view);
            if (index !== -1) {
                leaveObjects.splice(index, 1);
            }
        });
        leaveObjects.map(function(view) {
            view.trigger('mouseLeave', event);
        });

    }

    EventGenerator.prototype.findViewsForEvent = function(event) {
        var selector = createSelector(sceneModel.view.camera, event);
        var foundObjects = this.sceneViews.reduce(function(acc, view) {
            var distance = selector([view.sceneObject, view.hiddenSelectionObject]);
            if (distance !== undefined) {
                acc.push({view: view, distance: distance});
            }
            return acc;
        }, []);

        foundObjects.sort(function(a,b) {
            return a.distance > b.distance;
        });
        return _.pluck(foundObjects, 'view');
    }

    var createSelector = function(camera, event) {
        var mouse = {};
        mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
        mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;

        var vector = new THREE.Vector3( mouse.x, mouse.y, 0.5);
        var projector = new THREE.Projector();
        var mouse3D = projector.unprojectVector(vector, camera);
        var ray = new THREE.Ray(camera.position, null);
        ray.direction = mouse3D.subSelf(camera.position).normalize();

        return function(objects) {
            // Only intersect mesh objects
            var allMeshes = [];
            var searchFn = function(obj) {
                if (obj instanceof THREE.Mesh) {
                    allMeshes.push(obj);
                }
                if (obj.children && (obj.children.length > 0)) {
                    obj.children.map(searchFn);
                }
            }
            searchFn({children: objects});
            var intersects = ray.intersectObjects(allMeshes);
            if (intersects.length > 0) {
                return intersects[0].distance;
            } else {
                return undefined;
            }
        }
    }

    return new EventGenerator();

});