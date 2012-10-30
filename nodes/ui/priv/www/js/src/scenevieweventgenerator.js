define(['src/scene'], function(sceneModel) {

    var EventGenerator = function() {
        _.extend(this, Backbone.Events);
        var sceneViews = [],
            mouseOverViews = [],
            mouseDownOnDraggableViews = [],
            mouseIsDown = false;

        this.register = function(sceneView) {
            var index = sceneViews.indexOf(sceneView);
            if (index !== -1) {
                throw Error('Scene view already in event generator');
            }
            sceneViews.push(sceneView);
        }

        this.deregister= function(sceneView) {
            var index = sceneViews.indexOf(sceneView);
            if (index === -1) {
                throw Error('Scene view not found in event generator');
            }
            sceneViews.splice(index, 1);
        }

        this.mousedown = function(event) {
            mouseDownOnDraggableViews = this.findViewsForEvent(event).filter(function(view) {
                return view.draggable;
            });
            mouseIsDown = true;
        }

        this.mouseup = function(event) {
            if (mouseDownOnDraggableViews.length > 0) {
                mouseDownOnDraggableViews[0].trigger('dragEnded', event);
            }
            mouseDownOnDraggableViews = [];
            mouseIsDown = false;
        }

        this.mousemove = function(event) {

            var previousOverObjects = mouseOverViews.slice(0);
            var leaveObjects = mouseOverViews.slice(0);

            mouseOverViews = [];
            this.findViewsForEvent(event).map(function(view) {
                if (previousOverObjects.indexOf(view) === -1) {
                    view.trigger('mouseEnter', event);
                }
                mouseOverViews.push(view);
                var index = leaveObjects.indexOf(view);
                if (index !== -1) {
                    leaveObjects.splice(index, 1);
                }
            });
            leaveObjects.map(function(view) {
                view.trigger('mouseLeave', event);
            });

        }

        this.drag = function(event) {
            if (mouseDownOnDraggableViews.length > 0) {
                mouseDownOnDraggableViews[0].trigger('drag', event);
                return true;
            } else {
                return false;
            }
        }

        this.overClickable = function(event) {
            var clickableViews = mouseOverViews.filter(function(view) {
                return view.clickable;
            });
            return clickableViews.length > 0;
        }

        this.overDraggable = function(event) {
            var draggableViews = mouseOverViews.filter(function(view) {
                return view.draggable;
            });
            return draggableViews.length > 0;
        }

        this.click = function(event) {
            // Return value is used to deselect all of no scene view
            // click event is generated
            var clickableViews = mouseOverViews.filter(function(view) {
                return view.clickable;
            });
            if (clickableViews.length > 0) {
                clickableViews[0].trigger('click', event);
                this.trigger('sceneViewClick', {event: event, view: clickableViews[0]});
                return true;
            } else {
                return false;
            }
        }

        this.dblclick = function(event) {
            var clickableViews = mouseOverViews.filter(function(view) {
                return view.clickable;
            });
            if (clickableViews.length > 0) {
                clickableViews[0].trigger('dblclick', event);
            }
        }

        this.findViewsForEvent = function(event) {
            var selector = createSelector(sceneModel.view.camera, event);
            var foundObjects = sceneViews.reduce(function(acc, view) {
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