 define(['src/geometrygraph', 'src/selection', 'src/scenevieweventgenerator'], 
    function(geometryGraph, selection, sceneViewEventGenerator) {

    var Coordinator = function() {
        _.extend(this, Backbone.Events);

        this.dragThreshold = 10;
        this.dragging = false;

        var that = this;
        window.addEventListener('keydown', function(event) {
            that.trigger('keydown', event);
        }, false);

        $('#scene').mousemove(function(event) {
            that.mousemove(event);
        });
        $('#scene').mouseup(function(event) {
            that.trigger('mouseup', event);
            that.mouseup(event);
        });
         $('#scene').mouseleave(function(event) {
            that.mouseleave(event);
        });
        $('#scene').mousedown(function(event) {
            that.trigger('mousedown', event);
            that.mousedown(event);
        });
        $('#scene').mousewheel(function(event) {
            that.trigger('mousewheel', event);
        });

    }

    Coordinator.prototype.mousemove = function(event) {
        if (this.overThreshold(eventToPosition(event))) {
            event.mouseDownEvent = this.mouseDownEvent;
            this.dragging = true;
            this.trigger('drag', event);
        } else {
            this.trigger('mousemove', event);
            sceneViewEventGenerator.mousemove(event);
            if (sceneViewEventGenerator.overClickable()) {
                $('#scene').css('cursor', 'pointer');
            } else {
                $('#scene').css('cursor', '');
            }
        } 

    }

    Coordinator.prototype.mouseup = function(event) {
        if (!this.dragging) {
            this.trigger('click', event);
            if (!sceneViewEventGenerator.click(event)) {
                selection.deselectAll();
            }
        }
        this.mouseDownEvent = undefined;
        this.dragging = false;
    }

    Coordinator.prototype.mouseleave = function(event) {
        this.mouseDownEvent = undefined;
        this.dragging = false;
    }

    Coordinator.prototype.mousedown = function(event) {
        this.mouseDownEvent = event;
    }


    Coordinator.prototype.initiateTool = function(name) {
        selection.deselectAll();
        if (name === 'point') {
            geometryGraph.createPointPrototype();
        }
        if (name === 'line') {
            geometryGraph.createLinePrototype();
        }
    }

    Coordinator.prototype.overThreshold = function(pos2) {
        if (!this.mouseDownEvent) {
            return false;
        }
        var pos1 = eventToPosition(this.mouseDownEvent);
        var dx = Math.abs(pos1.x - pos2.x);
        var dy = Math.abs(pos1.y - pos2.y);
        return Math.sqrt(dx*dx + dy*dy) > this.dragThreshold;
    };

    var eventToPosition = function(event) {
        return {
            x: event.clientX,
            y: event.clientY,
        };
    }

    return new Coordinator();
});