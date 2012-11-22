 define(['src/geometrygraphsingleton', 'src/scenevieweventgenerator'], 
    function(geometryGraph, sceneViewEventGenerator) {

    var Coordinator = function() {
        _.extend(this, Backbone.Events);

        var dragThreshold = 10;
        var dragging = false;

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
        $('#scene').dblclick(function(event) {
            that.dblclick(event);
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

        this.mousemove = function(event) {
            if (this.overThreshold(eventToPosition(event))) {
                event.mouseDownEvent = this.mouseDownEvent;
                dragging = true;
                if (!sceneViewEventGenerator.drag(event)) {
                    this.trigger('drag', event);
                }
            } else {
                this.trigger('mousemove', event);
                sceneViewEventGenerator.mousemove(event);
                if (sceneViewEventGenerator.overClickable() || sceneViewEventGenerator.overDraggable()) {
                    $('#scene').css('cursor', 'pointer');
                } else {
                    $('#scene').css('cursor', '');
                }
            } 
        }

        this.mouseup = function(event) {
            if (!dragging) {

                // Prevent multiple click event on double click
                var now = new Date().getTime();
                var eventPosition = eventToPosition(event);

                var isSecondClickOfDoubleClick = false;
                if (this.lastClickTimestamp) {
                    var within300ms = (now - this.lastClickTimestamp < 300);
                    var dx = Math.abs(eventPosition.x - this.lastClickPosition .x);
                    var dy = Math.abs(eventPosition.y - this.lastClickPosition .y);
                    var isWithinThreshold = Math.sqrt(dx*dx + dy*dy) < dragThreshold;
                    isSecondClickOfDoubleClick = within300ms && isWithinThreshold;
                }
                    
                this.lastClickTimestamp = now;
                this.lastClickPosition = eventPosition;

                if (!isSecondClickOfDoubleClick) {
                    if (sceneViewEventGenerator.overClickable()) {
                        sceneViewEventGenerator.click(event);
                    } else {
                        this.trigger('sceneClick', event);
                    }
                }
            }
            sceneViewEventGenerator.mouseup(event);
            this.mouseDownEvent = undefined;
            dragging = false;
        }

        this.dblclick = function(event) {
            if (sceneViewEventGenerator.overClickable()) {
                sceneViewEventGenerator.dblclick(event);
            } else {
                this.trigger('sceneDblClick', event);
            }
        }

        this.mouseleave = function(event) {
            this.mouseDownEvent = undefined;
            dragging = false;
        }

        this.mousedown = function(event) {
            this.mouseDownEvent = event;
            sceneViewEventGenerator.mousedown(event);
        }

        this.overThreshold = function(pos2) {
            if (!this.mouseDownEvent) {
                return false;
            }
            var pos1 = eventToPosition(this.mouseDownEvent);
            var dx = Math.abs(pos1.x - pos2.x);
            var dy = Math.abs(pos1.y - pos2.y);
            return Math.sqrt(dx*dx + dy*dy) > dragThreshold;
        };

        // When a field input has focus, we want to prevent a 'click out'
        // on the scene to unselect the editing vertex, or any other scene 
        // click for that matter
        this.setFieldFocus = function(val) {
            this.fieldHasFocus = val;
        }
    }

    var eventToPosition = function(event) {
        return {
            x: event.clientX,
            y: event.clientY,
        };
    }

    return new Coordinator();
});