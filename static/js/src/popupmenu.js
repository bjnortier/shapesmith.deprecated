var SS = SS || {};
SS.popupMenu = function() {

    var that = {};

    var holding = false, cancelled = false, holdDelayTime = 500;
    // The reference counter is used to couple a mouse down and mouse up event, so
    // that the holding down of the button (delayed popup) is coupled correctly
    var referenceCounter = 0;

    var updateToolWheelPosition = function(event) {
	$('#toolWheel').css('left', event.clientX);
	$('#toolWheel').css('top', event.clientY);
    }

    var addActions = function() {
	if (selectionManager.getSelected().length == 0) {
	    $('#toolWheel').append($('#3Dprimitives'));
	    $('#toolWheel').append($('#2Dprimitives'));
	} else if (selectionManager.getSelected().length == 1) {
	    $('#toolWheel').append($('#edit'));
	    $('#toolWheel').append($('#transforms'));
	    $('#toolWheel').append($('#modifiers'));
	} else if (selectionManager.getSelected().length >= 2) {
	    $('#toolWheel').append($('#boolean'));
	}
    }

    var show = function() {
	$('#toolWheel').show();
	SS.UI_MOUSE_STATE.popupShowing = true;
    }
    
    var showIfNotCancelled = function(event) {
	if (!cancelled && 
            SS.UI_MOUSE_STATE.isFree() &&
            !SS.UI_EDITING_STATE.isEditing()) {
	    updateToolWheelPosition(event);
	    addActions();
	    show();
	}
    }

    that.disposeIfShowing = function() {
	if (SS.UI_MOUSE_STATE.popupShowing) {
	    $('#toolWheel').hide();
	    var toolbars = $('#toolWheel').children().detach();
	    $('#toolbarStaging').append(toolbars);
	}
        SS.UI_MOUSE_STATE.popupShowing = false;
    }

    that.onMouseUp = function(event) {
	holding = false;
	++referenceCounter;
    }

    that.onMouseDown = function(event) {
	var reference = referenceCounter;
	cancelled = false;
	holding = true;
	that.disposeIfShowing();
	
	if (event.button == 2) {
	    showIfNotCancelled(event);
	} else {
	    /*setTimeout(function() {
		if (holding && (reference == referenceCounter)) {
		    showIfNotCancelled(event);
		}
	    }, holdDelayTime);*/
	}
	
    }

    // Cancel the popup when for example the view has been rotated
    that.cancel = function() {
	cancelled = true;
    }

    return that;

}