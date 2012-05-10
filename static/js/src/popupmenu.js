var SS = SS || {};
SS.popupMenu = function() {

    var that = {};

    var updateToolWheelPosition = function(event) {
	$('#toolWheel').css('left', event.clientX);
	$('#toolWheel').css('top', event.clientY);
    }

    var addActions = function() {
	if (selectionManager.getSelected().length == 0) {
	    $('#toolWheel').append($('#3Dprimitives'));
	    $('#toolWheel').append($('#2Dprimitives'));
	    $('#toolWheel').append($('#1Dprimitives'));
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
    
    var showIfFree = function(event) {
	if (SS.UI_MOUSE_STATE.isFree() &&
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
	if ((event.button == 2) 
            ||
	    ((event.button == 0) && event.shiftKey)) {
	    showIfFree(event);
        } else {
            SS.UI_MOUSE_STATE.free();
        }
    }

    that.onMouseDown = function(event) {
        that.disposeIfShowing();
    }

    return that;

}
