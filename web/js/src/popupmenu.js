var SS = SS || {};
SS.popupMenu = function() {

    var that = {};

    var showing = false, cancelled = false, displayTime = 3000, autoHideReference = 0;

    var updateToolWheelPosition = function(event) {
	document.getElementById('toolWheel').addEventListener('mouseup', that.onMouseUp, false);
	showing = true;
	$('#toolWheel').css('left', event.clientX);
	$('#toolWheel').css('top', event.clientY);
    }

    var addActions = function() {
	if (selectionManager.getSelected().length == 0) {
	    $('#toolWheel').append($('#primitives'));
	} else if (selectionManager.getSelected().length == 1) {
	    $('#toolWheel').append($('#edit'));
	    $('#toolWheel').append($('#transforms'));
	    $('#toolWheel').append($('#copyTransforms'));
	} else if (selectionManager.getSelected().length == 2) {
	    $('#toolWheel').append($('#boolean'));
	}
    }

    var show = function() {
	$('#toolWheel').show();
	showing = true;
	var reference = autoHideReference;
	setTimeout(function() {
	    if (reference === autoHideReference) {
		that.disposeIfShowing();
	    }
	}, displayTime);
    }
    
    var showIfNotCancelled = function(event) {
	setTimeout(function() {
	    if (!cancelled && !SS.constructors.active) {
		updateToolWheelPosition(event);
		addActions();
		show();
	    }
	}, 200);
    }

    that.disposeIfShowing = function() {
	if (showing) {
	    $('#toolWheel').hide();
	    var toolbars = $('#toolWheel').children().detach();
	    $('#toolbarStaging').append(toolbars);
	    document.getElementById('toolWheel').removeEventListener('mouseup', that.onMouseUp, false);
	    ++autoHideReference;
	}
	showing = false;
    }

    that.onMouseUp = function(event) {
	lastMouseDownTime = new Date().getTime();
	showIfNotCancelled(event);
    }

    that.onMouseDown = function(event) {
	//if (showing) {
	//cancelled = true;
	//} else {
	    cancelled = false;
	//}
	that.disposeIfShowing();
    }

    // Cancel the popup when for example the view has been rotated
    that.cancel = function() {
	cancelled = true;
    }

    return that;

}