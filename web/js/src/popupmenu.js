var SS = SS || {};
SS.popupMenu = function() {

    var that = {};

    var showing = false, cancelled = false, displayTime = 5000, autoHideReference = 0;

    var updateToolWheelPosition = function() {
	document.getElementById('toolWheel').addEventListener('mouseup', that.onMouseUp, false);
	showing = true;
	$('#toolWheel').css('left', event.clientX);
	$('#toolWheel').css('top', event.clientY);
    }

    var addActions = function() {
	if (selectionManager.selected().length == 0) {
	    $('#toolWheel').append($('#primitives'));
	} else if (selectionManager.selected().length == 1) {
	    $('#toolWheel').append($('#edit'));
	    $('#toolWheel').append($('#transforms'));
	    $('#toolWheel').append($('#copyTransforms'));
	} else if (selectionManager.selected().length == 2) {
	    $('#toolWheel').append($('#boolean'));
	}
    }

    var show = function() {
	$('#toolWheel').show();

	var reference = autoHideReference;
	setTimeout(function() {
	    if (reference === autoHideReference) {
		that.disposeIfShowing();
	    }
	}, displayTime);
    }
    
    var showIfNotCancelled = function(event) {
	if (!cancelled && !SS.constructors.active) {
	    updateToolWheelPosition();
	    addActions();
	    show();
	}
    }

    that.disposeIfShowing = function() {
	if (showing) {
	    showing = false;
	    $('#toolWheel').hide();
	    var toolbars = $('#toolWheel').children().detach();
	    $('#toolbarStaging').append(toolbars);
	    document.getElementById('toolWheel').removeEventListener('mouseup', that.onMouseUp, false);
	    ++autoHideReference;
	}
    }

    that.onMouseUp = function(event) {
	lastMouseDownTime = new Date().getTime();
	showIfNotCancelled(event);
    }

    that.onMouseDown = function(event) {
	cancelled = false;
	that.disposeIfShowing();
    }

    // Cancel the popup when for example the view has been rotated
    that.cancel = function() {
	cancelled = true;
    }



    return that;

}