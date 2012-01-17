var SS = SS || {};
SS.popupMenu = function() {

    var that = {};


    var showing = false, holding = false, cancelled = false, holdDelayTime = 500;
    // The reference counter is used to couple a mouse down and mouse up event, so
    // that the holding down of the button (delayed popup) is coupled correctly
    var referenceCounter = 0;

    var updateToolWheelPosition = function(event) {
	showing = true;
	$('#toolWheel').css('left', event.clientX);
	$('#toolWheel').css('top', event.clientY);
    }

    var addActions = function() {
	if (selectionManager.getSelected().length == 0) {
	    $('#toolWheel').append($('#3Dprimitives'));
	    $('#toolWheel').append($('#1Dprimitives'));
	} else if (selectionManager.getSelected().length == 1) {
	    $('#toolWheel').append($('#edit'));
	    $('#toolWheel').append($('#transforms'));
	    $('#toolWheel').append($('#copyTransforms'));
	} else if (selectionManager.getSelected().length >= 2) {
	    $('#toolWheel').append($('#boolean'));
	}
    }

    var show = function() {
	$('#toolWheel').show();
	showing = true;
    }
    
    var showIfNotCancelled = function(event) {
	if (!cancelled && !SS.constructors.active) {
	    updateToolWheelPosition(event);
	    addActions();
	    show();
	}
    }

    that.disposeIfShowing = function() {
	if (showing) {
	    $('#toolWheel').hide();
	    var toolbars = $('#toolWheel').children().detach();
	    $('#toolbarStaging').append(toolbars);
	}
	showing = false;
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
	    setTimeout(function() {
		if (holding && (reference == referenceCounter)) {
		    showIfNotCancelled(event);
		}
	    }, holdDelayTime);
	}
	
    }

    // Cancel the popup when for example the view has been rotated
    that.cancel = function() {
	cancelled = true;
    }

    that.isShowing = function() {
	return showing;
    }

    return that;

}