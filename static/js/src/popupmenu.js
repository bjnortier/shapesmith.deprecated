var SS = SS || {};
SS.popupMenu = function() {

    var that = {};

    var updateToolWheelPosition = function(event) {
        $('#toolWheel').css('left', event.clientX);
        $('#toolWheel').css('top', event.clientY);
    }

    var addActions = function() {
        if (SS.selectionManager.getSelected().length == 0) {
            $('#toolWheel').append($('#3Dprimitives'));
            $('#toolWheel').append($('#2Dprimitives'));
            $('#toolWheel').append($('#1Dprimitives'));
        } else if (SS.selectionManager.getSelected().length == 1) {
            $('#toolWheel').append($('#edit'));
            $('#toolWheel').append($('#transforms'));
            $('#toolWheel').append($('#modifiers'));
        } else if (SS.selectionManager.getSelected().length >= 2) {
            $('#toolWheel').append($('#boolean'));
        }
    }

    var show = function() {
        $('#toolWheel').show();
        SS.mouseState.popupShowing = true;
    }
    
    var showIfFree = function(event) {
        if (SS.mouseState.isFree() &&
            !SS.editingState.isEditing()) {
            updateToolWheelPosition(event);
            addActions();
            show();
        }
    }

    that.disposeIfShowing = function() {
        if (SS.mouseState.popupShowing) {
            $('#toolWheel').hide();
            var toolbars = $('#toolWheel').children().detach();
            $('#toolbarStaging').append(toolbars);
        }
        SS.mouseState.popupShowing = false;
    }

    that.onMouseUp = function(event) {
        if ((event.button == 2) 
            ||
            ((event.button == 0) && event.shiftKey)) {
            showIfFree(event);
        } 
    }

    that.onMouseDown = function(event) {
        that.disposeIfShowing();
    }

    return that;

}
