function renderErrorMessage(error) {
    $('#messages-container').empty();
    if (error.validation) {
	// No need for a message as there is already validation feedback
	console.log(error);
    } else if (error.string) {
	$('#messages-container').append('<div class="error">' + error.string + '</div>');
    } else if (error.error) {
	$('#messages-container').append('<div class="error">' + error.error + '</div>');
    } else {
	$('#messages-container').append('<div class="error">Oops. An unknown problem occurred</div>');
    }
}

function renderSuccessMessage(message) {
    $('#messages-container').empty();
    $('#messages-container').append('<div class="info">' + message + '</div>');
}


function showSpinner() {
    if ($('#progress-container').children().size() == 0) {
        $('#progress-container').append(
	    '<div id="progress"><img src="images/progress-spinner.gif" alt="in progress"/></div>');
    }
    $('#progress').show();
}

function hideSpinner() {
    $('#progress').hide();
}

function Command(executeFn, undoFn, redoFn) {
    var executeFn = executeFn;
    var undoFn = undoFn;
    var redoFn = redoFn;

    this.execute = function() { executeFn(); };
    this.undo = function() { undoFn(); };
    this.redo = function() { redoFn(); };
}

function CommandStack() {
    var commands = [];
    var last_executed_index = -1;
    var successFn;

    this.execute = function(command) {
        showSpinner();
        command.execute();
        successFn = function() {
            commands.splice(last_executed_index + 1, commands.length - (last_executed_index) - 1);
            commands.push(command);
            last_executed_index  += 1;
	    // Page history
	    history.pushState(last_executed_index, null, window.location.href);
        }
    }

    this.popState = function(executed_index) {
	// Compare popped state to current state to determine if it's 
	// undo or redo
	if (executed_index <= last_executed_index) {
	    this.undo();
	} else if (executed_index >= last_executed_index + 1) {
	    this.redo();
	} else {
	    throw new Error('Invalid history pop event.');
	}
    }
    
    this.undo = function() {
	if (!this.canUndo()) {
	    renderErrorMessage({string:"Nothing to undo"});
	    return;
	}
        showSpinner();
        successFn = function() {
            last_executed_index -= 1;
        }
        commands[last_executed_index].undo();
    }

    this.redo = function() {
	if (!this.canRedo()) {
	    renderErrorMessage({string:"Nothing to redo"});
	    return;
	}
        showSpinner();
        successFn = function() {
            last_executed_index += 1;
        }
        commands[last_executed_index + 1].redo();
    };

    this.canUndo = function() {
        return last_executed_index >= 0;
    };

    this.canRedo = function() {
        return last_executed_index < commands.length - 1;
    };



    this.inProgressSuccess = function() {
        console.log("command in progress success");
        successFn();
	$('#messages-container').empty();
        hideSpinner();
    }

    this.inProgressFailure = function(error) {
        console.log("command in progress failure: " + JSON.stringify(error));
	renderErrorMessage(error);
        hideSpinner();
    }
    
}
