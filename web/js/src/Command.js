
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

    this.showSpinner = function() {}
    this.hideSpinner = function() {}
    this.renderErrorMessage = function(message) {}
    this.clearMessages = function() {}

    this.execute = function(command) {
        this.showSpinner();
        successFn = function() {
            commands.splice(last_executed_index + 1, commands.length - (last_executed_index) - 1);
            commands.push(command);
            last_executed_index  += 1;
	    // Page history
	    if (history) {
		history.pushState(last_executed_index, null, window.location.href);
	    }
        }
        command.execute();
    }

    this.undo = function() {
	if (!this.canUndo()) {
	    this.renderErrorMessage({string:"Nothing to undo"});
	    return;
	}
        this.showSpinner();
        successFn = function() {
            last_executed_index -= 1;
        }
        commands[last_executed_index].undo();
    }

    this.redo = function() {
	if (!this.canRedo()) {
	    this.renderErrorMessage({string:"Nothing to redo"});
	    return;
	}
        this.showSpinner();
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
    
    this.inProgressSuccess = function() {
        successFn();
	this.clearMessages();
        this.hideSpinner();
    }

    this.inProgressFailure = function(error) {
        console.log("command in progress failure: " + JSON.stringify(error));
	this.renderErrorMessage(error);
        this.hideSpinner();
    }
    
}
