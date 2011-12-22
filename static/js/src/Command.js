
function Command(executeFn, undoFn, redoFn) {
    var executeFn = executeFn;
    var undoFn = undoFn;
    var redoFn = redoFn;

    this.execute = function() { executeFn(); };
    this.undo = function() { undoFn(); };
    this.redo = function() { redoFn(); };
}

function CommandStack(ss) {
    var successFn;
    var commandInProgress;

    var undoStack = new Stack();
    var redoStack = new Stack();
    var ss = ss;

    this.renderErrorMessage = function(message) {
    }

    this.clearMessages = function() {
    }

    this.commit = function() {
	commandInProgress.fromCommit = SS.session.commit;
	ss.commit();
    }

    this.pop = function(commit) {
	if (undoStack.peek() && undoStack.peek().fromCommit === commit) {
	    console.info('UNDO: ' + undoStack.peek().fromCommit);
	    this.undo();
	    return true;
	}
	
	if (redoStack.peek() && redoStack.peek().toCommit === commit) {
	    console.info('REDO: ' + redoStack.peek().toCommit);
	    this.redo();
	    return true;
	}
    }

    this.execute = function(command) {

        successFn = function() {
	    ss.selectionManager.deselectAll();
	    command.toCommit = ss.session.commit;
	    undoStack.push(command);
        }
	ss.spinner.show();
	commandInProgress = command;
        command.execute();
    }

    this.undo = function() {
	ss.selectionManager.deselectAll();

	if (!this.canUndo()) {
	    throw "Nothing to undo";
	}
	ss.spinner.show();
        successFn = function() {
	    redoStack.push(undoStack.pop());
        }
	undoStack.peek().undo();
    }

    this.redo = function() {
	ss.selectionManager.deselectAll();

	if (!this.canRedo()) {
	    throw "Nothing to redo";
	}
	ss.spinner.show();
        successFn = function() {
	    undoStack.push(redoStack.pop());
        }
	redoStack.peek().redo();
    };

    this.canUndo = function() {
	return undoStack.peek() !== undefined;
    };

    this.canRedo = function() {
	return redoStack.peek() !== undefined;
    };

    
    this.success = function() {
        successFn();
	ss.spinner.hide();
	this.clearMessages();
    }

    this.error = function(error) {
	this.renderErrorMessage(error);
	ss.spinner.hide();
    }
    
}
