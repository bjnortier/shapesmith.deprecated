
function Command(executeFn, undoFn, redoFn) {
    var executeFn = executeFn;
    var undoFn = undoFn;
    var redoFn = redoFn;

    this.execute = function() { executeFn(); };
    this.undo = function() { undoFn(); };
    this.redo = function() { redoFn(); };
}

function CommandStack() {
    var successFn;
    var commandInProgress;

    var undoStack = new Stack();
    var redoStack = new Stack();

    _.extend(this, Backbone.Events);

    this.commit = function() {
        commandInProgress.fromCommit = SS.session.commit;
        SS.commit();
    }

    this.pop = function(commit) {
        this.trigger('beforePop');

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
            command.toCommit = SS.session.commit;
            undoStack.push(command);
        }
        SS.spinner.show();
        commandInProgress = command;
        command.execute();
    }

    this.undo = function() {
        if (!this.canUndo()) {
            throw "Nothing to undo";
        }
        SS.spinner.show();
        successFn = function() {
            redoStack.push(undoStack.pop());
        }
        undoStack.peek().undo();
    }

    this.redo = function() {
        if (!this.canRedo()) {
            throw "Nothing to redo";
        }
        SS.spinner.show();
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
        SS.spinner.hide();
    }

    this.error = function(error) {
        SS.renderErrorMessage(error);
        SS.spinner.hide();
    }
    
}
