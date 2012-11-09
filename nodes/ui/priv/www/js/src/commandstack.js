define([
    'lib/backbone-require',
    'src/stack',
    ], function(Backbone, Stack) {

    var CommandStack = function() {

        var undoStack = new Stack();
        var redoStack = new Stack();

        _.extend(this, Backbone.Events);

        this.do = function(command) {

            var successFn = function(sha) {
                command.fromCommit = $.getQueryParam("commit");
                var url = window.location.pathname + '?commit=' + sha;
                history.pushState({commit: sha}, SS.design, url);
                command.toCommit = sha;
                undoStack.push(command);

            };
            var erroFn = function(msg) {

            }
            command.do(successFn, erroFn);

        }

        this.undo = function() {
            if (!this.canUndo()) {
                throw Error("Nothing to undo");
            }
            undoStack.peek().undo();
            redoStack.push(undoStack.pop());
        }

        this.redo = function() {
            if (!this.canRedo()) {
                throw Error("Nothing to redo");
            }
            redoStack.peek().redo();
            undoStack.push(redoStack.pop());
        };    

        this.canUndo = function() {
            return undoStack.peek() !== undefined;
        };

        this.canRedo = function() {
            return redoStack.peek() !== undefined;
        };


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

        this.success = function(sha) {
            
        }

        this.error = function(msg) {
            console.error(msg);
        }
    }

    var commandStack = new CommandStack();

    window.onpopstate = function(event) { 
        var commit = (event.state && event.state.commit) || $.getQueryParam("commit");
        if (!commandStack.pop(commit)) {
            // No command stack available - load from disk
            console.error('load from disk');
        }
    };

    return commandStack;

});