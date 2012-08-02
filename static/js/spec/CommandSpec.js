describe("Command", function() {

    var doc;
    beforeEach(function() {
        doc = new GeomDocument();
    });

    it("should modify a document", function() {

        var node = new GeomNode({type: "cuboid", sha: '1'});
        
        var command = new Command(
            function() {
                doc.add(node);
            },
            function() {
                doc.remove(node);
            },
            function() {
                doc.add(node);
            }
        );
        
        expect(doc.rootNodes).toEqual([]);
        command.execute();
        expect(doc.rootNodes).toEqual([node]);
        command.undo();
        expect(doc.rootNodes).toEqual([]);
    });

});

describe("CommandStack", function() {

    var spinnerShowing = false;
    var ss = {
	spinner: {
	    show: function() {
		spinnerShowing = true;
	    },
	    hide: function() {
		spinnerShowing = false;
	    }
	},
	session: {
	    commit: 1
	},
	commit : function() {},
	renderErrorMessage: function() {},
	clearMessages: function() {},
	selectionManager: {
	    deselectAll: function() {
	    }
	}
    };
    
    var stack = new CommandStack(ss);
    var a = 0;

    var cmd = new Command(
        function() {
            a += 1;
	    stack.success();
        },
        function() {
            a -= 1;
	    stack.success();
        },
        function() {
            a += 1;
	    stack.success();
        }
    );
    
    it("can execute commands", function() {
        stack.execute(cmd);
        
        expect(a).toEqual(1);
        expect(stack.canUndo()).toEqual(true);
        expect(stack.canRedo()).toEqual(false);

        stack.execute(cmd);

        expect(a).toEqual(2);
        expect(stack.canUndo()).toEqual(true);
        expect(stack.canRedo()).toEqual(false);
    });

    it("can undo commands", function() {
        stack.undo();
        expect(a).toEqual(1);
        expect(stack.canUndo()).toEqual(true);
        expect(stack.canRedo()).toEqual(true);

        stack.undo();
        expect(a).toEqual(0);
        expect(stack.canUndo()).toEqual(false);
        expect(stack.canRedo()).toEqual(true);

    });

    it("can redo commands", function() {
        stack.redo();
        expect(a).toEqual(1);
        expect(stack.canUndo()).toEqual(true);
        expect(stack.canRedo()).toEqual(true);

        stack.redo();
        expect(a).toEqual(2);
        expect(stack.canUndo()).toEqual(true);
        expect(stack.canRedo()).toEqual(false);

    });

});