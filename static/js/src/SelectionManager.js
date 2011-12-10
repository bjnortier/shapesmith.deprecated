function SelectionManager() {

    var selected = [];

    this.getSelected = function() {
        var toReturn = [];
        for (var i in selected) {
            toReturn.push(selected[i]);
        }
        return toReturn;
    }

    this.size = function() {
        return selected.length;
    }

    this.shiftPick = function(sha) {
	var alreadySelected = false;
        for (var i in selected) {
            if (selected[i] == sha) {
                alreadySelected = true;
            } 
        }

        if (alreadySelected) {
            this.deselectSHA(sha);
        } else  {
            this.selectSHA(sha);
        }
    }
    
    this.pick = function(sha) {
        this.selectOnly(sha);
    }

    this.selectSHA = function(sha) {
        selected.push(sha);
        this.notify({selected : [sha]});
    }
    
    this.deselectSHA = function(sha) {
        selected.splice(selected.indexOf(sha), 1);
        this.notify({deselected : [sha]});
    }

    this.selectOnly = function(sha) {
        var deselected = [];
        var found = false;
        for (var i in selected) {
            if (selected[i] == sha) {
                found = true;
            } else {
                deselected.push(selected[i]);
            }
        }
        selected = [sha];
        
	if (deselected.length > 0) {
            this.notify({deselected : deselected});
	}
        if (!found) {
            this.notify({selected : [sha]});
        }
    }
    
    this.deselectAll = function() {
        if (selected.length > 0) {
            var deselected = selected;
            selected = [];
            this.notify({deselected : deselected});
        }
    }
    
    this.geomDocUpdated = function(event) {
	this.deselectAll();
    }

}
var selectionManager = new SelectionManager();
Observable.makeObservable(selectionManager);

