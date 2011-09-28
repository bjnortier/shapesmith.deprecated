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

    this.shiftPick = function(path) {
	var alreadySelected = false;
        for (var i in selected) {
            if (selected[i] == path) {
                alreadySelected = true;
            } 
        }

        if (alreadySelected) {
            this.deselectPath(path);
        } else  {
            this.selectPath(path);
        }
    }
    
    this.pick = function(path) {
        this.selectOnly(path);
    }

    this.selectPath = function(path) {
        selected.push(path);
        this.notify({selected : [path]});
    }
    
    this.deselectPath = function(path) {
        selected.splice(selected.indexOf(path), 1);
        this.notify({deselected : [path]});
    }

    this.selectOnly = function(path) {
        var deselected = [];
        var found = false;
        for (var i in selected) {
            if (selected[i] == path) {
                found = true;
            } else {
                deselected.push(selected[i]);
            }
        }
        selected = [path];
        
	if (deselected.length > 0) {
            this.notify({deselected : deselected});
	}
        if (!found) {
            this.notify({selected : [path]});
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

