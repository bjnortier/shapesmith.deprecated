function SelectionManager() {

    _.extend(this, Backbone.Events);

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

    this.shiftPick = function(id) {
	var alreadySelected = false;
        for (var i in selected) {
            if (selected[i] == id) {
                alreadySelected = true;
            } 
        }

        if (alreadySelected) {
            this.deselectID(id);
        } else  {
            this.selectID(id);
        }
    }
    
    this.pick = function(id) {
        this.selectOnly(id);
    }

    this.selectID = function(id) {
        selected.push(id);
        this.notify({selected : [id]});
        this.trigger('selected', [id]);
    }
    
    this.deselectID = function(id) {
        selected.splice(selected.indexOf(id), 1);
        this.notify({deselected : [id]});
        this.trigger('deselected', [id]);
    }

    this.selectOnly = function(id) {
        var deselected = [];
        var found = false;
        for (var i in selected) {
            if (selected[i] == id) {
                found = true;
            } else {
                deselected.push(selected[i]);
            }
        }
        selected = [id];
        
	if (deselected.length > 0) {
            this.notify({deselected : deselected});
            this.trigger('deselected', deselected);
	}
        this.notify({selected : [id]});
        this.trigger('selected', [id]);
    }
    
    this.deselectAll = function() {
        if (selected.length > 0) {
            var deselected = selected;
            selected = [];
            this.notify({deselected : deselected});
            this.trigger('deselected', deselected);
        }
    }

    
}

