function SelectionManager() {

    _.extend(this, Backbone.Events);

    var selected = [];

    this.getSelected = function() {
        return selected.map(function(id) { return id});
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
        this.trigger('selected', [id]);
    }
    
    this.deselectID = function(id) {
        var index = selected.indexOf(id);
        if (index !== -1) {
            selected.splice(selected.indexOf(id), 1);
            this.trigger('deselected', [id]);
        }
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
            this.trigger('deselected', deselected);
        }
        this.trigger('selected', [id]);
    }
    
    this.deselectAll = function() {
        if (selected.length > 0) {
            var deselected = selected;
            selected = [];
            this.trigger('deselected', deselected);
        }
    }

    
}

