define([], function() {
    
    var Manager = function() {

        _.extend(this, Backbone.Events);
        this.selected = [];
    }

    Manager.prototype.selectOnly = function(id) {
        var deselected = [];
        var found = false;
        for (var i in this.selected) {
            if (this.selected[i] === id) {
                found = true;
            } else {
                deselected.push(this.selected[i]);
            }
        }
        this.selected = [id];
        
        if (deselected.length > 0) {
            this.trigger('deselected', deselected);
        }
        this.trigger('selected', [id]);

    }

    Manager.prototype.addToSelection = function(id) {
        var alreadySelected = false;
        for (var i in this.selected) {
            if (this.selected[i] == id) {
                alreadySelected = true;
            } 
        }

        if (alreadySelected) {
            var index = this.selected.indexOf(id);
            if (index !== -1) {
                this.selected.splice(this.selected.indexOf(id), 1);
                this.trigger('deselected', [id]);
            }
        } else  {
            this.selected.push(id);
            this.trigger('selected', [id]);
        }
    }

    Manager.prototype.deselectAll = function() {
        if (this.selected.length > 0) {
            var deselected = this.selected;
            selected = [];
            this.trigger('deselected', deselected);
        }
    }

    return new Manager();

});