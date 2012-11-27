define([
    'lib/underscore-require', 
    'lib/backbone-require', 
    'src/geometrygraphsingleton', 
    'src/interactioncoordinator'], 
    function(_, Backbone, geometryGraph, coordinator, geomToolbarModel) {
    
    var Manager = function() {

        _.extend(this, Backbone.Events);
        this.selected = [];
        this.canSelect = false;

        // var that = this;
        // coordinator.on('sceneClick', function() {
        //     that.deselectAll();
        // });
        // geometryGraph.on('committed', function() {
        //     that.deselectAll();
        // });
        // geometryGraph.on('cancelled', function() {
        //     that.deselectAll();
        // });        

        this.selectOnly = function(id) {
            if (!this.canSelect) {
                return;
            }

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
                this.trigger('deselected', deselected, this.selected);
            }
            this.trigger('selected', [id], this.selected);

        }

        this.addToSelection = function(id) {
            if (!this.canSelect) {
                return
            }
            
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
                    this.trigger('deselected', [id], this.selected);
                }
            } else  {
                this.selected.push(id);
                this.trigger('selected', [id], this.selected);
            }
        }

        this.deselectAll = function() {
            if (this.selected.length > 0) {
                var deselected = this.selected;
                this.selected = [];
                this.trigger('deselected', deselected, this.selected);
            }
        }

        this.isSelected = function(id) {
            return this.selected.indexOf(id) !== -1;
        }
    }

    return new Manager();

});