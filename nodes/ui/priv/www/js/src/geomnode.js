define([], function() {
    

    var counter = 0;

    var GeomNode = function(options) {
        var options = options || {};

        this.id = ++counter; 
        this.parameters = options.parameters || {};
        this.editing = options.editing || false;
        this.addAnotherFn = options.addAnotherFn;

    }

    GeomNode.prototype.cloneNonEditing = function() {
        var newNode = new GeomNode();
        for (key in this.parameters) {
            newNode.parameters[key] = this.parameters[key];
        }
        return newNode;
    }  

    return {
        Node: GeomNode
    }

});