function Transform() {
    if (!arguments[0].type) {
        throw new Error("type is not defined");
    }

    this.editing = arguments[0].editing;
    this.type = arguments[0].type;
    this.parameters = arguments[0].parameters;
}

Transform.prototype.json = function() {
    return JSON.stringify({type: this.type,
                           parameters: this.parameters});
}

function GeomNode() {

    if (!arguments[0].type) {
        throw new Error("type is not defined");
    }

    this.editing = arguments[0].editing;
    this.type = arguments[0].type;
    this.path = arguments[0].path;
    this.parameters = arguments[0].parameters;
    this.parent = undefined;
    this.transforms = [];
    this.mesh = arguments[0].mesh;

    this.children = [];
    if (arguments[1]) {
        if (!typeof(arguments[1]) == "object") {
            throw new Error("Children must be array");
        }

        for (var i in arguments[1]) {
            arguments[1][i].parent = this;
            this.children.push(arguments[1][i]);
        }
    }


    
}

// TODO: Move test for multiple editing transforms from doc
// into this class

GeomNode.prototype.json = function() {
    // No need to do somethign special with parameters if they are not 
    // defined, as JSON.stringigy simply ignores those fields
    return JSON.stringify({type: this.type,
                           parameters: this.parameters,
                           children: this.children.map(function(child) {
                               return child.path;
                               }),
                           transforms: this.transforms.map(function(tx) {
                               return JSON.parse(tx.json());
                           })});
}