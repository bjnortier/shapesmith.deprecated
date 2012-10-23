define([], function() {

    var counters = {};

    var GeomNode = function(options) {
        var options = options || {};

        if (!counters[options.type]) {
            counters[options.type] = 0;
        }
        this.id = options.type + counters[options.type];
        ++counters[options.type]
        this.type = options.type;
        this.parameters = options.parameters || {};
        this.editing = options.editing || false;
        this.addAnotherFn = options.addAnotherFn;

    }

    GeomNode.prototype.cloneNonEditing = function() {
        var newNode = new GeomNode({type: this.type});
        newNode.parameters = copyObj(this.parameters);
        return newNode;
    }  

    var copyObj = function(value) {
        if ((value === null) || (value === undefined)) {
            return undefined;
        } if (Object.prototype.toString.call( value ) === '[object Array]') {
            return value.map(function(x) {
                return copyObj(x);
            });
        } else if (typeof(value) === 'object') {
            var returnObj = {};
            for (var key in value) {
                if (value.hasOwnProperty(key)) {
                    returnObj[key] = copyObj(value[key]);
                }
            }
            return returnObj;
        } else {
            return value;
        }
    }

    return {
        Node: GeomNode
    }

});