
define(['lib/underscore-require'], function(_) {

    var counters = {};

    var GeomNode = function(options) {
        var options = options || {};

        if (!options.hasOwnProperty('type')) {
            throw Error('No type');
        }
        this.type = options.type;
        
        if (options.id) {
            this.id = options.id;
        } else {
            if (!counters[options.type]) {
                counters[options.type] = 0;
            }
            this.id = options.type + counters[options.type];
            ++counters[options.type]
        } 

        if (options.name) {
            this.name = options.name;
        } else if (options.nameFromId === true) {
            this.name = this.id;
        }

        this.parameters = options.parameters || {};
        this.editing = options.editing || false;
        this.addAnotherFn = options.addAnotherFn;

    }

    GeomNode.prototype.cloneNonEditing = function() {
        var newNode = new this.constructor({
            type : this.type, 
            id   : this.id,
            name : this.name,
        });
        newNode.parameters = copyObj(this.parameters);
        return newNode;
    }  

    GeomNode.prototype.isNamed = function() {
        return this.name !== undefined;
    }  

    var copyObj = function(value) {
        if ((value === null) || (value === undefined)) {
            return undefined;
        } if (Object.prototype.toString.call(value) === '[object Array]') {
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

    var Point = function(options) {
        var options = options || {};
        options.type = 'point';
        options.parameters = options.parameters || {coordinate: {x: 0, y:0, z:0}};
        GeomNode.prototype.constructor.call(this, options);
    }

    _.extend(Point.prototype, GeomNode.prototype);

    var Polyline = function(options) {
        var options = options || {};
        options.type = 'polyline';
        options.parameters = options.parameters || {coordinates: [{x: 0, y:0, z:0}]};
        GeomNode.prototype.constructor.call(this, options);
    }

    _.extend(Polyline.prototype, GeomNode.prototype);


    return {
        Point    : Point,
        Polyline : Polyline,
    }

});