define(['lib/underscore-require', 'lib/backbone-require'], function(_, Backbone) {

    var counters = {};

    var GeomNode = function(options) {
        _.extend(this, Backbone.Events);
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
        } else {
            this.name = this.id;
        }

        this.implicit = options.implicit || false;
        this.parameters = options.parameters || {};
        this.editing = options.editing || false;
        this.proto = options.proto || false;
    }

    var resetIDCounters = function() {
        counters = {};
    }

    GeomNode.prototype.cloneNonEditing = function() {
        var newNode = new this.constructor({
            type     : this.type, 
            id       : this.id,
            name     : this.name,
            implicit : this.implicit,
        });
        newNode.parameters = copyObj(this.parameters);
        return newNode;
    }  

    GeomNode.prototype.cloneEditing = function() {
        var newNode = this.cloneNonEditing();
        newNode.editing = true;
        return newNode;
    }

    GeomNode.prototype.toJSON = function() {
        return JSON.stringify({
            type: this.type, 
            name: this.name,
            implicit: this.implicit,
            parameters: this.parameters,
        })
    }

    GeomNode.prototype.hasSameJSON = function(other) {
        return this.toJSON() === other.toJSON();
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
        resetIDCounters : resetIDCounters,
        Node     : GeomNode,
        Point    : Point,
        Polyline : Polyline,
    }

});