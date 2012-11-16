define(['lib/underscore-require', 'lib/backbone-require'], function(_, Backbone) {

    var counters = {};

    var validateIdOrName = function(idOrName) {
        var re = /^[a-zA-Z][a-zA-Z0-9_]*$/;
        if (!re.exec(idOrName)) {
            throw new Error('invalid id or name: ' + idOrName + '. Must match ^[a-zA-Z][a-zA-Z0-9_]+$')
        }
    }

    var GeomNode = function(options) {
        _.extend(this, Backbone.Events);
        var options = options || {};

        if (!options.hasOwnProperty('type')) {
            throw Error('No type');
        }
        this.type = options.type;
        
        if (options.hasOwnProperty('id')) {
            validateIdOrName(options.id);
            this.id = options.id;
        } else {
            if (!counters[options.type]) {
                counters[options.type] = 0;
            }
            this.id = options.type + counters[options.type];
            ++counters[options.type]
        } 

        if (options.hasOwnProperty('name')) {
            validateIdOrName(options.name);
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
            parameters : copyObj(this.parameters)
        });
        return newNode;
    }  

    GeomNode.prototype.cloneEditing = function() {
        var newNode = this.cloneNonEditing();
        newNode.editing = true;
        return newNode;
    }
    
    GeomNode.prototype.toJSONSubset = function() {
        return {
            type: this.type, 
            name: this.name,
            implicit: this.implicit,
            parameters: this.parameters,
        }
    }

    GeomNode.prototype.toJSON = function() {
        return JSON.stringify(this.toJSONSubset())
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

    // ---------- Types ----------

    var Variable = function(options) {
        if (!options.hasOwnProperty('name')) {
            throw Error('No name');
        }        
        if (!options.hasOwnProperty('parameters') || !options.parameters.hasOwnProperty('expression')) {
            throw Error('No expression');
        }
        options.type = 'variable';
        GeomNode.prototype.constructor.call(this, options);
    }

    _.extend(Variable.prototype, GeomNode.prototype);

    var Point = function(options) {
        var options = options || {};
        options.type = 'point';
        options.parameters = options.parameters || {coordinate: {x: '0', y:'0', z:'0'}};
        GeomNode.prototype.constructor.call(this, options);
    }

    _.extend(Point.prototype, GeomNode.prototype);

    var Polyline = function(options) {
        var options = options || {};
        options.type = 'polyline';
        options.parameters = options.parameters || {coordinates: [{x: '0', y:'0', z:'0'}]};
        GeomNode.prototype.constructor.call(this, options);
    }

    _.extend(Polyline.prototype, GeomNode.prototype);

    var Extrude = function(options) {
        var options = options || {};
        options.type = 'extrude';
        options.parameters = options.parameters || {vector: {u: '0', v:'0', n:'1'}, h: '1'  };
        GeomNode.prototype.constructor.call(this, options);
    }

    _.extend(Extrude.prototype, GeomNode.prototype);


    return {
        resetIDCounters : resetIDCounters,
        Node     : GeomNode,
        Variable : Variable,
        Point    : Point,
        Polyline : Polyline,
        Extrude  : Extrude,
        constructors: {
            'variable' : Variable,
            'point'    : Point,
            'polyline' : Polyline,
            'extrude'  : Extrude,
        }
    }

});