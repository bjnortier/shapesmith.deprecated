define([
    'lib/underscore-require', 
    'lib/backbone-require',
    'src/calculations',
    ], function(_, Backbone, calc) {

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
        this.workplane = options.workplane || {
            origin: new THREE.Vector3(),
            axis: new THREE.Vector3(0,0,1),
            angle: 0
        };
        this.parameters = options.parameters || {};
        this.editing = options.editing || false;
        this.proto = options.proto || false;
    }

    var resetIDCounters = function() {
        counters = {};
    }

    GeomNode.prototype.cloneNonEditing = function(options) {
        var options = options || {};
        var cloneOptions = _.extend(options, {
            type     : this.type, 
            id       : this.id,
            name     : this.name,
            implicit : this.implicit,
            workplane : calc.copyObj(this.workplane),
            parameters : calc.copyObj(this.parameters)
        })
        var newNode = new this.constructor(cloneOptions);
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
            workplane: this.workplane,
            parameters: this.parameters,
        }
    }

    GeomNode.prototype.toJSON = function() {
        return JSON.stringify(this.toJSONSubset())
    }

    GeomNode.prototype.hasSameJSON = function(other) {
        return this.toJSON() === other.toJSON();
    }



    // ---------- Types ----------

    // ---------- Workplane ----------

    var Workplane = function(options) {
        var options = options || {};
        options.type = 'workplane';
        options.parameters = options.parameters || {
            snap: 1.0
        };
        GeomNode.prototype.constructor.call(this, options);
    }

    Workplane.prototype.getExpressions = function() {
        return [
        ];
    }

    _.extend(Workplane.prototype, GeomNode.prototype);    

    // ---------- Variable ----------


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

    Variable.prototype.getExpressions = function() {
        return [
            this.parameters.expression,
        ];
    }

    _.extend(Variable.prototype, GeomNode.prototype);

    // ---------- Point ----------

    var Point = function(options) {
        var options = options || {};
        options.type = 'point';
        options.parameters = options.parameters || {coordinate: {x: '0', y:'0', z:'0'}};
        GeomNode.prototype.constructor.call(this, options);
    }

    Point.prototype.getExpressions = function() {
        return [
            this.parameters.coordinate.x, 
            this.parameters.coordinate.y, 
            this.parameters.coordinate.z,
        ];
    }

    _.extend(Point.prototype, GeomNode.prototype);

    // ---------- Polyline ----------

    var Polyline = function(options) {
        var options = options || {};
        options.type = 'polyline';
        options.parameters = options.parameters || {};
        GeomNode.prototype.constructor.call(this, options);
    }

    Polyline.prototype.getExpressions = function() {
        return [];
    }

    _.extend(Polyline.prototype, GeomNode.prototype);

    // ---------- Extrude ----------

    var Extrude = function(options) {
        var options = options || {};
        options.type = 'extrude';
        options.parameters = options.parameters || {vector: {u: '0', v:'0', n:'1'}, h: '1'};
        GeomNode.prototype.constructor.call(this, options);
    }

    Extrude.prototype.getExpressions = function() {
        return [
            this.parameters.vector.u, 
            this.parameters.vector.v, 
            this.parameters.vector.n,
            this.parameters.h,
        ];
    }

    _.extend(Extrude.prototype, GeomNode.prototype);


    return {
        resetIDCounters : resetIDCounters,
        Node            : GeomNode,
        Workplane       : Workplane,
        Variable        : Variable,
        Point           : Point,
        Polyline        : Polyline,
        Extrude         : Extrude,
        constructors: {
            'workplane'      : Workplane,
            'variable'       : Variable,
            'point'          : Point,
            'polyline'       : Polyline,
            'extrude'        : Extrude,
        }
    }

});