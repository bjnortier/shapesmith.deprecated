var SS = SS || {};

SS.WorkplaneNode = function() {
    this['origin'] = (arguments[0] && arguments[0].origin) || {x: 0, y:0, z:0};
    this['axis'] = (arguments[0] && arguments[0]['axis']) || {x: 0, y: 0, z:1};
    this['angle'] = (arguments[0] && arguments[0]['angle']) || 0;
    this['type'] = 'workplane';
}

SS.WorkplaneNode.prototype.editableCopy = function() {
    
    var properties = ['origin', 'axis', 'extents'];
    var copies = {};
    var that = this;
    properties.map(function(property) {
        copies[property] = {};
        for (key in that[property]) {
            copies[property][key] = that[property][key];
        }
    });
    copies['angle'] = that['angle'];
    copies['boundary'] = that['boundary'];

    return new SS.WorkplaneNode(copies);
}

SS.WorkplaneNode.prototype.serializableSubset = function() {
    var subset = SS.copyObj(this);
    delete subset['extents'];
    delete subset['boundary']
    return subset
}

SS.WorkplaneNode.prototype.isGlobalXY = function() {
    return (this.origin.x === 0) 
            &&
            (this.origin.y === 0)
            && 
            (this.origin.z === 0)
            &&  
            (this.axis.x === 0) 
            &&
            (this.axis.y === 0)
            && 
            (this.axis.z === 1)
            && 
            (this.angle === 0);
}

SS.WorkplaneNode.prototype.isEqual = function(that) {

    return (this.origin.x === that.origin.x) 
            &&
            (this.origin.y === that.origin.y)
            && 
            (this.origin.z === that.origin.z)
            &&  
            (this.axis.x === that.axis.x) 
            &&
            (this.axis.y === that.axis.y)
            && 
            (this.axis.z === that.axis.z)
            && 
            (this.angle === that.angle);
}