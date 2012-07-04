var SS = SS || {};

SS.WorkplaneNode = function() {
    this['origin'] = (arguments[0] && arguments[0].origin) || {x: 0, y:0, z:0};
    this['axis'] = (arguments[0] && arguments[0]['axis']) || {x: 0, y: 0, z:1};
    this['extents'] = (arguments[0] && arguments[0]['extents']) || {x: 60, y:60};
    this['angle'] = (arguments[0] && arguments[0]['angle']) || 0;
    this['boundary'] = (arguments[0] && arguments[0]['boundary']) || 50;
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