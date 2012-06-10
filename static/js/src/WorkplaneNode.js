var SS = SS || {};

SS.WorkplaneNode = function() {
    this['origin'] = (arguments[0] && arguments[0].origin) || {x: 0, y:0, z:0};
    this['u-axis'] = (arguments[0] && arguments[0]['u-axis']) || {x: 1, y: 0, z:0};
    this['w-axis'] = (arguments[0] && arguments[0]['w-axis']) || {x:0, y:0, z:1};
}

SS.WorkplaneNode.prototype.editableCopy = function() {
    
    var properties = ['origin', 'u-axis', 'w-axis'];
    var copies = {};
    var that = this;
    properties.map(function(property) {
        copies[property] = {};
        for (key in that[property]) {
            copies[property][key] = that[property][key];
        }
    });

    return new SS.WorkplaneNode(copies);
}