var SS = SS || {};

SS.WorkplaneNode = function() {
    this['origin'] = (arguments[0] && arguments[0].origin) || {x: 0, y:0, z:0};
    this['axis'] = (arguments[0] && arguments[0]['xis']) || {x: 0, y: 0, z:1};
    this['angle'] = (arguments[0] && arguments[0]['axis']) || 0;
}

SS.WorkplaneNode.prototype.editableCopy = function() {
    
    var properties = ['origin', 'axis'];
    var copies = {};
    var that = this;
    properties.map(function(property) {
        copies[property] = {};
        for (key in that[property]) {
            copies[property][key] = that[property][key];
        }
    });
    copies['angle'] = that['angle'];

    return new SS.WorkplaneNode(copies);
}