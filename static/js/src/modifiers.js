var SS = SS || {};
SS.modifier = {};

SS.modifier.Origin = function(spec) {
    var node = spec.node, updater = spec.updater, cursoid = spec.cursoid;
    
    var setOrigin = function(event) {
	updater.setOrigin({x: event.position.x,
                           y: event.position.y,
                           z: event.position.z});
    }

    var setCursoid = function() {
        cursoid.setPosition(node.origin);
    }

    this.dispose = function() {
	cursoid.off('cursoidUpdated', setOrigin);
        updater.off('updatedFromTree', setCursoid);
    }

    this.initialCursoidName = 'xy';

    var init = function() {
        cursoid.on('cursoidUpdated', setOrigin);
        updater.on('updatedFromTree', setCursoid);
        setCursoid();
    }

    init();
}

SS.modifier.UVW = function(spec) {
    var node = spec.node, updater = spec.updater, cursoid = spec.cursoid;
    
    var setUVW = function(event) {
	updater.setParams({u: event.position.x - node.origin.x,
                           v: event.position.y - node.origin.y,
                           w: event.position.z - node.origin.z});
    }

    var setCursoid = function() {
        cursoid.setPosition({x: node.origin.x + node.parameters.u,
                             y: node.origin.y + node.parameters.v,
                             z: node.origin.z + node.parameters.w});
    }

    this.dispose = function() {
	cursoid.off('cursoidUpdated', setUVW);
        updater.off('updatedFromTree', setCursoid);
    }

    this.initialCursoidName = 'xy';

    var init = function() {
        cursoid.on('cursoidUpdated', setUVW);
        updater.on('updatedFromTree', setCursoid);
        setCursoid();
    }

    init();
}

SS.modifier.UV = function(spec) {
    var node = spec.node, updater = spec.updater, cursoid = spec.cursoid;
    var uParameterName = spec.uParameterName || 'u', vParameterName = spec.vParameterName || 'v';

    
    var setUV = function(event) {
        var params = {};
        params[uParameterName] = event.position.x - node.origin.x;
        params[vParameterName] = event.position.y - node.origin.y;
	updater.setParams(params); 
    }

    var setCursoid = function() {
        
        cursoid.setPosition({x: node.origin.x + node.parameters[uParameterName],
		             y: node.origin.y + node.parameters[vParameterName],
		             z: node.origin.z});
        cursoid.setPosition(node.origin);
    }

    this.dispose = function() {
	cursoid.off('cursoidUpdated', setUV);
        updater.off('updatedFromTree', setCursoid);
    }

    this.initialCursoidName = 'xy';

    var init = function() {
        cursoid.on('cursoidUpdated', setUV);
        updater.on('updatedFromTree', setCursoid);
        setCursoid();
    }

    init();
}


SS.modifier.U2 = function(spec) {
    var node = spec.node, updater = spec.updater, cursoid = spec.cursoid;
    var u2ParameterName = spec.u2ParameterName || 'u2';

    var setU2 = function(event) {
        var params = {};
        params[u2ParameterName] = event.position.x - node.origin.x;
	updater.setParams(params); 
    }

    var setCursoid = function() {
        cursoid.setPosition({x: node.origin.x + node.parameters[u2ParameterName],
		             y: node.origin.y,
		             z: node.origin.z});
        cursoid.setPositionAndExclusions(node.origin, ['y', 'z']);
    }

    this.dispose = function() {
	cursoid.off('cursoidUpdated', setU2);
        updater.off('updatedFromTree', setCursoid);
    }

    this.initialCursoidName = 'xy';

    var init = function() {
        cursoid.on('cursoidUpdated', setU2);
        updater.on('updatedFromTree', setCursoid);
        setCursoid();
    }

    init();
}

SS.modifier.Radius = function(spec) {
    var node = spec.node, updater = spec.updater, cursoid = spec.cursoid;
    var parameterName = spec.parameterName || 'r';
    
    
    var setRadius = function(event) {
        var dx = event.position.x - node.origin.x;
        var dy = event.position.y - node.origin.y;
        var angle = Math.atan2(dy, dx);
	var r  = parseFloat(Math.sqrt(dx*dx + dy*dy).toFixed(3));
        
        var params = {};
        params[parameterName] = r;
	updater.setParams(params, {angle: angle});
    }

    var setCursoid = function() {
        var angle = (node.extra && node.extra.angle) || 0;
        var position = {x: node.origin.x + node.parameters[parameterName]*Math.cos(angle),
		        y: node.origin.y + node.parameters[parameterName]*Math.sin(angle),
		        z: node.origin.z};
        cursoid.setPositionAndExclusions(position, ['z']);
    }

    this.dispose = function() {
	cursoid.off('cursoidUpdated', setRadius);
        updater.off('updatedFromTree', setCursoid);
    }

    this.initialCursoidName = 'xy';

    var init = function() {
        cursoid.on('cursoidUpdated', setRadius);
        updater.on('updatedFromTree', setCursoid);
        setCursoid();
    }
    init();
}

SS.modifier.RadiusMinR1 = function(spec) {
    var node = spec.node, updater = spec.updater, cursoid = spec.cursoid;
    
    var setR2 = function(event) {
        var dx = event.position.x - node.origin.x;
        var dy = event.position.y - node.origin.y;
        var angle = Math.atan2(dy, dx);
	var r  = parseFloat(Math.sqrt(dx*dx + dy*dy).toFixed(3));

        var r2 = parseFloat((r - node.parameters.r1).toFixed(3));
        
	updater.setParams({r2: r2}, {angle: angle});
    }

    var setCursoid = function() {
        var angle = (node.extra && node.extra.angle) || 0;
        var r = node.parameters.r1 + node.parameters.r2;
        var position = {x: node.origin.x + r*Math.cos(angle),
		        y: node.origin.y + r*Math.sin(angle),
		        z: node.origin.z};
        cursoid.setPositionAndExclusions(position, ['y', 'z']);
    }

    this.dispose = function() {
	cursoid.off('cursoidUpdated', setR2);
        updater.off('updatedFromTree', setCursoid);
    }

    this.initialCursoidName = 'xy';

    var init = function() {
        cursoid.on('cursoidUpdated', setR2);
        updater.on('updatedFromTree', setCursoid);
        setCursoid();
    }
    init();
}

SS.modifier.W = function(spec) {
    var node = spec.node, updater = spec.updater, cursoid = spec.cursoid;
    var parameterName = spec.parameterName || 'w';
    
    var setW = function(event) {
        var w = event.position.z - node.origin.z;
        var params = {};
        params[parameterName] = w;
	updater.setParams(params);
    }

    var setCursoid = function() {
        var position = {x: node.origin.x,
		        y: node.origin.y,
		        z: node.origin.z + node.parameters[parameterName]};
        cursoid.setPositionAndExclusions(position, ['xy']);
    }

    this.dispose = function() {
	cursoid.off('cursoidUpdated', setW);
        updater.off('updatedFromTree', setCursoid);
    }

    this.initialCursoidName = 'xyz';

    var init = function() {
        cursoid.on('cursoidUpdated', setW);
        updater.on('updatedFromTree', setCursoid);
        setCursoid();
    }
    init();
}

SS.modifier.MajorMinorRadii = function(spec) {
    var node = spec.node, updater = spec.updater, cursoid = spec.cursoid;
    
    var setR1R2 = function(event) {
	var r1 = event.position.x - node.origin.x;
	var r2 = event.position.y - node.origin.y;
	updater.setParams({r1: r1, r2:r2});
    }

    var setCursoid = function() {
        var position = {x: node.origin.x + node.parameters.r1,
		        y: node.origin.y + node.parameters.r2,
		        z: node.origin.z};
        cursoid.setPositionAndExclusions(position, ['z']);
    }

    this.dispose = function() {
	cursoid.off('cursoidUpdated', setR1R2);
        updater.off('updatedFromTree', setCursoid);
    }

    this.initialCursoidName = 'xy';

    var init = function() {
        cursoid.on('cursoidUpdated', setR1R2);
        updater.on('updatedFromTree', setCursoid);
        setCursoid();
    }
    init();
}
