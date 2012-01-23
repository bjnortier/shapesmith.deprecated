var SS = SS || {};
SS.modifier = {};

SS.modifier.Origin = function(spec) {
    var geomNode = spec.geomNode, updater = spec.updater, cursoid = spec.cursoid;
    
    var setOrigin = function(event) {
	updater.setOrigin({x: event.position.x,
                           y: event.position.y,
                           z: event.position.z});
    }

    var setCursoid = function() {
        cursoid.setPosition(geomNode.origin);
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

SS.modifier.UV = function(spec) {
    var geomNode = spec.geomNode, updater = spec.updater, cursoid = spec.cursoid;
    var uParameterName = spec.uParameterName || 'u', vParameterName = spec.vParameterName || 'v';

    
    var setUV = function(event) {
        var params = {};
        params[uParameterName] = event.position.x - geomNode.origin.x;
        params[vParameterName] = event.position.y - geomNode.origin.y;
	updater.setParams(params); 
    }

    var setCursoid = function() {
        
        cursoid.setPosition({x: geomNode.origin.x + geomNode.parameters[uParameterName],
		             y: geomNode.origin.y + geomNode.parameters[vParameterName],
		             z: geomNode.origin.z});
        cursoid.setPosition(geomNode.origin);
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
    var geomNode = spec.geomNode, updater = spec.updater, cursoid = spec.cursoid;
    var u2ParameterName = spec.u2ParameterName || 'u2';

    var setU2 = function(event) {
        var params = {};
        params[u2ParameterName] = event.position.x - geomNode.origin.x;
	updater.setParams(params); 
    }

    var setCursoid = function() {
        cursoid.setPosition({x: geomNode.origin.x + geomNode.parameters[u2ParameterName],
		             y: geomNode.origin.y,
		             z: geomNode.origin.z});
        cursoid.setPositionAndExclusions(geomNode.origin, ['y', 'z']);
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
    var geomNode = spec.geomNode, updater = spec.updater, cursoid = spec.cursoid;
    var parameterName = spec.parameterName || 'r';
    
    
    var setRadius = function(event) {
        var dx = event.position.x - geomNode.origin.x;
        var dy = event.position.y - geomNode.origin.y;
        var angle = Math.atan2(dy, dx);
	var r  = parseFloat(Math.sqrt(dx*dx + dy*dy).toFixed(3));
        
        var params = {};
        params[parameterName] = r;
	updater.setParams(params, {angle: angle});
    }

    var setCursoid = function() {
        var angle = (geomNode.extra && geomNode.extra.angle) || 0;
        var position = {x: geomNode.origin.x + geomNode.parameters[parameterName]*Math.cos(angle),
		        y: geomNode.origin.y + geomNode.parameters[parameterName]*Math.sin(angle),
		        z: geomNode.origin.z};
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
    var geomNode = spec.geomNode, updater = spec.updater, cursoid = spec.cursoid;
    
    var setR2 = function(event) {
        var dx = event.position.x - geomNode.origin.x;
        var dy = event.position.y - geomNode.origin.y;
        var angle = Math.atan2(dy, dx);
	var r  = parseFloat(Math.sqrt(dx*dx + dy*dy).toFixed(3));

        var r2 = parseFloat((r - geomNode.parameters.r1).toFixed(3));
        
	updater.setParams({r2: r2}, {angle: angle});
    }

    var setCursoid = function() {
        var angle = (geomNode.extra && geomNode.extra.angle) || 0;
        var r = geomNode.parameters.r1 + geomNode.parameters.r2;
        var position = {x: geomNode.origin.x + r*Math.cos(angle),
		        y: geomNode.origin.y + r*Math.sin(angle),
		        z: geomNode.origin.z};
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
    var geomNode = spec.geomNode, updater = spec.updater, cursoid = spec.cursoid;
    var parameterName = spec.parameterName || 'w';
    
    var setW = function(event) {
        var w = event.position.z - geomNode.origin.z;
        var params = {};
        params[parameterName] = w;
	updater.setParams(params);
    }

    var setCursoid = function() {
        var position = {x: geomNode.origin.x,
		        y: geomNode.origin.y,
		        z: geomNode.origin.z + geomNode.parameters[parameterName]};
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
    var geomNode = spec.geomNode, updater = spec.updater, cursoid = spec.cursoid;
    
    var setR1R2 = function(event) {
	var r1 = event.position.x - geomNode.origin.x;
	var r2 = event.position.y - geomNode.origin.y;
	updater.setParams({r1: r1, r2:r2});
    }

    var setCursoid = function() {
        var position = {x: geomNode.origin.x + geomNode.parameters.r1,
		        y: geomNode.origin.y + geomNode.parameters.r2,
		        z: geomNode.origin.z};
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
