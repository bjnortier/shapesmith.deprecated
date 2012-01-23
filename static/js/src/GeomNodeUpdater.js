var SS = SS || {};
SS.GeomNodeUpdater = function(geomNode) {
    var geomNode = geomNode, that = this;

    evented(this);

    var updateTreeview = function() {
        Object.keys(geomNode.origin).map(function(key) {
            $('#' + key).val(geomNode.origin[key]);
        });
        Object.keys(geomNode.parameters).map(function(key) {
            $('#' + key).val(geomNode.parameters[key]);
        });
    }

    var updateFromTreeView = function() {
        Object.keys(geomNode.origin).map(function(key) {
            geomNode.origin[key] = parseFloat($('#' + key).val());
        });
        Object.keys(geomNode.parameters).map(function(key) {
            geomNode.parameters[key] = parseFloat($('#' + key).val());
        });
        that.fire({type: 'updatedFromTree'});
    }

    this.setOrigin = function(origin) {
        geomNode.origin = {x: origin.x, y:origin.y, z:origin.z};
	updateTreeview();
        that.fire({type: 'updatedFromCursoid'});
    }

    this.setParams = function(params, extra) {
        Object.keys(params).map(function(key) {
            geomNode.parameters[key] = params[key];
        });
        geomNode.extra = extra;

	updateTreeview();
        that.fire({type: 'updatedFromCursoid'});
    }

    this.setMeta = function(meta) {
        geomNode.meta = meta;
        that.fire({type: 'updatedFromCursoid'});
    }

    var init = function() {
        Object.keys(geomNode.origin).map(function(key) {
            $('#' + key).change(updateFromTreeView);
        });
        Object.keys(geomNode.parameters).map(function(key) {
            $('#' + key).change(updateFromTreeView);
        });
        updateTreeview();
    }
    
    init();
}
