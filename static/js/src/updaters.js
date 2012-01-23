var SS = SS || {};

SS.NodeUpdater = function(node) {
    var node = node, that = this;

    evented(this);

    var updateTreeview = function() {
        Object.keys(node.origin).map(function(key) {
            $('#' + key).val(node.origin[key]);
        });
        Object.keys(node.parameters).map(function(key) {
            $('#' + key).val(node.parameters[key]);
        });
    }

    var updateFromTreeView = function() {
        Object.keys(node.origin).map(function(key) {
            node.origin[key] = parseFloat($('#' + key).val());
        });
        Object.keys(node.parameters).map(function(key) {
            node.parameters[key] = parseFloat($('#' + key).val());
        });
        that.fire({type: 'updatedFromTree'});
    }

    this.setOrigin = function(origin) {
        node.origin = {x: origin.x, y:origin.y, z:origin.z};
	updateTreeview();
        that.fire({type: 'updatedFromCursoid'});
    }

    this.setParams = function(params, extra) {
        Object.keys(params).map(function(key) {
            node.parameters[key] = params[key];
        });
        node.extra = extra;

	updateTreeview();
        that.fire({type: 'updatedFromCursoid'});
    }

    this.setMeta = function(meta) {
        node.meta = meta;
        that.fire({type: 'updatedFromCursoid'});
    }

    var init = function() {
        Object.keys(node.origin).map(function(key) {
            $('#' + key).change(updateFromTreeView);
        });
        Object.keys(node.parameters).map(function(key) {
            $('#' + key).change(updateFromTreeView);
        });
        updateTreeview();
    }
    
    init();
}
