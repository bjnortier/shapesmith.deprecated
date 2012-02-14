var SS = SS || {};

SS.NodeUpdater = function(node) {
    var node = node, that = this;

    _.extend(this, Backbone.Events);

    var updateTreeview = function() {
        Object.keys(node.origin).map(function(key) {
            $('#' + key).val(node.origin[key]);
        });
        Object.keys(node.parameters).map(function(key) {
            $('#' + key).val(node.parameters[key]);
        });
    }

    var updateFromTreeView = function() {
        var schema = SS.schemas[node.type];
        
        Object.keys(node.origin).map(function(key) {
            node.origin[key] = parseFloat($('#' + key).val());
        });

        Object.keys(node.parameters).map(function(key) {

            var itemSchema = schema.properties.parameters.properties[key];
            if (itemSchema.type === 'string') {
                node.parameters[key] = $('#' + key).val();
            } else {
                node.parameters[key] = parseFloat($('#' + key).val());
            }
        });
        that.trigger('updatedFromTree');
    }

    this.setOrigin = function(origin) {
        node.origin = {x: origin.x, y:origin.y, z:origin.z};
	updateTreeview();
        that.trigger('updatedFromCursoid');
    }

    this.setParams = function(params, extra) {
        Object.keys(params).map(function(key) {
            node.parameters[key] = params[key];
        });
        node.extra = extra;

	updateTreeview();
        that.trigger('updatedFromCursoid');
    }

    this.setMeta = function(meta) {
        node.meta = meta;
        that.trigger('updatedFromCursoid');
    }

    var init = function() {
        Object.keys(node.origin).map(function(key) {
            $('#' + key).change(updateFromTreeView);
            $('#' + key).keyup(updateFromTreeView);
            $('#' + key).click(updateFromTreeView);
        });
        Object.keys(node.parameters).map(function(key) {
            $('#' + key).change(updateFromTreeView);
            $('#' + key).keyup(updateFromTreeView);
            $('#' + key).click(updateFromTreeView);
        });
        updateTreeview();
    }
    
    init();
}
