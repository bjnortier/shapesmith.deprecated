var SS = SS || {};


SS.TransformerInitiator = Backbone.Model.extend({
    
    initialize: function(attributes) {
        this.originalNode = attributes.geomNode;
        this.normalizedBoundingBox = SS.normalizedBoundingBoxForGeomNode(this.originalNode);
        this.normalizedCenter = SS.centerOfGeom(this.normalizedBoundingBox);

        this.views = [];

        selectionManager.on('deselected', this.deselected, this);
        selectionManager.on('selected', this.selected, this);
    },

    selected: function(selected) {
        if (selectionManager.size() !== 1) {
            this.destroy();
        }
    },

    deselected: function(deselected) {
        if ((deselected.length === 1) &&
            (deselected[0] === this.originalNode.id)) {
            this.destroy();
        }
    },

    destroy: function(event) {
        selectionManager.off('deselected', this.deselected);
        selectionManager.off('selected', this.selected);
        this.views.map(function(view) {
            view.remove();
        });
    },

});


SS.Transformer = SS.NodeModel.extend({

    initialize: function(attributes) {
        this.node = attributes.transform;
        this.originalNode = attributes.originalNode;
        this.editingNode = attributes.editingNode;

        if (!attributes.editingExisting) {
            this.boundingBox = SS.boundingBoxForGeomNode(this.editingNode);
            this.normalizedBoundingBox = SS.normalizedBoundingBoxForGeomNode(this.editingNode);
            this.normalizedCenter = SS.centerOfGeom(this.normalizedBoundingBox);
        }

        this.views = [
            new SS.NodeDOMView({model: this}),
            new SS.OkCancelView({model: this}),
        ];

        geom_doc.on('replace', this.geomDocReplace, this);
        command_stack.on('beforePop', this.cancel, this);
    },

    destroy: function() {
        this.views.map(function(view) {
            view.remove();
        });
        geom_doc.off('replace', this.geomDocReplace);
        command_stack.off('beforePop', this.cancel, this);
    },

    getBoundingBox: function() {
        return this.boundingBox;
    },

    tryCommit: function() {
        var cmd =  update_geom_command(this.originalNode, 
                                       this.editingNode, 
                                       this.editingNode); 
        command_stack.execute(cmd);
    },

    cancel: function() {
        this.destroy();
        geom_doc.replace(this.editingNode, 
                         this.originalNode);
    },

    geomDocReplace: function(original, replacement) {
        if (original === this.editingNode) {
            this.destroy();
        } 
        
    },


});


