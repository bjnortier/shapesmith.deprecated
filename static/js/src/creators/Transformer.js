var SS = SS || {};


SS.TransformerInitiator = Backbone.Model.extend({
    
    initialize: function(attributes) {
        this.originalNode = attributes.geomNode;
        this.normalizedBoundingBox = SS.normalizedBoundingBoxForGeomNode(this.originalNode);
        this.normalizedCenter = SS.centerOfGeom(this.normalizedBoundingBox);
        this.normalizedBoundingRadius = SS.normalizedBoundingRadius(this.normalizedBoundingBox);

        this.views = [];

        SS.selectionManager.on('deselected', this.deselected, this);
        SS.selectionManager.on('selected', this.selected, this);
    },

    selected: function(selected) {
        if (SS.selectionManager.size() !== 1) {
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
        SS.selectionManager.off('deselected', this.deselected);
        SS.selectionManager.off('selected', this.selected);
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
            this.normalizedBoundingRadius = SS.normalizedBoundingRadius(this.normalizedBoundingBox);
        }

        this.views = [
            new SS.NodeDOMView({model: this}),
            new SS.OkCancelView({model: this}),
        ];

        geom_doc.on('replace', this.geomDocReplace, this);
        SS.commandStack.on('beforePop', this.cancel, this);
    },

    destroy: function() {
        this.views.map(function(view) {
            view.remove();
        });
        geom_doc.off('replace', this.geomDocReplace);
        SS.commandStack.off('beforePop', this.cancel, this);
    },

    getBoundingBox: function() {
        return this.boundingBox;
    },

    tryCommit: function() {
        var cmd =  update_geom_command(this.originalNode, 
                                       this.editingNode, 
                                       this.editingNode); 
        SS.commandStack.execute(cmd);
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


