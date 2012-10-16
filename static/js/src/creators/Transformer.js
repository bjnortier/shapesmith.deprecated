var SS = SS || {};


SS.Transformer = SS.NodeModel.extend({

    initialize: function(attributes) {
        this.node = attributes.transform;
        this.originalNode = attributes.originalNode;
        this.editingNode = attributes.editingNode;

        if (!attributes.editingExisting) {
            this.boundingBox = SS.boundingBoxForGeomNode(this.editingNode);

            this.normalizedBoundingBox = this.attributes.sourceModel.normalizedBoundingBox;
            this.normalizedCenter = this.attributes.sourceModel.normalizedCenter;
            this.normalizedBoundingRadius = this.attributes.sourceModel.normalizedBoundingRadius;

        }

        this.views = [
            new SS.NodeDOMView({model: this}),
            new SS.OkCancelView({model: this}),
        ];

        SS.geomDoc.on('replace', this.geomDocReplace, this);
        SS.commandStack.on('beforePop', this.cancel, this);
    },

    destroy: function() {
        this.views.map(function(view) {
            view.remove();
        });
        SS.geomDoc.off('replace', this.geomDocReplace);
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
        SS.geomDoc.replace(this.editingNode, 
                         this.originalNode);
    },

    geomDocReplace: function(original, replacement) {
        if (original === this.editingNode) {
            this.destroy();
        } 
        
    },


});


