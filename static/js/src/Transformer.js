var SS = SS || {};

SS.transformers = {};
SS.transformers.centerOfGeom = function(boundingBox) {
    return new THREE.Vector3().add(boundingBox.min, 
                                   new THREE.Vector3().sub(boundingBox.max, boundingBox.min).divideScalar(2));
}

SS.TransformerInitiator = Backbone.Model.extend({
    
    initialize: function(attributes) {
        this.geomNode = attributes.geomNode;
        this.boundingBox = SS.boundingBoxForGeomNode(this.geomNode);
        this.center = SS.transformers.centerOfGeom(this.boundingBox);

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
            (deselected[0] === this.geomNode.id)) {
            
            this.destroy();
        }
    },

    destroy: function(event) {
        selectionManager.off('deselected', this.deselected);
        this.views.map(function(view) {
            view.remove();
        });
    },

});


SS.Transformer = SS.NodeModel.extend({

    initialize: function(attributes) {
        this.node = attributes.transform;
        this.transform = attributes.transform;
        this.originalNode = attributes.originalNode;
        this.editingNode = attributes.editingNode;

        if (!attributes.editingExisting) {
            this.boundingBox = SS.boundingBoxForGeomNode(this.editingNode);
            this.center = SS.transformers.centerOfGeom(this.boundingBox);
        }

        this.views = [
            new SS.NodeDOMView({model: this}),
            new SS.TransformOkCancelView({model: this}),
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

    updateFromDOMView: function() {
        SS.NodeModel.prototype.updateFromDOMView.call(this);

        if (!this.attributes.editingExisting) {
            this.trigger('change:model');
            this.boundingBox = SS.boundingBoxForGeomNode(this.editingNode);
            this.center = SS.transformers.centerOfGeom(this.boundingBox);
        }

        this.trigger('change');
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

SS.TransformOkCancelView = Backbone.View.extend({

    initialize: function() {
        this.render();
        this.model.on('change', this.update, this);
        SS.sceneView.on('cameraChange', this.update, this);
    },

    remove: function() {
        Backbone.View.prototype.remove.call(this);
        this.model.off('change', this.update);
        SS.sceneView.off('cameraChange', this.update);
    },

    render: function() {

        var template = '<table><tr><td><input class="ok" type="submit" value="Ok"/><input class="cancel" type="submit" value="Cancel"/></td></tr></table>';

        var transformTable = $.mustache(template, {});

        this.$el.html(transformTable);
        $('#floating-ok-cancel').append(this.$el);
    },

    update: function() {
        var projScreenMat = new THREE.Matrix4();
        projScreenMat.multiply(SS.sceneView.camera.projectionMatrix, 
                               SS.sceneView.camera.matrixWorldInverse);
        
        var xminmax = [this.model.boundingBox.min.x, this.model.boundingBox.max.x];
        var yminmax = [this.model.boundingBox.min.y, this.model.boundingBox.max.y];
        var zminmax = [this.model.boundingBox.min.z, this.model.boundingBox.max.z];
        var screenPosition = new THREE.Vector3(-2,2,0);
        for (var i = 0; i < 2; i++) {
            for (var j = 0; j < 2; ++j) {
                for (var k = 0; k < 2; ++k) {
                    var pos = new THREE.Vector3(xminmax[i], yminmax[j], zminmax[k]);
                    projScreenMat.multiplyVector3(pos);
                    screenPosition.x = Math.max(screenPosition.x, pos.x);
                    screenPosition.y = Math.min(screenPosition.y, pos.y);
                }
            }
        }
        
        var pixelPosition = {};
        pixelPosition.x = window.innerWidth * ((screenPosition.x+1)/2);
        pixelPosition.y = window.innerHeight * ((-screenPosition.y+1)/2);
        pixelPosition.x = Math.min(pixelPosition.x, window.innerWidth - 100);
        pixelPosition.y = Math.min(pixelPosition.y, window.innerHeight - 100);
        pixelPosition.x = Math.max(pixelPosition.x, 100);
        pixelPosition.y = Math.max(pixelPosition.y, 100);
       
        $('#floating-ok-cancel').css('left', pixelPosition.x);
	$('#floating-ok-cancel').css('top', pixelPosition.y);
    },
    
    events: {
        'click .ok' : 'ok',
        'click .cancel' : 'cancel',
    },

    ok: function() {
        this.model.tryCommit();
    },

    cancel: function() {
        this.model.cancel();
    },

});
