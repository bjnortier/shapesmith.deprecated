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

SS.Transformer = Backbone.Model.extend({

    initialize: function(attributes) {
        this.transform = attributes.transform;
        this.originalNode = attributes.originalNode;
        this.editingNode = attributes.editingNode;

        if (!attributes.editingExisting) {
            this.boundingBox = SS.boundingBoxForGeomNode(this.editingNode);
            this.center = SS.transformers.centerOfGeom(this.boundingBox);
        }

        this.views = [
            new SS.TransformDOMView({model: this}),
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
        var updateValues = function(object, schema) {
            for (key in schema.properties) {
                var itemSchema = schema.properties[key];
                if (itemSchema.type === 'string') {
                    object[key] = $('#' + key).val();
                } else {
                    object[key] = parseFloat($('#' + key).val());
                }
            }
        };

        var transform = this.transform;
        var schema = SS.schemas[transform.type];
        updateValues(transform.origin, schema.properties.origin);
        updateValues(transform.parameters, schema.properties.parameters);

        if (!this.attributes.editingExisting) {
            this.trigger('change:model');
            this.boundingBox = SS.boundingBoxForGeomNode(this.editingNode);
        }

        this.trigger('change');
    },

    tryCommit: function() {
        var cmd =  update_geom_command(this.originalNode, 
                                       this.editingNode, 
                                       this.editingNode); 
        command_stack.execute(cmd);
        console.log('Try Commit');
    },

    cancel: function() {
        geom_doc.replace(this.editingNode, 
                         this.originalNode);
    },

    geomDocReplace: function(original, replacement) {
        if (original === this.editingNode) {
            this.destroy();
        } 
        
    },


});

SS.SceneObjectView = Backbone.View.extend({
    
    initialize: function() {
        this.sceneObject = new THREE.Object3D(); 
	SS.sceneView.registerSceneObjectView(this);

        this.model.on('change', this.render, this);
    },

    clear: function() {
        var children = this.sceneObject.children;
        var that = this
        children.map(function(child) {
            that.sceneObject.remove(child);
        });
    },

    postRender: function() {
        SS.sceneView.scene.add(this.sceneObject);
    },

    remove: function() {
	SS.sceneView.deregisterSceneObjectView(this);
        SS.sceneView.scene.remove(this.sceneObject);
        this.model.off('change', this.render);
    },

});


SS.ActiveTransformerView = SS.SceneObjectView.extend({

    initialize: function() {
	SS.SceneObjectView.prototype.initialize.call(this);
	this.on('mouseEnter', this.highlight);
	this.on('mouseLeave', this.unhighlight);
        this.model.on("change", this.render, this);
    },

    active: true,

    recursiveHighlightFn:  function(object, opacity) {
        var functor = function(object) {
	    if (object.material) {
	        object.material.opacity = opacity;
	    }
	    if (object.children) {
	        object.children.map(functor);
	    }
        }
        functor(object);
    },

    highlight: function() {
	this.recursiveHighlightFn(this.sceneObject, 1.0);
    },

    unhighlight: function() {
	this.recursiveHighlightFn(this.sceneObject, 0.5);
    },

    remove: function() {
        SS.SceneObjectView.prototype.remove.call(this);
        this.off('mouseEnter', this.highlight);
	this.off('mouseLeave', this.unhighlight);
        this.model.off("change", this.render);
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
        pixelPosition.x = Math.min(window.innerWidth * ((screenPosition.x+1)/2), window.innerWidth - 100);
        pixelPosition.y = Math.min(window.innerHeight * ((-screenPosition.y+1)/2), window.innerHeight - 100);
       
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


SS.TransformDOMView = Backbone.View.extend({

    initialize: function() {
        this.render();
        this.model.on('change', this.update, this);
    },

    remove: function() {
        Backbone.View.prototype.remove.call(this);
        this.model.off('change', this.update);
    },

    render: function() {
        var geomNode = this.model.editingNode;
        var transform = this.model.transform;
        var schema = SS.schemas[transform.type];
    
        // Origin & Orientation
        var originTable = null;
        if (transform.origin) {
	    var originArr = ['x', 'y', 'z'].map(function(key) {
	        return {key: key, 
		        value: transform.origin[key], 
		        editing: transform.editing,
                        inputElement: renderInputElement(key, schema.properties.origin)
                       }
	    });
	    var originTemplate = '<table>{{#originArr}}<tr {{#editing}}class="field"{{/editing}}><td>{{key}}</td><td>{{^editing}}<span class="{{edit-class}}">{{value}}</span>{{/editing}}{{#editing}}{{{inputElement}}}{{/editing}}</td></tr>{{/originArr}}</table>';
	    var originTable = $.mustache(originTemplate, {originArr : originArr});
        }

        // Params
        var paramsArr = [];
        for (key in transform.parameters) {
            paramsArr.push({key: key,
                            value: transform.parameters[key],
                            editing: transform.editing,
                            inputElement: renderInputElement(key, schema.properties.parameters)
                           });
        }
        var parametersTemplate = '<table>{{#paramsArr}}<tr {{#editing}}class="field"{{/editing}}><td>{{key}}</td><td>{{^editing}}<span class="{{edit-class}}">{{value}}</span>{{/editing}}{{#editing}}{{{inputElement}}}{{/editing}}</td></tr>{{/paramsArr}}</table>';
        var paramsTable = $.mustache(parametersTemplate, {paramsArr : paramsArr});

        var template = '<table><tr><td>{{type}}</td></tr><tr><td>{{{originTable}}}</td></tr><tr><td>{{{paramsTable}}}</td></tr>{{#editing}}<tr><td><input class="ok" type="submit" value="Ok"/><input class="cancel" type="submit" value="Cancel"/></td></tr>{{/editing}}</table>';

        var view = {
            type: transform.type,
	    editing: transform.editing,
	    originTable: originTable,
	    paramsTable: paramsTable};
        var transformTable = $.mustache(template, view);

        this.$el.html(transformTable);
        $('#editing-area').append(this.$el);
        return this;
    },

    events: {
        'click .ok' : 'ok',
        'click .cancel' : 'cancel',
        'change .field': 'fieldChanged',
        'keyup .field': 'fieldChanged',
        'click .field': 'fieldChanged',
    },

    preventUpdate: false,

    update: function() {
        if (this.preventUpdate) {
            return;
        }

        var updateValues = function(object, schema) {
            for (key in schema.properties) {
                $('#' + key).val(object[key]);
            }
        };

        var transform = this.model.transform;
        var schema = SS.schemas[transform.type];
        updateValues(transform.origin, schema.properties.origin);
        updateValues(transform.parameters, schema.properties.parameters);



    },

    ok: function() {
        this.model.tryCommit();
    },

    cancel: function() {
        this.model.cancel();
    },

    fieldChanged: function() {
        this.preventUpdate = true;
        this.model.updateFromDOMView();
        this.preventUpdate = false;
    },
    
});
