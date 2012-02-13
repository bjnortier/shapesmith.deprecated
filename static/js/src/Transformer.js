var SS = SS || {};

SS.TransformerInitiator = Backbone.Model.extend({
    
    initialize: function() {
        this.boundingBox = SS.boundingBoxForGeomNode(this.attributes.geomNode);
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
            (deselected[0] === this.attributes.geomNode.id)) {
            
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

    initialize: function() {
        this.boundingBox = SS.boundingBoxForGeomNode(this.attributes.editingNode);
        this.center = SS.transformers.centerOfGeom(this.boundingBox);

        this.views = [];

        geom_doc.on('replace', this.geomDocReplace, this);
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

        var transform = this.attributes.transform;
        var schema = SS.schemas[transform.type];
        updateValues(transform.origin, schema.properties.origin);
        updateValues(transform.parameters, schema.properties.parameters);

        this.trigger('change:model');
        this.boundingBox = SS.boundingBoxForGeomNode(this.attributes.editingNode);

        this.trigger('change');
    },

    tryCommit: function() {
        var cmd =  update_geom_command(this.attributes.originalNode, 
                                       this.attributes.editingNode, 
                                       this.attributes.editingNode); 
        command_stack.execute(cmd);
        console.log('Try Commit');
    },

    cancel: function() {
        geom_doc.replace(this.attributes.editingNode, 
                         this.attributes.originalNode);
    },

    geomDocReplace: function(original, replacement) {
        if (original === this.attributes.editingNode) {
            this.destroy();
        }
    },

    destroy: function() {
        this.views.map(function(view) {
            view.remove();
        });
        geom_doc.off('replace', this.geomDocReplace);
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



SS.TransformDOMView = Backbone.View.extend({

    initialize: function() {
        this.render();
        this.model.on('change', this.update, this);
    },

    render: function() {
        var geomNode = this.model.attributes.editingNode;
        var transform = this.model.attributes.transform;
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

        var transform = this.model.attributes.transform;
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
