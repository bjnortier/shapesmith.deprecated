var SS = SS || {};

SS.NodeModel = Backbone.Model.extend({

    updateFromDOMView: function() {
        var updateValues = function(object, schema) {
            for (key in schema.properties) {
                var itemSchema = schema.properties[key];
                if (itemSchema.type === 'array') {
                    for (var k = 0; k < object[key].length; ++k) {
                        for (var subKey in object[key][k]) {
                            object[key][k][subKey] = parseFloat($('#' + subKey + '_' + k).val());
                        }
                    }
                } else if (itemSchema.type === 'string') {
                    object[key] = $('#' + key).val();
                } else {
                    object[key] = parseFloat($('#' + key).val());
                }
            }
        };

        var node = this.node;
        var schema = SS.schemas[node.type];
        updateValues(node.origin, schema.properties.origin);
        updateValues(node.parameters, schema.properties.parameters);
    },

    setParameters: function(parameters) {
        for (var key in parameters) {
            this.node.parameters[key] = parameters[key];
        }

        this.trigger('change:model');
        if (this.editingNode.sceneObjects) {
            this.boundingBox = SS.boundingBoxForGeomNode(this.editingNode);
            this.center = SS.transformers.centerOfGeom(this.boundingBox);
        }

        this.trigger('change');
    },

});


SS.NodeDOMView = Backbone.View.extend({

    initialize: function() {
        this.render();
        this.model.on('change', this.update, this);
    },

    remove: function() {
        Backbone.View.prototype.remove.call(this);
        this.model.off('change', this.update);
    },

    render: function() {
        var node = this.model.node;
        var schema = SS.schemas[node.type];
    
        // Origin & Orientation
        var originTable = null;
        if (node.origin) {
	    var originArr = ['x', 'y', 'z'].map(function(key) {
	        return {key: key, 
		        value: node.origin[key], 
		        editing: node.editing,
                        inputElement: renderInputElement(key, node.origin, schema.properties.origin)
                       }
	    });
	    var originTemplate = '<table>{{#originArr}}<tr {{#editing}}class="field"{{/editing}}><td>{{key}}</td><td>{{^editing}}<span class="{{edit-class}}">{{value}}</span>{{/editing}}{{#editing}}{{{inputElement}}}{{/editing}}</td></tr>{{/originArr}}</table>';
	    var originTable = $.mustache(originTemplate, {originArr : originArr});
        }

        // Params
        var paramsArr = [];
        for (key in node.parameters) {
            paramsArr.push({key: key,
                            value: node.parameters[key],
                            editing: node.editing,
                            inputElement: renderInputElement(key, node.parameters, schema.properties.parameters)
                           });
        }
        var parametersTemplate = '<table>{{#paramsArr}}<tr {{#editing}}class="field"{{/editing}}><td>{{key}}</td><td>{{^editing}}<span class="{{edit-class}}">{{value}}</span>{{/editing}}{{#editing}}{{{inputElement}}}{{/editing}}</td></tr>{{/paramsArr}}</table>';
        var paramsTable = $.mustache(parametersTemplate, {paramsArr : paramsArr});

        var template = '<table><tr><td>{{type}}</td></tr><tr><td>{{{originTable}}}</td></tr><tr><td>{{{paramsTable}}}</td></tr>{{#editing}}<tr><td><input class="ok" type="submit" value="Ok"/><input class="cancel" type="submit" value="Cancel"/></td></tr>{{/editing}}</table>';

        var view = {
            type: node.type,
	    editing: node.editing,
	    originTable: originTable,
	    paramsTable: paramsTable};
        var nodeTable = $.mustache(template, view);

        this.$el.html(nodeTable);
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
                if (schema.properties[key].type === 'array') {
                    for (var k = 0; k < object[key].length; ++k) {
                        for (subKey in object[key][k]) {
                            var id = '#' + subKey + '_' + k;
                            $(id).val(object[key][k][subKey])
                        }
                    }
                } else {
                    $('#' + key).val(object[key]);
                }
            }
        };

        var node = this.model.node;
        var schema = SS.schemas[node.type];
        updateValues(node.origin, schema.properties.origin);
        updateValues(node.parameters, schema.properties.parameters);

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
    },
    
});
