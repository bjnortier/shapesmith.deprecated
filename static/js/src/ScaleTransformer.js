var SS = SS || {};

SS.ScaleTransformerModel = Backbone.Model.extend({

    initialize: function(attributes) { 
        var that = this;

        this.boundingBox = SS.boundingBoxForGeomNode(this.attributes.editingNode);
        this.center = SS.transformers.centerOfGeom(this.boundingBox);
        this.anchorPosition = attributes.anchorFunction(this.boundingBox);

        this.attributes.arrowViews.map(function(arrowView) {
            arrowView.attach(that);
            arrowView.on('mouseDrag', function(event) {
                that.drag(event);
            });
            arrowView.render();
        });

        geom_doc.addListener(function(event) {
            that.geomDocUpdated(event);
        });

        var newViews = [
            new SS.ScaleGeomNodeView({model: this}),
            new SS.ScaleFactorView({model: this}),
            new SS.ScaleDOMView({model: this}),
        ];
        this.views = newViews.concat(this.attributes.arrowViews);
    },

    mouseDown: function(arrowView, event) {
        this.anchorPosition = arrowView.anchorFunction(this.boundingBox);
    },

    drag: function(event) {

        var dxFrom = this.anchorPosition.x - this.attributes.transform.origin.x;
        var dyFrom = this.anchorPosition.y - this.attributes.transform.origin.y;

        var r1 = Math.sqrt(dxFrom*dxFrom + dyFrom*dyFrom);

        var workplanePosition = SS.sceneView.determinePositionOnWorkplane(event);
        var dxTo = workplanePosition.x - this.attributes.transform.origin.x;
        var dyTo = workplanePosition.y - this.attributes.transform.origin.y;
        var r2 = Math.sqrt(dxTo*dxTo + dyTo*dyTo);

        var factor = parseFloat((r2/r1).toFixed(3));
        if (event.ctrlKey) {
            factor = Math.round(factor*10)/10;
        }

        this.attributes.transform.parameters.factor = factor;

        this.trigger('change:model');
        this.boundingBox = SS.boundingBoxForGeomNode(this.attributes.editingNode);

        this.trigger('change');

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

    destroy: function() {
        this.views.map(function(view) {
            view.remove();
        });
        geom_doc.removeListener(this.geomDocUpdated);
    },
    
    geomDocUpdated: function(event) {
        if (event.replace && (event.replace.original === this.attributes.editingNode)) {
            this.destroy();
        }
    }
        
});

SS.ScaleGeomNodeView = Backbone.View.extend({

    initialize: function() {
        this.render();
        this.model.on('change:model', this.render, this);
    },

    render: function() {
        var transform = this.model.attributes.transform;
        var scalePoint = new THREE.Vector3(transform.origin.x,
                                           transform.origin.y,
                                           transform.origin.z);

        // TODO: Replace with model for geom node
        SS.scaleGeomNodeRendering(this.model.attributes.originalNode, 
                                  this.model.attributes.editingNode, 
                                  scalePoint, 
                                  this.model.attributes.transform.parameters.factor);
    },

});

SS.ScaleDOMView = Backbone.View.extend({

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

SS.ScaleFactorView = SS.SceneObjectView.extend({
    
    initialize: function() {
	SS.SceneObjectView.prototype.initialize.call(this);
        this.model.on('change', this.render, this);
    },

    render: function() {
        this.clear();
        
        var factor = this.model.attributes.transform.parameters.factor;
        if (factor && (factor > 0)) {
            var textGeo = new THREE.TextGeometry('' + factor, {
	        size: 2, height: 0.01, curveSegments: 6,
	        font: 'helvetiker', weight: 'normal', style: 'normal',
	        bezelEnabled: false});
            var text = new THREE.Mesh(textGeo, 
                                      new THREE.MeshBasicMaterial({color: 0xffffff, 
                                                                   opacity: 0.8}));
            
            var boundingBox = SS.boundingBoxForGeomNode(this.model.attributes.editingNode);
            var center = SS.transformers.centerOfGeom(boundingBox);
            text.position.y = center.y - text.boundRadius/2;
            text.position.x = boundingBox.max.x + 5;
            text.rotation.z = Math.PI/2; 
            this.sceneObject.add(text);
        }

        this.postRender();
    },

    

});

