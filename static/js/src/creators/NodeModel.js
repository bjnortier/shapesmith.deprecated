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
        node.origin && updateValues(node.origin, schema.properties.origin);
        node.parameters && updateValues(node.parameters, schema.properties.parameters);
    },

    setParameters: function(parameters) {
        for (var key in parameters) {
            this.node.parameters[key] = parameters[key];
        }

        this.trigger('change:model');
        if (this.editingNode.sceneObjects) {
            this.boundingBox = SS.boundingBoxForGeomNode(this.editingNode);
            this.center = SS.centerOfGeom(this.boundingBox);
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
            paramsTable: paramsTable
        };
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
        node.origin && updateValues(node.origin, schema.properties.origin);
        node.parameters && updateValues(node.parameters, schema.properties.parameters);

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
        SS.sceneView.scene.remove(this.sceneObject);
        this.sceneObject = new THREE.Object3D(); 
    },

    postRender: function() {
        SS.sceneView.scene.add(this.sceneObject);
    },

    remove: function() {
        SS.sceneView.deregisterSceneObjectView(this);
        SS.sceneView.scene.remove(this.sceneObject);
        this.model.off('change', this.render);
    },

    priority: 0,

});


SS.InteractiveSceneView = SS.SceneObjectView.extend({

    initialize: function() {
        SS.SceneObjectView.prototype.initialize.call(this);
        this.on('mouseEnter', this.highlight);
        this.on('mouseLeave', this.unhighlight);
        this.updateCameraScale();
        SS.sceneView.on('cameraChange', this.cameraChange, this);
    },

    remove: function() {
        SS.SceneObjectView.prototype.remove.call(this);
        SS.sceneView.off('cameraChange', this.cameraChange, this);
        this.off('mouseEnter', this.highlight);
        this.off('mouseLeave', this.unhighlight);
    },

    priority: 1,
    active: true,
    cameraScale: 1,

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
        if (this.active) {
            this.recursiveHighlightFn(this.sceneObject, 1.0);
        }
    },

    unhighlight: function() {
        if (this.active) {
            this.recursiveHighlightFn(this.sceneObject, 0.5);
        }
    },

    updateCameraScale: function() {
        var cameraDistance = SS.sceneView.camera.position.length();
        var newScale = cameraDistance/150;
        if (newScale !== this.cameraScale) {
            this.cameraScale = newScale;
            return true;
        } else {
            return false;
        }
    },

    cameraChange: function() {
        if (this.updateCameraScale()) {
            this.render();
        }
    },
 
});

SS.OkCancelView = Backbone.View.extend({

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
        var table = '<table><tr><td><input class="ok" type="submit" value="Ok"/><input class="cancel" type="submit" value="Cancel"/></td></tr></table>';
        this.$el.html(table);
        $('#floating-dom-view .ok-cancel').append(this.$el);
    },

    update: function() {

        var pixelPosition = {x:  window.innerWidth/2 + 10,
                             y: window.innerHeight/2};
        var boundingBox = this.model.getBoundingBox();
        // Nested transforms don't have bounding boxes
        if (boundingBox) {
            var projScreenMat = new THREE.Matrix4();
            projScreenMat.multiply(SS.sceneView.camera.projectionMatrix, 
                                   SS.sceneView.camera.matrixWorldInverse);

            var xminmax = [boundingBox.min.x, boundingBox.max.x];
            var yminmax = [boundingBox.min.y, boundingBox.max.y];
            var zminmax = [boundingBox.min.z, boundingBox.max.z];
            var screenPositionMinMax = [new THREE.Vector3(2,2,0), new THREE.Vector3(-2,-2,0)];
            for (var i = 0; i < 2; i++) {
                for (var j = 0; j < 2; ++j) {
                    for (var k = 0; k < 2; ++k) {
                        var pos = new THREE.Vector3(xminmax[i], yminmax[j], zminmax[k]);
                        projScreenMat.multiplyVector3(pos);

                        screenPositionMinMax[0].x = Math.min(screenPositionMinMax[0].x, pos.x);
                        screenPositionMinMax[0].y = Math.min(screenPositionMinMax[0].y, pos.y);
                        screenPositionMinMax[1].x = Math.max(screenPositionMinMax[1].x, pos.x);
                        screenPositionMinMax[1].y = Math.max(screenPositionMinMax[1].y, pos.y);
                    }
                }
            }

            var centerYPosition = (screenPositionMinMax[0].y + screenPositionMinMax[1].y)/2;
            
            pixelPosition.x = window.innerWidth * ((screenPositionMinMax[1].x+1)/2) + 20;
            pixelPosition.y = window.innerHeight * ((-centerYPosition+1)/2) + 10;
            pixelPosition.x = Math.min(pixelPosition.x, window.innerWidth - 100);
            pixelPosition.y = Math.min(pixelPosition.y, window.innerHeight - 100);
            pixelPosition.x = Math.max(pixelPosition.x, 100);
            pixelPosition.y = Math.max(pixelPosition.y, 100);
        }
       
        $('#floating-dom-view').css('left', pixelPosition.x);
    $('#floating-dom-view').css('top', pixelPosition.y);
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


SS.toScreenCoordinates = function(worldCoordinates) {
    var projScreenMat = new THREE.Matrix4();
    projScreenMat.multiply(SS.sceneView.camera.projectionMatrix, 
                           SS.sceneView.camera.matrixWorldInverse);
    projScreenMat.multiplyVector3(worldCoordinates);
    return {
        x: window.innerWidth * ((worldCoordinates.x+1)/2),
        y: window.innerHeight * ((-worldCoordinates.y+1)/2)
    }
}
