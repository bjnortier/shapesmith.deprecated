var SS = SS || {};

SS.Text2DCreator = SS.PrimitiveCreator.extend({

    initialize: function(attributes) {
        SS.PrimitiveCreator.prototype.initialize.call(this, attributes);

        this.textPreview = new SS.Text2DPreview({model: this});
        this.views = this.views.concat([
            this.textPreview,
            new SS.Text2DTextView({model: this})
        ]);
        this.trigger('change', this);
    },

    setDefaultParamters: function() {
        this.node.parameters.text = 'Abc';
        this.node.parameters.font = 'DroidSerif';
    },

    mouseDownOnUV: function(corner) {
        this.activateCorner(corner);
    },

    getBoundingBox: function() {
        var boundingBox = SS.boundingBoxForSceneObject(this.textPreview.sceneObject);
        var origin = this.node.origin;
        ['min', 'max'].map(function(key) {
            boundingBox[key].x += origin.x;
            boundingBox[key].y += origin.y;
            boundingBox[key].z += origin.z;
        });
        return boundingBox;
    },


});

SS.Text2DPreview = SS.PreviewWithOrigin.extend({

    initialize: function() {
        SS.PreviewWithOrigin.prototype.initialize.call(this);
        this.render();
    },
    
    render: function() {
        this.clear();
        SS.PreviewWithOrigin.prototype.render.call(this);
        
        var origin = this.model.node.origin;
        var text = this.model.node.parameters.text;
        var font = this.model.node.parameters.font;

        var threeJSFonts = {'DroidSerif' : 'droid serif',
                           'OpenSans'   : 'open sans',
                           'Lobster'    : 'lobster 1.4',
                           'Inconsolata': 'inconsolata'};
        var threeJSFont = threeJSFonts[font];
        if (text && font) {

            var textGeom = new THREE.TextGeometry(text, {
		size: 10, 
		height: 0.01,
		curveSegments: curveSegments,
		font: threeJSFont,
		weight: 'normal',
		style: 'normal',
		bezelEnabled: false
	    });
	    var text = new THREE.Mesh(textGeom, new THREE.MeshBasicMaterial( { color: SS.materials.faceColor, transparent: true, opacity: 0.5 } ) );
            this.sceneObject.add(text);
        }


        this.postRender();
    },

});

SS.Text2DTextView = Backbone.View.extend({

    initialize: function() {
        this.render();
        this.model.on('change', this.update, this);
    },

    remove: function() {
        Backbone.View.prototype.remove.call(this);
        this.model.off('change', this.update);
    },

    render: function() {
        var text = this.model.node.parameters.text;
        var input = '<input class="field" id="floating-text" value="{{value}}"/>';
        this.$el.html($.mustache(input, {value: text}));
        $('#floating-dom-view .params').append(this.$el);
    },

    events: {
        'change .field': 'fieldChanged',
        'keyup .field': 'fieldChanged',
        'click .field': 'fieldChanged',
    },

    update: function() {
        $('#floating-text').val(this.model.node.parameters.text);
    },

    fieldChanged: function() {
        this.preventUpdate = true;
        this.model.setParameters({text: $('#floating-text').val()});
        this.preventUpdate = false;
    },

});
