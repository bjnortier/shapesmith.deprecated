var SS = SS || {};

SS.Text2DCreator = SS.Creator.extend({

    initialize: function(attributes) {
        SS.Creator.prototype.initialize.call(this, attributes);

        this.node.parameters.text = 'Abc';
        this.node.parameters.font = 'DroidSerif';

        this.textPreview = new SS.Text2DPreview({model: this});
        this.views = this.views.concat([
            this.textPreview
        ]);
        this.trigger('change', this);
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
	    var text = new THREE.Mesh(textGeom, new THREE.MeshBasicMaterial( { color: SS.constructors.faceColor, transparent: true, opacity: 0.5 } ) );
            this.sceneObject.add(text);
        }


        this.postRender();
    },

});

