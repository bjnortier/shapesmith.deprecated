define([
        'src/scene',
        'src/modelviews/overlaydomview'
    ],
    function(sceneModel, OverlayDOMView) {

    var MaterialsDOMView = OverlayDOMView.extend({

        className: 'materials',

        initialize: function(attributes) {
            this.render();
            this.optionsElement = this.$el.find('.options');
            this.optionsElement.hide();
            this.optionsElement.addClass('hidden');
            $('#scene').append(this.$el);
            OverlayDOMView.prototype.initialize.call(this);
            this.update();
        },

        render: function() {
            var template = 
                '<div class="title">COLORS...</div>' + 
                '<div class="options">' +
                    '<div class="colors">{{#colors}}' +
                        '<div class="color" data-color="{{hex}}" style="background-color:{{hex}}"></div>' +
                    '{{/colors}}</div>' +
                '<div>';
            var view = {
                colors: [
                    {hex: '#ffffff'},
                    {hex: '#6690cf'},
                    {hex: '#e6271c'},
                    {hex: '#6cbe32'},
                    {hex: '#f2f377'},
                    {hex: '#888888'},
                ],
            }
            this.$el.html($.mustache(template, view));
        },

        update: function() {
            var sceneObject = this.model.sceneView && this.model.sceneView.sceneObject;
            if (sceneObject) {
                var screenBox = new THREE.Box2();
                var findMaxBox = function(obj) {
                    if (obj.screenBox) {
                        screenBox.union(obj.screenBox);
                    }
                    if (obj.children.length > 0) {
                        obj.children.map(findMaxBox);
                    }
                }
                findMaxBox(sceneObject);
                this.$el.css('left', Math.max((screenBox.max.x + screenBox.min.x)/2 - this.$el.width()/2), 0);
                this.$el.css('top',  Math.max(screenBox.min.y - this.$el.height() - 30, 0));
            }
        },

        events: {
            'click .title'   : 'titleClick',
            'click .color'   : 'selectColor',
            'click .texture' : 'selectTexture',
        },

        titleClick: function() {
            if (this.optionsElement.hasClass('hidden')) {
                this.optionsElement.show();
                this.optionsElement.removeClass('hidden');
            } else {
                this.optionsElement.hide();
                this.optionsElement.addClass('hidden');
            }
        },

        selectColor: function(event) {
            this.model.vertex.parameters.material = {
                color: $(event.target).data('color')
            }
            this.model.tryCommit();
        },

        selectTexture: function(event) {
            this.model.vertex.parameters.material = {
                texture: $(event.target).data('texture')
            }
            this.model.tryCommit();
        },


    });

    return MaterialsDOMView;

});