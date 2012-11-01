define(['src/interactioncoordinator', 'src/geometrygraphsingleton'], 
    function(coordinator, geometryGraph) {

    var ItemModel = Backbone.Model.extend({

        initialize: function(attributes) {
            this.view = new ItemView({model: this});
            this.toolbarModel = attributes.toolbarModel;
        },

        click: function() {
            this.toolbarModel.itemClicked(this);
        },

        activate: function() {
            this.set('active', true);
        },

        deactivate: function() {
            this.set('active', false);
        },

    });

    var ItemView = Backbone.View.extend({

        tagName: "li",
        className: "item",

        initialize: function() {
            this.render();
            this.$el.addClass(this.model.name);
            this.model.on('change:active', this.updateActive, this);
        },

        render: function() {
            $(this.el).html('<img src=/ui/images/icons/' + this.model.icon + '/>');
            return this;
        },

        events: {
            'click' : 'click',
        },

        click: function() {
            this.model.click();
        },

        updateActive: function() {
            if (this.model.get('active')) {
                this.$el.addClass('active');
            } else {
                this.$el.removeClass('active');
                }
        },

    });

    var Model = Backbone.Model.extend({

        initialize: function(attributes) {
            this.name = attributes.name;
            this.view = new View({model: this});
            this.items = [];
            $('body').append(this.view.$el);
        },

        addItem: function(itemModel) {
            this.items.push(itemModel);
            this.view.$el.append(itemModel.view.$el);
        },

    });

    var View = Backbone.View.extend({

        tagName: "ol",
        className: "toolbar",

    });

    return  {
        ItemModel: ItemModel,
        ItemView: ItemView,
        Model: Model,
        View: View,
    }

    
});