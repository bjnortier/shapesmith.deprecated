define(['src/interactioncoordinator'], function(coordinator) {

    var ItemModel = Backbone.Model.extend({

        initialize: function(attributes) {
            this.active = false;
            this.view = new ItemView({model: this});
            coordinator.on('toolActivated', this.toolActivated, this);
        },

        click: function() {
            if (!this.active) {
                coordinator.activateTool(this.name);
            } else {
                coordinator.deactivateTool(this.name);
            }
        },

        toolActivated: function(name) {
            this.active = (name === this.name);
            if (this.active) {
                this.view.$el.addClass('activated');
            } else {
                this.view.$el.removeClass('activated');
            }
        },

    });

    var ItemView = Backbone.View.extend({

        tagName: "li",
        className: "item",

        initialize: function() {
            this.render();
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

    });

    var Model = Backbone.Model.extend({

        initialize: function(attributes) {
            this.name = attributes.name;
            this.view = new View({model: this});
            $('body').append(this.view.$el);
        },

        addItem: function(itemModel) {
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