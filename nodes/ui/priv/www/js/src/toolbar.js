define(['src/interactioncoordinator'], function(coordinator) {

    var ItemModel = Backbone.Model.extend({

        initialize: function(attributes) {
            this.view = new ItemView({model: this});
        },

        click: function() {
            coordinator.initiateTool(this.name, '/ui/images/icons/' + this.icon);
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