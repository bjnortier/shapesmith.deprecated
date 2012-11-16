define([
    'src/geometrygraphsingleton', 
    'src/interactioncoordinator',
    ], 
    function(geometryGraph, coordinator) {

    // ---------- Common ----------
    
    var Model = Backbone.Model.extend({

        initialize: function(vertex) {
            this.vertex = vertex;
            this.vertex.on('descendantChanged', this.descendantChanged, this);
        },

        destroy: function() {
            this.views.forEach(function(view) {
                view.remove();
            });
            this.views = [];
            this.vertex.off('descendantChanged', this.descendantChanged, this);

        },  

        descendantChanged: function(descendant) {
            this.vertex.trigger('change', this.vertex);
        },

    });


    var EditingModel = Model.extend({

        okCreate: function() {
            geometryGraph.commitCreate(this.vertex);
        },

        okEdit: function() {
            geometryGraph.commitEdit();
        },

        cancel: function() {
            geometryGraph.cancel(this.vertex);
        },

    });

    var EditingDOMView = Backbone.View.extend({

        tagName: 'tr',
        className: 'vertex editing',

        initialize: function() {
            this.render();
            this.$el.addClass(this.model.vertex.id);
            this.model.vertex.on('change', this.update, this);
        },

        remove: function() {
            Backbone.View.prototype.remove.call(this);
            this.model.vertex.off('change', this.update, this);
        },

        events: {
            'focusin .field'  : 'fieldFocusIn',
            'focusout .field' : 'fieldFocusOut',
            'change .field'   : 'fieldChange',
            // 'keyup .field'    : 'fieldChange',
        },

        fieldFocusIn: function(event) {
            coordinator.setFieldFocus(true);
        },

        fieldFocusOut: function(event) {
            coordinator.setFieldFocus(false);
        },

        fieldChange: function(event) {
            this.updateFromDOM();
            this.model.vertex.trigger('change', this.model.vertex);
        },

    });

    return {
        Model: Model,
        EditingModel: EditingModel,
        EditingDOMView: EditingDOMView,
    }
    

});

