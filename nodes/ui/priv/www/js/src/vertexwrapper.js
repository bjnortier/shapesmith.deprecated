define([
    'src/geometrygraphsingleton', 
    ], 
    function(geometryGraph) {

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
            geometryGraph.commitEdit(this.vertex);
        },

        cancel: function() {
            geometryGraph.cancel(this.vertex);
        },

    });

    return {
        Model: Model,
        EditingModel: EditingModel,
    }
    

});

