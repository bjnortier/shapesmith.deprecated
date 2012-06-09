var SS = SS || {};

SS.NodeDisplayModel = Backbone.Model.extend({
});
   
SS.NodeDisplayView = Backbone.View.extend({

    initialize: function() {
        this.render();
    },

    render: function() {
        this.$el.html(SS.renderEditingDOM(SS.schemas.workplane, this.model.node));
        $('#workplane').append(this.$el);
    },

    events: {
        "click .value": "edit"
    },

    edit: function() {
        alert('edit');
    },

});

SS.WorkplaneDisplayModel = SS.NodeDisplayModel.extend({

    initialize: function(attributes) {
        this.node = SS.sceneView.workplane.node;
        this.view = new SS.NodeDisplayView({model: this});
    },

    destroy: function() {
        this.view.remove();
    },

    events: {
        'click .ok' : 'ok',
        'click .cancel' : 'cancel',
        'change .field': 'fieldChanged',
        'keyup .field': 'fieldChanged',
        'click .field': 'fieldChanged',
    },

    ok: function() {
    },

    cancel: function() {
    },

    fieldChanged: function() {
    },

});

SS.WorkplaneEditorModel = SS.NodeModel.extend({

});

