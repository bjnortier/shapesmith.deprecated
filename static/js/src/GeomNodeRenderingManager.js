var SS = SS || {};

SS.GeomNodeRenderingManager = function() {

    this.geomDocAdd = function(geomNode) {
        if (geom_doc.isRoot(geomNode)) {
            SS.renderGeometry(geomNode);
        }
    }

    this.geomDocRemove = function(geomNode) {
        SS.hideGeometry(geomNode);
    }

    this.geomDocReplace = function(original, replacement) {
        this.geomDocRemove(original);
        this.geomDocAdd(replacement);
    }

    this.deselected = function(deselected) {
        for (var i in deselected) {
            var id = deselected[i];
            SS.unhighlightGeometry(geom_doc.findById(id))
        }
    }

    this.selected = function(selected) {
        for (var i in selected) {
            var id = selected[i];
            SS.highlightGeometry(geom_doc.findById(id));
        }
    }
    
    geom_doc.on('add', this.geomDocAdd, this);
    geom_doc.on('remove', this.geomDocRemove, this);
    geom_doc.on('replace', this.geomDocReplace, this);  

    selectionManager.on('selected', this.selected, this);
    selectionManager.on('deselected', this.deselected, this);

}