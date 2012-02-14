var SS = SS || {};

var selectionManager = new SelectionManager();

SS.selectionManager = selectionManager;
var command_stack = new CommandStack(SS);

var geom_doc = new GeomDocument();
var treeView = new TreeView();

SS.transformerManager = new SS.TransformerManager();
SS.geomNodeRenderingManager = new SS.GeomNodeRenderingManager();

SS.UI_STATE = new SS.UIState();

