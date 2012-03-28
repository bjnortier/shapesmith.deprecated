var SS = SS || {};

var selectionManager = new SelectionManager();

SS.selectionManager = selectionManager;
var command_stack = new CommandStack(SS);

var geom_doc = new GeomDocument();
var treeView = new TreeView();

SS.UI_MOUSE_STATE = new SS.UIMouseState();
SS.UI_EDITING_STATE = new SS.UIEditingState();

SS.transformerManager = new SS.TransformerManager();
SS.geomNodeRenderingManager = new SS.GeomNodeRenderingManager();


