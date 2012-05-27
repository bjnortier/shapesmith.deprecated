
#include "Builder.h"
#include "Tesselate.h"
#include "Transform.h"
#include "Util.h"
#include "base64.h"


TopoDS_Shape Builder::shape() {
    return shape_;
}

void Builder::ApplyOrigin(map< string, mValue > json) {
    if (!json["origin"].is_null()) {
        map< string, mValue > origin = json["origin"].get_obj();
        double x = Util::to_d(origin["x"]);
        double y = Util::to_d(origin["y"]);
        double z = Util::to_d(origin["z"]);
        gp_Trsf transformation = gp_Trsf();
        transformation.SetTranslation(gp_Vec(x,y,z));
        
        shape_ = BRepBuilderAPI_Transform(shape_, transformation).Shape();
    }
}

void Builder::ApplyTransform(map< string, mValue > transformJson) {
    string transformType = transformJson["type"].get_str();
    map< string, mValue > origin = transformJson["origin"].get_obj();
    map< string, mValue > parameters = transformJson["parameters"].get_obj();
    
    if (transformType == "rotate") {
        auto_ptr< Transformer<Rotate> > transformer(new Transformer<Rotate>(shape_, origin, parameters));
        shape_ = transformer->transformed_shape();
        return;
    } else if (transformType == "scale") {
        auto_ptr< Transformer<Scale> > transformer(new Transformer<Scale>(shape_, origin, parameters));
        shape_ = transformer->transformed_shape();
        return;
    } else if (transformType == "translate") {
        auto_ptr< Transformer<Translate> > transformer(new Transformer<Translate>(shape_, origin, parameters));
        shape_ = transformer->transformed_shape();
        return;
    } else if ((transformType == "mirror") || (transformType == "axis_mirror")) {
        auto_ptr< Transformer<AxisMirror> > transformer(new Transformer<AxisMirror>(shape_, origin, parameters));
        shape_ = transformer->transformed_shape();
        return;
    } else if (transformType == "plane_mirror") {
        auto_ptr< Transformer<PlaneMirror> > transformer(new Transformer<PlaneMirror>(shape_, origin, parameters));
        shape_ = transformer->transformed_shape();
        return;
    } 
    throw "transform type not found";
}

void Builder::ApplyTransforms(map< string, mValue > json) {
    if (!json["transforms"].is_null()) {
        mArray transforms = json["transforms"].get_array();
        for (unsigned int k = 0; k < transforms.size(); ++k) {
            mValue transformJson = transforms[k];
            this->ApplyTransform(transformJson.get_obj());
        }
    } 
}

#pragma mark 3D builders

void Builder3D::Mesh() {
    TopExp_Explorer Ex; 
    int numFaces = 0;
    for (Ex.Init(shape_, TopAbs_FACE); Ex.More(); Ex.Next()) { 
        ++numFaces;
    }
    
    if (numFaces > 0) {
        BRepMesh().Mesh(shape_, 1.0);
    }
}

void Builder3D::PostProcess(map< string, mValue > json) {
    this->ApplyOrigin(json);
    this->ApplyTransforms(json);
    this->Mesh();
}

CuboidBuilder::CuboidBuilder(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    double width = Util::to_d(parameters["u"]);
    double depth = Util::to_d(parameters["v"]);
    double height = Util::to_d(parameters["w"]);
	 
    map< string, mValue > origin = json["origin"].get_obj();
	 
    if (width < 0) {
	origin["x"] = Util::to_d(origin["x"]) + width;
	width = -width;
    }

    if (depth < 0) {
	origin["y"] = Util::to_d(origin["y"]) + depth;
	depth = -depth;
    }
	 
    if (height < 0) {
	origin["z"] = Util::to_d(origin["z"]) + height;
	height = -height;
    }
    json["origin"] = origin;
	 
    shape_ = BRepPrimAPI_MakeBox(width, depth, height).Shape();
    PostProcess(json);
}

SphereBuilder::SphereBuilder(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    double radius = Util::to_d(parameters["r"]);
    shape_ = BRepPrimAPI_MakeSphere(radius).Shape();
    PostProcess(json);    
}

CylinderBuilder::CylinderBuilder(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    double r = Util::to_d(parameters["r"]);
    double h = Util::to_d(parameters["h"]);
    
    map< string, mValue > origin = json["origin"].get_obj();
    if(h < 0) {
	origin["z"] = Util::to_d(origin["z"]) + h;
	h = -h;
    }
    json["origin"] = origin;
    
    shape_ = BRepPrimAPI_MakeCylinder(r, h).Shape();
    PostProcess(json);
}

ConeBuilder::ConeBuilder(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    double r1 = Util::to_d(parameters["r1"]);
    double r2 = 0.0;
    if (!parameters["r2"].is_null()) {
        r2 = Util::to_d(parameters["r2"]);
    }
    double h = Util::to_d(parameters["h"]);

    map< string, mValue > origin = json["origin"].get_obj();
    if(h < 0) {
	origin["z"] = Util::to_d(origin["z"]) + h;
	h = -h;
	swap(r1, r2);
    }
    json["origin"] = origin;
        
    shape_ = BRepPrimAPI_MakeCone(r1, r2, h).Shape();
    PostProcess(json);
}

WedgeBuilder::WedgeBuilder(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    double u1 = Util::to_d(parameters["u1"]);
    double u2 = Util::to_d(parameters["u2"]);
    double v = Util::to_d(parameters["v"]);
    double w = Util::to_d(parameters["w"]);

    map< string, mValue > origin = json["origin"].get_obj();
    if(w < 0) {
	origin["z"] = Util::to_d(origin["z"]) + w;
	w = -w;
    }
    json["origin"] = origin;
    shape_ = BRepPrimAPI_MakeWedge(u1, v, w, u2).Shape();
    PostProcess(json);
}


TorusBuilder::TorusBuilder(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    double r1 = Util::to_d(parameters["r1"]);
    double r2 = Util::to_d(parameters["r2"]);
    shape_ = BRepPrimAPI_MakeTorus(r1, r2).Shape();
    PostProcess(json);
}

STLImportBuilder::STLImportBuilder(string id, map< string, mValue > json) {
    
    // Temp filename
    char fileName[100];
    sprintf(fileName, "/tmp/%s.stl", id.c_str());
    
    // Decode base64
    string contents = json["contents"].get_str();
    string decoded = base64_decode(contents);
    
    ofstream tmpFile;
    tmpFile.open (fileName);
    tmpFile << decoded;
    tmpFile.close();
    
    // Read from temp file
    StlAPI_Reader reader;
    TopoDS_Shape shape;
    reader.Read(shape, fileName);
    
    shape_ = shape;
    
    // Delete file
    remove(fileName);
    
    this->Mesh();
}

#pragma mark 2D Builders

void Builder2D::Mesh() {
    TopExp_Explorer Ex; 
    int numFaces = 0;
    for (Ex.Init(shape_, TopAbs_FACE); Ex.More(); Ex.Next()) { 
        ++numFaces;
    }
    
    if (numFaces > 0) {
        BRepMesh().Mesh(shape_, 1.0);
    }
}

void Builder2D::PostProcess(map< string, mValue > json) {
    this->ApplyOrigin(json);

    this->ApplyTransforms(json);
    this->Mesh();
}

TopoDS_Shape rotate90DegreesAroundZ(TopoDS_Shape shape);

Ellipse2DBuilder::Ellipse2DBuilder(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    double r1 = Util::to_d(parameters["r1"]);
    double r2 = Util::to_d(parameters["r2"]);
    
    double from_angle = Util::to_d(parameters["from_angle"], 0);
    double to_angle = Util::to_d(parameters["to_angle"], 360);
    
    bool majorBigger = r1 >= r2;
    
    BRepBuilderAPI_MakeWire wire;
    
    gp_Elips ellipse;
    if (!majorBigger) {
        from_angle = from_angle - 90;
        to_angle = to_angle - 90;
        double tmp = r1;
        r1 = r2;
        r2 = tmp;
    }

    ellipse = gp_Elips(gp_Ax2(gp_Pnt(0,0,0),gp_Dir(0,0,1)), r1, r2);
    wire.Add(BRepBuilderAPI_MakeEdge(ellipse, from_angle/180*M_PI, to_angle/180*M_PI).Edge());

    if (!((from_angle == 0) && (to_angle == 360))) {
        wire.Add(BRepBuilderAPI_MakeEdge(gp_Pnt(r1*cos(to_angle/180*M_PI),
                                                r2*sin(to_angle/180*M_PI),
                                                0),
                                        gp_Pnt(0,0,0)));
        wire.Add(BRepBuilderAPI_MakeEdge(gp_Pnt(0,0,0),
                                         gp_Pnt(r1*cos(from_angle/180*M_PI),
                                                r2*sin(from_angle/180*M_PI),
                                                0)
                                         ));

    }
    
    TopoDS_Shape shape = BRepBuilderAPI_MakeFace(wire);
    
    if (majorBigger) {
        shape_ = shape;
    } else {
        shape_ = rotate90DegreesAroundZ(shape);
    }
    
    PostProcess(json);
}

Rectangle2DBuilder::Rectangle2DBuilder(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    double u = Util::to_d(parameters["u"]);
    double v = Util::to_d(parameters["v"]);
    
    gp_Pnt points[4] = {gp_Pnt(0 , 0 , 0), gp_Pnt(u , 0 , 0), gp_Pnt(u , v , 0), gp_Pnt(0 , v , 0)};
    
    TopoDS_Edge edges[4];
    for (int i = 0; i < 4; ++i) {
        edges[i] = BRepBuilderAPI_MakeEdge(GC_MakeSegment(points[i] , points[(i + 1) % 4]));
    }
    TopoDS_Wire wire = BRepBuilderAPI_MakeWire(edges[0], edges[1], edges[2], edges[3]);
    
    shape_ = BRepBuilderAPI_MakeFace(wire);
    PostProcess(json);  
}

Triangle2DBuilder::Triangle2DBuilder(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    
    mArray vertices = parameters["vertices"].get_array();
    gp_Pnt points[3];
    for (unsigned int k = 0; k < vertices.size(); ++k) {
        map< string, mValue > vertex = vertices[k].get_obj();
        points[k] = gp_Pnt(Util::to_d(vertex["u"]),
                           Util::to_d(vertex["v"]),
                           Util::to_d(vertex["w"]));
    }
    
    TopoDS_Edge edges[3];
    for (int i = 0; i < 3; ++i) {
        edges[i] = BRepBuilderAPI_MakeEdge(GC_MakeSegment(points[i] , points[(i + 1) % 3]));
    }
    TopoDS_Wire wire = BRepBuilderAPI_MakeWire(edges[0], edges[1], edges[2]);
    
    shape_ = BRepBuilderAPI_MakeFace(wire);
    PostProcess(json);  
}


#pragma mark 1D Builders

void Builder1D::PostProcess(map< string, mValue > json) {
    this->ApplyOrigin(json);
    this->ApplyTransforms(json);
}

Ellipse1DBuilder::Ellipse1DBuilder(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    double r1 = Util::to_d(parameters["r1"]);
    double r2 = Util::to_d(parameters["r2"]);
    
    double from_angle = Util::to_d(parameters["from_angle"], 0);
    double to_angle = Util::to_d(parameters["to_angle"], 360);
    
    bool majorBigger = r1 >= r2;
    
    BRepBuilderAPI_MakeWire wire;
    
    gp_Elips ellipse;
    if (!majorBigger) {
        from_angle = from_angle - 90;
        to_angle = to_angle - 90;
        double tmp = r1;
        r1 = r2;
        r2 = tmp;
    }
    
    ellipse = gp_Elips(gp_Ax2(gp_Pnt(0,0,0),gp_Dir(0,0,1)), r1, r2);
    wire.Add(BRepBuilderAPI_MakeEdge(ellipse, from_angle/180*M_PI, to_angle/180*M_PI).Edge());
    

    
    if (majorBigger) {
        shape_ = wire;
    } else {
        shape_ = rotate90DegreesAroundZ(wire);
    }
    
    PostProcess(json);
}

Bezier1DBuilder::Bezier1DBuilder(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    
    mArray vertices = parameters["vertices"].get_array();
    gp_Pnt points[4];
    for (unsigned int k = 0; k < vertices.size(); ++k) {
        map< string, mValue > vertex = vertices[k].get_obj();
        points[k] = gp_Pnt(Util::to_d(vertex["u"]),
                           Util::to_d(vertex["v"]),
                           Util::to_d(vertex["w"]));
    }
    
    TColgp_Array1OfPnt aPoles (1,4);
    aPoles(1) = points[0];
    aPoles(2) = points[1];
    aPoles(3) = points[2];
    aPoles(4) = points[3];
    Handle(Geom_BezierCurve) aCurve = new Geom_BezierCurve(aPoles);
    BRepBuilderAPI_MakeWire wire;
    wire.Add(BRepBuilderAPI_MakeEdge(aCurve));
    
    shape_ = wire;
    
    PostProcess(json);
}

PolylineBuilder::PolylineBuilder(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    
    mArray vertices = parameters["vertices"].get_array();
    BRepBuilderAPI_MakeWire wireBuilder;
    gp_Pnt previous;
    bool first = true;
    for (unsigned int k = 0; k < vertices.size(); ++k) {
        map< string, mValue > vertex = vertices[k].get_obj();
        gp_Pnt next = gp_Pnt(Util::to_d(vertex["u"]),
                             Util::to_d(vertex["v"]),
                             Util::to_d(vertex["w"]));
        if (first) {
            first = false;
        } else {
            TopoDS_Edge edge = BRepBuilderAPI_MakeEdge(GC_MakeSegment(previous, next));
            wireBuilder.Add(edge);
        }
        previous = next;
    }
    
    shape_ = wireBuilder.Shape();
    PostProcess(json);  
}


class TextComposer {
    
    gp_Pnt last_position_;
    gp_Pnt offset_;
    BRepBuilderAPI_MakeWire wire_maker_;
    FT_Vector character_pos_;
    vector<TopoDS_Wire> wires_;

   
private:
    
    double factor_;
    
    gp_Pnt ToPoint(FT_Vector* vec) {
        return gp_Pnt(offset_.X() + factor_*vec->x, 
                      offset_.Y() + factor_*vec->y, 
                      0);
    }
    
public:

    TextComposer(FT_Vector* character_pos) {
        // The spacing is slightly different in Three.js and Freetype, this is very close
        // to making the font render as height=10 in Three.js
        factor_ = 0.00312;
        offset_ = gp_Pnt(factor_*character_pos->x, 
                         factor_*character_pos->y, 
                         0);
        
    }
    
    vector<TopoDS_Wire> compose(FT_Outline* outline, FT_Outline_Funcs interface) {
        FT_Outline_Decompose(outline, &interface, this);
        if (wire_maker_.IsDone()) {
            wires_.push_back(wire_maker_.Wire());
        }
        return wires_;
    }

    int moveTo(FT_Vector* to) {
        if (wire_maker_.IsDone()) {
            wires_.push_back(wire_maker_.Wire());
        }
        BRepBuilderAPI_MakeWire tmp;
        wire_maker_ = tmp;
        
        last_position_ = ToPoint(to);
        return 0;
    };
    
    int lineTo(FT_Vector* to) {
        gp_Pnt new_position = ToPoint(to);
        TopoDS_Edge edge = BRepBuilderAPI_MakeEdge(GC_MakeSegment(last_position_, new_position));
        wire_maker_.Add(edge);
        last_position_ = new_position;
        return 0;
    };
    
    int conicTo(FT_Vector* control, FT_Vector* to) {
        gp_Pnt control_pt = ToPoint(control);
        gp_Pnt to_pt = ToPoint(to);
        
        TColgp_Array1OfPnt poles(1, 3);
        poles.SetValue(1, last_position_);
        poles.SetValue(2, control_pt);
        poles.SetValue(3, to_pt);

        Handle(Geom_BezierCurve) curve = new Geom_BezierCurve(poles); 
        TopoDS_Edge edge = BRepBuilderAPI_MakeEdge(curve);
        wire_maker_.Add(edge);

        last_position_ = to_pt;
        return 0;
    };
    
    int cubicTo(FT_Vector* control1, FT_Vector* control2, FT_Vector* to) {
        return 0;
    };

    
};

int moveToCallback(FT_Vector* to, TextComposer* composer) {
    return composer->moveTo(to);
};
int lineToCallback(FT_Vector* to, TextComposer* composer) {
    return composer->lineTo(to);
};
int conicToCallback(FT_Vector* control, FT_Vector* to, TextComposer* composer) {
    return composer->conicTo(control, to);
};
int cubicToCallback(FT_Vector* control1, FT_Vector* control2, FT_Vector* to, TextComposer* composer) {
    return composer->cubicTo(control1, control2, to);
};

Text2DBuilder::Text2DBuilder(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    string text = parameters["text"].get_str();
    const char* textChars = text.c_str();
    
    string font = parameters["font"].get_str();
    char font_path[256];
    sprintf(font_path, "./fonts/%s/%s.ttf", font.c_str(), font.c_str()   );
        
    FT_Library freetype_library;
    FT_Init_FreeType(&freetype_library);
    FT_Face     face; 
    FT_New_Face(freetype_library,
                font_path,
                0,
                &face);
    
    FT_Set_Char_Size(face, 50*64, 0, 100, 100);
    
    FT_Outline_Funcs interface;
    interface.move_to = (FT_Outline_LineTo_Func)moveToCallback;
    interface.line_to = (FT_Outline_LineTo_Func)lineToCallback;
    interface.conic_to = (FT_Outline_ConicTo_Func)conicToCallback;
    interface.cubic_to = (FT_Outline_CubicTo_Func)cubicToCallback;
    interface.shift = 0;
    interface.delta = 0;
    
    TopoDS_Builder builder;
    TopoDS_Compound compound;
    builder.MakeCompound(compound);
    
    FT_Vector character_pos = {0,0};
    for (unsigned int i = 0; i < text.length(); ++i) {
        FT_Load_Char(face, textChars[i], FT_LOAD_NO_BITMAP);
        auto_ptr<TextComposer> composer(new TextComposer(&character_pos));
        vector<TopoDS_Wire> wires = composer->compose(&face->glyph->outline, interface);
        
        // spaces have no wires
        if (wires.size() > 0) {
            vector<TopoDS_Wire>::iterator it = wires.begin();
            TopoDS_Shape character_shape = BRepBuilderAPI_MakeFace(*it);
            ++it;
        
            for (; it < wires.end(); ++it ) {
                TopoDS_Shape newFace = BRepBuilderAPI_MakeFace(*it);

                // If there is an intersection, cut the way that leaves faces
                TopoDS_Shape intersect = BRepAlgoAPI_Common(character_shape, newFace);
                if (TopExp_Explorer(intersect, TopAbs_FACE).More()) {
                    
                    TopoDS_Shape a = BRepAlgoAPI_Cut(character_shape, newFace);
                    TopoDS_Shape b = BRepAlgoAPI_Cut(newFace, character_shape);
                    
                    if (TopExp_Explorer(a, TopAbs_FACE).More()) {
                        character_shape = a;
                    } else {
                        character_shape = b;
                    }
                        
                } else {
                    character_shape = BRepAlgoAPI_Fuse(character_shape, newFace);
                }
            }
            
            builder.Add(compound, character_shape);
        }
        
        character_pos.x += face->glyph->advance.x;
        character_pos.y += face->glyph->advance.y;
    }
    
    shape_ = compound;
    
    FT_Done_Face(face);
    FT_Done_FreeType(freetype_library);
    
    PostProcess(json);
}

TopoDS_Shape rotate90DegreesAroundZ(TopoDS_Shape shape) {
    gp_Trsf transformation = gp_Trsf();
    transformation.SetRotation(gp_Ax1(gp_Pnt(0.0,0.0,0.0), gp_Dir(0.0,0.0,1.0)), M_PI/2);
    
    return BRepBuilderAPI_Transform(shape, transformation).Shape();
}

#pragma mark Boolean builders

void BuilderND::Mesh() {
    TopExp_Explorer ex(shape_, TopAbs_FACE);
    
    if (ex.More()) {
        BRepMesh().Mesh(shape_, TRIANGLE_SIZE);
    }

}

void BuilderND::PostProcess(map< string, mValue > json) {
    this->ApplyTransforms(json);
    this->Mesh();
}

TopoDS_Shape create_compound(TopExp_Explorer it) {
    
    TopoDS_Builder builder;
    TopoDS_Compound compound;
    builder.MakeCompound(compound);
    
    for(; it.More(); it.Next()) {
        builder.Add(compound, it.Current());
    }
    return compound;
}

TopoDS_Shape create_compound(TopoDS_Shape shape, TopAbs_ShapeEnum keepType, TopAbs_ShapeEnum avoidParentType) {
    return create_compound(TopExp_Explorer(shape, keepType, avoidParentType));
}

TopoDS_Shape create_compound(TopoDS_Shape shape, TopAbs_ShapeEnum keepType) {
    return create_compound(TopExp_Explorer(shape, keepType));
}


TopoDS_Shape performCompoundBoolean(TopoDS_Shape a, TopoDS_Shape b, boolean_op function) {
    
    
    TopoDS_Shape solidA = create_compound(a, TopAbs_SOLID);
    TopoDS_Shape solidB = create_compound(b, TopAbs_SOLID);
    TopoDS_Shape solids = function(solidA, solidB);
    
    
    TopoDS_Shape facesA = create_compound(a, TopAbs_FACE, TopAbs_SOLID);
    TopoDS_Shape facesB = create_compound(b, TopAbs_FACE, TopAbs_SOLID);
    TopoDS_Shape faces = function(facesA, facesB);
    
    TopoDS_Builder wires_builder;
    TopoDS_Compound wires_compound;
    wires_builder.MakeCompound(wires_compound);
    
    for(TopExp_Explorer it(a, TopAbs_WIRE, TopAbs_FACE); it.More(); it.Next()) {
        wires_builder.Add(wires_compound, it.Current());
    }
    for(TopExp_Explorer it(b, TopAbs_WIRE, TopAbs_FACE); it.More(); it.Next()) {
        wires_builder.Add(wires_compound, it.Current());
    }

    TopoDS_Builder builder;
    TopoDS_Compound compound;
    builder.MakeCompound(compound);
    builder.Add(compound, solids);
    builder.Add(compound, faces);
    builder.Add(compound, wires_compound);


    return compound;
}

BooleanBuilder::BooleanBuilder(map< string, mValue > json, vector<TopoDS_Shape>& shapes, boolean_op function, e_PostProcess post_process) {
    vector<TopoDS_Shape>::iterator it = shapes.begin();

    shape_ = (*it);
    ++it;
    
    for ( ; it < shapes.end(); ++it ) {
        shape_ = performCompoundBoolean((*it), shape_, function);
    }
    if (post_process == PostProcess_YES) {
        PostProcess(json);
    }
}

TopoDS_Shape fuse(const TopoDS_Shape& a, const TopoDS_Shape& b) {
    return BRepAlgoAPI_Fuse(a,b);
}

TopoDS_Shape common(const TopoDS_Shape& a, const TopoDS_Shape& b) {
    return BRepAlgoAPI_Common(a,b);
}

TopoDS_Shape cut(const TopoDS_Shape& a, const TopoDS_Shape& b) {
    return BRepAlgoAPI_Cut(a,b);
}

UnionBuilder::UnionBuilder(map< string, mValue > json, vector<TopoDS_Shape>& shapes, e_PostProcess post_process) 
    : BooleanBuilder(json, shapes, &fuse, post_process) {
}

IntersectBuilder::IntersectBuilder(map< string, mValue > json, vector<TopoDS_Shape>& shapes, e_PostProcess post_process) 
    : BooleanBuilder(json, shapes, &common, post_process) {
}

SubtractBuilder::SubtractBuilder(map< string, mValue > json, vector<TopoDS_Shape>& shapes, e_PostProcess post_process) 
    : BooleanBuilder(json, shapes, &cut, post_process) {
}

#pragma mark Modifier builders

PrismBuilder::PrismBuilder(map< string, mValue > json, TopoDS_Shape shape) {

    map< string, mValue > parameters = json["parameters"].get_obj();
    double u = Util::to_d(parameters["u"]);
    double v = Util::to_d(parameters["v"]);
    double w = Util::to_d(parameters["w"]);
    gp_Vec prismVec(u,v,w);

    try {
        shape_ = BRepPrimAPI_MakePrism(shape, prismVec);
    } catch (...) {
        throw could_not_make_prism();
    }
    PostProcess(json);
}

std::pair<gp_Pnt, gp_Pnt> get_start_and_end(TopoDS_Wire wire, TopoDS_Shape parent) {
    TopExp_Explorer vertexIt(wire, TopAbs_VERTEX);
    gp_Pnt first = BRep_Tool::Pnt(TopoDS::Vertex(vertexIt.Current()));
    first = first.Transformed(parent.Location().Transformation());
    
    vertexIt.Next();
    gp_Pnt last = first;
    for (; vertexIt.More(); vertexIt.Next()) {
        TopoDS_Vertex vertex = TopoDS::Vertex(vertexIt.Current());
        last = BRep_Tool::Pnt(vertex);
        last = last.Transformed(parent.Location().Transformation());
    }
    if (wire.Orientation() == TopAbs_FORWARD) {
        return pair<gp_Pnt, gp_Pnt>(first, last);
    } else {
        return pair<gp_Pnt, gp_Pnt>(last, first);
    }
}


FaceBuilder::FaceBuilder(map< string, mValue > json, vector<TopoDS_Shape>& shapes) {

    UnionBuilder unionBuilder(json, shapes, PostProcess_NO);
    TopoDS_Shape child = unionBuilder.shape();
    
    if (TopExp_Explorer(child, TopAbs_FACE).More()) {
        throw only_wires_allowed();
    }
    
    if (TopExp_Explorer(child, TopAbs_SOLID).More()) {
        throw only_wires_allowed();
    }
    
    vector<TopoDS_Wire> wires;
    for (TopExp_Explorer wireIt(child, TopAbs_WIRE); wireIt.More(); wireIt.Next()) { 
        wires.push_back(TopoDS::Wire(wireIt.Current()));
    }
    
    vector<TopoDS_Wire> sorted_wires;
    sorted_wires.push_back(*wires.begin());
    wires.erase(wires.begin());
    
    
    bool found = true;
    while((wires.size() > 0) && found) {
        gp_Pnt next_start = get_start_and_end(sorted_wires.back(), child).second;
        
        found = false;
        for (vector<TopoDS_Wire>::iterator it = wires.begin(); it < wires.end(); ++it) {
            
            pair<gp_Pnt, gp_Pnt> start_and_end = get_start_and_end(*it, child);
            if (start_and_end.first.IsEqual(next_start, 0.001)) {
                sorted_wires.push_back(*it);
                wires.erase(it);
                found = true;
                break;
            } else if (start_and_end.second.IsEqual(next_start, 0.001)) {
                sorted_wires.push_back(TopoDS::Wire((*it).Reversed()));
                wires.erase(it);
                found = true;
                break;
            }
        }
    }
    
    if (wires.size() > 0) {
        throw wires_not_a_loop();
    }

    BRepBuilderAPI_MakeWire face_wire;
    for(vector<TopoDS_Wire>::iterator it = sorted_wires.begin(); it < sorted_wires.end(); ++it) {
        face_wire.Add(*it);
    }
    
    try {
        shape_ = BRepBuilderAPI_MakeFace(face_wire);
    } catch (...) {
        throw could_not_make_face();
    }
    
    PostProcess(json);
}

SolidBuilder::SolidBuilder(map< string, mValue > json, vector<TopoDS_Shape>& shapes) {
    
    UnionBuilder unionBuilder(json, shapes, PostProcess_NO);
    TopoDS_Shape child = unionBuilder.shape();
    
    if (TopExp_Explorer(child, TopAbs_SOLID).More()) {
        throw only_faces_allowed();
    }
    
    try {
        BRep_Builder aBShell;
        TopoDS_Shell shellF;
        aBShell.MakeShell(shellF);
        for (TopExp_Explorer faceIt(child, TopAbs_FACE); faceIt.More(); faceIt.Next()) { 
            aBShell.Add(shellF, TopoDS::Face(faceIt.Current()));
        }
                
        ShapeFix_Solid shape_fix;
        shape_ = shape_fix.SolidFromShell(shellF);
        
        PostProcess(json);
    } catch (...) {
        throw could_not_make_solid();
    }
    

}

LoftBuilder::LoftBuilder(map< string, mValue > json, vector<TopoDS_Shape>& children) {
    
    BRepOffsetAPI_ThruSections loft_tool(Standard_True);

    
    for(vector<TopoDS_Shape>::iterator it = children.begin(); it < children.end(); ++it) {
        TopoDS_Shape child = *it;

        // No loose wires or solids allowed
        if (TopExp_Explorer(child, TopAbs_WIRE, TopAbs_FACE).More()) {
            //throw only_wires_allowed();
        }
        if (TopExp_Explorer(child, TopAbs_SOLID).More()) {
            //throw only_wires_allowed();
        }
        
        
        TopExp_Explorer wireIt(child, TopAbs_WIRE); 
        loft_tool.AddWire(TopoDS::Wire(wireIt.Current()));
        
        wireIt.Next();
        if (wireIt.More()) {
            throw only_one_wire_per_shape_allowed();
        }
    }
    
    loft_tool.CheckCompatibility(Standard_True);

        
    try {
        shape_ = loft_tool.Shape();
    } catch (...) {
        throw could_not_make_loft();
    }
    
    PostProcess(json);
}

RevolveBuilder::RevolveBuilder(map< string, mValue > json, TopoDS_Shape shape) {
    
    if (TopExp_Explorer(shape, TopAbs_SOLID).More()) {
        throw no_solids_allowed();
    }
    
    map< string, mValue > origin = json["origin"].get_obj();
    map< string, mValue > parameters = json["parameters"].get_obj();
    
    double x = Util::to_d(origin["x"]);
    double y = Util::to_d(origin["y"]);
    double z = Util::to_d(origin["z"]);
    
    double u = Util::to_d(parameters["u"]);
    double v = Util::to_d(parameters["v"]);
    double w = Util::to_d(parameters["w"]);
    
    double angle = Util::to_d(parameters["angle"]);
    
    gp_Ax1 axis(gp_Pnt(x,y,z),gp_Dir(u,v,w));
    shape_ = BRepPrimAPI_MakeRevol(shape,axis,angle/180*M_PI);
    PostProcess(json);
}

FilletBuilder::FilletBuilder(map< string, mValue > json, TopoDS_Shape shape) {
    
    BRepFilletAPI_MakeFillet mkFillet(shape);

    try {
            
        map< string, mValue > parameters = json["parameters"].get_obj();
        
        double r = Util::to_d(parameters["r"]);
        
        for (TopExp_Explorer edgeIt(shape, TopAbs_EDGE); edgeIt.More(); edgeIt.Next()) { 
            mkFillet.Add(r, TopoDS::Edge(edgeIt.Current()));
        }
        
    } catch (...) {
        throw non_fillitable_edge();
    }
    
    try {
        shape_ = mkFillet.Shape();
        PostProcess(json);

    } catch (...) {
        throw could_not_fillet();
    }
}


