#include "Builder.h"
#include "CompositeShape.h"   
#include "Tesselate.h"
#include "Transform.h"
#include "Util.h"


CompositeShape Builder::composite_shape() {
    return composite_shape_;
}

void Builder::ApplyOrigin(map< string, mValue > json) {
    if (!json["origin"].is_null()) {
        map< string, mValue > origin = json["origin"].get_obj();
        double x = Util::to_d(origin["x"]);
        double y = Util::to_d(origin["y"]);
        double z = Util::to_d(origin["z"]);
        gp_Trsf transformation = gp_Trsf();
        transformation.SetTranslation(gp_Vec(x,y,z));
        
        composite_shape_.set_three_d_shape(                                          BRepBuilderAPI_Transform(composite_shape_.three_d_shape(), transformation).Shape());
        composite_shape_.set_two_d_shape(                                          
            BRepBuilderAPI_Transform(composite_shape_.two_d_shape(), transformation).Shape());
        composite_shape_.set_one_d_shape(
            BRepBuilderAPI_Transform(composite_shape_.one_d_shape(), transformation).Shape());
    }
}

void Builder::ApplyTransform(map< string, mValue > transformJson) {
    string transformType = transformJson["type"].get_str();
    map< string, mValue > origin = transformJson["origin"].get_obj();
    map< string, mValue > parameters = transformJson["parameters"].get_obj();
    
    if (transformType == "rotate") {
        auto_ptr< Transformer<Rotate> > transformer(new Transformer<Rotate>(composite_shape_, origin, parameters));
        composite_shape_ = transformer->transformed_shape();
        return;
    } else if (transformType == "scale") {
        auto_ptr< Transformer<Scale> > transformer(new Transformer<Scale>(composite_shape_, origin, parameters));
        composite_shape_ = transformer->transformed_shape();
        return;
    } else if (transformType == "translate") {
        auto_ptr< Transformer<Translate> > transformer(new Transformer<Translate>(composite_shape_, origin, parameters));
        composite_shape_ = transformer->transformed_shape();
        return;
    } else if (transformType == "mirror") {
        auto_ptr< Transformer<Mirror> > transformer(new Transformer<Mirror>(composite_shape_, origin, parameters));
        composite_shape_ = transformer->transformed_shape();
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
    for (Ex.Init(composite_shape_.three_d_shape(), TopAbs_FACE); Ex.More(); Ex.Next()) { 
        ++numFaces;
    }
    
    if (numFaces > 0) {
        BRepMesh().Mesh(composite_shape_.three_d_shape(), 1.0);
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
	 
    composite_shape_.set_three_d_shape(BRepPrimAPI_MakeBox(width, depth, height).Shape());
    PostProcess(json);
}

SphereBuilder::SphereBuilder(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    double radius = Util::to_d(parameters["r"]);
    composite_shape_.set_three_d_shape(BRepPrimAPI_MakeSphere(radius).Shape());
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
    
    composite_shape_.set_three_d_shape(BRepPrimAPI_MakeCylinder(r, h).Shape());
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
        
    composite_shape_.set_three_d_shape(BRepPrimAPI_MakeCone(r1, r2, h).Shape());
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
    composite_shape_.set_three_d_shape(BRepPrimAPI_MakeWedge(u1, v, w, u2).Shape());                                         
    PostProcess(json);
}


TorusBuilder::TorusBuilder(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    double r1 = Util::to_d(parameters["r1"]);
    double r2 = Util::to_d(parameters["r2"]);
    composite_shape_.set_three_d_shape(BRepPrimAPI_MakeTorus(r1, r2).Shape());
    PostProcess(json);
}

#pragma mark 2D Builders

void Builder2D::Mesh() {
    TopExp_Explorer Ex; 
    int numFaces = 0;
    for (Ex.Init(composite_shape_.two_d_shape(), TopAbs_FACE); Ex.More(); Ex.Next()) { 
        ++numFaces;
    }
    
    if (numFaces > 0) {
        BRepMesh().Mesh(composite_shape_.two_d_shape(), 1.0);
    }
}

void Builder2D::PostProcess(map< string, mValue > json) {
    this->ApplyOrigin(json);

    this->ApplyTransforms(json);
    this->Mesh();
}

Ellipse2DBuilder::Ellipse2DBuilder(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    double r1 = Util::to_d(parameters["r1"]);
    double r2 = Util::to_d(parameters["r2"]);
    
    gp_Elips ellipse = gp_Elips(gp_Ax2(gp_Pnt(0,0,0),gp_Dir(0,0,1)), r1, r2);
    TopoDS_Edge edge = BRepBuilderAPI_MakeEdge(ellipse, 0, M_PI*2).Edge();
    TopoDS_Wire wire = BRepBuilderAPI_MakeWire(edge);
    TopoDS_Face face = BRepBuilderAPI_MakeFace(wire);
    composite_shape_.set_two_d_shape(face);
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
    
    gp_Elips ellipse = gp_Elips(gp_Ax2(gp_Pnt(0,0,0),gp_Dir(0,0,1)), r1, r2);
	composite_shape_.set_one_d_shape(BRepBuilderAPI_MakeEdge(ellipse, 0, M_PI*2).Edge());
    PostProcess(json);
}


#pragma mark Boolean builders

void BuilderND::Mesh() {
    TopExp_Explorer ex3D, ex2D;
    ex3D.Init(composite_shape_.three_d_shape(), TopAbs_FACE);
    ex2D.Init(composite_shape_.two_d_shape(), TopAbs_FACE);
    
    if (ex3D.More()) {
        BRepMesh().Mesh(composite_shape_.three_d_shape(), 1.0);
    }
    if (ex2D.More()) {
        BRepMesh().Mesh(composite_shape_.two_d_shape(), 1.0);
    }

}

void BuilderND::PostProcess(map< string, mValue > json) {
    this->ApplyOrigin(json);
    this->ApplyTransforms(json);
    this->Mesh();
}

TopoDS_Shape robustBoolean(TopoDS_Shape a, TopoDS_Shape b, boolean_op function) {
    if (!a.IsNull() && !b.IsNull()) {
        return function(a,b);
    } else if (a.IsNull()) {
        return b;
    } else {
        return a;
    }
}

BooleanBuilder::BooleanBuilder(map< string, mValue > json, vector<CompositeShape>& shapes, boolean_op function) {
    vector<CompositeShape>::iterator it = shapes.begin();

    composite_shape_.set_three_d_shape((*it).three_d_shape());
    composite_shape_.set_two_d_shape((*it).two_d_shape());
    composite_shape_.set_one_d_shape((*it).one_d_shape());
    ++it;
    
    for ( ; it < shapes.end(); ++it ) {
        composite_shape_.set_three_d_shape(robustBoolean((*it).three_d_shape(), composite_shape_.three_d_shape(), function));
        composite_shape_.set_two_d_shape(robustBoolean((*it).two_d_shape(), composite_shape_.two_d_shape(), function));

        composite_shape_.set_one_d_shape(robustBoolean((*it).one_d_shape(), composite_shape_.one_d_shape(), function));

    }
    PostProcess(json);
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

UnionBuilder::UnionBuilder(map< string, mValue > json, vector<CompositeShape>& shapes) 
    : BooleanBuilder(json, shapes, &fuse) {
}

IntersectBuilder::IntersectBuilder(map< string, mValue > json, vector<CompositeShape>& shapes) 
    : BooleanBuilder(json, shapes, &common) {
}

SubtractBuilder::SubtractBuilder(map< string, mValue > json, vector<CompositeShape>& shapes) 
    : BooleanBuilder(json, shapes, &cut) {
}

#pragma mark Modifier builders

PrismBuilder::PrismBuilder(map< string, mValue > json, CompositeShape shape) {

    map< string, mValue > parameters = json["parameters"].get_obj();
    double u = Util::to_d(parameters["u"]);
    double v = Util::to_d(parameters["v"]);
    double w = Util::to_d(parameters["w"]);
    gp_Vec prismVec(u,v,w);

    TopoDS_Shape empty;
    if (!shape.two_d_shape().IsNull()) {
        composite_shape_.set_three_d_shape(BRepPrimAPI_MakePrism(shape.two_d_shape(), prismVec));
    }
    if (!shape.one_d_shape().IsNull()) {
        composite_shape_.set_two_d_shape(BRepPrimAPI_MakePrism(shape.two_d_shape(), prismVec));
    }
    
    PostProcess(json);
}

