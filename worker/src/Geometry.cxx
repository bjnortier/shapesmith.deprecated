#include "Geometry.h"

double to_d(mValue value) {
    if (value.type() == int_type) {
        return (double)value.get_int();
    } else {
        return value.get_real();
    }
}

Cuboid::Cuboid(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    double width = to_d(parameters["u"]);
    double depth = to_d(parameters["v"]);
    double height = to_d(parameters["w"]);
	 
    map< string, mValue > origin = json["origin"].get_obj();
	 
    if (width < 0) {
	origin["x"] = to_d(origin["x"]) + width;
	width = -width;
    }

    if (depth < 0) {
	origin["y"] = to_d(origin["y"]) + depth;
	depth = -depth;
    }
	 
    if (height < 0) {
	origin["z"] = to_d(origin["z"]) + height;
	height = -height;
    }
    json["origin"] = origin;
	 
    shape_ = BRepPrimAPI_MakeBox(width, depth, height).Shape();
}

Sphere::Sphere(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    double radius = to_d(parameters["r"]);
    shape_ = BRepPrimAPI_MakeSphere(radius).Shape();
}

Cylinder::Cylinder(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    double r = to_d(parameters["r"]);
    double h = to_d(parameters["h"]);
    
    map< string, mValue > origin = json["origin"].get_obj();
    if(h < 0) {
	origin["z"] = to_d(origin["z"]) + h;
	h = -h;
    }
    json["origin"] = origin;
    
    shape_ = BRepPrimAPI_MakeCylinder(r, h).Shape() ;
}

Cone::Cone(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    double r1 = to_d(parameters["r1"]);
    double r2 = to_d(parameters["r2"]);
    double h = to_d(parameters["h"]);

    map< string, mValue > origin = json["origin"].get_obj();
    if(h < 0) {
	origin["z"] = to_d(origin["z"]) + h;
	h = -h;
	swap(r1, r2);
    }
    json["origin"] = origin;
        
    shape_ = BRepPrimAPI_MakeCone(r1, r2, h).Shape();
}

Wedge::Wedge(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    double u1 = to_d(parameters["u1"]);
    double u2 = to_d(parameters["u2"]);
    double v = to_d(parameters["v"]);
    double w = to_d(parameters["w"]);

    map< string, mValue > origin = json["origin"].get_obj();
    if(w < 0) {
	origin["z"] = to_d(origin["z"]) + w;
	w = -w;
    }
    json["origin"] = origin;
    shape_ = BRepPrimAPI_MakeWedge(u1, v, w, u2).Shape();                                                
}

Torus::Torus(map< string, mValue > json) {
    map< string, mValue > parameters = json["parameters"].get_obj();
    double r1 = to_d(parameters["r1"]);
    double r2 = to_d(parameters["r2"]);
    shape_ =BRepPrimAPI_MakeTorus(r1, r2).Shape();
}

Boolean::Boolean(vector<TopoDS_Shape>& shapes, boolean_op function) {
    vector<TopoDS_Shape>::iterator it = shapes.begin();
    shape_ = *it;
    ++it;
    for ( ; it < shapes.end(); ++it ) {
	shape_ = function(*it, shape_);
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

Union::Union(vector<TopoDS_Shape>& shapes) 
    : Boolean(shapes, &fuse) {
}

Intersect::Intersect(vector<TopoDS_Shape>& shapes) 
    : Boolean(shapes, &common) {
}

Subtract::Subtract(vector<TopoDS_Shape>& shapes) 
    : Boolean(shapes, &cut) {
}
