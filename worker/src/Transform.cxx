#include <json_spirit.h>
#include "OCC.h"

#include "Transform.h"
#include "Util.h"

using namespace std;
using namespace json_spirit;


Transform::Transform(CompositeShape composite_shape) {
    composite_shape_ = composite_shape;
}
    
CompositeShape Rotate::apply(double multiplier, 
                             map< string, mValue > origin, 
                            map< string, mValue > parameters) {
    
    double x = Util::to_d(origin["x"]);
    double y = Util::to_d(origin["y"]);
    double z = Util::to_d(origin["z"]);
    double u = Util::to_d(parameters["u"]);
    double v = Util::to_d(parameters["v"]);
    double w = Util::to_d(parameters["w"]);
    double angle = Util::to_d(parameters["angle"]);
    
    gp_Trsf transformation = gp_Trsf();
    transformation.SetRotation(gp_Ax1(gp_Pnt(x,y,z), gp_Dir(u,v,w)),
                               multiplier*Util::to_d(angle)/180*M_PI);
    
    composite_shape_.set_three_d_shape(                                          
        BRepBuilderAPI_Transform(composite_shape_.three_d_shape(), transformation).Shape());
    composite_shape_.set_two_d_shape(                                          
        BRepBuilderAPI_Transform(composite_shape_.two_d_shape(), transformation).Shape());
    composite_shape_.set_one_d_shape(
        BRepBuilderAPI_Transform(composite_shape_.one_d_shape(), transformation).Shape());
    return composite_shape_;
}

CompositeShape Scale::apply(double multiplier, 
                          map< string, mValue > origin, 
                          map< string, mValue > parameters) {
    
    double x = Util::to_d(origin["x"]);
    double y = Util::to_d(origin["y"]);
    double z = Util::to_d(origin["z"]);
    double factor = Util::to_d(parameters["factor"]);
    
    gp_Trsf transformation = gp_Trsf();
    transformation.SetScale(gp_Pnt(x, y, z), factor);
    
    composite_shape_.set_three_d_shape(BRepBuilderAPI_Transform(composite_shape_.three_d_shape(), transformation).Shape());
    composite_shape_.set_two_d_shape(BRepBuilderAPI_Transform(composite_shape_.two_d_shape(), transformation).Shape());
    composite_shape_.set_one_d_shape(BRepBuilderAPI_Transform(composite_shape_.one_d_shape(), transformation).Shape());
    return composite_shape_;

}

CompositeShape Mirror::apply(double multiplier, 
                           map< string, mValue > origin, 
                           map< string, mValue > parameters) {
    
    double x = Util::to_d(origin["x"]);
    double y = Util::to_d(origin["y"]);
    double z = Util::to_d(origin["z"]);
    double u = Util::to_d(parameters["u"]);
    double v = Util::to_d(parameters["v"]);
    double w = Util::to_d(parameters["w"]);
    
    gp_Trsf transformation = gp_Trsf();
    transformation.SetMirror(gp_Ax1(gp_Pnt(x, y, z), gp_Dir(u, v, w)));
    
    composite_shape_.set_three_d_shape(BRepBuilderAPI_Transform(composite_shape_.three_d_shape(), transformation).Shape());
    composite_shape_.set_two_d_shape(BRepBuilderAPI_Transform(composite_shape_.two_d_shape(), transformation).Shape());
    composite_shape_.set_one_d_shape(BRepBuilderAPI_Transform(composite_shape_.one_d_shape(), transformation).Shape());
    return composite_shape_;
}

CompositeShape Translate::apply(double multiplier, 
                              map< string, mValue > origin, 
                              map< string, mValue > parameters) {
    
    double u = Util::to_d(parameters["u"]);
    double v = Util::to_d(parameters["v"]);
    double w = Util::to_d(parameters["w"]);
    
    gp_Trsf transformation = gp_Trsf();
    transformation.SetTranslation(gp_Vec(multiplier*u, 
                                         multiplier*v, 
                                         multiplier*w));
    
    composite_shape_.set_three_d_shape(BRepBuilderAPI_Transform(composite_shape_.three_d_shape(), transformation).Shape());
    composite_shape_.set_two_d_shape(BRepBuilderAPI_Transform(composite_shape_.two_d_shape(), transformation).Shape());
    composite_shape_.set_one_d_shape(BRepBuilderAPI_Transform(composite_shape_.one_d_shape(), transformation).Shape());
    return composite_shape_;

}

