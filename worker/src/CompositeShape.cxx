#include "CompositeShape.h"
#include "Tesselate.h"

CompositeShape::CompositeShape() {
    
}

void CompositeShape::set_three_d_shape(TopoDS_Shape shape) {
    three_d_shape_ = shape;
}
void CompositeShape::set_two_d_shape(TopoDS_Shape shape) {
    two_d_shape_ = shape;
}
void CompositeShape::set_one_d_shape(TopoDS_Shape shape) {
    one_d_shape_ = shape;
}

TopoDS_Shape CompositeShape::three_d_shape() {
    return three_d_shape_;
}
TopoDS_Shape CompositeShape::two_d_shape() {
    return two_d_shape_;
}
TopoDS_Shape CompositeShape::one_d_shape() {
    return one_d_shape_;
}


mValue CompositeShape::Tesselate() {
    std::auto_ptr<Tesselator3D> tesselator3D(new Tesselator3D(three_d_shape_));
    std::auto_ptr<Tesselator3D> tesselator2D(new Tesselator3D(two_d_shape_));
    std::auto_ptr<Tesselator1D> tesselator1D(new Tesselator1D(one_d_shape_));
    
    mObject result;
    result["3d"] = tesselator3D->Tesselate();
    result["2d"] = tesselator2D->Tesselate();
    result["1d"] = tesselator1D->Tesselate();
    return result;
    
}
