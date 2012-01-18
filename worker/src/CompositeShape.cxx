#include "CompositeShape.h"
#include "Tesselate.h"

CompositeShape::CompositeShape() {
    
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
