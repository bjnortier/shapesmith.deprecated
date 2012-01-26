#ifndef SS_COMPOSITE_SHAPE
#define SS_COMPOSITE_SHAPE

#include <json_spirit.h>

#include "OCC.h"

using namespace json_spirit;

class CompositeShape {
public:
    CompositeShape();
    CompositeShape(const CompositeShape& other) {
        three_d_shape_ = other.three_d_shape_;
        two_d_shape_ = other.two_d_shape_;
        one_d_shape_ = other.one_d_shape_;
    }
    
    void set_three_d_shape(TopoDS_Shape shape);
    void set_two_d_shape(TopoDS_Shape shape);   
    void set_one_d_shape(TopoDS_Shape shape);
    
    TopoDS_Shape three_d_shape();
    TopoDS_Shape two_d_shape();
    TopoDS_Shape one_d_shape();
    
    mValue Tesselate();


private:
    TopoDS_Shape three_d_shape_;
    TopoDS_Shape two_d_shape_;
    TopoDS_Shape one_d_shape_;
    
};


#endif
