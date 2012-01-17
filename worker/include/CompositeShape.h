#ifndef SS_COMPOSITE_SHAPE
#define SS_COMPOSITE_SHAPE

#include "OCC.h"

class CompositeShape {
public:
    CompositeShape();
    CompositeShape(const CompositeShape& other) {
        three_d_shape_ = other.three_d_shape_;
        two_d_shape_ = other.two_d_shape_;
        one_d_shape_ = other.one_d_shape_;

    }
    void set_three_d_shape(TopoDS_Shape shape) {
        three_d_shape_ = shape;
    }
    void set_two_d_shape(TopoDS_Shape shape) {
        two_d_shape_ = shape;
    }
    void set_one_d_shape(TopoDS_Shape shape) {
        one_d_shape_ = shape;
    }


private:
    TopoDS_Shape three_d_shape_;
    TopoDS_Shape two_d_shape_;
    TopoDS_Shape one_d_shape_;
    
};


#endif
