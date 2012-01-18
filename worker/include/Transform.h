#ifndef SS_TRANSFORM
#define SS_TRANSFORM

#include <json_spirit.h>
#include "OCC.h"
#include "CompositeShape.h"

using namespace std;
using namespace json_spirit;

template <class T>
class Transformer {
    
public:
    Transformer(CompositeShape composite_shape, 
                map< string, mValue > origin,
                map< string, mValue > parameters) {
        transformed_shape_ = apply<T>(composite_shape, origin, parameters);
    }
    
    CompositeShape transformed_shape() { 
        return transformed_shape_;
    }

    
private: 
    CompositeShape transformed_shape_;
    
    template<typename U>
    static CompositeShape apply(CompositeShape composite_shape, 
                                map< string, mValue > origin,
                                map< string, mValue > parameters) {
        int n = parameters["n"].is_null() ? 0 : parameters["n"].get_int();
        
        if(n == 0) {
            auto_ptr<U> transform(new T(composite_shape));
            return transform->apply(1.0, origin, parameters);
        }
        
        vector<CompositeShape> copies(n+1); 
        copies[0] = composite_shape;
        
        int remaining = n;
        int grouping = 1;
        float multiplier = 1.0;
        int index = 1;
        
        while (remaining > 0) {
            
            int group_index = (int)(log(grouping)/log(2));
            CompositeShape obj_to_copy = copies[group_index];
            
            auto_ptr<T> transform(new T(obj_to_copy));
            CompositeShape transformedShape = transform->apply(multiplier, origin, parameters);
            
            copies[index].set_three_d_shape(BRepAlgoAPI_Fuse(transformedShape.three_d_shape(), 
                                                             copies[index - 1].three_d_shape()).Shape());
            copies[index].set_two_d_shape(BRepAlgoAPI_Fuse(transformedShape.two_d_shape(), 
                                                           copies[index - 1].two_d_shape()).Shape());
            copies[index].set_one_d_shape(BRepAlgoAPI_Fuse(transformedShape.one_d_shape(), 
                                                           copies[index - 1].one_d_shape()).Shape());
            
            multiplier = multiplier + grouping;
            remaining = remaining - grouping;
            ++index;
            
            if ((grouping * 2) < remaining) {
                grouping = grouping * 2;
            } else if (grouping > remaining) {
                grouping = grouping / 2;
            }
        }
        
        return copies[index - 1];
    }
    
    
};

class Transform {
protected:
    CompositeShape composite_shape_;
public:
    Transform(CompositeShape composite_shape);
    virtual ~Transform() {};

    virtual CompositeShape apply(double multiplier, 
                                 map< string, mValue > origin, 
                                 map< string, mValue > parameters) = 0;
};

class Rotate : public Transform {
    
public:
    Rotate(CompositeShape composite_shape) : Transform(composite_shape) {}
    virtual ~Rotate() {};
    
    virtual CompositeShape apply(double multiplier, 
                                 map< string, mValue > origin, 
                                 map< string, mValue > parameters);
};


class Scale : public Transform {
    
public:
    Scale(CompositeShape composite_shape) : Transform(composite_shape) {}
    virtual ~Scale() {};
    
    virtual CompositeShape apply(double multiplier, 
                                 map< string, mValue > origin, 
                                 map< string, mValue > parameters);
};

class Mirror : public Transform {
    
public:
    Mirror(CompositeShape composite_shape) : Transform(composite_shape) {}
    virtual ~Mirror() {};
    
    virtual CompositeShape apply(double multiplier, 
                                 map< string, mValue > origin, 
                                 map< string, mValue > parameters);
};

class Translate : public Transform {
    
public:
    Translate(CompositeShape composite_shape) : Transform(composite_shape) {}
    virtual ~Translate() {};
    
    virtual CompositeShape apply(double multiplier, 
                                 map< string, mValue > origin, 
                                 map< string, mValue > parameters);
};

#endif

