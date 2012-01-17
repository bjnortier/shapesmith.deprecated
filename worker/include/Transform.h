#include <json_spirit.h>
#include "OCC.h"
#include "Util.h"

using namespace std;
using namespace json_spirit;

template <class T>
class Transformer {
    
public:
    Transformer(TopoDS_Shape shape, 
			    map< string, mValue > origin,
                map< string, mValue > parameters) {
        transformed_shape_ = apply<T>(shape, origin, parameters);
    }
    
    TopoDS_Shape transformed_shape() {
        return transformed_shape_;
    }
    
private: 
    TopoDS_Shape transformed_shape_;
    
    template<typename U>
    static TopoDS_Shape apply(TopoDS_Shape shape, 
                              map< string, mValue > origin,
                              map< string, mValue > parameters) {
        int n = parameters["n"].is_null() ? 0 : parameters["n"].get_int();
        
        if(n == 0) {
            auto_ptr<U> transform(new T(shape));
            return transform->apply(1.0, origin, parameters);
        }
        
        vector<TopoDS_Shape> copies(n+1); 
        copies[0] = shape;
        
        int remaining = n;
        int grouping = 1;
        float multiplier = 1.0;
        int index = 1;
        
        while (remaining > 0) {
            
            int group_index = (int)(log(grouping)/log(2));
            TopoDS_Shape obj_to_copy = copies[group_index];
            
            auto_ptr<T> transform(new T(obj_to_copy));
            TopoDS_Shape transformedShape = transform->apply(multiplier, origin, parameters);

            copies[index] = BRepAlgoAPI_Fuse(transformedShape,
                                             copies[index - 1]).Shape();
            
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
    TopoDS_Shape shape_;
public:
    Transform(TopoDS_Shape shape) {
        shape_ = shape;
    }
    virtual ~Transform() {};

    virtual TopoDS_Shape apply(double multiplier, 
                               map< string, mValue > origin, 
                               map< string, mValue > parameters) = 0;
};

class Rotate : public Transform {
    
public:
    Rotate(TopoDS_Shape shape) : Transform(shape) {}
    virtual ~Rotate() {};
     
    virtual TopoDS_Shape apply(double multiplier, 
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
        BRepBuilderAPI_Transform brep_transform(shape_, transformation);
        return brep_transform.Shape();
    }
};

class Scale : public Transform {
    
public:
    Scale(TopoDS_Shape shape) : Transform(shape) {}
    virtual ~Scale() {};
    
    virtual TopoDS_Shape apply(double multiplier, 
                               map< string, mValue > origin, 
                               map< string, mValue > parameters) {
        
        double x = Util::to_d(origin["x"]);
        double y = Util::to_d(origin["y"]);
        double z = Util::to_d(origin["z"]);
        double factor = Util::to_d(parameters["factor"]);
        
        gp_Trsf transformation = gp_Trsf();
        transformation.SetScale(gp_Pnt(x, y, z), factor);
        BRepBuilderAPI_Transform brep_transform(shape_, transformation);
        return brep_transform.Shape();
    }
};

class Mirror : public Transform {
    
public:
    Mirror(TopoDS_Shape shape) : Transform(shape) {}
    virtual ~Mirror() {};
    
    virtual TopoDS_Shape apply(double multiplier, 
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
        BRepBuilderAPI_Transform brep_transform(shape_, transformation);
        return brep_transform.Shape();
    }
};

class Translate : public Transform {
    
public:
    Translate(TopoDS_Shape shape) : Transform(shape) {}
    virtual ~Translate() {};
    
    virtual TopoDS_Shape apply(double multiplier, 
                               map< string, mValue > origin, 
                               map< string, mValue > parameters) {
        
        double u = Util::to_d(parameters["u"]);
        double v = Util::to_d(parameters["v"]);
        double w = Util::to_d(parameters["w"]);

        gp_Trsf transformation = gp_Trsf();
        transformation.SetTranslation(gp_Vec(multiplier*u, 
                                             multiplier*v, 
                                             multiplier*w));
        
        BRepBuilderAPI_Transform brep_transform(shape_, transformation);
        return brep_transform.Shape();
    }
};


