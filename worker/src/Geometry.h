#include <json_spirit.h>
#include "OCC.h"

using namespace std;
using namespace json_spirit;

class Geometry3D {
private:
    void ApplyOrigin(map< string, mValue > json);
    void ApplyTransform(map< string, mValue > json);
    void ApplyTransforms(map< string, mValue > json);

protected:
    TopoDS_Shape shape_;
    void ApplyOriginAndTransforms(map< string, mValue > json);

public:
    TopoDS_Shape shape();
};

class Cuboid : public Geometry3D {

public:
    Cuboid(map< string, mValue > json);
    ~Cuboid();
};

class Sphere : public Geometry3D {
public:
    Sphere(map< string, mValue > json);
    ~Sphere();
};

class Cylinder : public Geometry3D {
public:
    Cylinder(map< string, mValue > json);
    ~Cylinder();
};

class Cone : public Geometry3D {
public:
    Cone(map< string, mValue > json);
    ~Cone();
};

class Wedge : public Geometry3D {
public:
    Wedge(map< string, mValue > json);
    ~Wedge();
};

class Torus : public Geometry3D {
public:
    Torus(map< string, mValue > json);
    ~Torus();
};

typedef TopoDS_Shape (*boolean_op)(const TopoDS_Shape&, const TopoDS_Shape&);

class Boolean : public Geometry3D {
public:
    Boolean(map< string, mValue > json, vector<TopoDS_Shape>& shapes, boolean_op fn);
};

class Union : public Boolean {
public:
    Union(map< string, mValue > json, vector<TopoDS_Shape>& shapes);
    ~Union();
};

class Subtract : public Boolean {
public:
    Subtract(map< string, mValue > json, vector<TopoDS_Shape>& shapes);
    ~Subtract();
};

class Intersect : public Boolean {
public:
    Intersect(map< string, mValue > json, vector<TopoDS_Shape>& shapes);
    ~Intersect();
};

