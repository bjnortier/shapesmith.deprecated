#ifndef SS_GEOMETRY
#define SS_GEOMETRY

#include <json_spirit.h>
#include "OCC.h"

using namespace std;
using namespace json_spirit;

class CompositeShape {
};

class Shape {
private:
    void ApplyOrigin(map< string, mValue > json);
    void ApplyTransform(map< string, mValue > json);
    void ApplyTransforms(map< string, mValue > json);
    virtual void Mesh() = 0;
    
protected:
    TopoDS_Shape shape_;
    void ToOriginTransformAndMesh(map< string, mValue > json);
    
public:
    TopoDS_Shape shape();
    virtual mValue Tesselate() = 0;

    
};

#pragma mark 3D primitives

class Shape3D : public Shape {
protected:
    virtual void Mesh();
    virtual mValue Tesselate();
public:
    Shape3D() {};
    
};

class Cuboid : public Shape3D {

public:
    Cuboid(map< string, mValue > json);
    ~Cuboid();
};

class Sphere : public Shape3D {
public:
    Sphere(map< string, mValue > json);
    ~Sphere();
};

class Cylinder : public Shape3D {
public:
    Cylinder(map< string, mValue > json);
    ~Cylinder();
};

class Cone : public Shape3D {
public:
    Cone(map< string, mValue > json);
    ~Cone();
};

class Wedge : public Shape3D {
public:
    Wedge(map< string, mValue > json);
    ~Wedge();
};

class Torus : public Shape3D {
public:
    Torus(map< string, mValue > json);
    ~Torus();
};

#pragma mark 1D Primitives

class Shape1D : public Shape {
protected:
    virtual void Mesh();
    virtual mValue Tesselate();
};

class Ellipse1D : public Shape1D {
public:
    Ellipse1D(map< string, mValue > json);
    ~Ellipse1D();
};

#pragma mark Booleans

typedef TopoDS_Shape (*boolean_op)(const TopoDS_Shape&, const TopoDS_Shape&);

class Boolean : public Shape3D {
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

#endif
