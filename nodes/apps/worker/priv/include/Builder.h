#ifndef SS_BUILDER
#define SS_BUILDER

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_STROKER_H


#include <json_spirit.h>
#include "OCC.h"

using namespace std;
using namespace json_spirit;

#define TRIANGLE_SIZE 1.0

class Builder {
public:
    Builder() {};
    virtual ~Builder() {};
    
protected:
    TopoDS_Shape shape_;
    
    void ApplyOrigin(map< string, mValue > json);
    void ApplyTransform(map< string, mValue > json);
    void ApplyTransforms(map< string, mValue > json);
    virtual void PostProcess(map< string, mValue > json) = 0;
    
    
    
public:
    TopoDS_Shape shape();
    
};

#pragma mark 3D primitives

class Builder3D : public Builder {
private:
    void Mesh();
protected:
    virtual void PostProcess(map< string, mValue > json);
    virtual ~Builder3D() {};
public:
    Builder3D() {};
    
};

class CuboidBuilder : public Builder3D {

public:
    CuboidBuilder(map< string, mValue > json);
    virtual ~CuboidBuilder() {};
};

class SphereBuilder : public Builder3D {
public:
    SphereBuilder(map< string, mValue > json);
    virtual ~SphereBuilder() {};
};

class CylinderBuilder : public Builder3D {
public:
    CylinderBuilder(map< string, mValue > json);
    virtual ~CylinderBuilder() {};
};

class ConeBuilder : public Builder3D {
public:
    ConeBuilder(map< string, mValue > json);
    virtual ~ConeBuilder() {};
};

class WedgeBuilder : public Builder3D {
public:
    WedgeBuilder(map< string, mValue > json);
    virtual ~WedgeBuilder() {};
};

class TorusBuilder : public Builder3D {
public:
    TorusBuilder(map< string, mValue > json);
    virtual ~TorusBuilder() {};
};

class STLImportBuilder : public Builder3D {
public:
    STLImportBuilder(string id, map< string, mValue > json);
    virtual ~STLImportBuilder() {};
};

#pragma mark 2D Primitives

class Builder2D : public Builder {
private:
    void Mesh();
protected:
    virtual void PostProcess(map< string, mValue > json);
    virtual ~Builder2D() {};
};

class Ellipse2DBuilder : public Builder2D {
public:
    Ellipse2DBuilder(map< string, mValue > json);
    virtual ~Ellipse2DBuilder() {};
};

class Rectangle2DBuilder : public Builder2D {
public:
    Rectangle2DBuilder(map< string, mValue > json);
    virtual ~Rectangle2DBuilder() {};
};

class Triangle2DBuilder : public Builder2D {
public:
    Triangle2DBuilder(map< string, mValue > json);
    virtual ~Triangle2DBuilder() {};
};

class Text2DBuilder : public Builder2D {
public:
    Text2DBuilder(map< string, mValue > json);
    virtual ~Text2DBuilder() {};
};

#pragma mark 1D Primitives

class Builder1D : public Builder {
protected:
    virtual void PostProcess(map< string, mValue > json);
    virtual ~Builder1D() {};
};

class Ellipse1DBuilder : public Builder1D {
public:
    Ellipse1DBuilder(map< string, mValue > json);
    virtual ~Ellipse1DBuilder() {};
};

class Bezier1DBuilder : public Builder1D {
public:
    Bezier1DBuilder(map< string, mValue > json);
    virtual ~Bezier1DBuilder() {};
};

class PolylineBuilder : public Builder1D {
public:
    PolylineBuilder(map< string, mValue > json);
    virtual ~PolylineBuilder() {};
};



#pragma mark Booleans

class BuilderND : public Builder {
private:
    void Mesh();
protected:
    virtual void PostProcess(map< string, mValue > json);
public:
    BuilderND() {};
    virtual ~BuilderND() {};
};

typedef TopoDS_Shape (*boolean_op)(const TopoDS_Shape&, const TopoDS_Shape&);

typedef enum e_PostProcess {
    PostProcess_YES,
    PostProcess_NO
} PostProcess;

class BooleanBuilder : public BuilderND {
public:
    BooleanBuilder(map< string, mValue > json, vector<TopoDS_Shape>& shapes, boolean_op fn, e_PostProcess post_process);
};

class UnionBuilder : public BooleanBuilder {
public:
    UnionBuilder(map< string, mValue > json, vector<TopoDS_Shape>& shapes, e_PostProcess post_process = PostProcess_YES);
    virtual ~UnionBuilder() {};
};

class SubtractBuilder : public BooleanBuilder {
public:
    SubtractBuilder(map< string, mValue > json, vector<TopoDS_Shape>& shapes, e_PostProcess post_process = PostProcess_YES);
    virtual ~SubtractBuilder() {};
};

class IntersectBuilder : public BooleanBuilder {
public:
    IntersectBuilder(map< string, mValue > json, vector<TopoDS_Shape>& shapes, e_PostProcess post_process = PostProcess_YES);
    virtual ~IntersectBuilder() {};
};

#pragma mark Modifiers

struct could_not_make_prism : std::exception { char const* what() const throw() { return "Sorry, the shape was too complicated to make a prism.";}  };


class PrismBuilder : public BuilderND {
public:
    PrismBuilder(map< string, mValue > json, TopoDS_Shape shape);
    ~PrismBuilder() {};
};

struct no_solids_allowed : std::exception { char const* what() const throw() { return "No solids allowed";}  };

class RevolveBuilder : public BuilderND {
public:
    RevolveBuilder(map< string, mValue > json, TopoDS_Shape shape);
    ~RevolveBuilder() {};
};

struct only_wires_allowed : std::exception { char const* what() const throw() { return "Only wire shapes allowed"; } };
struct wires_not_a_loop : std::exception { char const* what() const throw() { return "Wires are not a loop";}  };
struct could_not_make_face : std::exception { char const* what() const throw() { return "Could not make a face. Shapes not on a flat surface?";}  };

class FaceBuilder : public BuilderND {
public:
    FaceBuilder(map< string, mValue > json, vector<TopoDS_Shape>& shapes);
    ~FaceBuilder() {};
};

struct only_faces_allowed : std::exception { char const* what() const throw() { return "Only 2D shapes allowed"; } };
struct could_not_make_solid : std::exception { char const* what() const throw() { return "Could not make a solid. Are the faces connected?";}  };


class SolidBuilder : public BuilderND {
public:
    SolidBuilder(map< string, mValue > json, vector<TopoDS_Shape>& shapes);
    ~SolidBuilder() {};
};

struct could_not_make_loft : std::exception { 
    char const* what() const throw() { 
        return "Could not make a loft. Do the wires have the same number of edges?";
    }  
};
struct only_one_wire_per_shape_allowed : std::exception { 
    char const* what() const throw() { 
        return "Could not make a loft. Only one wire per shape allowed.";
    }  
};


class LoftBuilder : public BuilderND {
public:
    LoftBuilder(map< string, mValue > json, vector<TopoDS_Shape>& shapes);
    ~LoftBuilder() {};
};

struct could_not_fillet : std::exception { char const* what() const throw() { return "Could not fillet shape.";}  };
struct non_fillitable_edge : std::exception { char const* what() const throw() { return "The shape contains an edge that cannot be filleted.";}  };

class FilletBuilder : public BuilderND {
public:
    FilletBuilder(map< string, mValue > json, TopoDS_Shape shape);
    ~FilletBuilder() {};
};



    


#endif
