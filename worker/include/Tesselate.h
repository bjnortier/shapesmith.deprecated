#ifndef SS_TESSELATE
#define SS_TESSELATE

#include <json_spirit.h>

using namespace json_spirit;

class Tesselator {
public:
    Tesselator(TopoDS_Shape shape);
    mValue Tesselate();
private:
    TopoDS_Shape shape_;
};

class Tesselator1D {
public:
    Tesselator1D(TopoDS_Shape shape);
    mValue Tesselate();
private:
    TopoDS_Shape shape_;
};


class Tesselator3D {
public:
    Tesselator3D(TopoDS_Shape shape);
    mValue Tesselate();
private:
    TopoDS_Shape shape_;
};


#endif
