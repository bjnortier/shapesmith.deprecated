#include <math.h>
#include <iostream>
#include <vector>

#include "OCC.h"

class Line {
    TopoDS_Shape shape;

public:
    Line(gp_Pnt from, gp_Pnt to) {
	shape = BRepBuilderAPI_MakeEdge(from, to).Edge();
    }

    TopoDS_Shape getShape() {
	return shape;
    }

    int mesh() {
	BRepMesh().Mesh(shape, 1.0);
	
	int count = 0;
	TopExp_Explorer Ex;
	for (Ex.Init(shape,TopAbs_EDGE); Ex.More(); Ex.Next()) { 
	    //TopoDS_Face Face = TopoDS::Face(Ex.Current());
	    ++count;
	}
	return count;
    }
};

class Ellipse {
    TopoDS_Edge shape;
    
public:
    Ellipse(double majorRadius, double minorRadius) {
	gp_Elips ellipse = gp_Elips(gp_Ax2(gp_Pnt(0,0,0),gp_Dir(0,0,1)), majorRadius, minorRadius);
	shape = BRepBuilderAPI_MakeEdge(ellipse, 0, M_PI/2).Edge();
    }

    TopoDS_Shape getShape() {
	return shape;
    }

    std::vector<gp_Pnt> mesh() {

	BRepAdaptor_Curve curve_adaptor(shape);
	GCPnts_UniformDeflection discretizer;
	discretizer.Initialize(curve_adaptor, 0.001);

	std::vector<gp_Pnt> vertices;
	for (int i = 0; i < discretizer.NbPoints(); i++) {
	    gp_Pnt pt = curve_adaptor.Value(discretizer.Parameter(i + 1));
	    vertices.push_back(pt);
	}
	return vertices;
    }
};

class SphericalFace {
    TopoDS_Shape shape;

public:
    SphericalFace() {
	gp_Sphere sphere = gp_Sphere(gp_Ax3(gp_Pnt(0,0,0),gp_Dir(1,0,0)),150);
	shape = BRepBuilderAPI_MakeFace(sphere,0.1,0.7,0.2,0.9);
    }

    int numTriangles() {

	BRepMesh().Mesh(shape, 1.0);
	
	TopExp_Explorer Ex; 
	int index_offset = 0;
	for (Ex.Init(shape,TopAbs_FACE); Ex.More(); Ex.Next()) { 
        
	    TopoDS_Face Face = TopoDS::Face(Ex.Current());
	    TopLoc_Location Location = TopLoc_Location();
	    Handle(Poly_Triangulation) facing = BRep_Tool().Triangulation(Face,Location);
        
	    TColgp_Array1OfDir the_normal(facing->Nodes().Lower(), facing->Nodes().Upper());
	    Poly_Connect connect(facing);
	    StdPrs_ToolShadedShape().Normal(Face, connect, the_normal);
        
	    for (int i = 1; i <= facing->NbNodes(); ++i) {
	    	gp_Pnt vertex = facing->Nodes().Value(i);
	    	gp_Pnt transformedVtx = vertex.Transformed(Face.Location().Transformation());
	    }
        
	    for (int i = 1; i <= facing->NbTriangles(); ++i) {
	    	Poly_Triangle triangle = facing->Triangles().Value(i);
	    	Standard_Integer index1, index2, index3;
	    	triangle.Get(index1, index2, index3);
            
	    	// Step 1 - caluclate the normals of the triangles
	    	gp_Pnt vertex1 = facing->Nodes().Value(index1);
	    	gp_Pnt vertex2 = facing->Nodes().Value(index2);
	    	gp_Pnt vertex3 = facing->Nodes().Value(index3);
	    }
        
	    index_offset += facing->NbNodes();    
	}

	return  index_offset;
    } 
};


