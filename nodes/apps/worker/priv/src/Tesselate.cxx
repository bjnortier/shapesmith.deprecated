#include <json_spirit.h>

#include "OCC.h"
#include "Tesselate.h"

using namespace json_spirit;
using namespace std;

Tesselator::Tesselator(TopoDS_Shape shape)  {
    shape_ = shape;
}

mValue Tesselator::Tesselate() {
    std::auto_ptr<Tesselator3D> tesselator3D(new Tesselator3D(shape_));
    std::auto_ptr<Tesselator3D> tesselator2D(new Tesselator3D(shape_));
    std::auto_ptr<Tesselator1D> tesselator1D(new Tesselator1D(shape_));
    
    mObject result;
    result["faces"] = tesselator3D->Tesselate();
    result["edges"] = tesselator1D->Tesselate();
    return result;
}


Tesselator1D::Tesselator1D(TopoDS_Shape shape)  {
    shape_ = shape;
}

mValue Tesselator1D::Tesselate() {
    
    mArray positions;
    mArray segments;
    TopExp_Explorer Ex;
    
    int edgeCount = 0;
    for (TopExp_Explorer ex(shape_,TopAbs_EDGE); ex.More(); ex.Next()) { 
        edgeCount += 1;
    }
    
    bool hasFaces = TopExp_Explorer(shape_,TopAbs_FACE).More();
    
    // Don't tesselate the edges of 3D objects with a lot of edges, e.g. STL imports
    if (!(hasFaces && (edgeCount > 50))) {
        int index = 0;
        for (Ex.Init(shape_,TopAbs_EDGE); Ex.More(); Ex.Next()) { 
            
            TopoDS_Edge edge = TopoDS::Edge(Ex.Current());
            
            BRepAdaptor_Curve curve_adaptor(edge);
            GCPnts_UniformDeflection discretizer;
            discretizer.Initialize(curve_adaptor, 0.05);
            
            for (int i = 0; i < discretizer.NbPoints(); i++) {
                gp_Pnt pt = curve_adaptor.Value(discretizer.Parameter(i + 1));
                positions.push_back(pt.X());
                positions.push_back(pt.Y());
                positions.push_back(pt.Z());
            }
            int newIndex = index + discretizer.NbPoints()*3;
            
            mObject segment;
            segment["start"] = index;
            segment["end"]   = newIndex - 1;
            segments.push_back(segment);
            
            index = newIndex;
        }
    }
    
    mObject result;
    result["primitive"] = "polyline";
    result["positions"] = positions;
    result["segments"]  = segments;
    return result;
}

Tesselator3D::Tesselator3D(TopoDS_Shape shape)  {
    shape_ = shape;
}

mValue Tesselator3D::Tesselate() {
    
    mArray indices;
    mArray positions;
    mArray normalArr;
    
    TopExp_Explorer Ex; 
    int index_offset = 0;
    for (Ex.Init(shape_,TopAbs_FACE); Ex.More(); Ex.Next()) { 
        
        TopoDS_Face Face = TopoDS::Face(Ex.Current());
        TopLoc_Location Location = TopLoc_Location();
        Handle(Poly_Triangulation) facing = BRep_Tool().Triangulation(Face,Location);
        
        if (!facing.IsNull()) {
            TColgp_Array1OfDir the_normal(facing->Nodes().Lower(), facing->Nodes().Upper());
            Poly_Connect connect(facing);
            StdPrs_ToolShadedShape().Normal(Face, connect, the_normal);
            
            for (int i = 1; i <= facing->NbNodes(); ++i) {
                gp_Pnt vertex = facing->Nodes().Value(i);
                
                
                gp_Pnt transformedVtx = vertex.Transformed(Face.Location().Transformation());
                
                positions.push_back(transformedVtx.X());
                positions.push_back(transformedVtx.Y());
                positions.push_back(transformedVtx.Z());
                
                normalArr.push_back(the_normal(i).X());
                normalArr.push_back(the_normal(i).Y());
                normalArr.push_back(the_normal(i).Z());
            }
            
            for (int i = 1; i <= facing->NbTriangles(); ++i) {
                Poly_Triangle triangle = facing->Triangles().Value(i);
                Standard_Integer index1, index2, index3;
                
                if (Face.Orientation() == TopAbs_REVERSED) {
                    triangle.Get(index1, index3, index2);
                } else {
                    triangle.Get(index1, index2, index3);
                }
                 
                // Step 1 - caluclate the normals of the triangles
                gp_Pnt vertex1 = facing->Nodes().Value(index1);
                gp_Pnt vertex2 = facing->Nodes().Value(index2);
                gp_Pnt vertex3 = facing->Nodes().Value(index3);
                
                indices.push_back(index_offset + index1 - 1);
                indices.push_back(index_offset + index2 - 1);
                indices.push_back(index_offset + index3 - 1);
            }
            
            index_offset += facing->NbNodes();    
        }
    }
    
    mObject result;
    result["primitive"] = "triangles";
    result["positions"] = positions;
    result["normals"] = normalArr;
    result["indices"] = indices;
    
    return result;
}
