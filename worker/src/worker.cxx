/*
 Copyright 2011 Benjamin Nortier

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#include <stdio.h>
#include <iostream>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>

// OpenCASCADE
#include <BRep_Tool.hxx>
#include <BRepLib.hxx>
#include <BRepMesh.hxx>
#include <BRepPrimAPI_MakeBox.hxx>
#include <BRepPrimAPI_MakeSphere.hxx>
#include <BRepPrimAPI_MakeCylinder.hxx>
#include <BRepPrimAPI_MakeCone.hxx>
#include <BRepPrimAPI_MakeWedge.hxx>
#include <BRepPrimAPI_MakeTorus.hxx>
#include <BRepAlgoAPI_Fuse.hxx>
#include <BRepAlgoAPI_Cut.hxx>
#include <BRepAlgoAPI_Common.hxx>
#include <BRepBuilderAPI_Transform.hxx>

#include <StlAPI_Writer.hxx>

#include <gp.hxx>
#include <gp_Pnt.hxx>
#include <TopExp_Explorer.hxx>
#include <TopoDS.hxx>
#include <TopoDS_Edge.hxx>
#include <TopoDS_Face.hxx>
#include <Poly_Triangulation.hxx>
#include <TColgp_Array1OfDir.hxx>
#include <Poly_Connect.hxx>
#include <StdPrs_ToolShadedShape.hxx>

#include <FSD_BinaryFile.hxx>
#include <Storage_Data.hxx>
#include <ShapeSchema.hxx>
#include <PTColStd_TransientPersistentMap.hxx>
#include <MgtBRep.hxx>
#include <Storage_HSeqOfRoot.hxx>
#include <Storage_HSeqOfRoot.hxx>
#include <Storage_Root.hxx>
#include <PTColStd_PersistentTransientMap.hxx>



// Spirit
#include "json_spirit.h"

// Project
#include "WorkerConfig.h"
#include "base64.h"

using namespace std;
using namespace json_spirit;

// TODO: Tesselate in parallel
// TODO: compress 1.00000000 into 1.0 etc. Also reduce precision


map< string, TopoDS_Shape > unmeshed_shapes = map< string, TopoDS_Shape >();
map< string, TopoDS_Shape > meshed_shapes = map< string, TopoDS_Shape >();

TopoDS_Shape find_shape(string id) {
    map< string, TopoDS_Shape >::iterator meshedIt = meshed_shapes.find(id);
    if (meshedIt != meshed_shapes.end()) {
        return (*meshedIt).second;
    } 
    
    map< string, TopoDS_Shape >::iterator unmeshedIt = unmeshed_shapes.find(id);
    if (unmeshedIt != unmeshed_shapes.end()) {
        return (*unmeshedIt).second;
    }
    return TopoDS_Shape();
}

void mesh(string id) {
    TopoDS_Shape shape = unmeshed_shapes[id];

    // If we mesh a shape that has no faces, e.g. an empty intersect,
    // Meshing generates an exception
    TopExp_Explorer Ex; 
    int numFaces = 0;
    for (Ex.Init(shape,TopAbs_FACE); Ex.More(); Ex.Next()) { 
        ++numFaces;
    }
        
    if (numFaces > 0) {
        BRepMesh().Mesh(shape, 0.125);
    }
    
    meshed_shapes[id] = unmeshed_shapes[id];
    unmeshed_shapes.erase(id);
}
    
    
mValue tesselate(string id) {
    
    TopoDS_Shape shape = meshed_shapes[id];
    
    mArray indices;
    mArray positions;
    mArray normalArr;
    
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
            triangle.Get(index1, index2, index3);
            
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
    
    mObject result;
    result["primitive"] = "triangles";
    result["positions"] = positions;
    result["normals"] = normalArr;
    result["indices"] = indices;

    return result;
}

double get_double(mValue value) {
    if (value.type() == int_type) {
        return (double)value.get_int();
    } else {
        
        return value.get_real();
    }
}

#pragma mark Transforms


TopoDS_Shape copy_transform(TopoDS_Shape shape, 
                            gp_Trsf (*transformFn)(double, map< string, mValue >, map< string, mValue >), 
			    map< string, mValue > origin,
                            map< string, mValue > parameters) {

    int n = parameters["n"].get_int();

    if(n == 0) {
        gp_Trsf transformation = transformFn(1.0, origin, parameters);
        BRepBuilderAPI_Transform brep_transform(shape, transformation);
	return brep_transform.Shape();
    }
    
    TopoDS_Shape copies[n + 1]; // worst case memory requirement
    copies[0] = shape;
    
    int remaining = n;
    int grouping = 1;
    float multiplier = 1.0;
    int index = 1;
    
    while (remaining > 0) {
        
        int group_index = (int)(log(grouping)/log(2));
        TopoDS_Shape obj_to_copy = copies[group_index];
        
        gp_Trsf transformation = transformFn(multiplier, origin, parameters);
        BRepBuilderAPI_Transform brep_transform(obj_to_copy, transformation);
        copies[index] = BRepAlgoAPI_Fuse(brep_transform.Shape(),
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


gp_Trsf translateTransformation(double multiplier, map< string, mValue > origin, map< string, mValue > parameters) {
    mValue x = origin["x"];
    mValue y = origin["y"];
    mValue z = origin["z"];
    mValue u = parameters["u"];
    mValue v = parameters["v"];
    mValue w = parameters["w"];
    
    gp_Trsf transformation = gp_Trsf();
    transformation.SetTranslation(gp_Vec(multiplier*get_double(u), 
					 multiplier*get_double(v), 
                                         multiplier*get_double(w)));
    return transformation;
}

TopoDS_Shape translate(map<string, mValue> transform, TopoDS_Shape shape) {
    map< string, mValue > parameters = transform["parameters"].get_obj();
    map< string, mValue > origin = transform["origin"].get_obj();
    
    gp_Trsf (*transformFn)(double, map< string, mValue >, map< string, mValue >) = &translateTransformation;
    return copy_transform(shape, transformFn, origin, parameters);
}

TopoDS_Shape scale(map<string, mValue> transform, TopoDS_Shape shape) {
    map< string, mValue > origin = transform["origin"].get_obj();
    mValue x = origin["x"];
    mValue y = origin["y"];
    mValue z = origin["z"];

    map< string, mValue > parameters = transform["parameters"].get_obj();
    mValue factor = parameters["factor"];
    
    gp_Trsf transformation = gp_Trsf();
    transformation.SetScale(gp_Pnt(get_double(x), 
                                   get_double(y), 
                                   get_double(z)), 
                            get_double(factor));
    
    BRepBuilderAPI_Transform brep_transform(shape, transformation);
    TopoDS_Shape transformed_shape = brep_transform.Shape();
    
    return transformed_shape;
}


gp_Trsf rotateTransformation(double multiplier, map< string, mValue > origin, map< string, mValue > parameters) {
    mValue x = origin["x"];
    mValue y = origin["y"];
    mValue z = origin["z"];
    mValue u = parameters["u"];
    mValue v = parameters["v"];
    mValue w = parameters["w"];
    mValue angle = parameters["angle"];
    
    gp_Trsf transformation = gp_Trsf();
    transformation.SetRotation(gp_Ax1(gp_Pnt(get_double(x),
                                             get_double(y),
                                             get_double(z)), 
                                      gp_Dir(get_double(u),
                                             get_double(v),
                                             get_double(w))), 
                               multiplier*get_double(angle)/180*M_PI);
    
    return transformation;
}
    

TopoDS_Shape rotate(map<string, mValue> transform, TopoDS_Shape shape) {
    map< string, mValue > parameters = transform["parameters"].get_obj();
    map< string, mValue > origin = transform["origin"].get_obj();
    
    gp_Trsf (*transformFn)(double,  map< string, mValue >, map< string, mValue >) = &rotateTransformation;
    return copy_transform(shape, transformFn, origin, parameters);
}

TopoDS_Shape mirror(map<string, mValue> transform, TopoDS_Shape shape) {

    map< string, mValue > parameters = transform["parameters"].get_obj();
    map< string, mValue > origin = transform["origin"].get_obj();
    
    mValue x = origin["x"];
    mValue y = origin["y"];
    mValue z = origin["z"];
    mValue u = parameters["u"];
    mValue v = parameters["v"];
    mValue w = parameters["w"];
    int n = parameters["n"].get_int();
    
    gp_Trsf transformation = gp_Trsf();
    transformation.SetMirror(gp_Ax1(gp_Pnt(get_double(x),
                                           get_double(y),
                                           get_double(z)), 
                                    gp_Dir(get_double(u),
                                           get_double(v),
                                           get_double(w))));    
    BRepBuilderAPI_Transform brep_transform(shape, transformation);
    TopoDS_Shape transformed_shape = brep_transform.Shape();

    if (n == 1) {
	return BRepAlgoAPI_Fuse(transformed_shape, shape);
    } else {
	return transformed_shape;
    }
}

TopoDS_Shape copy_mirror(map<string, mValue> transform, TopoDS_Shape shape) {
    map< string, mValue > parameters = transform["parameters"].get_obj();
    mValue px = parameters["px"];
    mValue py = parameters["py"];
    mValue pz = parameters["pz"];
    mValue vx = parameters["vx"];
    mValue vy = parameters["vy"];
    mValue vz = parameters["vz"];
    
    gp_Trsf transformation = gp_Trsf();
    transformation.SetMirror(gp_Ax1(gp_Pnt(get_double(px),
                                           get_double(py),
                                           get_double(pz)), 
                                    gp_Dir(get_double(vx),
                                           get_double(vy),
                                           get_double(vz)))); 
    
    BRepBuilderAPI_Transform brep_transform(shape, transformation);
    TopoDS_Shape transformed_shape = brep_transform.Shape();
    
    TopoDS_Shape result = BRepAlgoAPI_Fuse(transformed_shape, shape);
    return result;
}





TopoDS_Shape applyTransform(map<string, mValue> transform, TopoDS_Shape shape) {
    mValue transformType = transform["type"];
    if (!transformType.is_null() && (transformType.type() == str_type) && (transformType.get_str() == string("translate"))) {
        return translate(transform, shape);
    }
    if (!transformType.is_null() && (transformType.type() == str_type) && (transformType.get_str() == string("scale"))) {
        return scale(transform, shape);
    }
    if (!transformType.is_null() && (transformType.type() == str_type) && (transformType.get_str() == string("rotate"))) {
        return rotate(transform, shape);
    }
    if (!transformType.is_null() && (transformType.type() == str_type) && (transformType.get_str() == string("mirror"))) {
        return mirror(transform, shape);
    }
    return shape;
}

TopoDS_Shape applyTransforms(TopoDS_Shape shape, map< string, mValue > geometry) {
    TopoDS_Shape transformedShape = shape;
    
    if (!geometry["origin"].is_null()) {
        map< string, mValue > origin = geometry["origin"].get_obj();
        mValue x = origin["x"];
        mValue y = origin["y"];
        mValue z = origin["z"];
        gp_Trsf transformation = gp_Trsf();
        transformation.SetTranslation(gp_Vec(get_double(x), get_double(y), get_double(z)));
        
        BRepBuilderAPI_Transform brep_transform(shape, transformation);
        transformedShape = brep_transform.Shape();
    }
    
    if (!geometry["transforms"].is_null() && (geometry["transforms"].type() == array_type)) {
        mArray transforms = geometry["transforms"].get_array();
        vector< mValue >::iterator it;
        for (unsigned int k = 0; k < transforms.size(); ++k) {
            mValue transform2 = transforms[k];
            transformedShape = applyTransform(transform2.get_obj(), transformedShape);
        }
    } 
    return transformedShape;
}

    
#pragma mark Primitives

string create_cuboid(string id, map< string, mValue > geometry) {
    map< string, mValue > parameters = geometry["parameters"].get_obj();
    mValue width = parameters["u"];
    mValue depth = parameters["v"];
    mValue height = parameters["w"];
    if (!width.is_null() && ((width.type() == real_type) || (width.type() == int_type))
        &&
        !depth.is_null() && ((depth.type() == real_type) || (depth.type() == int_type))
        &&
        !height.is_null() && ((height.type() == real_type) || (height.type() == int_type))) {
      
      map< string, mValue > origin = geometry["origin"].get_obj();
      if(get_double(width) < 0) {
	origin["x"] = get_double(origin["x"]) + get_double(width);
	width = -get_double(width);
      }

      if(get_double(depth) < 0) {
	origin["y"] = get_double(origin["y"]) + get_double(depth);
	depth = -get_double(depth);
      }
      
      if(get_double(height) < 0) {
	origin["z"] = get_double(origin["z"]) + get_double(height);
	height = -get_double(height);
      }
      geometry["origin"] = origin;
        
      TopoDS_Shape shape = BRepPrimAPI_MakeBox(get_double(width), 
					       get_double(depth), 
					       get_double(height)).Shape();
        
      unmeshed_shapes[id] = applyTransforms(shape, geometry);
      return "ok";
    }
    return "invalid geometry parameters";
}

string create_sphere(string id, map< string, mValue > geometry) {
    map< string, mValue > parameters = geometry["parameters"].get_obj();
    mValue radius = parameters["r"];
    if (!radius.is_null() && ((radius.type() == real_type) || (radius.type() == int_type))) {
        TopoDS_Shape shape = BRepPrimAPI_MakeSphere(get_double(radius)).Shape();
        unmeshed_shapes[id] = applyTransforms(shape, geometry);
        return "ok";
    }
    return "invalid geometry parameters";
}

string create_cylinder(string id, map< string, mValue > geometry) {
    map< string, mValue > parameters = geometry["parameters"].get_obj();
    mValue r = parameters["r"];
    mValue h = parameters["h"];
    if (!r.is_null() && ((r.type() == real_type) || (r.type() == int_type))
        &&
        !h.is_null() && ((h.type() == real_type) || (h.type() == int_type))) {

      map< string, mValue > origin = geometry["origin"].get_obj();
      if(get_double(h) < 0) {
	origin["z"] = get_double(origin["z"]) + get_double(h);
	h = -get_double(h);
	geometry["origin"] = origin;
      }
        
        TopoDS_Shape shape = BRepPrimAPI_MakeCylinder(get_double(r), 
                                                      get_double(h)).Shape();
        unmeshed_shapes[id] = applyTransforms(shape, geometry);
        return "ok";
    }
    return "invalid geometry parameters";
}

string create_cone(string id, map< string, mValue > geometry) {
    map< string, mValue > parameters = geometry["parameters"].get_obj();
    mValue r1 = parameters["r1"];
    mValue r2 = parameters["r2"];
    mValue h = parameters["h"];
    if (!r1.is_null() && ((r1.type() == real_type) || (r1.type() == int_type))
        &&
        !r2.is_null() && ((r2.type() == real_type) || (r2.type() == int_type))
        &&
        !h.is_null() && ((h.type() == real_type) || (h.type() == int_type))) {

      map< string, mValue > origin = geometry["origin"].get_obj();
      if(get_double(h) < 0) {
	origin["z"] = get_double(origin["z"]) + get_double(h);
	h = -get_double(h);
	geometry["origin"] = origin;

	double tmp = get_double(r1);
	r1 = r2;
	r2 = tmp;
      }
        
        TopoDS_Shape shape = BRepPrimAPI_MakeCone(get_double(r1), 
                                                  get_double(r2), 
                                                  get_double(h)).Shape();
        unmeshed_shapes[id] = applyTransforms(shape, geometry);
        return "ok";
    }
    return "invalid geometry parameters";
}

string create_wedge(string id, map< string, mValue > geometry) {
    map< string, mValue > parameters = geometry["parameters"].get_obj();
    mValue x1 = parameters["u1"];
    mValue x2 = parameters["u2"];
    mValue y = parameters["v"];
    mValue z = parameters["w"];
    if (!x1.is_null() && ((x1.type() == real_type) || (x1.type() == int_type))
        &&
        !x2.is_null() && ((x2.type() == real_type) || (x2.type() == int_type))
        &&
        !y.is_null() && ((y.type() == real_type) || (y.type() == int_type))
        &&
        !z.is_null() && ((z.type() == real_type) || (z.type() == int_type))) {

      map< string, mValue > origin = geometry["origin"].get_obj();
      if(get_double(z) < 0) {
	origin["z"] = get_double(origin["z"]) + get_double(z);
	z = -get_double(z);
      }
      geometry["origin"] = origin;
        
        TopoDS_Shape shape = BRepPrimAPI_MakeWedge(get_double(x1), 
                                                   get_double(y), 
                                                   get_double(z), 
                                                   get_double(x2)).Shape();
        unmeshed_shapes[id] = applyTransforms(shape, geometry);
        return "ok";
    }
    return "invalid geometry parameters";
}

string create_torus(string id, map< string, mValue > geometry) {
    map< string, mValue > parameters = geometry["parameters"].get_obj();
    mValue r1 = parameters["r1"];
    mValue r2 = parameters["r2"];
    if (!r1.is_null() && ((r1.type() == real_type) || (r1.type() == int_type))
        &&
        !r2.is_null() && ((r2.type() == real_type) || (r2.type() == int_type))) {
        
        TopoDS_Shape shape = BRepPrimAPI_MakeTorus(get_double(r1), 
                                                   get_double(r2)).Shape();
        unmeshed_shapes[id] = applyTransforms(shape, geometry);
        return "ok";
    }
    return "invalid geometry parameters";
}

#pragma mark Boolean

string create_union(string id, map< string, mValue > geometry) {
    mArray children = geometry["children"].get_array();
    if (children.size() < 2) {
        return("invalid children");
    }
    
    TopoDS_Shape boolean_shape = BRepAlgoAPI_Fuse(find_shape(children[0].get_str()), 
                                                  find_shape(children[1].get_str())).Shape();
    for (unsigned int i = 2; i < children.size(); ++i) {
        boolean_shape = BRepAlgoAPI_Fuse(boolean_shape,
                                         find_shape(children[i].get_str())).Shape();
    }
    unmeshed_shapes[id] = applyTransforms(boolean_shape, geometry);
    return "ok";
}

string create_intersect(string id, map< string, mValue > geometry) {
    mArray children = geometry["children"].get_array();
    if (children.size() < 2) {
        return("invalid children");
    }
    
    TopoDS_Shape boolean_shape = BRepAlgoAPI_Common(find_shape(children[0].get_str()), 
                                                    find_shape(children[1].get_str())).Shape();
    for (unsigned int i = 2; i < children.size(); ++i) {
        boolean_shape = BRepAlgoAPI_Common(boolean_shape,
                                           find_shape(children[i].get_str())).Shape();
    }
    unmeshed_shapes[id] = applyTransforms(boolean_shape, geometry);
    return "ok";
}

string create_subtract(string id, map< string, mValue > geometry) {
    mArray children = geometry["children"].get_array();
    if (children.size() != 2) {
        return("only 2 children supported");
    }
    
    TopoDS_Shape boolean_shape = BRepAlgoAPI_Cut(find_shape(children[1].get_str()), 
                                                 find_shape(children[0].get_str())).Shape();
    unmeshed_shapes[id] = applyTransforms(boolean_shape, geometry);
    return "ok";
}

string create_geometry(string id, map< string, mValue > geometry) {
    mValue geomType = geometry["type"];
    /*
     * Primitives
     */
    TopoDS_Shape shape;
    if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("cuboid"))) {
        return create_cuboid(id, geometry);
    } 
    if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("sphere"))) {
        return create_sphere(id, geometry);
    } 
    if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("cylinder"))) {
        return create_cylinder(id, geometry);
    } 
    if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("cone"))) {
        return create_cone(id, geometry);
    } 
    if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("wedge"))) {
        return create_wedge(id, geometry);
    } 
    if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("torus"))) {
        return create_torus(id, geometry);
    } 
    /*
     * Booleans
     */
    if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("union"))) {
        return create_union(id, geometry);
    }
    if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("subtract"))) {
        return create_subtract(id, geometry);
    }
    if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("intersect"))) {
        return create_intersect(id, geometry);
    }
    return "geometry type not found";
}

int read_exact(unsigned char *buf, int len) {
    int i, got=0;
    do {
        if ((i = read(0, buf+got, len-got)) <= 0)
            return(i);
        got += i;
    } while (got<len);
    return len;
}

int read_cmd(unsigned char* buf) {
    int len;
    if (read_exact(buf, 4) != 4)
        return(-1);
    len = (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3];
    return read_exact(buf, len);
}

int write_exact(const char *buf, int len) {
    int i, wrote = 0;
    do {
        if ((i = write(1, buf+wrote, len-wrote)) <= 0)
            return (i);
        wrote += i;
    } while (wrote<len);
    return len;
}

int write_cmd(const char *buf, int len) {
    char size_buf[4];
    size_buf[0] = (len >> 24) & 0xff;
    size_buf[1] = (len >> 16) & 0xff;
    size_buf[2] = (len >> 8) & 0xff;
    size_buf[3] = len & 0xff;
    write_exact(size_buf, 4);
    return write_exact(buf, len);
}

int main (int argc, char *argv[]) {
    
    unsigned char buf[1024*1024];
    int msg_size;
    
    while ((msg_size = read_cmd(buf)) > 0) {
        
        try {
            mValue value;
            // Set 0-delimiter for conversion to string
            buf[msg_size] = 0;
            string json_string = string((char*)buf);
            read(json_string, value);
            
            if (value.type() == obj_type) {
                map< string, mValue > objMap = mObject(value.get_obj());
                
                mValue msgType = objMap["type"];
                mValue id = objMap["id"];
                mValue geometry = objMap["geometry"];
                
                if (!msgType.is_null() && (msgType.type() == str_type) && (msgType.get_str() == string("create"))
                    && 
                    !id.is_null() && (id.type() == str_type)
                    && 
                    !geometry.is_null() && (geometry.type() == obj_type)) {
                    
                    string response = create_geometry(id.get_str(), mObject(geometry.get_obj()));
                    string output = write(mValue(response));
                    write_cmd(output.c_str(), output.size());
                    continue;
                }
                
                mValue tesselateId = objMap["tesselate"];
                if (!tesselateId.is_null() && (tesselateId.type() == str_type)) {
                    
                    mValue response;
                    if (unmeshed_shapes.find(tesselateId.get_str()) != unmeshed_shapes.end()) {
                        // Mesh it first
                        mesh(tesselateId.get_str());
                    }
                    
                    // It should now be in the map of meshed shapes
                    if (meshed_shapes.find(tesselateId.get_str()) != meshed_shapes.end()) {
                        response = tesselate(tesselateId.get_str());
                        
                    } else {
                        mObject error;
                        error["error"] = "not_found";
                        response = error;
                        
                    }
                    string output = write(response);
                    write_cmd(output.c_str(), output.size());
                    continue;
                }
                
                mValue existsId = objMap["exists"];
                if (!existsId.is_null() && (existsId.type() == str_type)) {
                    
                    
                    mValue response = !((unmeshed_shapes.find(existsId.get_str()) == unmeshed_shapes.end())
                                        &&
                                        (meshed_shapes.find(existsId.get_str()) == meshed_shapes.end()));
                    string output = write(response);
                    write_cmd(output.c_str(), output.size());
                    continue;
                }
                
                mValue purgeId = objMap["purge"];
                if (!purgeId.is_null() && (purgeId.type() == str_type)) {
                    
                    mValue response = true;
                    
                    map< string, TopoDS_Shape >::iterator it = unmeshed_shapes.find(purgeId.get_str());
                    if (it != unmeshed_shapes.end()) {
                        unmeshed_shapes.erase(it);
                        response = true;
                    } else {
                        it = meshed_shapes.find(purgeId.get_str());
                        if (it != meshed_shapes.end()) {
                            meshed_shapes.erase(it);
                            response = true;
                        }
                    }
                    
                    string output = write(response);
                    write_cmd(output.c_str(), output.size());
                    continue;
                }
		
                
                mValue filename = objMap["filename"];
                if (!msgType.is_null() && (msgType.type() == str_type) && (msgType.get_str() == string("stl"))
                    && 
                    !id.is_null() && (id.type() == str_type)
                    && 
                    !filename.is_null() && (filename.type() == str_type)) {
                    
                    mValue response;
                    TopoDS_Shape potentialShape = find_shape(id.get_str());
                    if (potentialShape.IsNull()) {
                        
                        mObject error;
                        error["error"] = "not_found";
                        response = error;

                    } else {
                    
                        string filenameStr = filename.get_str();
                        TopoDS_Shape shape = potentialShape;
                        
                        StlAPI_Writer writer;
                        writer.Write(shape, filenameStr.c_str());
                        
                        response = mValue("ok");
                    }
                    string output = write(response);
                    write_cmd(output.c_str(), output.size());
                    continue;
                }
                
                if (!msgType.is_null() && (msgType.type() == str_type) && (msgType.get_str() == string("serialize"))
                    &&
                    !id.is_null() && (id.type() == str_type)) {
                    
                    // Mesh it first if it is not meshed
                    if (unmeshed_shapes.find(id.get_str()) != unmeshed_shapes.end()) {
                        mesh(id.get_str());
                    }
                        
                    if (meshed_shapes.find(id.get_str()) == meshed_shapes.end()) {
                        mObject error;
                        error["error"] = "not_found";
                        string output = write(error);
                        write_cmd(output.c_str(), output.size());
                        continue;
                    }
                    
                    TopoDS_Shape shape = meshed_shapes[id.get_str()];
                    char fileName[50];
                    sprintf(fileName, "/tmp/%s.bin", id.get_str().c_str());
                    
                    // Write to temporary file
                    FSD_BinaryFile f;
                    f.Open(fileName, Storage_VSWrite);
                    Handle(Storage_Data) d = new Storage_Data;
                    
                    PTColStd_TransientPersistentMap aMap;
                    Handle(PTopoDS_HShape) aPShape = MgtBRep::Translate(shape, aMap, MgtBRep_WithTriangle);
                    
                    d->AddRoot("ObjectName", aPShape);
                    Handle(ShapeSchema) s = new ShapeSchema;
                    s->Write(f, d);
                    f.Close();
                    
                    // Read from tmp
                    int index = 0;
                    char s11nBuf[1024*1024];
                    
                    ifstream tmpFile(fileName);
                    while(!tmpFile.eof())
                    {
                        tmpFile.get(s11nBuf[index]);
                        ++index;
                    }
                    tmpFile.close();
                    
                    // Base64
                    std::string encoded = base64_encode(reinterpret_cast<const unsigned char*>(s11nBuf), index - 1);
                    
                    mObject result;
                    result["s11n"] = encoded;
                    string output = write(result);
                    write_cmd(output.c_str(), output.size());
                    continue;
                    
                }
                
                mValue s11n = objMap["s11n"];
                if (!msgType.is_null() && (msgType.type() == str_type) && (msgType.get_str() == string("deserialize"))
                    &&
                    !id.is_null() && (id.type() == str_type)
                    &&
                    !s11n.is_null() && (s11n.type() == str_type)) {
                    
                    // Temp filename
                    char fileName[50];
                    sprintf(fileName, "/tmp/%s.bin", id.get_str().c_str());

                    // Decode base64
                    std::string decoded = base64_decode(s11n.get_str());
                    
                    ofstream tmpFile;
                    tmpFile.open (fileName);
                    tmpFile << decoded;
                    tmpFile.close();
                    
                    // Read from temp file

                    FSD_BinaryFile f;
                    f.Open(fileName, Storage_VSRead);

                    Handle(ShapeSchema) s = new ShapeSchema;
                    Handle(Storage_Data) d = s->Read( f );
                    Handle(Storage_HSeqOfRoot)  roots = d->Roots();
                    Handle(Storage_Root) r = d->Find("ObjectName");
                        //= roots->Value(0);
                    Handle(Standard_Persistent) p = r->Object();
                    Handle(PTopoDS_HShape) aPShape  = Handle(PTopoDS_HShape)::DownCast(p);
                    f.Close();

                    
                    // Create the shape
                    PTColStd_PersistentTransientMap aMap;
                    TopoDS_Shape resultingShape;
                    MgtBRep::Translate(aPShape, aMap, resultingShape, MgtBRep_WithTriangle);
                    
                    meshed_shapes[id.get_str()] = resultingShape;
                    
                    mValue response = mValue("ok");
                    string output = write(response);
                    write_cmd(output.c_str(), output.size());                    
                    continue;
                    
                }
                
            }

	    if (value.type() == str_type) {
    	        string message = value.get_str();

		if (message == "purge_all") {
                    
		    unmeshed_shapes = map< string, TopoDS_Shape >();
		    meshed_shapes = map< string, TopoDS_Shape >();

		    mValue response = mValue("ok");
                    string output = write(response);
                    write_cmd(output.c_str(), output.size());
                    continue;
                }
	    }
            
            mObject error;
            error["error"] = "unknown message";
            string output = write(error);
            write_cmd(output.c_str(), output.size());
            
        } catch (exception& e) {
            
            mObject error;
            error["error"] = e.what();
            string output = write(error);
            write_cmd(output.c_str(), output.size());

        } catch (...) {

            mObject error;
            error["error"] = "exception!";
            string output = write(error);
            write_cmd(output.c_str(), output.size());
            
        }
        
    }
    
    return 0;
}
