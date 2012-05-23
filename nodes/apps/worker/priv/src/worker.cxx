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

#include <json_spirit.h>

#include "OCC.h"
#include "WorkerConfig.h"
#include "base64.h"

#include "Builder.h"
#include "Tesselate.h"
#include "Transform.h"

using namespace std;
using namespace json_spirit;

// TODO: Tesselate in parallel
// TODO: compress 1.00000000 into 1.0 etc. Also reduce precision

map< string, TopoDS_Shape > shapes = map< string, TopoDS_Shape >();

struct not_found : std::exception { 
    char const* what() const throw() {
        return "not_found";
    }
};

TopoDS_Shape find_shape(string id) {
    map< string, TopoDS_Shape >::iterator it = shapes.find(id);
    if (it != shapes.end()) {
        return (*it).second;
    }
    throw not_found();
}    

#pragma mark Geometry

template < typename T > string create_primitive(string id, map< string, mValue > json) {
    std::auto_ptr<Builder> builder(new T(json));
    shapes[id] = builder->shape();
    return "ok";
}

template < typename T > string create_boolean(string id, map< string, mValue > json) {
    mArray children = json["children"].get_array();
    vector<TopoDS_Shape> childShapes;
    for (unsigned int k = 0; k < children.size(); ++k) {
        TopoDS_Shape childShape = find_shape(children[k].get_str());
        childShapes.push_back(childShape);
    }
    
    std::auto_ptr<Builder> builder(new T(json, childShapes));
    shapes[id] = builder->shape();
    return "ok";
    
}

template < typename T > string create_modifier(string id, map< string, mValue > json) {
    mArray children = json["children"].get_array();
    TopoDS_Shape childShape = find_shape(children[0].get_str());
    
    std::auto_ptr<Builder> builder(new T(json, childShape));
    shapes[id] = builder->shape();
    return "ok";
}

string import_stl(string id, map< string, mValue > json) {

    // Temp filename
    char fileName[100];
    sprintf(fileName, "/tmp/%s.stl", id.c_str());
    
    // Decode base64
    string contents = json["contents"].get_str();
    string decoded = base64_decode(contents);
    
    ofstream tmpFile;
    tmpFile.open (fileName);
    tmpFile << decoded;
    tmpFile.close();
    
    // Read from temp file
    StlAPI_Reader reader;
    TopoDS_Shape shape;
    reader.Read(shape, fileName);
    
    // Mesh it
    TopExp_Explorer ex(shape, TopAbs_FACE);
    if (ex.More()) {
        BRepMesh().Mesh(shape, TRIANGLE_SIZE);
    }
    
    // Delete file
    remove(fileName);
    
    shapes[id] = shape;
    return "ok";
}

string create_geometry(string id, map< string, mValue > json) {
    string geomType = json["type"].get_str();
    
    // 3D Primitives
    if (geomType == "cuboid") {
        return create_primitive<CuboidBuilder>(id, json);
    } else if (geomType == "sphere") {
        return create_primitive<SphereBuilder>(id, json);
    } else if (geomType == "cylinder") {
        return create_primitive<CylinderBuilder>(id, json);
    } else if (geomType == "cone") {
        return create_primitive<ConeBuilder>(id, json);
    } else if (geomType == "wedge") {
        return create_primitive<WedgeBuilder>(id, json);
    } else if (geomType == "torus") {
        return create_primitive<TorusBuilder>(id, json);
        
    // 2D Primitives
    } else if (geomType == "ellipse2d") {
        return create_primitive<Ellipse2DBuilder>(id, json);
    } else if (geomType == "rectangle2d") {
        return create_primitive<Rectangle2DBuilder>(id, json);
    } else if (geomType == "triangle2d") {
        return create_primitive<Triangle2DBuilder>(id, json);
    } else if (geomType == "text2d") {
        return create_primitive<Text2DBuilder>(id, json);

    
    // 1D Primitives
    } else if (geomType == "ellipse1d") {
        return create_primitive<Ellipse1DBuilder>(id, json);
    } else if (geomType == "bezier") {
        return create_primitive<Bezier1DBuilder>(id, json);
    } else if (geomType == "polyline") {
        return create_primitive<PolylineBuilder>(id, json);

        
    // Modifiers
    } else if (geomType == "prism") {
        return create_modifier<PrismBuilder>(id, json);
    } else if (geomType == "revolve") {
        return create_modifier<RevolveBuilder>(id, json);
    } else if (geomType == "fillet") {
        return create_modifier<FilletBuilder>(id, json);
    } else if (geomType == "make_face") {
        return create_boolean<FaceBuilder>(id, json);
    } else if (geomType == "make_solid") {
        return create_boolean<SolidBuilder>(id, json);

    } else if (geomType == "loft") {
        return create_boolean<LoftBuilder>(id, json);
    
    // Booleans
    } else if (geomType == "union") {
        return create_boolean<UnionBuilder>(id, json);
    } else if (geomType == "subtract") {
        return create_boolean<SubtractBuilder>(id, json);
    } else if (geomType == "intersect") {
        return create_boolean<IntersectBuilder>(id, json);
    
    // STL Import
    } else if (geomType == "import_stl") {
        return import_stl(id, json);
    }
    
    return "geometry type not found";
}

#pragma mark Buffer reading & writing

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

#pragma mark export/import

mValue serialize_shape(string sha, TopoDS_Shape shape) {
    char fileName[100];
    sprintf(fileName, "/tmp/%s.bin", sha.c_str());
    
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

    // Delete file
    remove(fileName);
    
    // Base64
    std::string encoded = base64_encode(reinterpret_cast<const unsigned char*>(s11nBuf), index - 1);
    
    return encoded;
}

TopoDS_Shape deserialize_shape(string s11n, string sha) {
    
    // Temp filename
    char fileName[100];
    sprintf(fileName, "/tmp/%s.bin", sha.c_str());
    
    // Decode base64
    std::string decoded = base64_decode(s11n);
    
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
    
    // Delete file
    remove(fileName);
    
    // Create the shape
    PTColStd_PersistentTransientMap aMap;
    TopoDS_Shape resultingShape;
    MgtBRep::Translate(aPShape, aMap, resultingShape, MgtBRep_WithTriangle);
    
    // Mesh it
    TopExp_Explorer ex(resultingShape, TopAbs_FACE);
    if (ex.More()) {
        BRepMesh().Mesh(resultingShape, TRIANGLE_SIZE);
    }
    
    return resultingShape;
}




#pragma mark main()

int main (int argc, char *argv[]) {
    
    unsigned char buf[5*1024*1024];
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
                    
                    TopoDS_Shape shape = find_shape(tesselateId.get_str());
                    mValue response = Tesselator(shape).Tesselate();
                        
                    string output = write(response);
                    write_cmd(output.c_str(), output.size());
                    continue;
                }
                
                mValue existsId = objMap["exists"];
                if (!existsId.is_null() && (existsId.type() == str_type)) {
                    
                    mValue response = !(shapes.find(existsId.get_str()) == shapes.end());
                    string output = write(response);
                    write_cmd(output.c_str(), output.size());
                    continue;
                }
                
                mValue purgeId = objMap["purge"];
                if (!purgeId.is_null() && (purgeId.type() == str_type)) {
                    
                    mValue response = false;
                    map< string, TopoDS_Shape >::iterator it = shapes.find(purgeId.get_str());
                    if (it != shapes.end()) {
                        shapes.erase(it);
                        response = true;
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
                    
                    TopoDS_Shape shape = find_shape(id.get_str());
                    mValue response;
                    
                    
                    if (!TopExp_Explorer(shape, TopAbs_SOLID).More()) {
                        response = mValue("empty");
                    } else {
                        string filenameStr = filename.get_str();
                        
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
                    
                    TopoDS_Shape shape = find_shape(id.get_str());
                    mObject result;
                    result["s11n"] = serialize_shape(id.get_str(), shape);
                    
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

                    TopoDS_Shape shape = deserialize_shape(s11n.get_str(), id.get_str());
                    shapes[id.get_str()] = shape;
                    
                    mValue response = mValue("ok");
                    string output = write(response);
                    write_cmd(output.c_str(), output.size());                    
                    continue;
                    
                }
                
            }

            if (value.type() == str_type) {
    	        string message = value.get_str();
                
                if (message == "purge_all") {
                    
                    shapes = map< string, TopoDS_Shape >();
                    
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
