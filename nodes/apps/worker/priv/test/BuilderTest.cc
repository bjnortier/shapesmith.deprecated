#include "gtest/gtest.h"
#include "Builder.h"
#include "Tesselate.h"

TEST(BuilderTest, Ellipse1d) {
    
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    mObject parameters;
    parameters["r1"] = 10.0;
    parameters["r2"] = 5.0;
    
    
    mObject json;
    json["origin"] = origin;
    json["parameters"] = parameters;
    
    Ellipse1DBuilder builder(json);
    
    TopoDS_Shape shape = builder.shape();
    
    ASSERT_FALSE(shape.IsNull());
    
    auto_ptr<Tesselator> tesselator(new Tesselator(shape));
    mValue tesselation = tesselator->Tesselate();
    
    mArray facesTesselation = tesselation.get_obj()["faces"].get_obj()["positions"].get_array();
    mArray edgeTesselation = tesselation.get_obj()["edges"].get_obj()["positions"].get_array();
    
    
    ASSERT_EQ(facesTesselation.size(), 0);
    ASSERT_NE(edgeTesselation.size(), 0);
    
    
}

TEST(BuilderTest, Bezier1d) {
    
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    mObject parameters;
    
    mObject v0; v0["u"] = 0.0; v0["v"] = 0.0; v0["w"] = 0.0;
    mObject v1; v1["u"] = 10.0; v1["v"] = 0.0; v1["w"] = 0.0;
    mObject v2; v2["u"] = 0.0; v2["v"] = 10.0; v2["w"] = 0.0;
    mObject v3; v3["u"] = 10.0; v3["v"] = 10.0; v3["w"] = 0.0;
    
    mArray vertices;
    vertices.push_back(v0);
    vertices.push_back(v1);
    vertices.push_back(v2);
    vertices.push_back(v3);
    parameters["vertices"] = vertices;
    
    
    mObject json;
    json["origin"] = origin;
    json["parameters"] = parameters;
    
    Bezier1DBuilder builder(json);
    
    TopoDS_Shape shape = builder.shape();
    
    ASSERT_FALSE(shape.IsNull());
    
    auto_ptr<Tesselator> tesselator(new Tesselator(shape));
    mValue tesselation = tesselator->Tesselate();
    
    mArray facesTesselation = tesselation.get_obj()["faces"].get_obj()["positions"].get_array();
    mArray edgeTesselation = tesselation.get_obj()["edges"].get_obj()["positions"].get_array();
    
    
    ASSERT_EQ(facesTesselation.size(), 0);
    ASSERT_NE(edgeTesselation.size(), 0);
    
    
}

TEST(BuilderTest, Ellipse2d) {
    
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    mObject parameters;
    parameters["r1"] = 10.0;
    parameters["r2"] = 5.0;
    
    
    mObject json;
    json["origin"] = origin;
    json["parameters"] = parameters;
    
    Ellipse2DBuilder builder(json);
    
    TopoDS_Shape shape = builder.shape();
    
    ASSERT_FALSE(shape.IsNull());
    
    auto_ptr<Tesselator> tesselator(new Tesselator(shape));
    mValue tesselation = tesselator->Tesselate();
    
    mArray facesTesselation = tesselation.get_obj()["faces"].get_obj()["positions"].get_array();
    mArray edgeTesselation = tesselation.get_obj()["edges"].get_obj()["positions"].get_array();
    
    
    ASSERT_NE(facesTesselation.size(), 0);
    ASSERT_NE(edgeTesselation.size(), 0);
    
}

TEST(BuilderTest, Rectangle2d) {
    
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    mObject parameters;
    parameters["u"] = 10.0;
    parameters["v"] = 5.0;
    
    
    mObject json;
    json["origin"] = origin;
    json["parameters"] = parameters;
    
    Rectangle2DBuilder builder(json);
    
    TopoDS_Shape shape = builder.shape();
    
    ASSERT_FALSE(shape.IsNull());
    
    auto_ptr<Tesselator> tesselator(new Tesselator(shape));
    mValue tesselation = tesselator->Tesselate();
    
    mArray facesTesselation = tesselation.get_obj()["faces"].get_obj()["positions"].get_array();
    mArray edgeTesselation = tesselation.get_obj()["edges"].get_obj()["positions"].get_array();
    
    
    ASSERT_NE(facesTesselation.size(), 0);
    ASSERT_NE(edgeTesselation.size(), 0);
    
}



TEST(BuilderTest, EllipseWithMajorSmallerThanMinor) {
    
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    mObject parameters;
    parameters["r1"] = 5.0;
    parameters["r2"] = 10.0;
    parameters["from_angle"] = 30.0;
    parameters["to_angle"] = 45.0;
    
    mObject json;
    json["origin"] = origin;
    json["parameters"] = parameters;
    
    Ellipse2DBuilder builder(json);
    
    TopoDS_Shape shape = builder.shape();
    ASSERT_FALSE(shape.IsNull());
    
    auto_ptr<Tesselator> tesselator(new Tesselator(shape));
    mValue tesselation = tesselator->Tesselate();
    
    mArray facesTesselation = tesselation.get_obj()["faces"].get_obj()["positions"].get_array();
    mArray edgeTesselation = tesselation.get_obj()["edges"].get_obj()["positions"].get_array();
    
    
    ASSERT_NE(facesTesselation.size(), 0);
    ASSERT_NE(edgeTesselation.size(), 0);
    
}


TEST(BuilderTest, Cone1) {
    
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    
    mObject parameters;
    parameters["r1"] = 10.0;
    parameters["h"] = 10.0;
    parameters["r2"] = 5.0;
    
    mObject json;
    json["origin"] = origin;
    json["parameters"] = parameters;
    
    ConeBuilder builder(json);
    
    TopoDS_Shape shape = builder.shape();
    ASSERT_FALSE(shape.IsNull());
    
    
    auto_ptr<Tesselator> tesselator(new Tesselator(shape));
    mValue tesselation = tesselator->Tesselate();
    
    mArray facesTesselation = tesselation.get_obj()["faces"].get_obj()["positions"].get_array();
    mArray edgeTesselation = tesselation.get_obj()["edges"].get_obj()["positions"].get_array();
    
    
    ASSERT_NE(facesTesselation.size(), 0);
    ASSERT_NE(edgeTesselation.size(), 0);
    
    
    
}

TEST(BuilderTest, Prism) {
    
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    
    mObject ellipseParameters;
    ellipseParameters["r1"] = 10.0;
    ellipseParameters["r2"] = 5.0;
    
    mObject ellipseJson;
    ellipseJson["origin"] = origin;
    ellipseJson["parameters"] = ellipseParameters;
    
    Ellipse2DBuilder ellipse2Dbuilder(ellipseJson);
    
    TopoDS_Shape ellipse2d = ellipse2Dbuilder.shape();
    
    ASSERT_FALSE(ellipse2d.IsNull());
    
    mObject prismJson;
    mObject prismParameters;
    prismParameters["u"] = 0.0;
    prismParameters["v"] = 0.0;
    prismParameters["w"] = 5.0;
    
    
    prismJson["origin"] = origin;
    prismJson["parameters"] = prismParameters;
    
    PrismBuilder prismBuilder(prismJson, ellipse2d);
    TopoDS_Shape prism = prismBuilder.shape();
    
    ASSERT_FALSE(prism.IsNull());
    
}

TEST(BuilderTest, Revolve) {
    
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    
    mObject ellipseParameters;
    ellipseParameters["r1"] = 10.0;
    ellipseParameters["r2"] = 5.0;
    
    mObject ellipseJson;
    ellipseJson["origin"] = origin;
    ellipseJson["parameters"] = ellipseParameters;
    
    Ellipse2DBuilder ellipse2Dbuilder(ellipseJson);
    
    TopoDS_Shape ellipse2d = ellipse2Dbuilder.shape();
    
    ASSERT_FALSE(ellipse2d.IsNull());
    
    mObject revolveJson;
    mObject revolveParameters;
    revolveParameters["u"] = 0.0;
    revolveParameters["v"] = 0.0;
    revolveParameters["w"] = 5.0;
    revolveParameters["angle"] = M_PI/2;
    
    
    revolveJson["origin"] = origin;
    revolveJson["parameters"] = revolveParameters;
    
    RevolveBuilder revolveBuilder(revolveJson, ellipse2d);
    TopoDS_Shape revolve = revolveBuilder.shape();
    
    ASSERT_FALSE(revolve.IsNull());
    
}

TEST(BuilderTest, MakeSolidFrom1DRevolve) {
    
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    
    mObject ellipseParameters;
    ellipseParameters["r1"] = 10.0;
    ellipseParameters["r2"] = 5.0;
    ellipseParameters["from_angle"] = 0;
    ellipseParameters["to_angle"] = 180;
    
    mObject ellipseJson;
    ellipseJson["origin"] = origin;
    ellipseJson["parameters"] = ellipseParameters;
    
    Ellipse1DBuilder ellipse1Dbuilder(ellipseJson);
    
    TopoDS_Shape ellipse1d = ellipse1Dbuilder.shape();
    
    ASSERT_FALSE(ellipse1d.IsNull());
    
    mObject revolveJson;
    mObject revolveParameters;
    revolveParameters["u"] = 1.0;
    revolveParameters["v"] = 0.0;
    revolveParameters["w"] = 0.0;
    revolveParameters["angle"] = M_PI*2;
    
    revolveJson["origin"] = origin;
    revolveJson["parameters"] = revolveParameters;
    
    RevolveBuilder revolveBuilder(revolveJson, ellipse1d);
    TopoDS_Shape revolve = revolveBuilder.shape();
    ASSERT_FALSE(revolve.IsNull());
    ASSERT_FALSE(TopExp_Explorer(revolve, TopAbs_SOLID).More());
    
    mObject empty;
    vector<TopoDS_Shape> group1;
    group1.push_back(revolve);
    SolidBuilder makeSolid(empty, group1);
    
    ASSERT_FALSE(makeSolid.shape().IsNull());
    
    ASSERT_TRUE(TopExp_Explorer(makeSolid.shape(), TopAbs_SOLID).More());
    
    BRepClass3d_SolidClassifier classifier(makeSolid.shape());
    classifier.PerformInfinitePoint(0.1);
    ASSERT_FALSE(classifier.State() == TopAbs_IN);
    
    
}

TEST(BuilderTest, Boolean1D) {
    
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    
    mObject ellipse1Parameters;
    ellipse1Parameters["r1"] = 10.0;
    ellipse1Parameters["r2"] = 10.0;
    
    mObject ellipse2Parameters;
    ellipse2Parameters["r1"] = 20.0;
    ellipse2Parameters["r2"] = 10.0;
    
    mObject ellipse1Json;
    ellipse1Json["origin"] = origin;
    ellipse1Json["parameters"] = ellipse1Parameters;
    
    mObject ellipse2Json;
    ellipse2Json["origin"] = origin;
    ellipse2Json["parameters"] = ellipse2Parameters;
    
    
    Ellipse1DBuilder ellipse1Builder(ellipse1Json);
    Ellipse1DBuilder ellipse2Builder(ellipse2Json);
    
    mObject unionJson;
    vector<TopoDS_Shape> group1;
    group1.push_back(ellipse1Builder.shape());
    group1.push_back(ellipse2Builder.shape());
    SubtractBuilder union1(unionJson, group1);
    
    ASSERT_FALSE(union1.shape().IsNull());
    
}

TEST(BuilderTest, MakeFaceFromWires) {
    
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    
    mObject ellipse1Parameters;
    ellipse1Parameters["r1"] = 10.0;
    ellipse1Parameters["r2"] = 10.0;
    ellipse1Parameters["from_angle"] = 0.0;
    ellipse1Parameters["to_angle"] = 180.0;
    
    mObject ellipse2Parameters;
    ellipse2Parameters["r1"] = 10.0;
    ellipse2Parameters["r2"] = 20.0;
    ellipse2Parameters["from_angle"] = 0.0;
    ellipse2Parameters["to_angle"] = 180.0;
    
    mObject ellipse1Json;
    ellipse1Json["origin"] = origin;
    ellipse1Json["parameters"] = ellipse1Parameters;
    
    mObject ellipse2Json;
    ellipse2Json["origin"] = origin;
    ellipse2Json["parameters"] = ellipse2Parameters;
    
    
    Ellipse1DBuilder ellipse1Builder(ellipse1Json);
    Ellipse1DBuilder ellipse2Builder(ellipse2Json);
    
    mObject empty;
    vector<TopoDS_Shape> group1;
    group1.push_back(ellipse1Builder.shape());
    group1.push_back(ellipse2Builder.shape());
    FaceBuilder makeFace(empty, group1);
    
    ASSERT_FALSE(makeFace.shape().IsNull());
    
    
}

TEST(BuilderTest, Polyline) {
    
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    
    mObject polylineParameters;
    
    mObject v0; v0["u"] = 0.0; v0["v"] = 0.0; v0["w"] = 0.0;
    mObject v1; v1["u"] = 2.0; v1["v"] = 2.0; v1["w"] = 0.0;
    
    mArray vertices1;
    vertices1.push_back(v0);
    vertices1.push_back(v1);
    polylineParameters["vertices"] = vertices1;
    
    mObject polylineJson;
    polylineJson["origin"] = origin;
    polylineJson["parameters"] = polylineParameters;
    
    PolylineBuilder polylineBuilder(polylineJson);
    ASSERT_FALSE(polylineBuilder.shape().IsNull());
    
    auto_ptr<Tesselator> tesselator(new Tesselator(polylineBuilder.shape()));
    mValue tesselation = tesselator->Tesselate();
    
    mArray facesTesselation = tesselation.get_obj()["faces"].get_obj()["positions"].get_array();
    mArray edgeTesselation = tesselation.get_obj()["edges"].get_obj()["positions"].get_array();
    
    ASSERT_EQ(facesTesselation.size(), 0);
    ASSERT_NE(edgeTesselation.size(), 0);
    
}


TEST(BuilderTest, MakeFaceFromWiresRespectOrientation) {
    
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    
    mObject ellipse1Parameters;
    ellipse1Parameters["r1"] = 10.0;
    ellipse1Parameters["r2"] = 10.0;
    ellipse1Parameters["from_angle"] = 0.0;
    ellipse1Parameters["to_angle"] = 90.0;
    
    mObject ellipse1Json;
    ellipse1Json["origin"] = origin;
    ellipse1Json["parameters"] = ellipse1Parameters;
    
    mObject bezier1Parameters;
    
    mObject v0; v0["u"] = 0.0; v0["v"] = 0.0; v0["w"] = 0.0;
    mObject v1; v1["u"] = 2.0; v1["v"] = 2.0; v1["w"] = 0.0;
    mObject v2; v2["u"] = 2.0; v2["v"] = 2.0; v2["w"] = 0.0;
    mObject v3; v3["u"] = 10.0; v3["v"] = 0.0; v3["w"] = 0.0;
    mObject v3_2; v3_2["u"] = 0.0; v3_2["v"] = 10.0; v3_2["w"] = 0.0;
    
    mArray vertices1;
    vertices1.push_back(v0);
    vertices1.push_back(v1);
    vertices1.push_back(v2);
    vertices1.push_back(v3);
    bezier1Parameters["vertices"] = vertices1;
    
    mObject bezier2Parameters;
    
    mArray vertices2;
    vertices2.push_back(v0);
    vertices2.push_back(v1);
    vertices2.push_back(v2);
    vertices2.push_back(v3_2);
    bezier2Parameters["vertices"] = vertices2;
    
    
    mObject bezier1json;
    bezier1json["origin"] = origin;
    bezier1json["parameters"] = bezier1Parameters;
    
    mObject bezier2json;
    bezier2json["origin"] = origin;
    bezier2json["parameters"] = bezier2Parameters;
    
    Ellipse1DBuilder ellipse1Builder(ellipse1Json);
    Bezier1DBuilder bezier1Builder(bezier1json);
    Bezier1DBuilder bezier2Builder(bezier2json);
    
    mObject empty;
    vector<TopoDS_Shape> group1;
    group1.push_back(ellipse1Builder.shape());
    group1.push_back(bezier1Builder.shape());
    group1.push_back(bezier2Builder.shape());
    FaceBuilder makeFace(empty, group1);
    ASSERT_FALSE(makeFace.shape().IsNull());
    
}

TEST(BuilderTest, MakeFaceFromWiresNotALoop) {
    
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    
    mObject ellipse1Parameters;
    ellipse1Parameters["r1"] = 10.0;
    ellipse1Parameters["r2"] = 10.0;
    ellipse1Parameters["from_angle"] = 0.0;
    ellipse1Parameters["to_angle"] = 180.0;
    
    mObject ellipse2Parameters;
    ellipse2Parameters["r1"] = 50.0;
    ellipse2Parameters["r2"] = 50.0;
    ellipse2Parameters["from_angle"] = 0.0;
    ellipse2Parameters["to_angle"] = 180.0;
    
    mObject ellipse1Json;
    ellipse1Json["origin"] = origin;
    ellipse1Json["parameters"] = ellipse1Parameters;
    
    mObject ellipse2Json;
    ellipse2Json["origin"] = origin;
    ellipse2Json["parameters"] = ellipse2Parameters;
    
    
    Ellipse1DBuilder ellipse1Builder(ellipse1Json);
    Ellipse1DBuilder ellipse2Builder(ellipse2Json);
    
    mObject emptyJson;
    vector<TopoDS_Shape> group1;
    group1.push_back(ellipse1Builder.shape());
    group1.push_back(ellipse2Builder.shape());
    
    try {
        FaceBuilder makeFace(emptyJson, group1);
        ASSERT_TRUE(false);
    } catch (wires_not_a_loop& e) {
        ASSERT_STREQ("Wires are not a loop", e.what());
    }
    
}


TEST(BuilderTest, Loft) {
    
    mObject origin1;
    origin1["x"] = 0.0; origin1["y"] = 0.0; origin1["z"] = 0.0;
    
    mObject origin2;
    origin2["x"] = 0.0; origin2["y"] = 0.0; origin2["z"] = 10.0;
    
    mObject ellipse1Parameters;
    ellipse1Parameters["r1"] = 10.0;
    ellipse1Parameters["r2"] = 10.0;
    ellipse1Parameters["from_angle"] = 0.0;
    ellipse1Parameters["to_angle"] = 180.0;
    
    mObject ellipse2Parameters;
    ellipse2Parameters["r1"] = 50.0;
    ellipse2Parameters["r2"] = 50.0;
    ellipse2Parameters["from_angle"] = 0.0;
    ellipse2Parameters["to_angle"] = 180.0;
    
    mObject ellipse1Json;
    ellipse1Json["origin"] = origin1;
    ellipse1Json["parameters"] = ellipse1Parameters;
    
    mObject ellipse2Json;
    ellipse2Json["origin"] = origin2;
    ellipse2Json["parameters"] = ellipse2Parameters;
    
    Ellipse2DBuilder ellipse1Builder(ellipse1Json);
    Ellipse2DBuilder ellipse2Builder(ellipse2Json);
    
    mObject loftJson;
    vector<TopoDS_Shape> group1;
    group1.push_back(ellipse1Builder.shape());
    group1.push_back(ellipse2Builder.shape());
    
    mObject empty;
    LoftBuilder loft(loftJson, group1);
    
    
    ASSERT_FALSE(loft.shape().IsNull());
    
}

TEST(BuilderTest, Workplane) {
    
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    
    mObject cuboidParameters;
    cuboidParameters["u"] = 10.0;
    cuboidParameters["v"] = 10.0;
    cuboidParameters["w"] = 10.0;

    mObject workplaneOrigin;
    workplaneOrigin["x"] = 0.0;
    workplaneOrigin["y"] = 0.0;
    workplaneOrigin["z"] = 10.0;
    
    mObject workplaneAxis;
    workplaneAxis["x"] = 0.0;
    workplaneAxis["y"] = 0.0;
    workplaneAxis["z"] = 1.0;

    mObject workplane;
    workplane["origin"] = workplaneOrigin;
    workplane["axis"]   = workplaneAxis;
    workplane["angle"]  = 45.0;

    mObject cuboidJson;
    cuboidJson["origin"] = origin;
    cuboidJson["parameters"] = cuboidParameters;
    cuboidJson["workplane"]  = workplane;
    
    CuboidBuilder cuboidBuilder(cuboidJson);
    ASSERT_FALSE(cuboidBuilder.shape().IsNull());

}


TEST(BuilderTest, Fillet) {
    
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    
    mObject cuboidParameters;
    cuboidParameters["u"] = 10.0;
    cuboidParameters["v"] = 10.0;
    cuboidParameters["w"] = 10.0;
    
    mObject cuboidJson;
    cuboidJson["origin"] = origin;
    cuboidJson["parameters"] = cuboidParameters;
    
    CuboidBuilder cuboidBuilder(cuboidJson);
    ASSERT_FALSE(cuboidBuilder.shape().IsNull());
    
    mObject filletJSON;
    mObject filletParameters;
    filletParameters["r"] = 1.0;
    filletJSON["parameters"] = filletParameters;
    
    FilletBuilder filletBuilder(filletJSON, cuboidBuilder.shape());
    ASSERT_FALSE(filletBuilder.shape().IsNull());
    
}


TEST(BuilderTest, Boolean) {
    
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    
    mObject cuboidParameters;
    cuboidParameters["u"] = 10.0;
    cuboidParameters["v"] = 10.0;
    cuboidParameters["w"] = 5.0;
    
    mObject cuboidJson;
    cuboidJson["origin"] = origin;
    cuboidJson["parameters"] = cuboidParameters;
    
    CuboidBuilder cuboidBuilder(cuboidJson);
    
    mObject ellipseParameters;
    ellipseParameters["r1"] = 10.0;
    ellipseParameters["r2"] = 5.0;
    
    mObject ellipseJson;
    ellipseJson["origin"] = origin;
    ellipseJson["parameters"] = ellipseParameters;
    
    Ellipse2DBuilder ellipseBuilder(ellipseJson);
    
    mObject subtractJson;
    vector<TopoDS_Shape> group1;
    group1.push_back(cuboidBuilder.shape());
    group1.push_back(ellipseBuilder.shape());
    SubtractBuilder subtract1(subtractJson, group1);
    
    ASSERT_FALSE(subtract1.shape().IsNull());
    
    vector<TopoDS_Shape> group2;
    group2.push_back(ellipseBuilder.shape());
    group2.push_back(cuboidBuilder.shape());
    
    SubtractBuilder subtract2(subtractJson, group2);
    
    ASSERT_FALSE(subtract2.shape().IsNull());
    
}

TEST(BuilderTest, Boolean2) {
    
    mObject origin1, origin2;
    origin1["x"] = 0.0; origin1["y"] = 0.0; origin1["z"] = 0.0;
    origin2["x"] = 10.0; origin2["y"] = 0.0; origin2["z"] = 0.0;
    
    mObject ellipseParameters;
    ellipseParameters["r1"] = 10.0;
    ellipseParameters["r2"] = 5.0;
    
    mObject ellipseJson1;
    ellipseJson1["origin"] = origin1;
    ellipseJson1["parameters"] = ellipseParameters;
    
    mObject ellipseJson2;
    ellipseJson2["origin"] = origin2;
    ellipseJson2["parameters"] = ellipseParameters;
    
    
    Ellipse2DBuilder ellipseBuilder1(ellipseJson1);
    Ellipse2DBuilder ellipseBuilder2(ellipseJson2);
    
    TopoDS_Builder builderA;
    TopoDS_Compound compoundA;
    builderA.MakeCompound(compoundA);
    builderA.Add(compoundA, ellipseBuilder1.shape());
    
    TopoDS_Builder builderB;
    TopoDS_Compound compoundB;
    builderB.MakeCompound(compoundB);
    builderB.Add(compoundB, ellipseBuilder2.shape());
    
    TopoDS_Shape fuse = BRepAlgoAPI_Fuse(compoundA, compoundB);
    
    int numFaces = 0;
    for (TopExp_Explorer faces(fuse, TopAbs_FACE); faces.More(); faces.Next()) { 
        ++numFaces;
    }
    ASSERT_EQ(3, numFaces);
    
}

TEST(BuilderTest, Boolean3) {
    
    mObject originA, originB, originC, originD;
    originA["x"] = 0.0; originA["y"] = 0.0;  originA["z"] = 0.0;
    originB["x"] = 0.0; originB["y"] = 15.0; originB["z"] = 0.0;
    originC["x"] = 0.0; originC["y"] = 5.0;  originC["z"] = 0.0;
    originD["x"] = 0.0; originD["y"] = 20.0; originD["z"] = 0.0;
    
    mObject ellipseParameters;
    ellipseParameters["r1"] = 20.0;
    ellipseParameters["r2"] = 10.0;
    
    mObject ellipseAJson;
    ellipseAJson["origin"] = originA;
    ellipseAJson["parameters"] = ellipseParameters;
    
    mObject ellipseBJson;
    ellipseBJson["origin"] = originB;
    ellipseBJson["parameters"] = ellipseParameters;
    
    mObject ellipseCJson;
    ellipseCJson["origin"] = originC;
    ellipseCJson["parameters"] = ellipseParameters;
    
    mObject ellipseDJson;
    ellipseDJson["origin"] = originA;
    ellipseDJson["parameters"] = ellipseParameters;
    
    Ellipse2DBuilder ellipseBuilderA(ellipseAJson);
    Ellipse2DBuilder ellipseBuilderB(ellipseBJson);
    Ellipse2DBuilder ellipseBuilderC(ellipseCJson);
    Ellipse2DBuilder ellipseBuilderD(ellipseDJson);
    
    vector<TopoDS_Shape> group1;
    group1.push_back(ellipseBuilderA.shape());
    group1.push_back(ellipseBuilderB.shape());
    
    vector<TopoDS_Shape> group2;
    group2.push_back(ellipseBuilderC.shape());
    group2.push_back(ellipseBuilderD.shape());
    
    mObject subtractJson;
    SubtractBuilder subtract1(subtractJson, group1);
    SubtractBuilder subtract2(subtractJson, group2);
    
    ASSERT_EQ(TopAbs_COMPOUND, subtract1.shape().ShapeType());
    
    mObject prismOrigin;
    prismOrigin["x"] = 0.0; prismOrigin["y"] = 0.0; prismOrigin["z"] = 0.0;
    
    mObject prismParameters;
    prismParameters["u"] = 0.0; prismParameters["v"] = 0.0; prismParameters["w"] = 10.0;
    
    mObject prismJson;
    prismJson["origin"] = prismOrigin;
    prismJson["parameters"] = prismParameters;
    
    PrismBuilder prism1(prismJson, subtract1.shape());
    PrismBuilder prism2(prismJson, subtract2.shape());
    
    ASSERT_FALSE(prism1.shape().IsNull());
    ASSERT_FALSE(prism2.shape().IsNull());
    BRepAlgoAPI_Fuse(prism1.shape(), prism2.shape());
    
    
    vector<TopoDS_Shape> finalGroup;
    finalGroup.push_back(prism1.shape());
    finalGroup.push_back(prism2.shape());
    
    
    IntersectBuilder final(subtractJson, finalGroup);
    
    ASSERT_FALSE(final.shape().IsNull());
    
}

TEST(BuilderTest, Text) {
    
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    mObject parameters;
    parameters["text"] = "12 3";
    parameters["font"] = "OpenSans";
    
    
    mObject json;
    json["origin"] = origin;
    json["parameters"] = parameters;
    
    Text2DBuilder builder(json);
    
    TopoDS_Shape shape = builder.shape();
    
    ASSERT_FALSE(shape.IsNull());
    
    
}
