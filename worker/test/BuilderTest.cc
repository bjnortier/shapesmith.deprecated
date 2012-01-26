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

