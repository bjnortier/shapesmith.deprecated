#include "gtest/gtest.h"
#include "Builder.h"

TEST(BuilderTest, Ellipse) {
 
    mObject origin;
    origin["x"] = 0.0; origin["y"] = 0.0; origin["z"] = 0.0;
    mObject parameters;
    parameters["r1"] = 10.0;
    parameters["r2"] = 5.0;


    mObject json;
    json["origin"] = origin;
    json["parameters"] = parameters;
    
    Ellipse1DBuilder builder(json);

    CompositeShape composite_shape = builder.composite_shape();

    ASSERT_TRUE(composite_shape.three_d_shape().IsNull());
    ASSERT_TRUE(composite_shape.two_d_shape().IsNull());
    ASSERT_FALSE(composite_shape.one_d_shape().IsNull());

    mValue tesselation = composite_shape.Tesselate();
    
    mArray positions3d = tesselation.get_obj()["3d"].get_obj()["positions"].get_array();
    mArray positions2d = tesselation.get_obj()["2d"].get_obj()["positions"].get_array();
    mArray positions1d = tesselation.get_obj()["1d"].get_obj()["positions"].get_array();

    
    ASSERT_EQ(positions3d.size(), 0);
    ASSERT_EQ(positions2d.size(), 0);
    ASSERT_NE(positions1d.size(), 0);

    
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
    
    CompositeShape composite_shape = builder.composite_shape();
    
    ASSERT_FALSE(composite_shape.three_d_shape().IsNull());
    ASSERT_TRUE(composite_shape.two_d_shape().IsNull());
    ASSERT_TRUE(composite_shape.one_d_shape().IsNull());
    
    mValue tesselation = composite_shape.Tesselate();
    
    mArray positions3d = tesselation.get_obj()["3d"].get_obj()["positions"].get_array();
    mArray positions2d = tesselation.get_obj()["2d"].get_obj()["positions"].get_array();
    mArray positions1d = tesselation.get_obj()["1d"].get_obj()["positions"].get_array();
    
    
    ASSERT_NE(positions3d.size(), 0);
    ASSERT_EQ(positions2d.size(), 0);
    ASSERT_EQ(positions1d.size(), 0);
    
    
}

