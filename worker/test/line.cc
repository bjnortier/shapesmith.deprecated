#include <gp_Pnt.hxx>
#include "gtest/gtest.h"
#include "Line.h"

// TEST(LineTest, Simple) {
//     gp_Pnt from = gp_Pnt(0,0,0);
//     gp_Pnt to   = gp_Pnt(1,1,1);    
//     Line line = Line(from, to);
 
//     ASSERT_FALSE(line.getShape().IsNull());
    
//     ASSERT_EQ(line.mesh(), 0);
    
// }

TEST(LineTest, Ellipse) {
    Ellipse ellipse = Ellipse(1.0, 1.0);
 
    ASSERT_FALSE(ellipse.getShape().IsNull());
    
    std::vector<gp_Pnt> mesh = ellipse.mesh();
    ASSERT_EQ(mesh.size(), 1);
    
}

TEST(FaceTest, Face) {
    SphericalFace spherical = SphericalFace(); 
    ASSERT_EQ(spherical.numTriangles(), 0);
}


