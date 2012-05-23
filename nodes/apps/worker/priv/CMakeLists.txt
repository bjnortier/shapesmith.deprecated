cmake_minimum_required (VERSION 2.6)
project (worker)

# The version number.
set (worker_VERSION_MAJOR 0)
set (worker_VERSION_MINOR 2)

# configure a header file to pass some of the CMake settings
# to the source code
configure_file (
  "${PROJECT_SOURCE_DIR}/WorkerConfig.h.in"
  "${PROJECT_BINARY_DIR}/WorkerConfig.h"
  )

# add the binary tree to the search path for include files
# so that we will find TutorialConfig.h
include_directories("${PROJECT_BINARY_DIR}")
include_directories("include")	
include_directories("/usr/include/oce")
include_directories("/usr/local/include")
include_directories("/usr/local/include/oce")
include_directories("/usr/local/include/freetype2")
include_directories("/usr/local/inc")
include_directories("/opt/local/include")
include_directories("/opt/local/include/freetype2/")
include_directories("/usr/include/freetype2/")
include_directories("/usr/X11R6/include")
include_directories("/usr/X11R6/include/freetype2")
include_directories("/usr/X11/include")
include_directories("/usr/X11/include/freetype2")

set(ALL_CLASSES src/worker.cxx src/Builder.cxx src/Transform.cxx src/Util.cxx src/Tesselate.cxx src/base64.cpp)

add_executable(worker  src/worker.cxx ${ALL_CLASSES})

# Unit tests
enable_testing()

# http://www.cmake.org/cmake/help/cmake-2-8-docs.html
find_package(GTest REQUIRED)

add_executable(run-tests test/BuilderTest.cc src/Builder.cxx src/Transform.cxx src/Tesselate.cxx src/Util.cxx src/base64.cpp)
GTEST_ADD_TESTS(run-tests "" test/BuilderTest.cc)
target_link_libraries(run-tests ${GTEST_BOTH_LIBRARIES})

set(CMAKE_CXX_FLAGS "-DHAVE_CONFIG_H")

set(ALL_LIBS -L/usr/local/lib -L/opt/local/lib -L/usr/X11R6/lib -L/usr/X11/lib -ljson_spirit -lpthread -lfreetype -lPTKernel -lTKAdvTools -lTKBin -lTKBinL -lTKBinTObj -lTKBinXCAF -lTKBO -lTKBool -lTKBRep -lTKCAF -lTKCDF -lTKernel -lTKFeat -lTKFillet -lTKG2d -lTKG3d -lTKGeomAlgo -lTKGeomBase -lTKHLR -lTKIGES -lTKLCAF -lTKMath -lTKMesh -lTKMeshVS -lTKNIS -lTKOffset -lTKOpenGl -lTKPCAF -lTKPLCAF -lTKPrim -lTKPShape -lTKService -lTKShapeSchema -lTKShHealing -lTKStdLSchema  -lTKStdSchema -lTKSTEP -lTKSTEP209 -lTKSTEPAttr -lTKSTEPBase -lTKSTL -lTKTopAlgo -lTKV2d -lTKV3d -lTKXMesh -lTKXml -lTKXmlL -lTKXmlTObj -lTKXmlXCAF -lTKXSBase)

target_link_libraries(worker ${ALL_LIBS})
target_link_libraries(run-tests ${ALL_LIBS})

file(COPY "${PROJECT_SOURCE_DIR}/fonts" DESTINATION "${PROJECT_BINARY_DIR}")



