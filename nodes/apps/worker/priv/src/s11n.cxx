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


int main (int argc, char *argv[]) {

    string fileName = "2df0fc386d95df214f53aa25dc0c871a8b764cd3.in";
    
    
    // Read from temp file
    BRep_Builder builder;
    TopoDS_Shape shape;
    BRepTools::Read(shape, fileName.c_str(), builder);

    // Delete file
    //remove(fileName);
    
    // Mesh it
    TopExp_Explorer ex(shape, TopAbs_FACE);
    if (ex.More()) {
        BRepMesh().Mesh(shape, TRIANGLE_SIZE);
    }

    cout << "ok!\n";
    
    return 0;
}
