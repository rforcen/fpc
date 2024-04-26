// fpc interface for convex_hull
/*
 on windows : 
 
g++ -static -shared -O3 -o quickHull3d.dll Face.cpp FaceList.cpp HalfEdge.cpp Point3d.cpp QuickHull3D.cpp  Vector3d.cpp Vertex.cpp VertexList.cpp Waterman.cpp fpc_interface.cpp

*/

#include "QuickHull3D.h"
#include "Point3d.h"

extern "C"
{

  void*newConvex(size_t n_vertices, float *vertices) {
    vector<double> vv; // copy vv=vertices
    for (size_t i = 0; i < n_vertices * 3; i++)
      vv.push_back(vertices[i]);

    return new QuickHull3D(vv);
  }

  void deleteConvex(QuickHull3D*qh) {
    if (qh!=NULL) delete qh;
  }

  void getSizes(QuickHull3D*qh, size_t&nFaces, size_t&nVertexes) {
    auto faces = qh->getFaces(); // faces & vertexes
    // faces list
    nFaces = 0;

    for (auto f : faces) // count faces
      nFaces += f.size() + 1;

    nVertexes = qh->getNumVertices();
  }

  void getFaces(QuickHull3D*qh, int*oFaces) { // oFaces should have been allocated
    auto faces = qh->getFaces();

    // populate o_face: |n_intems|items,.,,.
    size_t i = 0;
    for (auto f : faces)
    {
      oFaces[i++] = f.size();
      for (auto ix : f)
        oFaces[i++] = ix;
    }
  }

  void getVertexes(QuickHull3D*qh, float*oVertices) {
    auto i=0;
    for (auto c : qh->getScaledVertex())
     oVertices[i++]=(float)c;
  }

}
