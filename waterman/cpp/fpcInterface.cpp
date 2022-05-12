// fpc interface

#include "Waterman.h"

extern "C" {

vector<vector<int>> faces;
vector<double> coords;

void generateWaterman(float radius, int &nfaces, int &nVertexes,
                      int &maxNFace) {
  WatermanPoly wp;
  QuickHull3D hull(wp.genPoly(radius));

  faces = hull.getFaces(); // faces & vertexes
  coords = hull.getScaledVertex();

  nfaces = 0; // count # item in face + len(1)
  maxNFace = 0;
  for (auto face : faces) {
    nfaces += face.size() + 1;
    maxNFace = std::max(maxNFace, (int)face.size());
  }

  nVertexes = coords.size();
}

void getWatermanMesh(int *oFaces, double *oVertex) {
  int iface = 0; // line up faces
  for (auto face : faces) {
    oFaces[iface++] = (int)face.size();
    std::copy(face.begin(), face.end(), oFaces + iface);
    iface += face.size();
  }

  std::copy(coords.begin(), coords.end(), oVertex);
}

void releaseWatermanMesh() {
  faces.clear();
  coords.clear();
}
};
