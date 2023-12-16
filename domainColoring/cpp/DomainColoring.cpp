// g++ -O3 -c DomainColoring.cpp

#include "DomainColoring.h"

extern "C"
{
  DomainColoring *dcCreate(uint32_t *image, int w, int h, char *expr)
  {
    return new DomainColoring(image, w, h, string(expr));
  }
  // f32image mus be w*h*3 size
  DomainColoring *dcCreatef32(float *f32image, int w, int h, char *expr)
  {
    return new DomainColoring(f32image, w, h, string(expr));
  }

  bool dcGetError(DomainColoring *dc) { return dc->getError(); }

  void dcGenImage(DomainColoring *dc)
  {
    dc->genImage();
  }
  void dcGenImageMT(DomainColoring *dc)
  {
    dc->genImageMT();
  }

  void dcGenImageMTf32(DomainColoring *dc)
  {
    dc->genImageMTf32();
  }

  void dcFree(DomainColoring *dc) {
	delete dc;
  }
}

/*
#include <iostream>
int main()
{
  int w = 1024 * 2, h = w;
  uint32_t *image = new uint32_t[w * h];
  DomainColoring dc(image, w, h, "acos((1+i)*log(sin(z^3-1)/z))");

  if (dc.getError())
    std::cout << "syntax error: " << dc.getErrorMsg();
  else
    dc.genImage();

  delete image;
  return 0;
}
*/
