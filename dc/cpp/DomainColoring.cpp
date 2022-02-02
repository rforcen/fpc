// g++ -O3 -c DomainColoring.cpp

#include "DomainColoring.h"

extern "C"
{
  DomainColoring *dcCreate(uint32_t *image, int w, int h, char *expr)
  {
    return new DomainColoring(image, w, h, string(expr));
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