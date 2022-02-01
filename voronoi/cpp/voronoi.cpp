// Voronoi tiles
// g++ -O3 -c voronoi.cpp

#include "Thread.h"
#include <climits>

typedef uint32_t u32;

class Point
{
public:
  Point(u32 x, u32 y, u32 color) : x(x), y(y), color(color) {}
  static Point rand(int w, int h) { return Point(::rand() % w, ::rand() % h, 0xff000000 | (::rand() % 0xffffff)); }
  u32 x, y, color;
};

class Voronoi
{
public:
  int w, h;
  vector<Point> points;
  u32 *image = nullptr;

  Voronoi(u32 *image, int w, int h, int npts, Point *_points) : w(w), h(h), image(image)
  {
    for (auto i = 0; i < npts; i++)
      points.push_back(_points[i]);
  }

  inline int distSq(int i, int j, Point &p)
  {
    int xd = i - p.x, yd = j - p.y;
    return xd * xd + yd * yd;
  }

  void gen_pixel(int index)
  {
    auto i = index % w, j = index / w;

    int dist = INT_MAX;
    u32 color = 0;
    for (auto p : points)
    {
      int d = distSq(i, j, p);
      if (d < 3)
      {
        color = 0;
        break;
      }

      if (d < dist)
      {
        dist = d;
        color = p.color;
      }
    }
    image[index] = (color == 0) ? 0xff000000 : color;
  }

  void generate_st()
  {
    for (int index = 0; index < w * h; index++)
      gen_pixel(index);
  }

  void generate_mt()
  {
    Thread(w * h).run(
        [this](int index)
        {
          gen_pixel(index);
        });
  }
};

extern "C"
{

  void genVoronoiMT(u32 *image, u32 w, u32 h, u32 npts, Point *_points)
  {
    Voronoi(image, w, h, npts, _points).generate_mt();
  }
};

/*
void test_voronoi()
{
  const int n = 2800, n_points = n / 2;
  vector<Point> points;

  // generate random points
  for (int i = 0; i < n_points; i++)
    points.push_back(Point::rand(n, n));

  printf("voronoi %dx%d=%d, points:%d, on %d cpus...\n", n, n, n * n, n_points, Thread::nCpus());

  Voronoi voronoi = Voronoi(n, n, points);

  voronoi.generate_mt();
}
*/
