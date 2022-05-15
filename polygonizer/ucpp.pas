{c interface for polygonizer.c }
unit uCpp;

{$mode ObjFPC}{$H+}
{$link cpp/polygonizer.o}// link with 'c' polygonizer.o

interface

uses
  Classes, SysUtils, Common, uMesh, v3, Math;

procedure _polygonize_C(vec3func: TVec3CFunc; bounds, size: single; var mesh: TMesh);

implementation

{ polygonizer.cpp interface }
procedure polygonize(vec3func: TVec3CFunc; bounds, size, x, y, z: double);
  cdecl; external;
function blob(x, y, z: double): double; cdecl; external;

var
  gvertices: TPVertices;  external;
  gtriangles : TPTriangles; external;
  gntris : integer; external;


procedure _polygonize_C(vec3func: TVec3CFunc; bounds, size: single; var mesh: TMesh);
var
  i, nv: integer;
  vtx: ^TPVertex;
  tri: ^TPTriangle;
  pos: vec3;
  mx: single = -1e32;

begin
  polygonize(@blob, 60, 0.05, 0, 0, 0);   // 60, 0.05

  mesh.shape.Clear;
  vtx := gvertices.vertex;
  nv := gvertices.Count;

  for i := 0 to nv - 1 do
  begin
    pos := mkvec3(vtx^.position.x, vtx^.position.y, vtx^.position.z);
    mx := max(mx, maxabsvec3(pos));
    Inc(vtx);
  end;

  vtx := gvertices.vertex;
  for i := 0 to nv - 1 do
  begin
    pos := mkvec3(vtx^.position.x, vtx^.position.y, vtx^.position.z) / mx;
    mesh.shape.append(mkVertex(pos, mkvec3(vtx^.normal.x, vtx^.normal.y, vtx^.normal.z),
      mkvec3(0, 0, 0), mkvec3(0.5, 0.5, 0)));
    Inc(vtx);
  end;


  mesh.trigs.Clear;

  tri := gtriangles.trig;
  for i := 0 to gtriangles.Count - 1 do
  begin
    mesh.trigs.append(mkTrig(tri^.i1, tri^.i2, tri^.i3));
    Inc(tri);
  end;
end;

end.
