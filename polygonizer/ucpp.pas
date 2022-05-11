{c interface}
unit uCpp;

{$mode ObjFPC}{$H+}
// link with 'c' polygonizer.o

{$link cpp/polygonizer.o}

interface

uses
  Classes, SysUtils;


{ polygonizer.cpp interface }
type
  TVec3Func = function(x, y, z: double): double; cdecl;

  TPPoint = record
    x,y,z:double;
    end;

  TPTriangle = record
    i1,i2,i3:uint32;
  end;

  TPVertex = record
    position, normal:TPPoint;
    end;

  TPVertices = record
    count, max:integer;
    vertex : ^TPVertex;
  end;

  TPTriangles = record
    count, max:integer;
    trig : ^TPTriangle;
  end;

procedure polygonize(vec3func: TVec3Func; bounds, size, x, y, z: double); cdecl; external;
function blob(x, y, z: double): double; cdecl; external;

var
  gvertices: TPVertices;  external;
  gtriangles : TPTriangles; external;
  gntris : integer; external;


implementation

end.


//procedure TForm1.doPoly_cpp;
//var
//  i : integer;
//  vtx : ^TPVertex;
//  tri : ^TPTriangle;
//begin
//  polygonize(@blob, 60, 0.05, 0, 0, 0);

//  setLength(vertexes, gvertices.count);
//  setLength(trigs, gtriangles.count);

//  vtx:=gvertices.vertex;
//  for i:=0 to gvertices.count-1 do begin
//    vertexes[i].pos:=mkvec3(vtx^.position.x, vtx^.position.y, vtx^.position.z);
//    vertexes[i].norm:=mkvec3(vtx^.normal.x, vtx^.normal.y, vtx^.normal.z);
//    inc(vtx);
//  end;

//  tri := gtriangles.trig;
//  for i:=0 to gtriangles.count-1 do begin
//    trigs[i] := mkvec3u(tri^.i1, tri^.i2, tri^.i3);
//    inc(tri);
//  end;
//end;


