unit common;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  int = integer;
  TPint = ^integer;
  TVec12i = array[0..11] of int;
  TVec2ds = array of array of single;

  vec3Func = function(x, y, z: single): single;
  TVec3CFunc = function(x, y, z: double): double; cdecl;

  TPPoint = record   // c ffi interface
    x, y, z: double;
  end;

  TPTriangle = record
    i1, i2, i3: uint32;
  end;

  TPVertex = record
    position, normal: TPPoint;
  end;

  TPVertices = record
    Count, max: integer;
    vertex: ^TPVertex;
  end;

  TPTriangles = record
    Count, max: integer;
    trig: ^TPTriangle;
  end;


implementation

end.
