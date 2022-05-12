unit common;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, gcontnrs;

type
  TPint = ^integer;
  TVecInt = array of integer;
  TFace = TVecInt;
  TFaces = specialize TGenVector<TVecInt>;

  TPoint3d = record
    x, y, z: double;
  end;
  TPPoint3d = ^TPoint3d;

function mkp3d(x, y, z: double): TPoint3d;

implementation

function mkp3d(x, y, z: double): TPoint3d;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

end.
