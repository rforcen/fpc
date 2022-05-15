{nim interface}
unit uNim;

{$mode ObjFPC}{$H+}

// link with 'c' and libengine.a
{$linklib c}
{$linklib nim/polygonizer.a}
{$linklib openctm}

interface

uses
  Classes, SysUtils, v3, common, uMesh;


{ libpolygonizer.a interface }
const
  FuncNames: array of string =
    ('Sphere', 'Blob', 'NordstarndWeird', 'DecoCube', 'Cassini',
    'Orth', 'Orth3', 'Pretzel', 'Tooth', 'Pilz', 'Bretzel', 'BarthDecic',
    'Clebsch0', 'Clebsch', 'Chubs', 'Chair', 'Roman', 'TangleCube', 'Goursat', 'Sinxyz');

procedure polygonizer_Nim(func: vec3Func; bounds: single; resol: int; var mesh: TMesh);

implementation

type
  TPolygonizerNim = pointer;

function CreatePolygonizer(bounds: single; idiv: integer;
  func: vec3Func): TPolygonizerNim;
  cdecl; external;
procedure freePolygonizer(var polyg: TPolygonizerNim); cdecl; external;
procedure writeCTM(polyg: TPolygonizerNim; Name: PChar); cdecl; external;
function getNVertex(polyg: TPolygonizerNim): integer; cdecl; external;
function getNTrigs(polyg: TPolygonizerNim): integer; cdecl; external;
procedure getMesh(polyg: TPolygonizerNim; vertexes: pointer; trigs: pointer);
  cdecl; external;

procedure polygonizer_Nim(func: vec3Func; bounds: single; resol: int; var mesh: TMesh);
var
  polyg: TPolygonizerNim;
  vertexes: array of TVertex = nil;
  vtx: TVertex;
  trigs: array of TTrig = nil;
  i: int;
begin
  if bounds = 0 then bounds := 0.1;

  polyg := CreatePolygonizer(bounds, resol, func);

  setLength(vertexes, getNVertex(polyg));
  setLength(trigs, getNTrigs(polyg));

  getMesh(polyg, @vertexes[0], @trigs[0]);

  mesh.shape.Clear;
  mesh.trigs.clear;

  mesh.shape.Reserve(length(vertexes));
  for i := 0 to high(vertexes) do
  begin
    vtx := vertexes[i];
    mesh.shape.Append(mkVertex(vtx.pos, vtx.norm, mkvec3(0, 0, 0), vtx.color));
  end;

  mesh.trigs.Reserve(length(trigs));
  for i := 0 to high(trigs) do
    mesh.trigs.Append(trigs[i]);
end;



end.
