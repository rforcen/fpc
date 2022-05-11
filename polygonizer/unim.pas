{nim interface}
unit uNim;
{$mode ObjFPC}{$H+}

// link with 'c' and libengine.a
{$linklib c}
{$linklib nim/polygonizer.a}
{$linklib openctm}

interface

uses
  Classes, SysUtils, v3;


{ libpolygonizer.a interface }
const
  FuncNames: array of string =
    ('Sphere', 'Blob', 'NordstarndWeird', 'DecoCube', 'Cassini',
    'Orth', 'Orth3', 'Pretzel', 'Tooth', 'Pilz', 'Bretzel', 'BarthDecic',
    'Clebsch0', 'Clebsch', 'Chubs', 'Chair', 'Roman', 'TangleCube', 'Goursat', 'Sinxyz');

type
  TPolygonizer = pointer;

  TVertex = record
    pos, norm, uv, color: vec3;
  end;


function newPolygonizer(bounds: single; idiv: integer; funcidx: integer): TPolygonizer;
  cdecl; external;
procedure freePolygonizer(var polyg: TPolygonizer); cdecl; external;
procedure writeCTM(polyg: TPolygonizer; Name: PChar); cdecl; external;
function getNVertex(polyg: TPolygonizer): integer; cdecl; external;
function getNTrigs(polyg: TPolygonizer): integer; cdecl; external;
procedure getMesh(polyg: TPolygonizer; vertexes: pointer; trigs: pointer);
  cdecl; external;


implementation

end.

//procedure TForm1.doPoly_nim;
//begin
//  resol := seResol.Value;
//  nfunc := lbNames.ItemIndex;
//  if nfunc = -1 then nfunc := 0;
//  bounds := fsBounds.Value;
//  if bounds = 0 then bounds := 0.1;

//  if polyg <> nil then
//    freePolygonizer(polyg);

//  t0 := now;

//  polyg := newPolygonizer(bounds, resol, nfunc);
//  setLength(vertexes, getNVertex(polyg));
//  setLength(trigs, getNTrigs(polyg));
//  getMesh(polyg, @vertexes[0], @trigs[0]);

//  lap := MilliSecondsBetween(now, t0);

//  dispStat;

//  OpenGLControl1.Invalidate;
//end;
