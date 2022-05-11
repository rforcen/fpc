unit uMesh;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, v3, gcontnrs;

type
  int = integer;

  TTrig = record // vec3u;
    a, b, c: int;
    class operator = (x, y: TTrig): boolean;
    function toVec3u: vec3u;
  end;

  TVertex = record
    pos, norm, uv, color: vec3;
    class operator = (const a, b: TVertex): boolean;
  end;

  TVertexList = specialize TGenVector<TVertex>; // specialize TFPGlist<TVertex>;
  TTrigList = specialize TGenVector<TTrig>;

  TMesh = record
    shape: TVertexList;
    trigs: TTrigList;
  end;

  function mkVertex(pos, norm, uv, color: vec3): TVertex;
  function mkTrig(a, b, c: int): TTrig; inline;


implementation

// TTrig
class operator TTrig. = (x, y: TTrig): boolean;
begin
  Result := (x.a = y.a) and (x.b = y.b) and (x.c = y.c);
end;

function TTrig.toVec3u: vec3u; inline;
begin
  Result := mkvec3u(a, b, c);
end;

function mkTrig(a, b, c: int): TTrig; inline;
begin
  Result.a := a;
  Result.b := b;
  Result.c := c;
end;

// TVertex
function mkVertex(pos, norm, uv, color: vec3): TVertex;
begin
  Result.pos := pos;
  Result.norm := norm;
  Result.uv := uv;
  Result.color := color;
end;

class operator TVertex. = (const a, b: TVertex): boolean;
begin
  Result := (a.pos = b.pos) and (a.norm = b.norm) and (a.uv = b.uv) and
    (a.color = b.color);
end;

end.
