{ wrapping interface with fpc_interface.cpp }

unit iWaterman;

{$mode ObjFPC}{$H+}
{$linklib quickHull3d.dll}

interface

uses
  Classes, SysUtils, glCommon;

function quickHull3d(const verticesIn: TVertexes): TPolyhedron;

implementation

type
  size_t = int64;


  // fpc_interface.cpp wrap

function newConvex(const n_vertices: size_t; const vertices: psingle): pointer;
  cdecl; external;
procedure deleteConvex(qh: pointer); cdecl; external;

procedure getSizes(qh: pointer; var nFaces: size_t; var nVertexes: size_t);
  cdecl; external;
procedure getFaces(qh: pointer; oFaces: pinteger); cdecl; external;
procedure getVertexes(qh: pointer; oVertexes: psingle); cdecl; external;

{--------------}
function flatFaces(const faces: TFace): TFaces;
var
  i, n: integer;
  face: TFace = nil;
begin
  Result := nil;
  i := 0;

  while i < length(faces) do   // #items, items..., #items, items...
  begin
    n := faces[i]; // n_items
    face := copy(faces, i + 1, n);
    insert(face, Result, 0);

    Inc(i, n + 1);
  end;
end;

function quickHull3d(const verticesIn: TVertexes): TPolyhedron;
var
  qh: pointer;
  faces: TFace = nil;
  nVertexes: size_t = 0;
  nFaces: size_t = 0;
begin
  qh := newConvex(length(verticesIn), @verticesIn[0]);

  getSizes(qh, nFaces, nVertexes);

  Result.vertexes := nil;
  setLength(Result.vertexes, nVertexes);
  getVertexes(qh, @Result.vertexes[0]);

  setLength(faces, nFaces);
  getFaces(qh, @faces[0]);

  Result.faces := flatFaces(faces);
  Result.calcNormals;

  deleteConvex(qh);

end;

end.
