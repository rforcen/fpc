{ mesh used as opencl float4 }
unit mesh;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TVertex = packed record
    x, y, z, w: single;
  end; // float4

  TMesh = packed record
    position, normal, color, texture: TVertex;
  end;
  TMeshes = array of TMesh;
  TFace = array of integer;
  TFaces = array of TFace;

function generateFaces(n: integer): TFaces; // for a n x n spherical mesh
procedure WriteWRL(const mesh: TMeshes; const faces: TFaces; const Name: string);

{------------}

implementation

function generateFaces(n: integer): TFaces; // for a n x n spherical mesh
var
  i, j: integer;

  procedure addFace(const _face: TFace);
  begin
    insert(_face, Result, high(Result));
  end;

begin
  Result := nil;

  for i := 0 to n - 2 do
  begin
    for j := 0 to n - 2 do
      addFace([(i + 1) * n + j, (i + 1) * n + j + 1, i * n + j + 1, i * n + j]);
    addFace([(i + 1) * n, (i + 1) * n + n - 1, i * n, i * n + n - 1]);
  end;

  for i := 0 to n - 2 do
    addFace([i, i + 1, n * (n - 1) + i + 1, n * (n - 1) + i]);
end;

procedure WriteWRL(const mesh: TMeshes; const faces: TFaces; const Name: string);
var
  f: TextFile;
  s: TMesh;
  p: TVertex;
  i, j: integer;
begin
  AssignFile(f, Name);
  Rewrite(f);

  if IOResult = 0 then
  begin
    WriteLn(f, '#VRML V2.0 utf8');
    WriteLn(f, '');
    WriteLn(f, '# Spherical Harmonics:');
    WriteLn(f, '');
    WriteLn(f, '# lights on');
    WriteLn(f, 'DirectionalLight {  direction -.5 -1 0   intensity 1  color 1 1 1 }');
    WriteLn(f, 'DirectionalLight {  direction  .5  1 0   intensity 1  color 1 1 1 }');
    WriteLn(f, '');
    WriteLn(f, 'Shape {');
    WriteLn(f, '    # default material');
    WriteLn(f, '    appearance Appearance {');
    WriteLn(f, '        material Material { }');
    WriteLn(f, '    }');
    WriteLn(f, '    geometry IndexedFaceSet {');
    WriteLn(f, '        coord Coordinate {');
    WriteLn(f, '            point [');

    for s in mesh do
    begin
      p := s.position;
      WriteLn(f, Format('                %f %f %f', [p.x, p.y, p.z]));
    end;

    WriteLn(f, '            ]');
    WriteLn(f, '        }');
    WriteLn(f, '        color Color {');
    WriteLn(f, '            color [');

    for s in mesh do
    begin
      p := s.color;
      WriteLn(f, Format('                %f %f %f', [p.x, p.y, p.z]));
    end;

    WriteLn(f, '            ]');
    WriteLn(f, '        }');
    WriteLn(f, '        normal Normal {');
    WriteLn(f, '            vector [');

    // Normals
    for s in mesh do
    begin
      p := s.normal;
      WriteLn(f, Format('                %f %f %f', [p.x, p.y, p.z]));
    end;

    WriteLn(f, '            ]');
    WriteLn(f, '        }');
    WriteLn(f, '        coordIndex [');

    // Faces
    for i := 0 to High(faces) do
    begin
      for j := 0 to High(faces[i]) do
        Write(f, Format('%d ', [faces[i][j]]));
      WriteLn(f, '-1,');
    end;

    WriteLn(f, '        ]');
    WriteLn(f, '        colorPerVertex TRUE');
    WriteLn(f, '        convex TRUE');
    WriteLn(f, '        solid TRUE');
    WriteLn(f, '    }');
    WriteLn(f, '}');
  end;

  CloseFile(f);
end;

end.
