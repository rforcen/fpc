unit upoly;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Math, RegExpr, uCommon, uVec3, uPlato, uJohnson;

type
  CPoly = class
  public
    Name: string;
    faces: TFaces;
    vertexes, normals, centers, colors: TVertexes;
    areas: vecf;

    //constructor Create; overload;
    constructor Create(tpoly: TFacesVertexes); overload;
    constructor Create(_name: string; _faces: TFaces; _vertexes: TVertexes); overload;

    procedure calcNormals;
    procedure calcAreas;
    procedure calcCenters;
    procedure calcColors;
    procedure reColor;
    function avgNormals: TVertexes;
    procedure scaleVertexes;
    function centroid(face: TFace): TVertex;
    procedure normalize;
    function nVertexes: integer;
    function maxFaceIndex: integer;

    procedure calcAll;
    function check: boolean;
  end;

function pyramid(n: integer = 4): CPoly;
function prism(n: integer = 4): CPoly;
function antiprism(n: integer = 4): CPoly;
function cupola(n: integer = 3; alpha: single = 0; Height: single = 0): CPoly;
function anticupola(n: integer = 3; alpha: single = 0; Height: single = 0): CPoly;

function polyhedron(Name: char; n: integer = 0; alpha: single = 0;
  Height: single = 0): CPoly;
function polyhedron(Name: string): CPoly;

function range(left, right: integer; inclusive: boolean): TFace;

implementation

function polyhedron(Name: char; n: integer = 0; alpha: single = 0;
  Height: single = 0): CPoly;
begin
  case Name of  // TCIODPnAnUnahVnahJn
    'T': exit(CPoly.Create(Polyhedrons[0]));
    'C': exit(CPoly.Create(Polyhedrons[1]));
    'I': exit(CPoly.Create(Polyhedrons[2]));
    'O': exit(CPoly.Create(Polyhedrons[3]));
    'D': exit(CPoly.Create(Polyhedrons[4]));
    'P': exit(prism(n));
    'A': exit(antiprism(n));
    'U': exit(cupola(n, alpha, Height));
    'V': exit(anticupola(n, alpha, Height));
    'J': exit(CPoly.Create(Johnson[n]));
    else
      exit(CPoly.Create);
  end;
end;

function polyhedron(Name: string): CPoly;
var
  n: integer = 0;
  alpha, Height: single;
  re: TRegExpr;
  pt: char;

  function s2f(s: string): single;
  begin
    if not TryStrToFloat(s, Result) then
      Result := 0;
  end;

begin
  pt := Name[1];
  re := TRegExpr.Create;

  case pt of
    'T', 'C', 'I', 'O', 'D': Result := polyhedron(pt);
    'P', 'A', 'J':
    begin
      re.Expression := '(\d+)';
      if re.exec(Name) then
      begin
        n := StrToInt(re.match[1]);
        Result := polyhedron(pt, n);
      end
      else
        Result := CPoly.Create;
    end;
    'U', 'V':
    begin
      re.Expression := '(\d+)(,(([0-9]*[.])?[0-9]+))?(,(([0-9]*[.])?[0-9]+))?';
      if re.exec(Name) then
      begin
        n := StrToInt(re.match[1]);
        alpha := s2f(re.match[3]);
        Height := s2f(re.match[6]);
        Result := polyhedron(pt, n, alpha, Height);
      end
      else
        Result := CPoly.Create;
    end;
    else
      Result := CPoly.Create;
  end;
  re.Free;
end;


// CPoly

constructor CPoly.Create(tpoly: TFacesVertexes);
begin
  Name := tpoly.Name;
  faces := tpoly.faces;
  vertexes := tpoly.vertexes;
end;

constructor CPoly.Create(_name: string; _faces: TFaces; _vertexes: TVertexes);
begin
  self.Name := _name;
  self.faces := _faces;
  self.vertexes := _vertexes;
end;

procedure CPoly.calcNormals;
var
  nface: integer;
  face: TFace;
begin
  if length(normals) <> length(faces) then
  begin
    setLength(normals, length(faces));

    nface := 0;
    for face in faces do
    begin
      normals[nface] := normal(vertexes[face[0]], vertexes[face[1]], vertexes[face[2]]);
      Inc(nface);
    end;

  end;
end;

procedure CPoly.calcAreas;
var
  fc, f, fl: integer;
  face: TFace;
  v1, v2: TVertex;
  vsum: TVertex = (0, 0, 0);
begin

  if length(areas) <> length(faces) then
  begin
    if length(normals) <> length(faces) then
      calcNormals; // requires normals

    setLength(areas, length(faces));

    f := 0;
    for face in faces do
    begin
      zero(vsum);

      fl := length(face);
      v1 := vertexes[face[fl - 2]];
      v2 := vertexes[face[fl - 1]];

      for fc in face do
      begin
        vsum += cross(v1, v2);
        v1 := v2;
        v2 := vertexes[fc];
      end;
      areas[f] := abs(dot(normals[f], vsum)) / 2;
      Inc(f);
    end;

  end;

end;

procedure CPoly.calcCenters;
var
  nface, i: integer;
  fcenter: TVertex = (0, 0, 0);
  face: TFace;
begin
  if length(centers) <> length(faces) then
  begin
    setLength(centers, length(faces));

    nface := 0;
    for face in faces do
    begin
      zero(fcenter);
      for i in face do
        fcenter += vertexes[i];

      centers[nface] := fcenter / length(face);
      Inc(nface);
    end;

  end;
end;

procedure CPoly.reColor;
begin
  setLength(colors, 0);
  calcColors;
end;

procedure CPoly.calcColors;

  function sigDigs(f: single; nsigs: integer = 2): integer;
  var
    mant: single;
  begin
    //Result := floor(f * 100);
    if f = 0 then
      exit(0);
    mant := f / power(10, floor(log10(f)));
    Result := integer(floor(mant * power(10, (nsigs - 1))));
  end;

const
  nColors = 256;
type
  TColorDict = specialize TFPGmap<integer, TVertex>;

var
  colorDict: TColorDict;
  area: single;
  colPalette: TVertexes;
  i: integer = 0;

begin
  if length(colors) <> length(faces) then
  begin
    calcAreas;  // requires areas

    colPalette := randomPalette(nColors);

    colorDict := TColorDict.Create;
    colorDict.Duplicates := dupIgnore;

    for area in areas do
      colorDict.add(sigDigs(area), colPalette[colorDict.Count mod nColors]);

    setLength(colors, length(areas));
    for area in areas do
    begin
      colors[i] := colorDict[sigDigs(area)];
      Inc(i);
    end;

    colorDict.Free;

  end;
end;

procedure CPoly.calcAll;
begin
  calcNormals;
  calcAreas;
  calcCenters;

  calcColors;
end;

function CPoly.avgNormals: TVertexes;
var
  face: TFace;
  ix, ixf: integer;
  normalV, v1, v2, v3: TVertex;
begin
  ixf := 0;

  Result := nil;
  setlength(Result, length(faces));

  for face in faces do
  begin

    normalV := 0;
    v1 := vertexes[face[length(face) - 2]];
    v2 := vertexes[face[length(face) - 1]];

    for ix in face do
    begin
      v3 := vertexes[ix];
      normalV += normal(v1, v2, v3);
      v1 := v2;
      v2 := v3;  // shift over one
    end;

    Result[ixf] := unitv(normalV); // normalize
    Inc(ixf);
  end;

end;

procedure CPoly.scaleVertexes;
var
  mx: single;
  v: TVertex;
  i: integer;
begin
  mx := -single.MaxValue;
  for v in vertexes do
    mx := max(mx, maxAbs(v));

  if mx <> 0 then
    for i := 0 to high(vertexes) do
      vertexes[i] /= mx;
end;

function CPoly.centroid(face: TFace): TVertex;
var
  ic: integer;
begin
  Result := 0;  // calc centroid of face
  for ic in face do
    Result += vertexes[ic];
  Result /= length(face);
end;

// calculated poly's (pyramid, prism, antiprism, cupola, anticupola)

function pyramid(n: integer = 4): CPoly;
var
  theta: single; // pie angle
  Height: single = 1;
  vertexes: TVertexes = nil;
  faces: TFaces = nil;
  i: integer;

begin
  theta := (2 * PI) / n;

  setLength(vertexes, n + 1);
  for i := 0 to n - 1 do
    vertexes[i] := makeVertex(-cos(i * theta), -sin(i * theta), -0.2);
  vertexes[n] := makeVertex(0, 0, Height); // apex

  setLength(faces, n + 1);
  faces[0] := range(n - 1, 0, True); // base
  for i := 0 to n - 1 do            // n triangular sides
    faces[i + 1] := makeFace([i, (i + 1) mod n, n]);

  Result := CPoly.Create('Y' + IntToStr(n), faces, vertexes);
end;

function prism(n: integer = 4): CPoly;
var
  theta, h: single;
  vertexes: TVertexes = nil;
  faces: TFaces = nil;
  i: integer;
begin
  theta := (2 * PI) / n; // pie angle
  h := sin(theta / 2);      // half-edge

  setLength(vertexes, n + n);
  for i := 0 to n - 1 do
    vertexes[i] := makeVertex(-cos(i * theta), -sin(i * theta), -h);
  for i := 0 to n - 1 do
    vertexes[i + n] := makeVertex(-cos(i * theta), -sin(i * theta), h);

  // # vertex #'s 0 to n-1 around one face, vertex #'s n to 2n-1 around other

  setLength(faces, 2 + n);
  faces[0] := range(n - 1, 0, True);
  faces[1] := range(n, 2 * n, False);

  for i := 0 to n - 1 do
    faces[i + 2] := makeFace([i, (i + 1) mod n, ((i + 1) mod n) + n, i + n]);

  Result := CPoly.Create('P' + IntToStr(n), faces, vertexes);
end;

function antiprism(n: integer = 4): CPoly;
var
  theta, h, r, f: single;
  vertexes: TVertexes = nil;
  faces: TFaces = nil;
  i: integer;
begin
  theta := (2 * PI) / n; // pie angle
  h := sqrt(1 - (4 / ((4 + (2 * cos(theta / 2))) - (2 * cos(theta)))));
  r := sqrt(1. - (h * h));
  f := sqrt((h * h) + power(r * cos(theta / 2), 2));
  // correction so edge midpoints (not vertexes) on unit sphere
  r := -r / f;
  h := -h / f;

  setLength(vertexes, n + n);
  for i := 0 to n - 1 do
    vertexes[i] := makeVertex(r * cos(i * theta), r * sin(i * theta), h);
  for i := 0 to n - 1 do
    vertexes[i + n] := makeVertex(r * cos((i + 0.5) * theta), r * sin(
      (i + 0.5) * theta), -h);

  faces := nil;
  setLength(faces, 2 + n * 2);
  faces[0] := range(n - 1, 0, True);
  faces[1] := range(n, (2 * n) - 1, True); // top
  for i := 0 to n - 1 do
  begin // 2n triangular sides
    faces[i * 2 + 2] := makeFace([i, (i + 1) mod n, i + n]);
    faces[i * 2 + 3] := makeFace([i, i + n, ((((n + i) - 1) mod n) + n)]);
  end;

  Result := CPoly.Create('A' + IntToStr(n), faces, vertexes);

end;

function cupola(n: integer = 3; alpha: single = 0; Height: single = 0): CPoly;
var
  s, rb, rt: single;
  vertexes: TVertexes = nil;
  faces: TFaces = nil;
  i: integer;
begin
  if n < 2 then
    exit(CPoly.Create);

  s := 1.0; // alternative face/height scaling
  rb := s / 2 / sin(PI / 2 / n);
  rt := s / 2 / sin(PI / n);

  if Height = 0 then
    Height := (rb - rt);
  // set correct height for regularity for n=3,4,5
  if (n >= 3) and (n <= 5) then
    Height := s * sqrt(1 - 1 / 4 / sin(PI / n) / sin(PI / n));
  // init 3N vertexes
  setLength(vertexes, n * 3);

  // fill vertexes
  for i := 0 to n - 1 do
  begin

    vertexes[i * 2] := makeVertex(rb * cos(PI * (2 * i) / n + PI / 2 / n + alpha),
      rb * sin(PI * (2 * i) / n + PI / 2 / n + alpha), 0.0);
    vertexes[2 * i + 1] := makeVertex(rb * cos(PI * (2 * i + 1) / n +
      PI / 2 / n - alpha), rb * sin(PI * (2 * i + 1) / n + PI / 2 / n - alpha), 0.0);
    vertexes[2 * n + i] := makeVertex(rt * cos(2 * PI * i / n), rt *
      sin(2 * PI * i / n), Height);
  end;

  setLength(faces, 2 + n * 2);
  faces[0] := range(2 * n - 1, 0, True);
  faces[1] := range(2 * n, 3 * n - 1, True); // base, top
  for i := 0 to n - 1 do
  begin
    // n triangular sides and n square sides
    faces[i * 2 + 2] := makeFace([(2 * i + 1) mod (2 * n), (2 * i + 2) mod
      (2 * n), 2 * n + (i + 1) mod n]);
    faces[i * 2 + 3] := makeFace([2 * i, (2 * i + 1) mod (2 * n),
      2 * n + (i + 1) mod n, 2 * n + i]);
  end;

  Result := CPoly.Create('U' + IntToStr(n), faces, vertexes);
end;

function anticupola(n: integer = 3; alpha: single = 0; Height: single = 0): CPoly;
var
  s, rb, rt: single;
  vertexes: TVertexes = nil;
  faces: TFaces = nil;
  i: integer;
begin
  if n < 3 then
    exit(CPoly.Create);

  s := 1.0; // alternative face/height scaling
  rb := s / 2 / sin(PI / 2 / n);
  rt := s / 2 / sin(PI / n);

  if Height = 0 then
    Height := (rb - rt);

  // init 3N vertexes
  setLength(vertexes, n * 3);

  // fill vertexes
  for i := 0 to n - 1 do
  begin

    vertexes[2 * i] :=
      makeVertex(rb * cos(PI * (2 * i) / n + alpha), rb *
      sin(PI * (2 * i) / n + alpha), 0.0);
    vertexes[2 * i + 1] :=
      makeVertex(rb * cos(PI * (2 * i + 1) / n - alpha), rb *
      sin(PI * (2 * i + 1) / n - alpha), 0.0);
    vertexes[2 * n + i] :=
      makeVertex(rt * cos(2 * PI * i / n), rt * sin(2 * PI * i / n), Height);
  end;
  // create faces
  setLength(faces, 2 + n * 3);
  faces[0] := range(2 * n - 1, 0, True);
  faces[1] := range(2 * n, 3 * n - 1, True); // base
  for i := 0 to n - 1 do
  begin
    // n triangular sides and n square sides
    faces[i * 3 + 2] := makeFace([(2 * i) mod (2 * n), (2 * i + 1) mod
      (2 * n), 2 * n + (i) mod n]);
    faces[i * 3 + 3] := makeFace([2 * n + (i + 1) mod n, (2 * i + 1) mod
      (2 * n), (2 * i + 2) mod (2 * n)]);
    faces[i * 3 + 4] := makeFace([2 * n + (i + 1) mod n, 2 * n +
      (i) mod n, (2 * i + 1) mod (2 * n)]);
  end;

  Result := CPoly.Create('V' + IntToStr(n), faces, vertexes);
end;

// range

function range(left, right: integer; inclusive: boolean): TFace;
type
  TIntList = specialize TFPGlist<integer>;
var
  facel: TIntList;
  ascending: boolean;
  iend, i: integer;

begin
  facel := TIntList.Create;

  ascending := left < right;

  if not inclusive then
    iend := right
  else
  begin
    if ascending then
      iend := right + 1
    else
      iend := right - 1;
  end;

  i := left;
  if ascending then
  begin
    while i < iend do
    begin
      facel.add(i);
      Inc(i);
    end;
  end
  else
  begin
    while i > iend do
    begin
      facel.add(i);
      Dec(i);
    end;
  end;

  Result := nil;
  setLength(Result, facel.Count);
  for i := 0 to facel.Count - 1 do
    Result[i] := facel[i];

  facel.Free;
end;

function CPoly.nVertexes: integer;
var
  face: TFace;
begin
  Result := 0;
  for face in faces do // count vertexes
    Result += length(face);
end;

function CPoly.maxFaceIndex: integer;
var
  face: TFace;
  ix: integer;
begin
  Result := -1;
  for face in faces do
    for ix in face do
      Result := max(Result, ix);
end;

procedure CPoly.normalize;  // remove unused vertexes
var
  oldNew: array of integer = nil;
  face: TFace;
  ix, i, nv, nvdx: integer;
  usedVtx: TVertexes = nil;

begin
  setLength(oldNew, maxFaceIndex + 1);
  for i := 0 to high(oldNew) do oldNew[i] := -1;

  setLength(usedVtx, nVertexes);   // used vtx in faces
  nv := 0;
  nvdx := 0;
  for face in faces do
  begin
    for ix in face do
    begin
      if oldNew[ix] = -1 then
      begin
        oldNew[ix] := nvdx;
        Inc(nvdx);

        usedVtx[nv] := vertexes[ix];
        Inc(nv);
      end;
    end;
  end;

  setLength(usedVtx, nv); // reassign face index
  for ix := 0 to high(faces) do
    for i := 0 to high(faces[ix]) do
      faces[ix][i] := oldNew[faces[ix][i]];

  vertexes := usedVtx;

  setLength(centers, 0);
  setLength(normals, 0);
  setLength(colors, 0);
  setLength(areas, 0);
end;

function CPoly.check: boolean;
var
  face: TFace;
begin
  for face in faces do
    if length(face) < 3 then exit(False);
  Result := True;
end;

end.
