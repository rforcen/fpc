// transformations

unit uTrans;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, DateUtils, uCommon, uPoly, flags, fgl, uVec3;

procedure transformPoly(t: char; var poly: CPoly);
procedure transformPoly(ts: string; var poly: CPoly);
procedure randTrans(var poly: CPoly);

// transformations
procedure kisN(var poly: CPoly; n: integer = 0; apexDist: single = 0.2);
procedure ambo(var poly: CPoly);
procedure gyro(var poly: CPoly);
procedure propellor(var poly: CPoly);
procedure reflect(var poly: CPoly);
procedure dual(var poly: CPoly);
procedure chamfer(var poly: CPoly; dist: single = 0.05);
procedure whirl(var poly: CPoly);
procedure quinto(var poly: CPoly);
procedure insetN(var poly: CPoly; n: integer = 0; insetDist: single = 0.3;
  popoutDist: single = -0.1);
procedure extrudeN(var poly: CPoly; n: integer = 0);
procedure loft(var poly: CPoly; n: integer = 0; alpha: single = 0);
procedure hollow(var poly: CPoly; insetDist: single = 0.2; thickness: single = 0.1);

implementation

procedure randTrans(var poly: CPoly);
const
  tr: string = 'kagprdcwqnxlHPu';
begin
  randomize;
  while length(poly.faces) < 5000 do
    transformPoly(tr[random(length(tr) - 1) + 1], poly);
end;

procedure transformPoly(ts: string; var poly: CPoly);
var
  t: char;
begin
  for t in ts do
    transformPoly(t, poly);
end;

procedure transformPoly(t: char; var poly: CPoly);
begin
  case t of
    'k': kisN(poly);
    'a': ambo(poly);
    'g': gyro(poly);
    'p': propellor(poly);
    'r': reflect(poly);
    'd': dual(poly);
    'c': chamfer(poly);
    'w': whirl(poly);
    'q': quinto(poly);
    'n': insetN(poly);
    'x': extrudeN(poly);
    'l': loft(poly);
    //'H': hollow(poly);
    else;
  end;
end;

procedure kisN(var poly: CPoly; n: integer = 0; apexDist: single = 0.2);
var
  flag: TFlag;
  iv2, fname: Ti4;
  face: TFace;
  v1, v2, nface: integer;
  fToken: integer;
begin
  fToken := makeToken('f');

  poly.calcCenters;
  poly.calcNormals;

  flag := TFlag.Create(poly.nVertexes);
  for nface := 0 to high(poly.faces) do
  begin
    face := poly.faces[nface];
    fname := mki4(fToken, nface);

    v1 := face[high(face)];
    for v2 in face do
    begin
      iv2 := mki4(v2);
      flag.addVertex(iv2, poly.vertexes[v2]); // poly.vtx
      if (n = 0) or (length(face) = n) then
      begin
        flag.addVertex(fname, poly.centers[nface] +
          (poly.normals[nface] * apexdist));
        flag.addFace([mki4(v1), iv2, fname]);
        // raised center
      end
      else
        flag.addFace(mki4(nface), mki4(v1), mki4(v2));

      v1 := v2;
    end;
  end;

  flag.toPoly('k', poly);
  flag.Free;
end;


procedure ambo(var poly: CPoly);
var
  dualToken: integer;
  flag: TFlag;

  face: TFace;
  v1, v2, v3, flen: integer;
  m12, m23: Ti4;
  fOrigin: TVecInt4 = nil;
  io: integer = 0;
begin
  dualToken := makeToken('d');
  flag := TFlag.Create(poly.nVertexes);

  for face in poly.faces do
  begin
    flen := length(face);
    v1 := face[flen - 2];
    v2 := face[flen - 1];

    setLength(fOrigin, flen);
    io := 0;

    for v3 in face do
    begin
      m12 := i4Min(v1, v2);
      m23 := i4Min(v2, v3);

      if v1 < v2 then
        flag.addVertex(m12, midpoint(poly.vertexes[v1], poly.vertexes[v2]));

      // One whose face corresponds to the original f:
      fOrigin[io] := m12;
      Inc(io);

      // Another flag whose face  corresponds to (the truncated) v2:
      flag.addFace(mki4(dualToken, v2), m23, m12);

      // shift over one
      v1 := v2;
      v2 := v3;
    end;

    flag.addFace(fOrigin);
  end;
  flag.toPoly('a', poly);
  flag.Free;

end;

procedure gyro(var poly: CPoly);
var
  cntrToken: integer;
  flag: TFlag;

  face: TFace;
  nface: integer;
  v1, v2, v3, flen: integer;

begin
  cntrToken := makeToken('c');

  flag := TFlag.Create(poly.vertexes, poly.nVertexes);

  poly.calcCenters;

  nface := 0;
  for face in poly.faces do
  begin
    flen := length(face);
    v1 := face[flen - 2];
    v2 := face[flen - 1];

    flag.addVertex(mki4(cntrToken, nface), poly.centers[nface]);

    for v3 in face do
    begin
      flag.addVertex(
        mki4(v1, v2),
        oneThird(poly.vertexes[v1], poly.vertexes[v2])); // new v in face

      // 5 new faces
      flag.addFace(
        [mki4(cntrToken, nface), mki4(v1, v2), mki4(v2, v1), mki4(v2),
        mki4(v2, v3)]);

      // shift over one
      v1 := v2;
      v2 := v3;
    end;

    Inc(nface);
  end;

  flag.toPoly('g', poly);
  flag.Free;

end;


procedure propellor(var poly: CPoly);
var
  flag: TFlag;

  face: TFace;
  nface: integer;
  v1, v2, v3, flen: integer;

begin
  flag := TFlag.Create(poly.vertexes, poly.nVertexes);

  nface := 0;
  for face in poly.faces do
  begin
    flen := length(face);
    v1 := face[flen - 2];
    v2 := face[flen - 1];

    for v3 in face do
    begin
      flag.addVertex(
        mki4(v1, v2),
        oneThird(poly.vertexes[v1], poly.vertexes[v2]));

      // new v in face, 1/3rd along edge
      flag.addFace(mki4(nface), mki4(v1, v2), mki4(v2, v3));

      // five new flags
      flag.addFace([mki4(v1, v2), mki4(v2, v1), mki4(v2), mki4(v2, v3)]);

      // shift over one
      v1 := v2;
      v2 := v3;
    end;

    Inc(nface);
  end;

  flag.toPoly('p', poly);
  flag.Free;

end;

procedure reflect(var poly: CPoly);

  procedure reverse(var face: TFace);
  var
    i, tmp: integer;
  begin
    for i := 0 to high(face) div 2 do
    begin
      tmp := face[i];
      face[i] := face[high(face) - i];
      face[high(face) - i] := tmp;
    end;
  end;

var
  i: integer;
begin
  for i := 0 to high(poly.vertexes) do
    poly.vertexes[i] := -poly.vertexes[i];
  for i := 0 to high(poly.faces) do
    reverse(poly.faces[i]);
  poly.Name += 'r';
end;




procedure dual(var poly: CPoly);
type
  TFaceMap = specialize TFPGmap<Ti4, integer>;

var
  faceMap: TFaceMap = nil;
  flag: TFlag;
  nface: integer;
  face: TFace;
  v1, v2: integer;

  procedure genFaceMap;
  var
    i: integer;
  begin
    faceMap := TFaceMap.Create;
    faceMap.Capacity := length(poly.faces) * 5;

    for i := 0 to high(poly.faces) do
    begin
      face := poly.faces[i];
      v1 := face[high(face)];
      for v2 in face do
      begin
        faceMap.add(mki4(v1, v2), i);
        v1 := v2;
      end;
    end;
  end;

begin
  flag := TFlag.Create(poly.nVertexes);

  genFaceMap;
  poly.calcCenters;

  for nface := 0 to high(poly.faces) do
  begin
    face := poly.faces[nface];
    v1 := face[high(face)];
    flag.addVertex(mki4(nface), poly.centers[nface]);
    for v2 in face do
    begin
      flag.addFace(mki4(v1), mki4(faceMap[mki4(v2, v1)]), mki4(nface));
      v1 := v2; // current becomes previous
    end;
  end;

  faceMap.Free;

  flag.toPoly('d', poly);
  flag.Free;

end;

procedure chamfer(var poly: CPoly; dist: single = 0.05);
var
  flag: TFlag;
  nface: integer;
  face: TFace;
  v1, v2: integer;
  v1new, v2new, facename: Ti4;
  origToken, hexToken: integer;

begin
  origToken := makeToken('o');
  hexToken := makeToken('h');

  flag := TFlag.Create(poly.nVertexes);
  poly.calcNormals;

  for nface := 0 to high(poly.faces) do
  begin
    face := poly.faces[nface];

    v1 := face[high(face)];
    v1new := mki4(nface, v1);

    for v2 in face do
    begin
      flag.addVertex(mki4(v2), poly.vertexes[v2] * (1.0 + dist));
      // Add a new vertex, moved parallel to normal.
      v2new := mki4(nface, v2);

      flag.addVertex(v2new, poly.vertexes[v2] + (poly.normals[nface] * dist * 1.5));

      // Four new flags:
      // One whose face corresponds to the original face:
      flag.addFace(mki4(origToken, nface), v1new, v2new);

      // And three for the edges of the new hexagon:
      if v1 < v2 then
        facename := mki4(hexToken, v1, v2)
      else
        facename := mki4(hexToken, v2, v1);

      flag.addFace(facename, mki4(v2), v2new);
      flag.addFace(facename, v2new, v1new);
      flag.addFace(facename, v1new, mki4(v1));

      v1 := v2;
      v1new := v2new;
    end;
  end;

  flag.toPoly('c', poly);
  flag.Free;

end;


procedure whirl(var poly: CPoly);
var
  flag: TFlag;
  nface, flen: integer;
  face: TFace;
  v1, v2, v3: integer;
  cntrToken, cToken: integer;
  v1_2: TVertex;
  cv1name, cv2name: Ti4;

begin
  cntrToken := makeToken('n');
  cToken := makeToken('c');

  flag := TFlag.Create(poly.vertexes, poly.nVertexes);
  poly.calcCenters;

  for nface := 0 to high(poly.faces) do
  begin
    face := poly.faces[nface];
    flen := length(face);
    v1 := face[flen - 2];
    v2 := face[flen - 1];

    for v3 in face do
    begin
      v1_2 := oneThird(poly.vertexes[v1], poly.vertexes[v2]);
      flag.addVertex(mki4(v1, v2), v1_2);
      // New vertices near center of face

      cv1name := mki4(cntrToken, nface, v1);
      cv2name := mki4(cntrToken, nface, v2);

      flag.addVertex(cv1name, unitv(oneThird(poly.centers[nface], v1_2)));

      // New hexagon for each original edge
      flag.addFace([cv1name, mki4(v1, v2), mki4(v2, v1), mki4(v2),
        mki4(v2, v3), cv2name]);

      // New face in center of each old face
      flag.addFace(mki4(cToken, nface), cv1name, cv2name);

      v1 := v2; // shift over one
      v2 := v3;
    end;
  end;

  flag.toPoly('w', poly);
  flag.Free;

end;


procedure quinto(var poly: CPoly);
var
  flag: TFlag;
  nface, flen: integer;
  face: TFace;
  v1, v2, v3: integer;
  t12, ti12, t23, ti23, iv2: Ti4;
  centroid: TVertex;
  vi4: array of Ti4 = nil;
  iv4: integer;
  midpt, innerpt: TVertex;
begin

  flag := TFlag.Create(poly.nVertexes);
  poly.calcCenters;

  for nface := 0 to high(poly.faces) do
  begin
    face := poly.faces[nface];
    flen := length(face);
    centroid := poly.centers[nface];

    v1 := face[flen - 2];
    v2 := face[flen - 1];

    setLength(vi4, length(face));
    iv4 := 0;

    for v3 in face do
    begin
      t12 := i4Min(v1, v2);
      ti12 := i4Min(nface, v1, v2);
      t23 := i4Min(v2, v3);
      ti23 := i4Min(nface, v2, v3);
      iv2 := mki4(v2);

      // for each face-corner, we make two new points:
      midpt := midpoint(poly.vertexes[v1], poly.vertexes[v2]);
      innerpt := midpoint(midpt, centroid);

      flag.addVertex(t12, midpt);
      flag.addVertex(ti12, innerpt);

      // and add the old corner-vertex
      flag.addVertex(iv2, poly.vertexes[v2]);

      // pentagon for each vertex in original face

      flag.addFace([ti12, t12, iv2, t23, ti23]);

      // inner rotated face of same vertex-number as original
      vi4[iv4] := ti12;
      Inc(iv4);

      // shift over one
      v1 := v2;
      v2 := v3;
    end;
    flag.addFace(vi4);
  end;

  flag.toPoly('w', poly);
  flag.Free;

end;


procedure insetN(var poly: CPoly; n: integer = 0; insetDist: single = 0.3;
  popoutDist: single = -0.1);
var
  flag: TFlag;
  nface: integer;
  face: TFace;
  v1, v2: integer;

  fToken, exToken: integer;
  foundAny: boolean = False;

begin
  flag := TFlag.Create(poly.vertexes, poly.nVertexes);
  poly.calcNormals;
  poly.calcCenters;

  fToken := makeToken('f');
  exToken := makeToken('e');

  for nface := 0 to high(poly.faces) do
  begin
    face := poly.faces[nface];
    v1 := face[high(face)];

    for v2 in face do
    begin
      if (length(face) = n) or (n = 0) then
      begin
        foundAny := True;

        flag.addVertex(mki4(fToken, nface, v2),
          tween(poly.vertexes[v2], poly.centers[nface], insetDist) +
          (poly.normals[nface] * popoutDist));

        flag.addFace([mki4(v1), mki4(v2), mki4(fToken, nface, v2),
          mki4(fToken, nface, v1)]);
        // new inset, extruded face
        flag.addFace(mki4(exToken, nface), mki4(fToken, nface, v1),
          mki4(fToken, nface, v2));
      end
      else
      begin
        flag.addFace(mki4(nface), mki4(v1), mki4(v2)); // same old flag, if non-n
      end;

      v1 := v2;
    end;

  end;

  if not foundAny then
    writeln(format('No %0d - fold components were found.', [n]));

  flag.toPoly('n', poly);
  flag.Free;

end;

procedure extrudeN(var poly: CPoly; n: integer = 0);
begin
  insetN(poly, n, 0.0, 0.1);
  poly.Name := 'x' + poly.Name;
end;

procedure loft(var poly: CPoly; n: integer = 0; alpha: single = 0);
begin
  insetN(poly, n, alpha, 0);
  poly.Name := 'l' + poly.Name;
end;

procedure hollow(var poly: CPoly; insetDist: single = 0.2; thickness: single = 0.1);
var
  flag: TFlag;
  nface: integer;
  face: TFace;
  v1, v2: integer;
  normals: TVertexes;

  finToken, fdwnToken, vToken: integer;
begin
  finToken := makeToken('f');   // tokens
  fdwnToken := makeToken('d');
  vToken := makeToken('v');

  flag := TFlag.Create(poly.vertexes, poly.nVertexes);
  normals := poly.avgNormals;
  poly.calcCenters;

  for nface := 0 to high(poly.faces) do
  begin
    face := poly.faces[nface];
    v1 := face[high(face)];

    for v2 in face do
    begin
      // new inset vertex for every vert in face
      flag.addVertex(mki4(finToken, nface, vToken, v2),
        tween(poly.vertexes[v2], poly.centers[nface], insetDist));

      flag.addVertex(mki4(fdwnToken, nface, vToken, v2),
        tween(poly.vertexes[v2], poly.centers[nface], insetDist) -
        (normals[nface] * thickness));

      flag.addFace([mki4(v1), mki4(v2), mki4(finToken, nface, vToken, v2),
        mki4(finToken, nface, vToken, v1)]);

      flag.addFace([mki4(finToken, nface, vToken, v1),
        mki4(finToken, nface, vToken, v2), mki4(fdwnToken, nface, vToken, v2),
        mki4(fdwnToken, nface, vToken, v1)]);

      v1 := v2; // current becomes previous
    end;

  end;

  flag.toPoly('H', poly);
  flag.Free;

end;

end.
