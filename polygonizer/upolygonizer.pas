unit upolygonizer;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, implicitFuncs, v3, uMesh, Math, uCtm, contnrs, gcontnrs;

const
  FuncNames: array of string =
    ('Sphere', 'Blob', 'NordstarndWeird', 'DecoCube', 'Cassini',
    'Orth', 'Orth3', 'Pretzel', 'Tooth', 'Pilz', 'Bretzel', 'BarthDecic',
    'Clebsch0', 'Clebsch', 'Chubs', 'Chair', 'Roman', 'TangleCube', 'Goursat', 'Sinxyz');

type
  int = integer;
  TPint = ^integer;
  TVec12i = array[0..11] of int;
  TVec2ds = array of array of single;

  TEdge = record
    index, startVertexIndex, endVertexIndex: int;
    connectedEdge0, connectedEdge1: ^TEdge;
  end;

  TFace = record
    index: int;
    edges: array of ^TEdge;
    ambiguous: boolean;
  end;

  TCube = record
    index: int;
    edges: array of ^TEdge;
    faces: array of TFace;
  end;

  TLookupTable = record
    cubes: array of TCube;
  end;

  TEdgeKey = record
    i0, j0, k0, i1, j1, k1: int;
    class operator > (const a, b: TEdgeKey): boolean;
    class operator < (const a, b: TEdgeKey): boolean;
  end;

  pTEdgeKey = ^TEdgeKey;
  pTEdge = ^TEdge;

  TPolygonizer = class(TObject)
  private
    _min, _max, d, nd: vec3;
    idiv: vec3int;
    isovalue, sBounds: single;
    func:
    function(x, y, z: single): single;
    lookUpTable: TLookupTable;

  private
    procedure sample(var plane: TVec2ds; z: single);
    function calcNormal(v: vec3): vec3;

  public
    scale: single;
    mesh: TMesh;

  public
    // polygonizer methods
    constructor Create(_bounds: single; _idiv: int; _func: vec3Func);
    destructor Destroy; override;

    procedure polygonize;
    procedure scalePolygonizer;
    procedure writeCTM(Name: string);
  end;



implementation
// aux's

// TFPHashList helpers
generic function box<T>(item: T): pointer;
type
  pt = ^T;
begin
  new(pt(Result));
  pt(Result)^ := item;
end;

procedure disposeMapItems(Data, {%H-}arg: pointer);
begin
  dispose(TPint(Data));
end;

// TEdgeKey
class operator TEdgeKey. > (const a, b: TEdgeKey): boolean; inline;
begin
  if a.i0 > b.i0 then exit(True);
  if a.i0 < b.i0 then exit(False);
  if a.j0 > b.j0 then exit(True);
  if a.j0 < b.j0 then exit(False);
  if a.k0 > b.k0 then exit(True);
  if a.k0 < b.k0 then exit(False);

  if a.i1 > b.i1 then exit(True);
  if a.i1 < b.i1 then exit(False);
  if a.j1 > b.j1 then exit(True);
  if a.j1 < b.j1 then exit(False);
  if a.k1 > b.k1 then exit(True);
  if a.k1 < b.k1 then exit(False);
  Result := False;
end;

class operator TEdgeKey. < (const a, b: TEdgeKey): boolean; inline;
begin
  if a.i0 < b.i0 then exit(True);
  if a.i0 > b.i0 then exit(False);
  if a.j0 < b.j0 then exit(True);
  if a.j0 > b.j0 then exit(False);
  if a.k0 < b.k0 then exit(True);
  if a.k0 > b.k0 then exit(False);

  if a.i1 < b.i1 then exit(True);
  if a.i1 > b.i1 then exit(False);
  if a.j1 < b.j1 then exit(True);
  if a.j1 > b.j1 then exit(False);
  if a.k1 < b.k1 then exit(True);
  if a.k1 > b.k1 then exit(False);
  Result := False;
end;

operator in (i: pTEdge; a: array of pTEdge): boolean; inline;
var
  b: pTEdge;
begin
  Result := False;
  for b in a do
    if i = b then exit(True);
end;



// evaluate func
procedure TPolygonizer.sample(var plane: TVec2ds; z: single);
var
  i, j: int;
  x, y: single;
begin
  for j := 0 to idiv[1] do
  begin
    y := _min[1] + j * d[1];
    for i := 0 to idiv[0] do
    begin
      x := _min[0] + i * d[0];
      plane[j][i] := func(x, y, z);
    end;
  end;
end;


function TPolygonizer.calcNormal(v: vec3): vec3;
var
  x, y, z, l, f: single;
begin

  x := v[0];
  y := v[1];
  z := v[2];
  f := func(x, y, z);

  Result := mkvec3(-(func(x + nd[0], y, z) - f) / nd[0],
    -(func(x, y + nd[1], z) - f) / nd[1], -(func(x, y, z + nd[2]) - f) / nd[2]);

  l := distance(Result);

  if l <> 0 then Result /= l;
end;



// TFPHashList usage
//procedure testMap;
//var
//  map: TFPHashList;
//  ss: shortstring;
//  pint: ^int;
//  index:int;

//begin
//  map := TFPHashList.Create;

//  ss := newEdgeKeyss(mkvec3int(1, 2, 3), mkvec3int(4, 5, 6));
//  maAdd(ss, box(111222));

//  index := TPInt(maFind(ss))^;

//  pint := maFind(newEdgeKeyss(mkvec3int(4, 5, 6), mkvec3int(1, 2, 3)));

//  if pint <> nil then
//    index := pint^;

//  maForEachCall(@disposeMapItems, nil);

//  maClear;
//  maFree;
//end;

// polygonize
procedure TPolygonizer.polygonize;

  function newEdgeKeyss(p0, p1: vec3int): shortstring;

    function EdgeKey2ss(const edgeKey: TEdgeKey): shortstring;
    begin
      setLength(Result, sizeof(TEdgeKey));
      move(edgeKey, Result[1], sizeof(TEdgeKey));
    end;

    // edgekey
    function newEdgeKey(p0, p1: vec3int): TEdgeKey; inline;
    begin
      if p1 < p1 then
      begin
        Result.i0 := p0[0];
        Result.j0 := p0[1];
        Result.k0 := p0[2];
        Result.i1 := p1[0];
        Result.j1 := p1[1];
        Result.k1 := p1[2];
      end
      else
      begin
        Result.i0 := p1[0];
        Result.j0 := p1[1];
        Result.k0 := p1[2];
        Result.i1 := p0[0];
        Result.j1 := p0[1];
        Result.k1 := p0[2];
      end;
    end;

  begin
    Result := EdgeKey2ss(newEdgeKey(p0, p1));
  end;

  function lerp(t: single; v0, v1: vec3): vec3;
  begin
    Result := mkvec3(v0[0] + t * (v1[0] - v0[0]), v0[1] + t *
      (v1[1] - v0[1]), v0[2] + t * (v1[2] - v0[2]));
  end;

  function getConnectedEdge(e: TEdge; index: int): pTEdge; inline;
  begin
    if (index <> 0) and (index <> 1) then
      raise Exception.Create('edge index out of sBounds');

    if index = 0 then Result := e.connectedEdge0
    else
      Result := e.connectedEdge1;
  end;

  function getEdgeConnectivity(c: TCube; connectionSwitches: TVec12i): TVec12i;
  const
    initArray: TVec12i = (-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1);
  var
    faceIndex, edgeIndex: int;
    face: TFace;
    edge: TEdge;
  begin
    Result := initArray;

    for faceIndex := 0 to 6 - 1 do
    begin
      face := c.faces[faceIndex];
      if (face.ambiguous = False) and (connectionSwitches[faceIndex] <> 0) then
        raise Exception.Create('face in cube is not ambigous');

      for edgeIndex := 0 to 4 - 1 do
      begin
        edge := face.edges[edgeIndex]^;
        if getConnectedEdge(edge, 0) in face.edges then
          Result[edge.index] :=
            getConnectedEdge(edge, connectionSwitches[faceIndex])^.index;
      end;
    end;
  end;

type
  vecss = array of array of single;

var
  edgeToIndex: array[0..11] of int = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  indexMap: TFPHashList;
  upperPlane: vecss = nil;
  lowerPlane: vecss = nil;
  tmpPlane: vecss;
  eps: single;

  i, j, k, ii, cubeIndex, p2val, edgeIndex, faceIndex: int;

  x1, x2, y1, y2, z1, z2, t, d0, d1: single;

  v: vec3;

  values: array[0..7] of single;
  positionsD: array [0..7] of vec3;
  positionsI: array[0..7] of vec3int;
  cube: TCube;
  face: TFace;
  edge: TEdge;
  connectionSwitches: TVec12i;
  connectivity: TVec12i;

  index0, index1, index2: int;

  keyss: shortstring; // map support
  pIndex: TPint;

begin

  setLength(upperPlane, idiv[1] + 1, idiv[0] + 1);
  setLength(lowerPlane, idiv[1] + 1, idiv[0] + 1);

  indexMap := TFPHashList.Create;

  if isovalue = 0 then eps := 1.0e-5
  else
    eps := isovalue * 1.0e-5;

  mesh.shape.Clear; // clear mesh
  mesh.trigs.Clear;

  d := (_max - _min) / idiv;
  nd := d * 0.001;

  sample(lowerPlane, _min[2]);

  for k := 0 to idiv[2] - 1 do
  begin
    z1 := _min[2] + k * d[2];
    z2 := _min[2] + (k + 1) * d[2];

    sample(upperPlane, z2);

    for j := 0 to idiv[1] - 1 do
    begin
      y1 := _min[1] + j * d[1];
      y2 := _min[1] + (j + 1) * d[1];

      for i := 0 to idiv[0] - 1 do
      begin

        x1 := _min[0] + i * d[0];
        x2 := _min[0] + (i + 1) * d[0];

        // Set sampled function values on each corner of the cube
        values[0] := lowerPlane[j][i];
        values[1] := lowerPlane[j + 1][i];
        values[2] := lowerPlane[j + 1][i + 1];
        values[3] := lowerPlane[j][i + 1];
        values[4] := upperPlane[j][i];
        values[5] := upperPlane[j + 1][i];
        values[6] := upperPlane[j + 1][i + 1];
        values[7] := upperPlane[j][i + 1];

        // Adjust the function values which are almost same as the
        // isovalue
        for ii := 0 to 7 do
          if abs(values[ii] - isovalue) < eps then values[ii] += 10.0 * eps;


        // Calculate index into the lookup table
        cubeIndex := 0;
        p2val := 1;

        for ii := 0 to 7 do
        begin
          if values[ii] > isovalue then cubeIndex += p2val;
          p2val := p2val shl 1;
        end;

        // Skip the empty cube
        if (cubeIndex = 0) or (cubeIndex = 255) then
          continue;

        // Set up corner positions of the cube (fixed array MUCH faster than dyn)
        positionsD[0] := mkvec3(x1, y1, z1);
        positionsD[1] := mkvec3(x1, y2, z1);
        positionsD[2] := mkvec3(x2, y2, z1);
        positionsD[3] := mkvec3(x2, y1, z1);
        positionsD[4] := mkvec3(x1, y1, z2);
        positionsD[5] := mkvec3(x1, y2, z2);
        positionsD[6] := mkvec3(x2, y2, z2);
        positionsD[7] := mkvec3(x2, y1, z2);

        positionsI[0] := mkvec3int(i, j, k);
        positionsI[1] := mkvec3int(i, j + 1, k);
        positionsI[2] := mkvec3int(i + 1, j + 1, k);
        positionsI[3] := mkvec3int(i + 1, j, k);
        positionsI[4] := mkvec3int(i, j, k + 1);
        positionsI[5] := mkvec3int(i, j + 1, k + 1);
        positionsI[6] := mkvec3int(i + 1, j + 1, k + 1);
        positionsI[7] := mkvec3int(i + 1, j, k + 1);

        // Find the cube edges which have intersection points with the isosurface
        cube := lookupTable.cubes[cubeIndex];

        for edgeIndex := 0 to 12 - 1 do
        begin
          edge := cube.edges[edgeIndex]^;
          if getConnectedEdge(edge, 0) <> nil then
          begin
            keyss := newEdgeKeyss(positionsI[edge.startVertexIndex],
              positionsI[edge.endVertexIndex]);
            pIndex := indexMap.Find(keyss);

            if pIndex <> nil then
              edgeToIndex[edgeIndex] := pIndex^
            else
            begin
              t := (isovalue - values[edge.startVertexIndex]) /
                (values[edge.endVertexIndex] - values[edge.startVertexIndex]);
              v := lerp(t, positionsD[edge.startVertexIndex],
                positionsD[edge.endVertexIndex]);

              mesh.shape.Append(mkVertex(v, calcNormal(v),
                mkvec3(0, 0, 0), mkvec3(0.5, 0.5, 0)));

              edgeToIndex[edgeIndex] := mesh.shape.High;
              indexMap.Add(keyss, specialize box<int>(mesh.shape.High));
            end;
          end;
        end;
        // Resolve topological ambiguity on cube faces

        for faceIndex := 0 to 6 - 1 do
        begin
          face := cube.faces[faceIndex];
          if face.ambiguous then
          begin
            d0 := values[face.edges[0]^.endVertexIndex] -
              values[face.edges[0]^.startVertexIndex];
            d1 := values[face.edges[2]^.endVertexIndex] -
              values[face.edges[2]^.startVertexIndex];
            t := (isovalue - values[face.edges[1]^.startVertexIndex]) /
              (values[face.edges[1]^.endVertexIndex] -
              values[face.edges[1]^.startVertexIndex]);
            if t > -d0 / (d1 - d0) then connectionSwitches[faceIndex] := 1
            else
              connectionSwitches[faceIndex] := 0;
          end
          else
            connectionSwitches[faceIndex] := 0;
        end;


        // Get the connectivity graph of the cube edges and trace
        // it to generate triangles

        connectivity := getEdgeConnectivity(cube, connectionSwitches);
        for edgeIndex := 0 to 12 - 1 do
        begin
          if connectivity[edgeIndex] <> -1 then
          begin
            index0 := edgeIndex;
            index1 := connectivity[index0];
            index2 := connectivity[index1];

            mesh.trigs.append(mkTrig(edgeToIndex[index0], edgeToIndex[index1],
              edgeToIndex[index2]));

            connectivity[index0] := -1;
            connectivity[index1] := -1;

            if connectivity[index2] <> index0 then
            begin
              connectivity[index0] := index2;
              continue;
            end;
            connectivity[index2] := -1;
          end;
        end;

      end; // i
    end; // j

    // Swap the lower and upper plane
    tmpPlane := lowerPlane;
    lowerPlane := upperPlane;
    upperPlane := tmpPlane;

  end; //k

  indexMap.ForEachCall(@disposeMapItems, nil);
  indexMap.Clear;
  indexMap.Free;
end;


procedure TPolygonizer.scalePolygonizer;

var
  trig: TTrig;
  ix: int;
begin
  scale := -1e32;
  for trig in mesh.trigs do
    for ix in trig.toVec3u do
      scale := max(scale, maxvec3(mesh.shape[ix].pos));
end;

constructor TPolygonizer.Create(_bounds: single; _idiv: int; _func: vec3Func);


  function initCubes: TLookupTable;
    // cube
    function newCube(index: int): TCube;
      // faceFactory
      function createFace(faceindex, bitPatternOnCube: int;
        edges: array of pTEdge): TFace;
      const
        FACE_VERTICES: array[0..5, 0..3] of int =
          ((0, 1, 2, 3), (0, 1, 5, 4), (0, 3, 7, 4), (4, 5, 6, 7),
          (3, 2, 6, 7), (1, 2, 6, 5));
        FACE_EDGES: array[0..5, 0..3] of int =
          ((0, 1, 2, 3), (0, 9, 4, 8), (3, 11, 7, 8), (4, 5, 6, 7),
          (2, 10, 6, 11), (1, 10, 5, 9));
        EDGE_CONNECTIVITY_ON_FACE: array of array of array of int =
          (((-1, -1, -1, -1), ()), ((-1, -1, -1, 0), ()),
          ((1, -1, -1, -1), ()), ((-1, -1, -1, 1), ()), ((-1, 2, -1, -1), ()),
          ((-1, 0, -1, 2), (-1, 2, -1, 0)),
          ((2, -1, -1, -1), ()), ((-1, -1, -1, 2), ()), ((-1, -1, 3, -1), ()),
          ((-1, -1, 0, -1), ()),
          ((1, -1, 3, -1), (3, -1, 1, -1)), ((-1, -1, 1, -1), ()),
          ((-1, 3, -1, -1), ()), ((-1, 0, -1, -1), ()), ((3, -1, -1, -1), ()),
          ((-1, -1, -1, -1), ()));
        CW: int = 1;
        // CCW: int = 0;
        FACE_ORIENTATION: array [0..5] of int = (1, 0, 1, 0, 1, 0);
        // [CW, CCW, CW, CCW, CW, CCW]

      var
        bitPatternOnFace, i, vertexIndex: int;
        connectivity: array of array of int;

        function isAmbiguousBitPattern(bitPatternOnFace: int): boolean;
        begin
          Result := (bitPatternOnFace = 5) or (bitPatternOnFace = 10);
        end;

        function buildBitPatternOnFace(bitPatternOnCube, faceIndex: int): int;

          function isBitOn(bitPatternOnCube, vertexIndex: int): boolean;
          begin
            Result := (bitPatternOnCube and (1 shl vertexIndex)) <> 0;
          end;

        var
          vertexIndex: int;

        begin
          Result := 0;
          for vertexIndex := 0 to 4 - 1 do
            if isBitOn(bitPatternOnCube, FACE_VERTICES[faceIndex][vertexIndex]) then
              Result := Result or (1 shl vertexIndex);
        end;

        procedure setConnectedEdge(var e: TEdge; index: int; edge: pTEdge); inline;
        begin
          if (index <> 0) and (index <> 1) then
            raise Exception.Create('edge index out of sBounds');
          if index = 0 then e.connectedEdge0 := edge
          else
            e.connectedEdge1 := edge;
        end;

      begin

        bitPatternOnFace := buildBitPatternOnFace(bitPatternOnCube, faceIndex);

        Result.index := faceindex;
        setLength(Result.edges, 4);

        for i := 0 to 3 do
          Result.edges[i] := edges[FACE_EDGES[faceIndex][i]];

        Result.ambiguous := isAmbiguousBitPattern(bitPatternOnFace);

        connectivity := EDGE_CONNECTIVITY_ON_FACE[bitPatternOnFace];

        for i := 0 to 2 - 1 do
        begin
          if length(connectivity[i]) <> 0 then
            for vertexIndex := 0 to 4 - 1 do
            begin
              if connectivity[i][vertexIndex] <> -1 then
                if FACE_ORIENTATION[faceIndex] = CW then
                  setConnectedEdge(Result.edges[vertexIndex]^, i,
                    Result.edges[connectivity[i][vertexIndex]])
                else
                  setConnectedEdge(Result.edges[connectivity[i][vertexIndex]]^, i,
                    Result.edges[vertexIndex]);
            end;
        end;
      end;

      // edge
      function newEdge(index: int): TEdge; inline;
      const
        EDGE_VERTICES: array[0..11, 0..1] of int =
          ((0, 1), (1, 2), (3, 2), (0, 3), (4, 5), (5, 6), (7, 6), (4, 7),
          (0, 4), (1, 5), (2, 6), (3, 7));
      begin
        Result.index := index;
        Result.startVertexIndex := EDGE_VERTICES[index][0];
        Result.endVertexIndex := EDGE_VERTICES[index][1];
      end;

    var
      edgeIndex, faceIndex: int;
    begin
      Result.index := index;

      setLength(Result.edges, 12);
      for edgeIndex := 0 to 12 - 1 do
      begin
        Result.edges[edgeIndex] := specialize box<TEdge>(newEdge(edgeIndex));
      end;
      setLength(Result.faces, 6);
      for faceindex := 0 to 6 - 1 do Result.faces[faceindex] :=
          createFace(faceindex, index, Result.edges);
    end;

  var
    i: int;
  begin
    Result.cubes := nil;
    setLength(Result.cubes, 256);
    for i := 0 to 256 - 1 do Result.cubes[i] := newCube(i);
  end;

var
  b: single;
begin
  sBounds := _bounds;
  b := _bounds;
  _min := mkvec3(-b, -b, -b);
  _max := mkvec3(b, b, b);
  idiv := mkvec3int(_idiv, _idiv, _idiv);
  isovalue := 0;
  func := _func;
  scale := 1;
  lookupTable := initCubes;
  mesh.shape := TVertexList.Create;
  mesh.trigs := TTrigList.Create;

  polygonize;
  scalePolygonizer;
end;

destructor TPolygonizer.Destroy;
var
  pe: ^TEdge;
  cube: TCube;
begin
  mesh.shape.Free;
  mesh.trigs.Free;

  for cube in lookUpTable.cubes do
    for pe in cube.edges do
      dispose(pe);
end;

procedure TPolygonizer.writeCTM(Name: string);
begin
  writeMeshCTM(mesh, Name);
end;

end.
