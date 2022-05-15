unit uPolygonizerC;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, gcontnrs, uMesh, v3;

type
  TimplFunc = function(x, y, z: double): double;

procedure polygonize(func: TimplFunc; size, bounds, x, y, z: double; var mesh: TMesh);

// implicit funcs
function dsphere(x, y, z: double): double;
function dblob(x, y, z: double): double; inline;

implementation

const
  TET = 0;   // use tetrahedral decomposition
  NOTET = 1; // no tetrahedral decomposition
  RES = 40; // # converge iterations
  L = 0;   // left direction:  -x; -i
  R = 1;   // right direction:  +x; +i
  B = 2;   // bottom direction: -y; -j
  T = 3;   // top direction:  +y; +j
  N = 4;   // near direction:  -z; -k
  F = 5;   // far direction:  +z; +k
  LBN = 0; // left bottom near corner
  LBF = 1; // left bottom far corner
  LTN = 2; // left top near corner
  LTF = 3; // left top far corner
  RBN = 4; // right bottom near corner
  RBF = 5; // right bottom far corner
  RTN = 6; // right top near corner
  RTF = 7; // right top far corner

  HASHBIT = 5;

  LB = 0; // left bottom edge
  LT = 1; // left top edge
  LN = 2; // left near edge
  LF = 3; // left far edge
  RB = 4; // right bottom edge
  RT = 5; // right top edge
  RN = 6; // right near edge
  RF = 7; // right far edge
  BN = 8; // bottom near edge
  BF = 9; // bottom far edge
  TN = 10; // top near edge
  TF = 11; // top far edge

  corner1: array[0..12 - 1] of integer = (LBN, LTN, LBN, LBF, RBN, RTN, RBN,
    RBF, LBN, LBF, LTN, LTF);
  corner2: array[0..12 - 1] of integer = (LBF, LTF, LTN, LTF, RBF, RTF, RTN,
    RTF, RBN, RBF, RTN, RTF);
  leftface: array[0..12 - 1] of integer = (B, L, L, F, R, T, N, R, N, B, T, F);
  // face on left when going corner1 to corner2
  rightface: array[0..12 - 1] of integer = (L, T, N, L, B, R, R, F, B, F, N, T);


type
  int = integer;

  TPoint = record // a three-dimensional TPoint
    x, y, z: double;      // its coordinates
  end;

  TTest = record // TTest the function for a signed value
    p: TPoint;            // location of TTest
    Value: double;       // function value at p
    ok: boolean;             // if value is of correct sign
  end;

  TVertex = record   // surface TVertex
    position, normal: TPoint; // position and surface normal
  end;

  TVertices = specialize TGenVector<TVertex>;

  //record // list of TVertices in polygonization
  //  Count, max: int;         // # TVertices, max # allowed
  //  vertex: array of TVertex;         // dynamically allocated
  //end;

  TTriangle = record
    i1, i2, i3: int;
  end;

  TTriangles = specialize TGenVector<TTriangle>;

  //  record
  //  Count, max: int;
  //  trig: array of TTriangle;
  //end;

  TCorner = record  // TCorner of a cube
    i, j, k: int;           // (i, j, k) is index within lattice
    x, y, z, Value: double; // location and function value
  end;

  TCube = record // partitioning cell (TCube)
    i, j, k: int;        // lattice location of TCube
    corners: array [0..7] of ^TCorner; // eight corners
  end;

  TCubes = record // linked list of TCubes acting as stack
    cube: TCube;    // a single TCube
    Next: ^TCubes;  // remaining elements
  end;

  TCenterlist = record // list of TCube locations
    i, j, k: int;              // TCube location
    Next: ^TCenterlist;  // remaining elements
  end;

  TCornerlist = record // list of corners
    i, j, k: int;              // TCorner id
    Value: double;             // TCorner value
    Next: ^TCornerlist;  // remaining elements
  end;

  TEdgelist = record     // list of edges
    i1, j1, k1, i2, j2, k2: int; // edge TCorner ids
    vid: int;                    // TVertex id
    Next: ^TEdgelist;      // remaining elements
  end;

  TIntlist = record // list of integers
    i: int;                 // an integer
    Next: ^TIntlist;  // remaining elements
  end;

  TIntlists = record // list of list of integers
    list: ^TIntlist;   // a list of integers
    Next: ^TIntlists;  // remaining elements
  end;

  //pTPoint = ^TPoint;
  //pTTriangle = ^TTriangle;
  pTCorner = ^TCorner;
  pTCenterlist = ^TCenterlist;
  pTCornerlist = ^TCornerlist;
  pTEdgelist = ^TEdgelist;
  pTIntlist = ^TIntlist;
  pTIntlists = ^TIntlists;
  pTCubes = ^TCubes;
  //pTCube = ^TCube;

  TProcess = record // parameters, function, storage
    func: TimplFunc;  // implicit surface function
    triproc:
    function(i1, i2, i3: int): int;
    // TTriangle output function triangle2
    size, delta: double;    // TCube size, normal delta
    bounds: double; // int;            // TCube range within lattice
    start: TPoint;           // start TPoint on surface
    cubes: pTCubes;          // active TCubes
    vertices: TVertices;     // surface TVertices
    centers: array of pTCenterlist;  // ** TCube center hash table
    corners: array of pTCornerlist;  // ** TCorner value hash table
    edges: array of pTEdgelist;      // ** edge and TVertex id hash table
  end;


///// globals
var
  glefthanded: int = 0;
  gtriangles: TTriangles;
  cubetable: array [0..256 - 1] of pTIntLists;
  doneCubeTable: boolean = False;


{ the LBN corner of cube (i, j, k), corresponds with location
(start.x+(i-.5)*size, start.y+(j-.5)*size, start.z+(k-.5)*size) }
function rand: double;
begin
  Result := random;
end;  // random number between 0 and 1

function HASHSIZE: integer;
begin
  Result := 1 shl (3 * HASHBIT);
end;  // hash table size (32768)

function MASK: integer;
begin
  Result := (1 shl HASHBIT) - 1;
end;

function HASH(i, j, k: int): int;
begin
  Result := (((((i and MASK) shl HASHBIT) or (j and MASK)) shl HASHBIT) or (k and MASK));
end;

function BIT(i, _bit: int): int;
begin
  Result := ((i shr _bit) and 1);
end;

function FLIP(i, bit: int): int;
begin
  Result := i xor (1 shl bit);
end; // flip the given bit of i


function mkTriangle(i1, i2, i3: int): TTriangle;
begin
  Result.i1 := i1;
  Result.i2 := i2;
  Result.i3 := i3;
end;


procedure addtotriangles(var triangles: TTriangles; t: TTriangle);
begin
  triangles.Append(t);
end;

function triangle2(i1, i2, i3: int): int;
var
  t: TTriangle;
  temp: int;
begin
  t := mkTriangle(i1, i2, i3);

  if glefthanded = 0 then
  begin
    temp := t.i2;
    t.i2 := t.i3;
    t.i3 := temp;
  end;
  addtotriangles(gtriangles, t);
  Result := 1;
end;

// nextcwedge: tc next clockwise edge from given edge around given face

function nextcwedge(edge, face: int): int;

  function tc(bl: boolean; a, b: int): int;
  begin
    if bl then Result := a
    else
      Result := b;
  end;

begin
  Result := 0;
  case (edge) of
    LB: Result := tc(face = L, LF, BN);
    LT: Result := tc(face = L, LN, TF);
    LN: Result := tc(face = L, LB, TN);
    LF: Result := tc(face = L, LT, BF);
    RB: Result := tc(face = R, RN, BF);
    RT: Result := tc(face = R, RF, TN);
    RN: Result := tc(face = R, RT, BN);
    RF: Result := tc(face = R, RB, TF);
    BN: Result := tc(face = B, RB, LN);
    BF: Result := tc(face = B, LB, RF);
    TN: Result := tc(face = T, LT, RN);
    TF: Result := tc(face = T, RT, LF);
    else
      Result := 0;
  end;
end;

// otherface: return face adjoining edge that is not the given face
function otherface(edge, face: int): int;
var
  other: int;
begin
  other := leftface[edge];
  if face = other then Result := rightface[edge]
  else
    Result := other;
end;


// make cube
procedure makecubetable;
var
  i, e, c, start, edge, face: int;
  done: array [0..12 - 1] of int;
  pos: array [0..8 - 1] of int;
  ints, tmp: pTIntList;
  lists: pTIntLists;
begin

  for i := 0 to 256 - 1 do
  begin
    for e := 0 to 12 - 1 do
      done[e] := 0;
    for c := 0 to 8 - 1 do
      pos[c] := BIT(i, c);
    for e := 0 to 12 - 1 do
    begin
      if (done[e] = 0) and (pos[corner1[e]] <> pos[corner2[e]]) then
      begin
        ints := nil;
        new(lists);
        start := e;
        edge := e;
        // get face that is to right of edge from pos to neg corner:
        if pos[corner1[e]] <> 0 then face := rightface[e]
        else
          face := leftface[e];

        while True do
        begin
          edge := nextcwedge(edge, face);
          done[edge] := 1;
          if pos[corner1[edge]] <> pos[corner2[edge]] then
          begin
            tmp := ints;
            new(ints);

            ints^.i := edge;
            ints^.Next := tmp; // add edge to head of list */
            if edge = start then
              break;
            face := otherface(edge, face);
          end;
        end;
        lists^.list := ints; // add ints to head of table entry */
        lists^.Next := cubetable[i];
        cubetable[i] := lists;
      end;
    end;
  end;
end;

{ setcenter: set (i,j,k) entry of table[]
  return 1 if already set; otherwise, set and return 0 }

function setcenter(var table: array of pTCenterList; i, j, k: int): int;
var
  index: int;
  newcl, l, q: pTCenterlist;
begin
  index := HASH(i, j, k);
  q := table[index];

  l := q;
  while l <> nil do
  begin
    if (l^.i = i) and (l^.j = j) and (l^.k = k) then exit(1);
    l := l^.Next;
  end;

  new(newcl);
  newcl^.i := i;
  newcl^.j := j;
  newcl^.k := k;
  newcl^.Next := q;
  table[index] := newcl;
  Result := 0;
end;

// setedge: set vertex id for edge

procedure setedge(var table: array of pTEdgelist; i1, j1, k1, i2, j2, k2, vid: int);
var
  index, t: int;
  newel: pTEdgelist;
begin

  if (i1 > i2) or ((i1 = i2) and ((j1 > j2) or ((j1 = j2) and (k1 > k2)))) then
  begin
    t := i1;
    i1 := i2;
    i2 := t;
    t := j1;
    j1 := j2;
    j2 := t;
    t := k1;
    k1 := k2;
    k2 := t;
  end;

  index := HASH(i1, j1, k1) + HASH(i2, j2, k2);
  new(newel);
  newel^.i1 := i1;
  newel^.j1 := j1;
  newel^.k1 := k1;
  newel^.i2 := i2;
  newel^.j2 := j2;
  newel^.k2 := k2;
  newel^.vid := vid;
  newel^.Next := table[index];
  table[index] := newel;
end;


function find(sign: int; p: TProcess; x, y, z: double): TTest;
var
  i, tvgtz: int;
  test: TTest;
  range: double;
begin
  range := p.size;
  test.ok := True;
  for i := 0 to 10000 do
  begin
    test.p.x := x + range * (RAND() - 0.5);
    test.p.y := y + range * (RAND() - 0.5);
    test.p.z := z + range * (RAND() - 0.5);
    test.Value := p.func(test.p.x, test.p.y, test.p.z);

    if test.Value > 0 then tvgtz := 1
    else
      tvgtz := 0;

    if sign = tvgtz then  // if (sign == (test.value > 0.0))
      exit(test);

    range *= 1.0005; // slowly expand search outwards
  end;
  test.ok := False;
  Result := test;
end;


// converge: from two points of differing sign, converge to zero crossing

procedure converge(p1, p2: TPoint; v: double; func: TimplFunc; var p: TPoint);
var
  i: int = 0;
  pos, neg: TPoint;
begin

  if v < 0 then
  begin
    pos.x := p2.x;
    pos.y := p2.y;
    pos.z := p2.z;
    neg.x := p1.x;
    neg.y := p1.y;
    neg.z := p1.z;
  end
  else
  begin
    pos.x := p1.x;
    pos.y := p1.y;
    pos.z := p1.z;
    neg.x := p2.x;
    neg.y := p2.y;
    neg.z := p2.z;
  end;

  while True do
  begin
    p.x := 0.5 * (pos.x + neg.x);
    p.y := 0.5 * (pos.y + neg.y);
    p.z := 0.5 * (pos.z + neg.z);
    Inc(i);
    if i = RES then exit;

    if func(p.x, p.y, p.z) > 0 then
    begin
      pos.x := p.x;
      pos.y := p.y;
      pos.z := p.z;
    end
    else
    begin
      neg.x := p.x;
      neg.y := p.y;
      neg.z := p.z;
    end;
  end;
end;

{ setcorner: return corner with the given lattice location
   set (and cache) its function value }

function setcorner(var p: TProcess; i, j, k: int): pTCorner;
var
  c: pTCorner;
  l: pTCornerlist;
  index: int;

begin
  // for speed, do corner value caching here
  new(c);
  index := HASH(i, j, k);
  l := p.corners[index];
  c^.i := i;
  c^.x := p.start.x + (i - 0.5) * p.size;
  c^.j := j;
  c^.y := p.start.y + (j - 0.5) * p.size;
  c^.k := k;
  c^.z := p.start.z + (k - 0.5) * p.size;
  while l <> nil do
  begin
    if (l^.i = i) and (l^.j = j) and (l^.k = k) then
    begin
      c^.Value := l^.Value;
      exit(c);
    end;
    l := l^.Next;
  end;
  new(l);
  l^.i := i;
  l^.j := j;
  l^.k := k;
  l^.Value := p.func(c^.x, c^.y, c^.z);
  c^.Value := l^.Value;
  l^.Next := p.corners[index];
  p.corners[index] := l;
  Result := c;
end;

// getedge: return vertex id for edge; return -1 if not set

function getedge(table: array of pTEdgelist; i1, j1, k1, i2, j2, k2: int): int;
var
  q: pTEdgelist;
  t: int;
begin

  if (i1 > i2) or ((i1 = i2) and ((j1 > j2) or ((j1 = j2) and (k1 > k2)))) then
  begin
    t := i1;
    i1 := i2;
    i2 := t;
    t := j1;
    j1 := j2;
    j2 := t;
    t := k1;
    k1 := k2;
    k2 := t;
  end;
  q := table[HASH(i1, j1, k1) + HASH(i2, j2, k2)];
  while q <> nil do
  begin
    if (q^.i1 = i1) and (q^.j1 = j1) and (q^.k1 = k1) and (q^.i2 = i2) and
      (q^.j2 = j2) and (q^.k2 = k2) then
      exit(q^.vid);
    q := q^.Next;
  end;
  Result := -1;
end;

// vnormal: compute unit length surface normal at point
procedure vnormal(point: TPoint; p: TProcess; var v: TPoint);
var
  f: double;
begin
  f := p.func(point.x, point.y, point.z);
  v.x := p.func(point.x + p.delta, point.y, point.z) - f;
  v.y := p.func(point.x, point.y + p.delta, point.z) - f;
  v.z := p.func(point.x, point.y, point.z + p.delta) - f;
  f := sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
  if f <> 0 then
  begin
    v.x /= f;
    v.y /= f;
    v.z /= f;
  end;
end;

// addtovertices: add v to sequence of vertices

procedure addtovertices(var vertices: TVertices; v: TVertex);
begin
  vertices.append(v);
end;

function vertid(c1, c2: pTCorner; p: TProcess): int;
var
  v: TVertex;
  a, b: TPoint;
  vid: int;
begin
  vid := getedge(p.edges, c1^.i, c1^.j, c1^.k, c2^.i, c2^.j, c2^.k);
  if vid <> -1 then
    exit(vid); // previously computed
  a.x := c1^.x;
  a.y := c1^.y;
  a.z := c1^.z;
  b.x := c2^.x;
  b.y := c2^.y;
  b.z := c2^.z;

  converge(a, b, c1^.Value, p.func, v.position); // position
  vnormal(v.position, p, v.normal);                    // normal
  addtovertices(p.vertices, v);                        // save vertex
  vid := p.vertices.Size - 1;
  setedge(p.edges, c1^.i, c1^.j, c1^.k, c2^.i, c2^.j, c2^.k, vid);
  Result := vid;
end;


{ dotet: triangulate the tetrahedron
  b, c, d should appear clockwise when viewed from a
  exit( 0 if client aborts, 1 otherwise }

function dotet(cube: TCube; c1, c2, c3, c4: int; var p: TProcess): int;
var
  a, b, c, d: pTCorner;
  index: int = 0;
  apos, bpos, cpos, dpos: boolean;
  e1, e2, e3, e4, e5, e6: int;

begin

  a := cube.corners[c1];
  b := cube.corners[c2];
  c := cube.corners[c3];
  d := cube.corners[c4];

  apos := (a^.Value > 0);
  if apos then
    index += 8;
  bpos := (b^.Value > 0.0);
  if bpos then
    index += 4;
  cpos := (c^.Value > 0.0);
  if cpos then
    index += 2;
  dpos := (d^.Value > 0.0);
  if dpos then
    index += 1;
  // index is now 4-bit number representing one of the 16 possible cases */
  if apos <> bpos then
    e1 := vertid(a, b, p);
  if apos <> cpos then
    e2 := vertid(a, c, p);
  if apos <> dpos then
    e3 := vertid(a, d, p);
  if bpos <> cpos then
    e4 := vertid(b, c, p);
  if bpos <> dpos then
    e5 := vertid(b, d, p);
  if cpos <> dpos then
    e6 := vertid(c, d, p);
  // 14 productive tetrahedral cases (0000 and 1111 do not yield polygons
  case (index) of
    1: exit(p.triproc(e5, e6, e3));
    2: exit(p.triproc(e2, e6, e4));
    3: exit(p.triproc(e3, e5, e4) and p.triproc(e3, e4, e2));
    4: exit(p.triproc(e1, e4, e5));
    5: exit(p.triproc(e3, e1, e4) and p.triproc(e3, e4, e6));
    6: exit(p.triproc(e1, e2, e6) and p.triproc(e1, e6, e5));
    7: exit(p.triproc(e1, e2, e3));
    8: exit(p.triproc(e1, e3, e2));
    9: exit(p.triproc(e1, e5, e6) and p.triproc(e1, e6, e2));
    10: exit(p.triproc(e1, e3, e6) and p.triproc(e1, e6, e4));
    11: exit(p.triproc(e1, e5, e4));
    12: exit(p.triproc(e3, e2, e4) and p.triproc(e3, e4, e5));
    13: exit(p.triproc(e6, e2, e4));
    14: exit(p.triproc(e5, e3, e6));
    else;
  end;
  Result := 1;
end;

function docube(cube: TCube; p: Tprocess): int;
var
  polys: pTIntLists;
  edges: pTIntlist;
  i, index, a, b, c, Count: int;
  c1, c2: pTCorner;

begin
  index := 0;
  for i := 0 to 8 - 1 do
    if cube.corners[i]^.Value > 0.0 then
      index += (1 shl i);
  polys := cubetable[index];
  while polys <> nil do
  begin

    a := -1;
    b := -1;
    Count := 0;

    edges := polys^.list;
    while edges <> nil do
    begin
      c1 := cube.corners[corner1[edges^.i]];
      c2 := cube.corners[corner2[edges^.i]];
      c := vertid(c1, c2, p);
      Inc(Count);
      triangle2(a, b, c);
      if Count < 3 then
        a := b;
      b := c;

      edges := edges^.Next;
    end;
    polys := polys^.Next;
  end;
  Result := 1;
end;

procedure testface(i, j, k: int; old: TCube; face, c1, c2, c3, c4: int; var p: TProcess);
var
  newc: TCube;
  oldcubes: pTCubes;
  facebit: array[0..6 - 1] of int = (2, 2, 1, 1, 0, 0);
  n, bitv: int;
  pos: boolean;
begin

  oldcubes := p.cubes;
  pos := old.corners[c1]^.Value > 0.0;
  bitv := facebit[face];

  // test if no surface crossing, cube out of bounds, or already visited:
  if ((old.corners[c2]^.Value > 0) = pos) and ((old.corners[c3]^.Value > 0) = pos) and
    ((old.corners[c4]^.Value > 0) = pos) then
    exit;

  if (abs(i) > p.bounds) or (abs(j) > p.bounds) or (abs(k) > p.bounds) then
    exit;

  if setcenter(p.centers, i, j, k) <> 0 then
    exit;

  // create newc cube:

  newc.i := i;
  newc.j := j;
  newc.k := k;

  for n := 0 to 8 - 1 do
    newc.corners[n] := nil;

  newc.corners[FLIP(c1, bitv)] := old.corners[c1];
  newc.corners[FLIP(c2, bitv)] := old.corners[c2];
  newc.corners[FLIP(c3, bitv)] := old.corners[c3];
  newc.corners[FLIP(c4, bitv)] := old.corners[c4];

  for n := 0 to 8 - 1 do
    if newc.corners[n] = nil then
      newc.corners[n] := setcorner(p, i + BIT(n, 2), j + BIT(n, 1), k + BIT(n, 0));

  // add cube to top of stack:
  new(p.cubes);
  p.cubes^.cube := newc;
  p.cubes^.Next := oldcubes;
end;

generic procedure disposeList<T>(p: T);
var
  i, l, prev: T;
begin
  i := p;
  while i <> nil do
  begin
    prev := i;
    i := i^.Next;
    dispose(prev);
  end;
end;

procedure releaseCubeTable;
var
  cti, ctl, ctp: pTIntlists;
  ili, ilp: pTIntlist;
begin
  // cubetable
  for cti in cubetable do
  begin
    ctl := cti;
    while ctl <> nil do
    begin
      ctp := ctl;
      ctl := ctl^.Next;

      ili := ctp^.list; // release the list
      while ili <> nil do
      begin
        ilp := ili;
        ili := ili^.Next;
        dispose(ilp);
      end;

      dispose(ctp);
    end;
  end;
end;

procedure releaseMem(var p: TProcess);
var
  edgei, edgec, edgep: pTEdgelist;
  coi, col, cop: pTCornerlist;
  cei, cel, cep: pTCenterlist;
  copi: pTCorner;
  cui, cup: pTCubes;
begin
  // p.edges
  for edgei in p.edges do
  begin
    edgec := edgei;
    while edgec <> nil do
    begin
      edgep := edgec;
      edgec := edgec^.Next;
      dispose(edgep);
    end;
  end;

  // p.corners
  for coi in p.corners do
  begin
    col := coi;
    while col <> nil do
    begin
      cop := col;
      col := col^.Next;
      dispose(cop);
    end;
  end;

  // p.centers
  for cei in p.centers do
  begin
    cel := cei;
    while cel <> nil do
    begin
      cep := cel;
      cel := cel^.Next;
      dispose(cep);
    end;
  end;

  // p.cubes
  cui := p.cubes;
  while cui <> nil do
  begin
    cup := cui;
    cui := cui^.Next;

    // corners array
    for copi in cup^.cube.corners do
      dispose(copi);

    dispose(cup);
  end;

end;

////////////////// poligonize

procedure polygonize(func: TimplFunc; size, bounds, x, y, z: double; var mesh: TMesh);
// triproc=triangle2, mode=TET
var
  p: TProcess;
  vin, vout: TTest;
  nn, noabort: int;
  c: TCube;

  trig: TTriangle;
  vertex: Tvertex;

begin

  p.func := func;
  p.triproc := @triangle2;
  p.size := size;
  p.bounds := bounds;
  p.delta := size / (RES * RES);

  // allocate hash tables and build cube polygon table:
  setLength(p.centers, HASHSIZE);
  setLength(p.corners, HASHSIZE);
  setLength(p.edges, 2 * HASHSIZE);

  if not doneCubeTable then makecubetable();

  // find point on surface, beginning search at (x, y, z):
  randomize;
  vin := find(1, p, x, y, z);
  vout := find(0, p, x, y, z);
  if (not vin.ok) or (not vout.ok) then
    raise Exception.Create('can''t find starting point');

  converge(vin.p, vout.p, vin.Value, p.func, p.start);

  // push initial cube on stack:
  new(p.cubes);
  p.cubes^.cube.i := 0;
  p.cubes^.cube.j := 0;
  p.cubes^.cube.k := 0;
  p.cubes^.Next := nil;

  // set corners of initial cube:
  for nn := 0 to 8 - 1 do
    p.cubes^.cube.corners[nn] := setcorner(p, BIT(nn, 2), BIT(nn, 1), BIT(nn, 0));

  p.vertices := TVertices.Create;
  gtriangles := TTriangles.Create;

  setcenter(p.centers, 0, 0, 0);

  while p.cubes <> nil do
  begin // process active cubes till none left

    c := p.cubes^.cube;

    if {mode = TET} True then  // either decompose into tetrahedra and polygonize:
      noabort := dotet(c, LBN, LTN, RBN, LBF, p) and
        dotet(c, RTN, LTN, LBF, RBN, p) and dotet(c, RTN, LTN, LTF, LBF, p) and
        dotet(c, RTN, RBN, LBF, RBF, p) and dotet(c, RTN, LBF, LTF, RBF, p) and
        dotet(c, RTN, LTF, RTF, RBF, p)

    // or polygonize the cube directly:
    else
      noabort := docube(c, p);

    if noabort = 0 then
      exit;

    // pop current cube from stack
    p.cubes := p.cubes^.Next;

    // test six face directions, maybe add to stack:
    testface(c.i - 1, c.j, c.k, c, L, LBN, LBF, LTN, LTF, p);
    testface(c.i + 1, c.j, c.k, c, R, RBN, RBF, RTN, RTF, p);
    testface(c.i, c.j - 1, c.k, c, B, LBN, LBF, RBN, RBF, p);
    testface(c.i, c.j + 1, c.k, c, T, LTN, LTF, RTN, RTF, p);
    testface(c.i, c.j, c.k - 1, c, N, LBN, LTN, RBN, RTN, p);
    testface(c.i, c.j, c.k + 1, c, F, LBF, LTF, RBF, RTF, p);
  end;

  //create mesh
  mesh.shape.Free;
  mesh.trigs.free;

  mesh.shape := TVertexList.Create;
  for vertex in p.vertices do
    mesh.shape.Append(mkVertex(
      mkvec3(vertex.position.x, vertex.position.y, vertex.position.z),
      mkvec3(vertex.normal.x, vertex.normal.y, vertex.normal.z),
      mkvec3(0, 0, 0), mkvec3(0.5, 0.5, 0)));

  nn := gtriangles.size;
  mesh.trigs := TTrigList.Create;
  for  trig in gtriangles do
    mesh.trigs.append(mkTrig(trig.i1, trig.i2, trig.i3));

  gtriangles.free;
  p.vertices.free;

  releaseMem(p);
end;

// implicit funcs
function dsphere(x, y, z: double): double; inline;
var
  rsq: double;
begin
  rsq := x * x + y * y + z * z;
  if rsq < 1e-5 then rsq := 1e-5;
  Result := 1.0 / rsq;
end;

function dblob(x, y, z: double): double; inline;
begin
  Result := 4 - dsphere(x + 1.0, y, z) - dsphere(x, y + 1.0, z) - dsphere(x, y, z + 1.0);
end;

end.
