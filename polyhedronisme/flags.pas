unit flags;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCommon, fgl, uPoly;

type
  TMapi4v = specialize TFPGmap<Ti4, TVertexIndex>;
  TListMix = specialize TFPGlist<TMapIndex>;
  TListvi4 = specialize TFPGlist<TVecInt4>;

  TFlag = class
    faces: TFaces;
    vertexes: TVertexes;
    v: TMapi4v;
    m: TListMix;
    fcs: TListvi4;
    vIndex: integer;
    valid: boolean;

    constructor Create(const estimation: integer = 1000); overload;
    constructor Create(const _vertexes: Tvertexes; const estimation: integer = 1000);
      overload;
    destructor Free;
    procedure initFPGs(const estimation: integer);

    procedure setVertexes(const _vertexes: TVertexes);
    procedure addFace(const i0, i1, i2: Ti4);
    procedure addFace(const vf: TVecInt4);
    procedure addVertex(const _vIndex: integer; const ix: Ti4; const vtx: TVertex);
    procedure addVertex(const ix: Ti4; const vtx: TVertex);

    function findVertexIndex(const ix: Ti4): integer;
    function findM(i0, i1: Ti4): Ti4;
    function fromToM: TListInt;
    procedure indexVertexes;
    procedure toPoly(const tok: string; var poly: CPoly);
    procedure process_m;
    procedure process_fcs;
  end;

implementation


constructor TFlag.Create(const estimation: integer = 1000); overload;
begin
  initFPGs(estimation);
end;

constructor TFlag.Create(const _vertexes: Tvertexes; const estimation: integer);
begin
  initFPGs(estimation);
  setVertexes(_vertexes);
end;

procedure TFlag.initFPGs(const estimation: integer);
begin
  v := TMapi4v.Create;
  v.Capacity := estimation;
  v.Sorted := True;
  v.Duplicates := dupIgnore;

  fcs := TListvi4.Create;
  fcs.Capacity := estimation;

  m := TListMix.Create;
  m.Capacity := estimation;
end;

destructor TFlag.Free;
begin
  v.Free;
  fcs.Free;
  m.Free;
end;

procedure TFlag.addFace(const i0, i1, i2: Ti4);
begin
  m.add(makeMapIndex(i0, i1, i2));
end;

procedure TFlag.addFace(const vf: TVecInt4);
begin
  fcs.add(vf);
end;

procedure TFlag.addVertex(const _vIndex: integer; const ix: Ti4; const vtx: TVertex);
var
  vix: TVertexIndex;
begin
  vix := mkVtxIdx(_vIndex, vtx);
  v.add(ix, vix);
end;

procedure TFlag.addVertex(const ix: Ti4; const vtx: TVertex);
begin
  addVertex(vIndex, ix, vtx);
  Inc(vIndex);
end;

function TFlag.findVertexIndex(const ix: Ti4): integer;
begin
  Result := v[ix].index;
end;

procedure TFlag.setVertexes(const _vertexes: TVertexes);
var
  i: integer;
begin
  v.Capacity := length(_vertexes);
  for i := 0 to high(_vertexes) do
    addVertex(0, mki4(i), _vertexes[i]);
end;

function TFlag.findM(i0, i1: Ti4): Ti4;

  function lowerBound(l, r: integer; const x: TMapIndex): integer;
  var
    mid: integer;
  begin
    if r >= l then
    begin
      mid := l + (r - l) div 2;
      if m[mid] > x then
        exit(lowerBound(l, mid - 1, x));
      exit(lowerBound(mid + 1, r, x));
    end
    else
      exit(l); // lower bound
  end;

begin
  Result := m[lowerBound(0, m.Count - 1, makeMapIndex(i0, i1))].v[2];
end;

procedure TFlag.indexVertexes; // v, numerate vertexes index & create vertexes[]
var
  i: integer;
  tv: TVertexIndex;
begin
  // v. is map sorted
  setLength(vertexes, v.Count); // numerate & create vertexes[]
  for i := 0 to v.Count - 1 do
  begin // <index, vertex>
    tv := v.Data[i];
    tv.index := i;
    v.Data[i] := tv;
    vertexes[i] := v.Data[i].vertex;
  end;
end;

procedure TFlag.toPoly(const tok: string; var poly: CPoly);
begin
  valid := True;

  indexVertexes;
  setLength(faces, 0);
  process_m;

  if valid then
  begin
    process_fcs;

    poly.Name := tok + poly.Name;     // generate poly
    poly.faces := faces;
    poly.vertexes := vertexes;

    poly.simplify;
  end;
end;

procedure TFlag.process_fcs;
var
  fOff, i: integer;
  face: TFace = nil;
  fc: TVecInt4;
  vix: Ti4;

begin
  fOff := length(faces);
  setLength(faces, fcs.Count + fOff);

  for fc in fcs do
  begin
    setLength(face, length(fc));
    i := 0;
    for vix in fc do
    begin
      face[i] := findVertexIndex(vix);
      Inc(i);
    end;
    faces[fOff] := face;
    Inc(fOff);
  end;
end;

function TFlag.fromToM: TListInt;
var
  c0: Ti4;
  from: integer = 0;
  i: integer;
begin
  Result := TListInt.Create;
  Result.Capacity := m.Count;

  c0 := m[0].v[0];

  for i := 0 to m.Count - 1 do
  begin
    if m[i].v[0] <> c0 then
    begin
      Result.add(from);
      from := i;
      c0 := m[i].v[0];
    end;
  end;
  Result.add(from);
end;

procedure TFlag.process_m;
const
  MAX_ITERS: integer = 100;
var
  ft: TListInt = nil;
  fti, i: integer;
  _v0, _v, _m0: Ti4;
  facel: TListInt = nil;

  topCounter: integer = 0;
begin

  if m.Count > 0 then
  begin
    m.Sort(@cmpMapIndex);
    ft := fromToM;
    setLength(faces, ft.Count);

    for fti := 0 to ft.Count - 1 do
    begin
      i := ft[fti];

      _v0 := m[i].v[2];
      _v := _v0;
      _m0 := m[i].v[0];

      // traverse _m0
      facel := TListInt.Create;

      topCounter := 0;
      repeat
        facel.add(findVertexIndex(_v));
        _v := findM(_m0, _v);

        Inc(topCounter);
        if topCounter > MAX_ITERS then
        begin
          valid := False;
          break;
        end;
      until _v = _v0;

      faces[fti] := toArray(facel);
      facel.Free;

      if not valid then
        break;
    end;
  end;

  ft.Free;
end;

end.
