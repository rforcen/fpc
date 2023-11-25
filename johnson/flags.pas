unit flags;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCommon, fgl, uPoly, uvec3, uListX;

type

  TListI4v = specialize TListX<TReci4v>;
  TListMix = specialize TListX<TMapIndex>;
  TListvi4 = specialize TFPGlist<TVecInt4>;

  TFlag = class
    faces: TFaces;
    vertexes: TVertexes;
    v: TListI4v;
    m: TListMix;
    fcs: TListvi4;
    valid: boolean;

    constructor Create; overload;
    constructor Create(const _vertexes: Tvertexes);
      overload;
    destructor Free;
    procedure initFPGs;

    procedure setVertexes(const _vertexes: TVertexes; const withUnit: boolean = False);
    procedure addFace(const i0, i1, i2: Ti4);
    procedure addFace(const vf: TVecInt4);
    procedure addVertex(const ix: Ti4; const vtx: TVertex);

    function findVertexIndex(const ix: Ti4): integer;
    function findM(i0, i1: Ti4): Ti4;
    function fromToM: TListInt;
    procedure indexVertexes;
    procedure toPoly(const tok: string; var poly: CPoly);
    procedure process_m;
    procedure process_fcs;
    function check: boolean;
  end;

implementation

constructor TFlag.Create; overload;
begin
  initFPGs;
end;

constructor TFlag.Create(const _vertexes: Tvertexes);
begin
  initFPGs;
  setVertexes(_vertexes, true);
end;

procedure TFlag.initFPGs;
begin
  v := TListI4v.Create;
  fcs := TListvi4.Create;
  m := TListMix.Create;

  valid := True;
end;

destructor TFlag.Free;
begin
  fcs.Free;
  m.Free;
  v.Free;
end;

procedure TFlag.addFace(const i0, i1, i2: Ti4); inline;
begin
  m.add(makeMapIndex(i0, i1, i2));
end;

procedure TFlag.addFace(const vf: TVecInt4); inline;
begin
  fcs.add(vf);
end;

procedure TFlag.addVertex(const ix: Ti4; const vtx: TVertex); inline;
begin
  v.add(mki4v(ix, 0, vtx));
end;

function TFlag.findVertexIndex(const ix: Ti4): integer;
begin
  Result := v.binaryFind(mki4v(ix), @cmpReci4v).vix.index;
end;

procedure TFlag.setVertexes(const _vertexes: TVertexes; const withUnit: boolean = False);
var
  i: integer;
begin
  for i := 0 to high(_vertexes) do
    if withUnit then addVertex(mki4(i), unitv(_vertexes[i]))
    else
      addVertex(mki4(i), _vertexes[i]);
end;

function TFlag.findM(i0, i1: Ti4): Ti4;
begin
  Result := m.lowerBound(makeMapIndex(i0, i1), @cmpMapIndex).v[2];
end;

procedure TFlag.indexVertexes; // v, numerate vertexes index & create vertexes[]
var
  index: integer;
begin
  v.sortUnique(@cmpReci4v);
  setLength(vertexes, v.Count);

  for index := 0 to v._high do
  begin // <index, vertex>
    v[index] := mki4v(v[index], index);
    vertexes[index] := v[index].vix.vertex;
  end;
end;

procedure TFlag.toPoly(const tok: string; var poly: CPoly);

begin
  indexVertexes;

  setLength(faces, 0);
  process_m;

  if valid and check then
  begin
    process_fcs;

    poly.Name := tok + poly.Name;     // generate poly
    poly.faces := faces;
    poly.vertexes := vertexes;

    poly.normalize;
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

  for i := 0 to m._high do
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

function TFlag.check: boolean;
var
  face: TFace;
begin
  for face in faces do
    if length(face) < 3 then exit(False);
  Result := True;
end;

end.
