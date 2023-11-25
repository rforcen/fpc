unit uCommon;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, fgl, uvec3;

type
  vecf = array of single;
  TFace = array of integer;
  TVertex = TVec3;
  TFaces = array of TFace;
  TVertexes = array of TVertex;
  TListInt = specialize TFPGlist<integer>;

  TFacesVertexes = record
    Name: string;
    faces: TFaces;
    vertexes: TVertexes;
  end;

  Ti4 = record
    v: array[0..3] of integer;
    class operator = (const a, b: Ti4): boolean;
    class operator > (const a, b: Ti4): boolean;
    class operator < (const a, b: Ti4): boolean;
  end;

  TVecInt4 = array of Ti4;

  TVertexIndex = record
    index: integer;
    vertex: TVertex;
    procedure setIndex(_index: integer);
    class operator = (const a, b: TVertexIndex): boolean;
  end;

  TMapIndex = record
    v: array[0..2] of Ti4;
    class operator = (const a, b: TMapIndex): boolean;
    class operator > (const a, b: TMapIndex): boolean;
    class operator < (const a, b: TMapIndex): boolean;
  end;

  TReci4v = record
    i4: Ti4;
    vix: TVertexIndex;
    class operator = (const a, b: TReci4v): boolean;
    procedure setIndex(i: integer);
  end;

  TI4Vix = record
    index: Ti4;
    vix: TVertexIndex;
    constructor Create(_index: Ti4; _vix: TVertexIndex);
    class operator = (const a, b: TI4Vix): boolean;
  end;

  TColors = array of byte;

const
  zeroi4: Ti4 = (v: (0, 0, 0, 0));


// util funcs
function makeVertex(x, y, z: single): TVertex; inline;
function makeFace(args: array of integer): TFace;
function makeToken(c: char): integer;
function randomPalette(n: integer): TVertexes;
function toArray(l: TListInt): TFace;
function getColor(i: integer): TColors;

// i4
function cmpi4(const a, b: Ti4): integer; inline;
function mki4(i0: integer = -1; i1: integer = -1; i2: integer = -1;
  i3: integer = -1): Ti4; inline;
function i4Min(v1, v2: integer): Ti4;
function i4Min(i, v1, v2: integer): Ti4;

// TReci4v
function mki4v(const i4: Ti4; const index: integer; const vertex: TVertex): TReci4v;
function mki4v(const r4: TReci4v; const index: integer): TReci4v;
function mki4v(const i4: Ti4): TReci4v;
function cmpReci4v(const a, b: TReci4v): integer;

// MapIndex
function makeMapIndex(i0, i1, i2: Ti4): TMapIndex;
function makeMapIndex(i0, i1: Ti4): TMapIndex;
function makeMapIndex(i0: Ti4): TMapIndex;
function cmpMapIndex(const a, b: TMapIndex): integer;

// I4Vix
function cmpI4Vix(const a, b: TI4Vix): integer;

// TVertexIndex
function mkVtxIdx(index: integer): TVertexIndex;
function mkVtxIdx(index: integer; vertex: TVertex): TVertexIndex;


implementation  //////////////////////////

function getColor(i: integer): TColors;
const
  FaceColors: array of array of byte = // from palette
    ((142, 202, 230), (33, 158, 188), (2, 48, 71), (255, 183, 3), (251, 133, 0));
begin
  Result := FaceColors[(i - 3) mod length(FaceColors)];
end;

// TVertexIndex
function mkVtxIdx(index: integer): TVertexIndex;
begin
  Result.index := index;
end;

function mkVtxIdx(index: integer; vertex: TVertex): TVertexIndex;
begin
  Result.index := index;
  Result.vertex := vertex;
end;

procedure TVertexIndex.setIndex(_index: integer);
begin
  index := _index;
end;

class operator TVertexIndex. = (const a, b: TVertexIndex): boolean;
begin
  Result := (a.index = b.index) and (a.vertex = b.vertex);
end;

// TReci4v
class operator TReci4v. = (const a, b: TReci4v): boolean;
begin
  Result := (a.i4 = b.i4) and (a.vix = b.vix);
end;

procedure TReci4v.setIndex(i: integer); inline;
begin
  vix.index := i;
end;

function cmpReci4v(const a, b: TReci4v): integer;
begin
  if a.i4 < b.i4 then exit(-1);
  if a.i4 > b.i4 then exit(+1);
  Result := 0;
end;

function mki4v(const i4: Ti4; const vix: TVertexIndex): TReci4v;
begin
  Result.i4 := i4;
  Result.vix := vix;
end;

function mki4v(const i4: Ti4): TReci4v;
begin
  Result.i4 := i4;
end;

function mki4v(const i4: Ti4; const index: integer; const vertex: TVertex): TReci4v;
begin
  Result.i4 := i4;
  Result.vix.index := index;
  Result.vix.vertex := vertex;
end;

function mki4v(const r4: TReci4v; const index: integer): TReci4v;
begin
  Result := r4;
  Result.vix.index := index;
end;


// TMapIndex
function makeMapIndex(i0, i1, i2: Ti4): TMapIndex;
begin
  Result.v[0] := i0;
  Result.v[1] := i1;
  Result.v[2] := i2;
end;

function makeMapIndex(i0, i1: Ti4): TMapIndex;
begin
  Result := makeMapIndex(i0, i1, zeroi4);
end;

function makeMapIndex(i0: Ti4): TMapIndex;
begin
  Result := makeMapIndex(i0, zeroi4, zeroi4);
end;

class operator TMapIndex. = (const a, b: TMapIndex): boolean;
begin
  if a.v[0] <> b.v[0] then
    exit(False);
  if a.v[1] <> b.v[1] then
    exit(False);
  Result := True;
end;

class operator TMapIndex. > (const a, b: TMapIndex): boolean;
var
  i: integer;
begin
  for i := 0 to high(a.v) do
    if a.v[i] <> b.v[i] then
      exit(a.v[i] > b.v[i]);
  Result := False;
end;

class operator TMapIndex. < (const a, b: TMapIndex): boolean;
var
  i: integer;
begin
  for i := 0 to high(a.v) do
    if a.v[i] <> b.v[i] then
      exit(a.v[i] < b.v[i]);
  Result := False;
end;

function cmpMapIndex(const a, b: TMapIndex): integer;
begin
  if a.v[0] <> b.v[0] then
    exit(cmpi4(a.v[0], b.v[0]));
  if a.v[1] <> b.v[1] then
    exit(cmpi4(a.v[1], b.v[1]));
  Result := cmpi4(a.v[2], b.v[2]);
end;

// TI4Vix
constructor TI4Vix.Create(_index: Ti4; _vix: TVertexIndex);
begin
  index := _index;
  vix := _vix;
end;

class operator TI4Vix. = (const a, b: TI4Vix): boolean;
begin
  Result := a.index = b.index;
end;

function cmpI4Vix(const a, b: TI4Vix): integer;
begin
  Result := cmpi4(a.index, b.index);
end;

// i4
class operator Ti4. = (const a, b: Ti4): boolean;
var
  i: integer;
begin
  for i := 0 to 3 do
    if a.v[i] <> b.v[i] then
      exit(False);
  Result := True;
end;

class operator Ti4. > (const a, b: Ti4): boolean;
var
  i: integer;
begin
  for i := 0 to 3 do
    if a.v[i] <> b.v[i] then
      exit(a.v[i] > b.v[i]);
  Result := False;
end;

class operator Ti4. < (const a, b: Ti4): boolean;
var
  i: integer;
begin
  for i := 0 to 3 do
    if a.v[i] <> b.v[i] then
      exit(a.v[i] < b.v[i]);
  Result := False;
end;

function cmpi4(const a, b: Ti4): integer;
var
  i: integer;
begin
  for i := 0 to 3 do
    if a.v[i] <> b.v[i] then
      exit(a.v[i] - b.v[i]);
  Result := 0;
end;

function mki4(i0: integer = -1; i1: integer = -1; i2: integer = -1;
  i3: integer = -1): Ti4;
begin
  Result.v[0] := i0 + 1;
  Result.v[1] := i1 + 1;
  Result.v[2] := i2 + 1;
  Result.v[3] := i3 + 1;
end;

function i4Min(v1, v2: integer): Ti4;
begin
  if v1 < v2 then
    Result := mki4(v1, v2)
  else
    Result := mki4(v2, v1);
end;

function i4Min(i, v1, v2: integer): Ti4;
begin
  if v1 < v2 then
    Result := mki4(i, v1, v2)
  else
    Result := mki4(i, v2, v1);
end;

// utils
function makeVertex(x, y, z: single): TVertex;
begin
  Result[0] := x;
  Result[1] := y;
  Result[2] := z;
end;

function makeFace(args: array of integer): TFace;
var
  i: integer;
begin
  Result := nil;
  setLength(Result, length(args));
  for i := 0 to high(args) do
    Result[i] := args[i];
end;

function makeToken(c: char): integer;
begin
  Result := integer(c) shl (8 * (sizeof(integer) - 1));
end;

function toArray(l: TListInt): TFace;
var
  i: integer;
begin
  Result := nil;
  setLength(Result, l.Count);
  for i := 0 to l.Count - 1 do
    Result[i] := l[i];
end;

function randomPalette(n: integer): TVertexes;

  function hsl2rgb(h, s, l: single): TVertex;

    function hue2rgb(p, q, t: single): single;
    begin
      if t < 0. then
        t += 1;
      if t > 1 then
        t -= 1;
      if t < 1 / 6 then
        exit(p + (q - p) * 6. * t);
      if t < 1 / 2 then
        exit(q);
      if t < 2 / 3 then
        exit(p + (q - p) * (2 / 3 - t) * 6);
      Result := p;
    end;

  var
    p, q: single;
  begin

    if s = 0 then
      exit(makeVertex(l, l, l)); // acromatic
    if l < 0.5 then
      q := l * (1. + s)
    else
      q := l + s - l * s;
    p := 2. * l - q;
    Result := makeVertex(hue2rgb(p, q, h + 1 / 3), hue2rgb(p, q, h),
      hue2rgb(p, q, h - 1 / 3));

  end;

  function rndColor: TVertex;

    function rnd: single;
    begin
      Result := random(1000) / 1000;
    end;

  begin
    Result := hsl2rgb(rnd, 0.5 * rnd + 0.3, 0.5 * rnd + 0.45);
  end;

var
  i: integer;
begin
  randomize;
  Result := nil;
  setlength(Result, n);
  for i := low(Result) to high(Result) do
    Result[i] := rndColor;
end;


end.
