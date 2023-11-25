unit usphericalharmonics;

{$mode objfpc}{$H+}

interface

uses v3;

type
  Trig = vec3u;
  CodeVec = array[0..7] of integer;

  TTask = (tkVertex, tkNormal);

  Vertex = record
    pos, norm, uv, color: vec3;
  end;

  Mesh = class
    shape: array of Vertex;
    trigs: array of Trig;

    constructor Create(n: integer);
  end;

  SphericalHarmonics = class
  public
    n, size, color_map, nn: integer;
    maxVal: single;
    code: CodeVec;
    amesh: Mesh;

    constructor Create(_n, ncode, _color_map: integer);

    function calcVertex(theta, phi: single): vec3;
    procedure generateFaces;
    procedure setVertex(index: integer);
    procedure setNormal(i: integer);
    procedure genNormals;
    procedure genShapeMT;
    procedure writePLY(Name: string);
    procedure writePLYStream(Name: string);
    procedure genShapeMTT; // TThread
  end;


implementation

uses
  Classes, SysUtils, Math, colorMap, UTF8Process, shCodes; // cthreads

// Vertex
function mkVertex(pos, norm, uv, color: vec3): Vertex; inline;
begin
  Result.pos := pos;
  Result.norm := norm;
  Result.uv := uv;
  Result.color := color;
end;

//  sh
function code2vec(code: integer): CodeVec;
var
  m, i: integer;
begin
  m := SphericalHarmonicsCodes[code mod length(SphericalHarmonicsCodes)];

  for i := low(Result) to high(Result) do
  begin
    Result[7 - i] := m mod 10;
    m := m div 10;
  end;
end;

function SphericalHarmonics.calcVertex(theta, phi: single): vec3;
var
  r: single;
begin
  r :=
    sin(code[0] * phi) ** code[1] + cos(code[2] * phi) ** code[3] +
    sin(code[4] * theta) ** code[5] + cos(code[6] * theta) ** code[7];

  Result := mkvec3(r * sin(phi) * cos(theta), r * cos(phi), r *
    sin(phi) * sin(theta));
end;

// ColorMap.c wrap
//{$link ColorMap.o}
//{$linklib c}

//function colorMap(vp, _vmin, _vmax: single; cm: integer) : vec3; cdecl; external;

procedure SphericalHarmonics.setVertex(index: integer);
const
  PI2 = PI * 2;
var
  dx, du, dv, u, v, _v, colorOffset: single;
  i, j: integer;
  position, color, texture: vec3;
begin
  dx := 1 / n;
  du := PI2 * dx; // Theta
  dv := PI * dx;  // Phi
  i := index div n;
  j := index mod n;
  u := du * i;
  v := dv * j;
  if i and 1 = 0 then
    colorOffset := u
  else
    colorOffset := u + du;


  position := calcVertex(u, v);

  for _v in position do // update maxVal
    maxVal := max(maxVal, abs(_v));

  //_normal := normal(position, calcVertex(code, u + du, v), calcVertex(code, u, v + dv));
  color := colorMap.colorMap(colorOffset, 0, PI2, color_map);
  texture := mkvec3(i * dx, j * dx, 0);

  amesh.shape[index] := mkVertex(position, vec3Zero, color, texture);
  // normals calc after
end;

procedure SphericalHarmonics.setNormal(i: integer);
var
  ix1, ixn: integer;
begin
  ix1 := i + 1;
  ixn := i + n;
  if ix1 = nn then
    ix1 := i - 1;
  if ixn >= nn then
    ixn := i - n;
  amesh.shape[i].norm := normal(amesh.shape[i].pos, amesh.shape[ix1].pos,
    amesh.shape[ixn].pos);
end;

procedure SphericalHarmonics.genNormals;
var
  i: integer;
begin
  for i := 0 to nn - 1 do
    setNormal(i);
end;

// MT section
type

  pSphericalHarmonics = ^SphericalHarmonics;

  MTParam = class    // param to thread func
    th, nth: integer; // thread #
    tsk: TTask;
    psh: pSphericalHarmonics;

    constructor Create(_th, _nth: integer; _tsk: TTask; _psh: pSphericalHarmonics);
  end;

  pMTParam = ^MTParam;

constructor MTParam.Create(_th, _nth: integer; _tsk: TTask; _psh: pSphericalHarmonics);
begin
  th := _th;
  nth := _nth;
  tsk := _tsk;
  psh := _psh;
end;

// called from BeginThread
function setShape(p: pointer): ptrint;
var
  i: integer;
begin

  with pMTParam(p)^, psh^ do
  begin
    i := th; // offset to image
    while i < nn do
    begin
      case tsk of
        tkVertex: setVertex(i);
        tkNormal: setNormal(i);
      end;
      i := i + nth;
    end;
  end;
  Result := 0;
end;


procedure SphericalHarmonics.genShapeMT;
// BeginThread / WaitForThreadTerminate
var
  nth, th: integer;
  ths: array of TThreadID;
  ath: TThreadID;
  thParam: array of MTParam;
  tsk: TTask;

begin
  nth := GetSystemThreadCount;
  setlength(ths, nth);
  setlength(thParam, nth);

  for tsk in [tkVertex, tkNormal] do
  begin
    for th := low(ths) to high(ths) do
    begin
      thParam[th] := MTParam.Create(th, nth, tsk, @self);
      ths[th] := BeginThread(@setShape, @thParam[th]);
    end;

    for ath in ths do
      WaitForThreadTerminate(ath, 0);
  end;
end;


// TThread
type
  SHThread = class(TThread)
  private
    psh: pSphericalHarmonics;
    th, nth: integer;
    tsk: TTask;
  protected
    procedure Execute; override;
  public
    constructor Create(_th, _nth: integer; _psh: pSphericalHarmonics; _tsk: TTask);
  end;

constructor SHThread.Create(_th, _nth: integer; _psh: pSphericalHarmonics; _tsk: TTask);
begin
  inherited Create(False); // suspended=false -> start
  FreeOnTerminate := True;

  th := _th;
  nth := _nth;
  psh := _psh;
  tsk := _tsk;
end;

procedure SHThread.Execute;
var
  i: integer;
begin
  with psh^ do
  begin
    i := th; // thread index is image offset
    while i < nn do
    begin
      case tsk of
        tkVertex: setVertex(i);
        tkNormal: setNormal(i);
      end;

      i := i + nth;
    end;
  end;
end;

procedure SphericalHarmonics.genShapeMTT; // TThread
var
  nth, th: integer;
  ths: array of SHThread;
  ath: SHThread;
  tsk: TTask;

begin
  nth := GetSystemThreadCount;
  setlength(ths, nth);

  for tsk in [tkVertex, tkNormal] do
  begin
    for th := low(ths) to high(ths) do
      ths[th] := SHThread.Create(th, nth, @self, tsk);

    for ath in ths do
      ath.WaitFor;
  end;
end;
// TThread



procedure SphericalHarmonics.generateFaces;
type
  vec2x3u = array[0..1] of vec3u;

  function triangularize(a, b, c, d: integer): vec2x3u; inline;
  begin
    Result[0] := mkvec3u(a, b, c);
    Result[1] := mkvec3u(a, c, d);
  end;

var
  i, j, k: integer;
  v4: vec2x3u;

begin
  k := 0;

  for i := 0 to n - 2 do
  begin
    for j := 0 to n - 2 do
    begin
      v4 := triangularize((i + 1) * n + j, (i + 1) * n + j + 1,
        i * n + j + 1, i * n + j);
      amesh.trigs[k] := v4[0];
      Inc(k);
      amesh.trigs[k] := v4[1];
      Inc(k);
    end;
    v4 := triangularize((i + 1) * n, (i + 1) * n + n - 1, i * n, i * n + n - 1);
    amesh.trigs[k] := v4[0];
    Inc(k);
    amesh.trigs[k] := v4[1];
    Inc(k);
  end;

  for i := 0 to n - 2 do
  begin
    v4 := triangularize(i, i + 1, n * (n - 1) + i + 1, n * (n - 1) + i);
    amesh.trigs[k] := v4[0];
    Inc(k);
    amesh.trigs[k] := v4[1];
    Inc(k);
  end;
end;

constructor Mesh.Create(n: integer);
var
  n1: integer;
begin
  n1 := n - 1;
  setLength(shape, n * n);
  setLength(trigs, 2 * n1 * (n1 + 2));  // 2*((n1*n1+n1)+n1)
end;

constructor SphericalHarmonics.Create(_n, ncode, _color_map: integer);
begin
  n := _n;
  nn := n * n;
  code := code2Vec(ncode);
  color_map := _color_map;

  amesh := Mesh.Create(n);

  genShapeMT;

  //genShapeMTT;
  generateFaces;
end;

procedure SphericalHarmonics.writePLY(Name: string);   // slow write
type
  byteFile = file of byte;

  procedure writev3(var f: byteFile; v: vec3);
  begin
    BlockWrite(f, v, sizeof(v));
  end;

const
  plyHeader = 'ply' + #10 + 'format binary_little_endian 1.0' + #10 +
    'comment polygonizer generated' + #10 + 'element vertex %0d' +
    #10 + 'property float x' + #10 + 'property float y' + #10 +
    'property float z' + #10 + 'property float nx' + #10 + 'property float ny' +
    #10 + 'property float nz' + #10 + 'property uchar red' + #10 +
    'property uchar green' + #10 + 'property uchar blue' + #10 +
    'element face %1d' + #10 + 'property list uchar int vertex_indices' +
    #10 + 'end_header' + #10;
var
  f: byteFile;
  sHead: string;
  i, j: integer;
  c3b: array[0..2] of byte;

begin
  // replace nvertex / n trigs in header
  sHead := format(plyHeader, [length(amesh.shape), length(amesh.trigs)]);

  AssignFile(f, Name);
  Rewrite(f, 1);

  BlockWrite(f, sHead[1], length(sHead)); // write header

  for i := low(amesh.shape) to high(amesh.shape) do
  begin
    writev3(f, amesh.shape[i].pos);  // vertex(pos), normal(norm)
    writev3(f, amesh.shape[i].norm);
    for j := 0 to 2 do // color as 3 bytes 0..255
      c3b[j] := round(amesh.shape[i].color[j] * 255.0) mod 255;
    BlockWrite(f, c3b, sizeof(c3b));
  end;
  c3b[0] := 3;
  for i := low(amesh.trigs) to high(amesh.trigs) do
  begin
    BlockWrite(f, c3b, 1);
    BlockWrite(f, amesh.trigs[i], sizeof(amesh.trigs[i]));
  end;
  CloseFile(f);
end;

procedure SphericalHarmonics.writePLYStream(Name: string); // fast memory stream write
var
  ms: TMemoryStream;
  sHead: string = 'ply' + #10 + 'format binary_little_endian 1.0' +
    #10 + 'comment polygonizer generated' + #10 + 'element vertex %0d' +
    #10 + 'property float x' + #10 + 'property float y' + #10 +
    'property float z' + #10 + 'property float nx' + #10 + 'property float ny' +
    #10 + 'property float nz' + #10 + 'property uchar red' + #10 +
    'property uchar green' + #10 + 'property uchar blue' + #10 +
    'element face %1d' + #10 + 'property list uchar int vertex_indices' +
    #10 + 'end_header' + #10;
  j: integer;
  v: Vertex;
  t: Trig;
  c3b: array[0..2] of byte;
begin
  // replace nvertex / n trigs in header
  sHead := format(sHead, [length(amesh.shape), length(amesh.trigs)]);

  ms := TMemoryStream.Create;
  ms.Write(sHead[1], length(sHead)); // write header

  for v in amesh.shape do
  begin
    ms.Write(v.pos, sizeof(vec3));  // vertex(pos), normal(norm)
    ms.Write(v.norm, sizeof(vec3));
    for j := 0 to 2 do // color as 3 bytes 0..255
      c3b[j] := round(v.color[j] * 255.0) mod 255;
    ms.Write(c3b, sizeof(c3b));
  end;

  c3b[0] := 3;
  for t in amesh.trigs do
  begin
    ms.Write(c3b, 1);
    ms.Write(t, sizeof(Trig));
  end;

  ms.SaveToFile(Name); // done -> write
  ms.Destroy;
end;

end.
