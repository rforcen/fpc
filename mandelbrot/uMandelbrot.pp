// mandelbrot unit


unit uMandelbrot;

{$mode objfpc}{$H+}

interface

uses ipf, uComplex, SysUtils, UTF8Process, cthreads, Classes, ColorMap;

type

  Mandelbrot = class
  public
    w, h, iters: integer;
    center, range, cr: complex;
    image: array of uint32;
    pallete: array of uint32;
    difw, difh, scale: double;

    constructor Create(_w, _h, _iters, _colorSet: integer; _center, _range: complex);

    procedure print;
    function do_scale(i, j: integer): complex; inline;
    function size: integer;
    function nBytes: integer;
    procedure genPixel(index: integer);
    procedure genImage;
    procedure genImageMT;
    procedure genImageMTT;

    procedure genMTCPPf32; // cpp  wrappers
    procedure genMTCPPf64;
    procedure genMTCPPf128;
    procedure genMPFR;

    procedure writeBin(Name: string);
    procedure writePPM(Name: string);

    procedure genInterpolatedPallete;
    procedure colorMapPallete(_colorSet: integer);
  end;

  pMandelbrot = ^Mandelbrot;

  bytefile = file of byte;

implementation

const
  Black: uint32 = $ff000000;


  firePallete: array[0..255] of
    uint32 = (0, 0, 4, 12, 16, 24, 32, 36, 44, 48, 56, 64, 68, 76,
    80, 88, 96, 100, 108, 116, 120, 128, 132, 140, 148, 152, 160, 164,
    172, 180, 184, 192, 200, 1224, 3272, 4300, 6348, 7376, 9424,
    10448, 12500, 14548, 15576, 17624, 18648, 20700, 21724, 23776,
    25824, 26848, 28900, 29924, 31976, 33000, 35048, 36076, 38124,
    40176, 41200, 43248, 44276, 46324, 47352, 49400, 51452, 313596,
    837884, 1363196, 1887484, 2412796, 2937084, 3461372, 3986684,
    4510972, 5036284, 5560572, 6084860, 6610172, 7134460, 7659772,
    8184060, 8708348, 9233660, 9757948, 10283260, 10807548, 11331836,
    11857148, 12381436, 12906748, 13431036, 13955324, 14480636, 15004924,
    15530236, 16054524, 16579836, 16317692, 16055548, 15793404, 15269116,
    15006972, 14744828, 14220540, 13958396, 13696252, 13171964, 12909820,
    12647676, 12123388, 11861244, 11599100, 11074812, 10812668, 10550524,
    10288380, 9764092, 9501948, 9239804, 8715516, 8453372, 8191228,
    7666940, 7404796, 7142652, 6618364, 6356220, 6094076, 5569788,
    5307644, 5045500, 4783356, 4259068, 3996924, 3734780, 3210492,
    2948348, 2686204, 2161916, 1899772, 1637628, 1113340, 851196,
    589052, 64764, 63740, 62716, 61692, 59644, 58620, 57596, 55548,
    54524, 53500, 51452, 50428, 49404, 47356, 46332, 45308, 43260,
    42236, 41212, 40188, 38140, 37116, 36092, 34044, 33020, 31996,
    29948, 28924, 27900, 25852, 24828, 23804, 21756, 20732, 19708,
    18684, 16636, 15612, 14588, 12540, 11516, 10492, 8444, 7420, 6396,
    4348, 3324, 2300, 252, 248, 244, 240, 236, 232, 228, 224, 220,
    216, 212, 208, 204, 200, 196, 192, 188, 184, 180, 176, 172, 168,
    164, 160, 156, 152, 148, 144, 140, 136, 132, 128, 124, 120, 116,
    112, 108, 104, 100, 96, 92, 88, 84, 80, 76, 72, 68, 64, 60, 56,
    52, 48, 44, 40, 36, 32, 28, 24, 20, 16, 12, 8, 0, 0);


constructor Mandelbrot.Create(_w, _h, _iters, _colorSet: integer;
  _center, _range: complex);
var
  i: integer;
begin
  w := _w;
  h := _h;
  iters := _iters;
  center := _center;
  range := _range;
  cr := cinit(range.re, range.re);

  difw := (range.im - range.re) / w;
  difh := (range.im - range.re) / h;
  scale := 0.8 * w / h;

  setlength(image, w * h);
  setLength(pallete, iters);

  if iters > length(firePallete) then
    genInterpolatedPallete
  else
  begin
    for i := low(firePallete) to high(firePallete) do
      pallete[i] := Black or firePallete[i];
  end;


  FillDWord(image[0], w * h, Black); // image := $ff000000
end;

procedure Mandelbrot.colorMapPallete(_colorSet: integer);
var
  i: integer;
  color: vec3;
begin
  for i := low(pallete) to high(pallete) do
  begin
    color := colorMap.colorMap(i / iters, 0, 1, _colorSet);
    pallete[i] := Black or (round(color[0] * 255) shl 16) or
      (round(color[1] * 255) shl 8) or round(color[2] * 255);
    //pallete[i]:=random($00ffffff) or $ff000000;
  end;
end;

procedure Mandelbrot.genInterpolatedPallete;
var
  i: integer;
  x, y, d2s: array of extended;
  term: longint = 0;
  n: longint = length(firePallete);
  xi, yi: extended;

begin
  setLength(x, n);
  setLength(y, n);
  setLength(d2s, n - 2);

  // gen x=0..n-1, y(firePallete)
  for i := low(firePallete) to high(firePallete) do
  begin
    x[i] := i;
    y[i] := firePallete[i];
  end;

  // Interpolation
  ipfisn(n - 1, x[0], y[0], d2s[0], term);

  if term = 1 then // ok ?
  begin
    for i := low(pallete) to high(pallete) do // set interpolated values
    begin
      xi := i * n / length(pallete);
      yi := ipfspn(n - 1, x[0], y[0], d2s[0], xi, term);

      if yi < 0 then
        yi := 0;

      pallete[i] := Black or round(yi);
    end;
  end;
end;

function Mandelbrot.do_scale(i, j: integer): complex;
begin
  Result := cr + cinit(difw * i, difh * j);
end;

procedure Mandelbrot.genPixel(index: integer);

  function abs2(z: complex): double; inline;
  begin
    Result := (z.re * z.re) + (z.im * z.im);
  end;

var
  c0, z: complex;
  ix, k: integer;

begin

  ix := iters;
  c0 := scale * do_scale(index mod w, index div w) - center;
  z := c0;

  for k := 0 to iters do
  begin

    z := z * z + c0; //  z*z is the typical 2nd order fractal

    if abs2(z) > 4.0 then
    begin
      ix := k;
      break;
    end;
  end;

  if ix <> iters then
    image[index] := pallete[ix];

end;

procedure Mandelbrot.genImage; // single thread sequential
var
  i: integer;
begin
  for i := low(image) to high(image) do
    genPixel(i);
end;

function Mandelbrot.size: integer;
begin
  Result := w * h;
end;

function Mandelbrot.nBytes: integer;
begin
  Result := w * h * sizeof(uint32);
end;

// MT section,  2 implementations:
// 1. BeginThread / WaitForThreadTerminate
// 2. TThread

type
  MTParam = class    // param to thread func
    th, nth: integer; // thread #
    pmandel: pMandelbrot;

    constructor Create(_th, _nth: integer; _pmandel: pMandelbrot);
  end;

  pMTParam = ^MTParam;

constructor MTParam.Create(_th, _nth: integer; _pmandel: pMandelbrot);
begin
  th := _th;
  nth := _nth;
  pmandel := _pmandel;
end;

// called from BeginThread
function genPixels(p: pointer): ptrint;
var
  pparam: pMTParam;
  i: integer;
begin
  pparam := p;

  with pparam^, pmandel^ do
  begin
    i := th; // offset to image
    while i < high(image) do
    begin
      genPixel(i);
      i := i + nth;
    end;
  end;
  Result := 0;
end;

procedure Mandelbrot.genImageMT;  // BeginThread / WaitForThreadTerminate
var
  nth, th: integer;
  ths: array of TThreadID;
  thParam: array of MTParam;

begin
  nth := GetSystemThreadCount;
  setlength(ths, nth);
  setlength(thParam, nth);

  for th := low(ths) to high(ths) do
  begin
    thParam[th] := MTParam.Create(th, nth, @self);
    ths[th] := BeginThread(@genPixels, @thParam[th]);
  end;

  for th := low(ths) to high(ths) do
  begin
    WaitForThreadTerminate(ths[th], 0);
    thParam[th].Free;
  end;
end;

// MTrunner = class(TThrread)
type
  MTrunner = class(TThread)
  private
    pmandel: pMandelbrot;
    th, nth: integer;
  protected
    procedure Execute; override;
  public
    constructor Create(_th, _nth: integer; _pmandel: pMandelbrot);
  end;

constructor MTrunner.Create(_th, _nth: integer; _pmandel: pMandelbrot);
begin
  inherited Create(False); // suspended=false -> start
  FreeOnTerminate := False;

  th := _th;
  nth := _nth;
  pmandel := _pmandel;

end;

procedure MTrunner.Execute;
var
  i: integer;
begin
  with pmandel^ do
  begin
    i := th; // thread index is image offset
    while i < high(image) do
    begin
      genPixel(i);
      i := i + nth;
    end;
  end;
end;

procedure Mandelbrot.genImageMTT; // TThread   slower than BeginThread
var
  nth, th: integer;
  ths: array of MTrunner;

begin
  nth := GetSystemThreadCount;
  setlength(ths, nth);

  for th := low(ths) to high(ths) do
    ths[th] := MTrunner.Create(th, nth, @self);

  for th := low(ths) to high(ths) do
  begin
    ths[th].WaitFor;
    ths[th].Free;
  end;
end;


// c++ interface
{$linklib c}
{$linklib stdc++}
{$linklib gcc_s}
{$linklib m}
{$linklib mpfr}
{$linklib mandelbrot.a}


procedure genMandelbrotMTf32(image: pointer; w, h, iters: uint32;
  center, range: complex); cdecl; external;
procedure genMandelbrotMTf64(image: pointer; w, h, iters: uint32;
  center, range: complex); cdecl; external;
procedure genMandelbrotMTf128(image: pointer; w, h, iters: uint32;
  center, range: complex); cdecl; external;
procedure genMandelbrotMTmpreal(image, pallete: pointer; w, h, iters: uint32;
  center, range: complex); cdecl; external;

procedure Mandelbrot.genMTCPPf32; // fpc wrapper to cpp
begin
  genMandelbrotMTf32(@image[0], w, h, iters, center, range);
end;

procedure Mandelbrot.genMTCPPf64;
begin
  genMandelbrotMTf64(@image[0], w, h, iters, center, range);
end;

procedure Mandelbrot.genMTCPPf128;
begin
  genMandelbrotMTf128(@image[0], w, h, iters, center, range);
end;

procedure Mandelbrot.genMPFR;
begin
  genMandelbrotMTmpreal(@image[0], @pallete[0], w, h, iters, center, range);
end;


procedure Mandelbrot.print;
begin
  writeln('w:', w, ', h:', h, ', center:', center.re, ',', center.im, ', range:',
    range.re, range.im);
end;

procedure Mandelbrot.writeBin(Name: string);
var
  f: file;
begin
  AssignFile(f, Name);
  Rewrite(f, 1);
  BlockWrite(f, image[0], w * h * 4);
  CloseFile(f);
end;


procedure Mandelbrot.writePPM(Name: string);

  procedure writeString(var f: bytefile; s: string);
  begin
    BlockWrite(f, s[1], length(s));
  end;

var
  f: bytefile;

begin
  AssignFile(f, Name);
  Rewrite(f, 1); // ppm header
  writeString(f, format('P7'#10'WIDTH %0:d'#10'HEIGHT %1:d'#10'DEPTH 4'#10'MAXVAL 255'#10'TUPLTYPE RGB_ALPHA'#10'ENDHDR'#10'', [w, h]));
  BlockWrite(f, image[0], nBytes);
  CloseFile(f);
end;


end.
