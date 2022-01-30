// mandelbrot unit


unit uMandelbrot;

{$mode objfpc}{$H+}

interface

uses ucomplex, SysUtils, UTF8Process, cthreads;

const
  Black: uint32 = $ff000000;

var
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

type

  Mandelbrot = class
  public
    w, h, iters: integer;
    center, range, cr: complex;
    image: array of uint32;
    difw, difh, scale: real;

    constructor Create(_w, _h, _iters: integer; _center, _range: complex);

    procedure print;
    function do_scale(i, j: integer): complex; inline;
    function size: integer;
    procedure genPixel(index: integer);
    procedure genImage;
    procedure genImageMT;
    procedure writeBin(Name: string);
    procedure writePPM(Name: string);

  end;

  pMandelbrot = ^Mandelbrot;

  bytefile = file of byte;

implementation

constructor Mandelbrot.Create(_w, _h, _iters: integer; _center, _range: complex);
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

  FillDWord(image[0], w * h, Black); // image := $ff000000

  for i := low(firePallete) to high(firePallete) do // firePallete:=$ff000000
    firePallete[i] := firePallete[i] or Black;
end;

function Mandelbrot.do_scale(i, j: integer): complex;
begin
  Result := cr + cinit(difw * i, difh * j);
end;

procedure Mandelbrot.genPixel(index: integer);

  function abs2(z: complex): real;
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

    z := z * z + c0; // z*z is the typical 2nd order fractal

    if abs2(z) > 4.0 then
    begin
      ix := k;
      break;
    end;
  end;

  if ix < iters then
    image[index] := firePallete[ix shl 2];

end;

procedure Mandelbrot.genImage;
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

// MT section
type
  MTParam = record    // param to thread func
    th, nth: integer; // thread #
    pmandel: pMandelbrot;
  end;
  pMTParam = ^MTParam;

function genPixels(p: pointer): ptrint;
var
  pparam: pMTParam;
  i: integer;
begin
  pparam := p;

  with pparam^, pmandel^ do
  begin
    i := th; // offset to image
    while i <= high(image) do
    begin
      genPixel(i);
      i := i + nth;
    end;
  end;
  Result := 0;
end;

procedure Mandelbrot.genImageMT;
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

    thParam[th].th := th;
    thParam[th].nth := nth;
    thParam[th].pmandel := @self;

    ths[th] := BeginThread(@genPixels, @thParam[th]);
  end;

  for th := low(ths) to high(ths) do
    WaitForThreadTerminate(ths[th], 0);
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
  BlockWrite(f, image[0], w * h * 4);
  CloseFile(f);
end;


end.
