unit domainColoring;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, zCompiler, uComplex, Math;

type

  TResultType = (rt_u32, rt_f32);


  { TDomainColoring }

  TDomainColoring = class
  public
    ci: complex;
    zComp: TZCompiler;

    image: array of uint32;
    f32image: array of single;
    w, h: integer;
    rt: TResultType; // u32, f32

  public
    constructor Create(_rt: TResultType; _w, _h: integer; _expr: string); overload;
    destructor Destroy; override;
    procedure genImage;
    procedure genImageMT;

    function getError: string;
  private
    procedure genPixel(i, j: integer);
    function HSV2int(_h, _s, _v: double): uint32;
    function rgb2u32(r, g, b: double): uint32;
    procedure HSV2rgb(_h, _s, _v: double; var r, g, b: double);
  end;

implementation

const
  PI2 = PI * 2;
  E = exp(1);

{ TDomainColoring }

constructor TDomainColoring.Create(_rt: TResultType; _w, _h: integer; _expr: string);
begin
  w := _w;
  h := _h;
  ci := cinit(1, 0);
  rt := _rt;

  zComp := TZCompiler.Create;
  zComp.compile(_expr);

  case rt of
    rt_u32: setLength(image, w * h);
    rt_f32: setLength(f32image, w * h * 3) // rgb
  end;
end;


destructor TDomainColoring.Destroy;
begin
  zComp.Free;
  image := nil;
  f32image := nil;

  inherited Destroy;
end;

procedure TDomainColoring.genImage;

var
  limit, rmi, rma, imi, ima, im, re, a, m, ranges, rangee, k, kk, sat, val: double;
  i, j: integer;
  v: complex;

  function pow3(x: double): double; inline;
  begin
    Result := x * x * x;
  end;

begin

  limit := PI;

  rmi := -limit;
  rma := limit;
  imi := -limit;
  ima := limit;

  for j := 0 to h - 1 do
  begin
    im := ima - (ima - imi) * j / (h - 1);
    for i := 0 to w - 1 do
    begin
      re := rma - (rma - rmi) * i / (w - 1);

      v := zComp.execute(cinit(re, im)); // evaluate here

      a := carg(v);  // arg
      while (a < 0) do
        a += PI2;
      a /= PI2;

      m := cmod(v); // abs
      ranges := 0;
      rangee := 1;

      while (m > rangee) do
      begin
        ranges := rangee;
        rangee *= E;
      end;

      k := (m - ranges) / (rangee - ranges);
      if k < 0.5 then kk := k * 2
      else
        kk := 1 - (k - 0.5) * 2;

      sat := 0.4 + (1 - pow3((1 - (kk)))) * 0.6;
      val := 0.6 + (1 - pow3((1 - (1 - kk)))) * 0.4;

      image[j * w + i] := HSV2int(a, sat, val);
    end;
  end;

end;

procedure TDomainColoring.genPixel(i, j: integer);
var
  limit, rmi, rma, imi, ima, im, re, a, m, ranges, rangee, k, kk, sat,
  val, r, g, b: double;
  v: complex;

  function pow3(x: double): double; inline;
  begin
    Result := x * x * x;
  end;

begin

  limit := PI;

  rmi := -limit;
  rma := limit;
  imi := -limit;
  ima := limit;

  im := ima - (ima - imi) * j / (h - 1);
  re := rma - (rma - rmi) * i / (w - 1);

  v := zComp.execute(cinit(re, im)); // evaluate here

  a := carg(v);  // arg
  while (a < 0) do
    a += PI2;
  a /= PI2;

  m := cmod(v); // abs
  ranges := 0;
  rangee := 1;

  while (m > rangee) do
  begin
    ranges := rangee;
    rangee *= E;
  end;

  k := (m - ranges) / (rangee - ranges);
  if k < 0.5 then kk := k * 2
  else
    kk := 1 - (k - 0.5) * 2;

  sat := 0.4 + (1 - pow3((1 - (kk)))) * 0.6;
  val := 0.6 + (1 - pow3((1 - (1 - kk)))) * 0.4;

  case rt of
    rt_u32: image[j * w + i] := HSV2int(a, sat, val);
    rt_f32: begin
      r := 0;
      g := 0;
      b := 0;
      HSV2rgb(a, sat, val, r, g, b);
      f32image[(j * w + i) * 3 + 0] := r;
      f32image[(j * w + i) * 3 + 1] := g;
      f32image[(j * w + i) * 3 + 2] := b;
    end;

  end;
end;

function TDomainColoring.getError: string;
begin
  Result := zComp.getErrMessage;
end;

function TDomainColoring.HSV2int(_h, _s, _v: double): uint32;
var
  r, g, b, z, f, p, q, t: double;
  i: integer;

begin
  // convert hsv to int with alpha 0xff00000
  r := 0;
  g := 0;
  b := 0;

  if _s = 0 then
  begin
    r := _v;
    g := _v;
    b := _v;
  end
  else
  begin
    if _h = 1 then
      _h := 0;
    z := floor(_h * 6);
    i := round(z);
    f := _h * 6 - z;
    p := _v * (1 - _s);
    q := _v * (1 - _s * f);
    t := _v * (1 - _s * (1 - f));

    case i of
      0: begin
        r := _v;
        g := t;
        b := p;
      end;
      1: begin
        r := q;
        g := _v;
        b := p;
      end;
      2: begin
        r := p;
        g := _v;
        b := t;
      end;
      3: begin
        r := p;
        g := q;
        b := _v;
      end;
      4: begin
        r := t;
        g := p;
        b := _v;
      end;
      5: begin
        r := _v;
        g := p;
        b := q;
      end;
    end;
  end;
  Result := rgb2u32(r, g, b);
end;

procedure TDomainColoring.HSV2rgb(_h, _s, _v: double; var r, g, b: double);
var
  z, f, p, q, t: double;
  i: integer;

begin
  // convert hsv to int with alpha 0xff00000
  r := 0;
  g := 0;
  b := 0;

  if _s = 0 then
  begin
    r := _v;
    g := _v;
    b := _v;
  end
  else
  begin
    if _h = 1 then
      _h := 0;
    z := floor(_h * 6);
    i := round(z);
    f := _h * 6 - z;
    p := _v * (1 - _s);
    q := _v * (1 - _s * f);
    t := _v * (1 - _s * (1 - f));

    case i of
      0: begin
        r := _v;
        g := t;
        b := p;
      end;
      1: begin
        r := q;
        g := _v;
        b := p;
      end;
      2: begin
        r := p;
        g := _v;
        b := t;
      end;
      3: begin
        r := p;
        g := q;
        b := _v;
      end;
      4: begin
        r := t;
        g := p;
        b := _v;
      end;
      5: begin
        r := _v;
        g := p;
        b := q;
      end;
    end;
  end;
end;

function TDomainColoring.rgb2u32(r, g, b: double): uint32;
var
  c, color: uint32;
begin
  color := $ff000000;
  // alpha = 0xff
  c := round(255 * r) and $ff;
  color := color or c;
  c := round(255 * g) and $ff;
  color := color or (c << 8);
  c := round(255 * b) and $ff;
  color := color or (c << 16);
  Result := color;
end;


// TThread
type

  pDomainColoring = ^TDomainColoring;

  DCThread = class(TThread)
  private
    pdc: pDomainColoring;
    th, nth: integer;
  protected
    procedure Execute; override;
  public
    constructor Create(_th, _nth: integer; _pdc: pDomainColoring);
  end;

constructor DCThread.Create(_th, _nth: integer; _pdc: pDomainColoring);
begin
  inherited Create(False); // suspended=false -> start
  FreeOnTerminate := False;

  th := _th;
  nth := _nth;
  pdc := _pdc;
end;

procedure DCThread.Execute;
var
  i, n: integer;
begin
  with pdc^ do
  begin
    n := w * h;
    i := th; // thread index is image offset
    while i < n do
    begin
      genPixel(i mod w, i div w);

      i := i + nth;
    end;
  end;
end;


// MT generation
procedure TDomainColoring.genImageMT; // TThread
var
  nth, th: integer;
  ths: array of DCThread;

begin
  nth := GetCPUCount;
  setlength(ths, nth);

  for th := low(ths) to high(ths) do
    ths[th] := DCThread.Create(th, nth, @self);

  for th := low(ths) to high(ths) do
  begin
    ths[th].WaitFor;
    ths[th].Free;
  end;
end;

end.
