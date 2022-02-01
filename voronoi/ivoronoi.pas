unit iVoronoi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UTF8Process;

type
  Point = record
    x, y : integer;
    color: uint32;
  end;

  Voronoi = class
    public
    points: array of Point;
    image: array of uint32;
    w, h: uint32;

    constructor Create(_w, _h, npts: integer);

    procedure genPixel(index: integer);
    procedure genImage;
    procedure genImageMTT; // TThread
    procedure genMTCPP; // c++ ffi
  end;

  pVoronoi = ^Voronoi;



implementation

constructor Voronoi.Create(_w, _h, npts: integer);
var
  i: integer;
begin
  randomize;

  w := _w;
  h := _h;
  SetLength(points, npts);
  setLength(image, w * h);

  for i := low(points) to high(points) do
  begin
    with points[i] do
    begin
      x := random(w);
      y := random(h);
      color := $ff000000 or random($00ffffff);
    end;
  end;
end;


procedure Voronoi.genPixel(index: integer);

  function distSqr(x, y, px, py: integer): integer; inline;
  begin
    Result := (x - px) * (x - px) + (y - py) * (y - py);
  end;

var
  dist, ind, p, d, i, j: integer;
  isCenter: boolean = False;

begin
  i := index mod w;
  j := index div w;

  dist := maxint;
  ind := 0;

  for p := low(points) to high(points) do
  begin
    d := distSqr(i, j, points[p].x, points[p].y);

    if d < 2 then // black dot in center
    begin
      isCenter := True;
      break;
    end;

    if d < dist then
    begin
      dist := d;
      ind := p;
    end;
  end;

  if isCenter then
    image[index] := $ff000000
  else
    image[index] := points[ind].color;

end;

procedure Voronoi.genImage;
var
  index: integer;
begin
  for index := 0 to w * h do
  begin
    genPixel(index);
  end;
end;



type  // MTrunner = class(TThrread)

  MTrunner = class(TThread)
  private
    pvrn: pVoronoi;
    th, nth: integer;
  protected
    procedure Execute; override;
  public
    constructor Create(_th, _nth: integer; _pvrn: pVoronoi);
  end;

constructor MTrunner.Create(_th, _nth: integer; _pvrn: pVoronoi);
begin
  inherited Create(False); // suspended=false -> start
  FreeOnTerminate := True;

  th := _th;
  nth := _nth;
  pvrn := _pvrn;

end;

procedure MTrunner.Execute;
var
  i: integer;
begin
  with pvrn^ do
  begin
    i := th; // thread index is image offset
    while i <= high(image) do
    begin
      genPixel(i);
      i := i + nth;
    end;
  end;
end;

procedure Voronoi.genImageMTT; // TThread
var
  nth, th: integer;
  ths: array of MTrunner;

begin
  nth := GetSystemThreadCount;
  setlength(ths, nth);

  for th := low(ths) to high(ths) do
    ths[th] := MTrunner.Create(th, nth, @self);

  for th := low(ths) to high(ths) do
    ths[th].WaitFor;

end;

// c++ interface
{$link cpp/voronoi.o}

{$linklib c}
{$linklib stdc++}
{$linklib gcc_s}
procedure genVoronoiMT(image : pointer; w, h, npts : uint32; points:pointer); cdecl; external;

procedure Voronoi.genMTCPP; // fpc wrapper to cpp
begin
 genVoronoiMT(@image[0], w, h, length(points), @points[0]);
end;

end.
