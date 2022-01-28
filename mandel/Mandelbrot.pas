//#!/usr/bin/instantfpc
program Mandelbrot;
{$mode objfpc}{$H+}

uses uMandelbrot, u32toimage, ucomplex, SysUtils, DateUtils;

procedure DoMandelbrot;
var 
  w, h, iters : integer;
  center, range : complex;
  mandel : uMandelbrot.Mandelbrot;

  t0 : TDateTime;

begin
  w:=1024*2; h:=w; iters:=200;
  center := cinit(0.5, 0.0);
  range := cinit(-2.0, 2.0);

  mandel:= uMandelbrot.Mandelbrot.create(w,h,iters, center, range);

  write('mandel ',w,'x',h,',iters:',iters,'...');
  t0:=now;

  mandel.genImage;
  
  writeln('lap:', MilliSecondsBetween(now, t0), 'ms');

  mandel.writeJPG('mandel.jpg');

  mandel.free;
end;

procedure testMem;
var 
  iters : integer;
  mandel : uMandelbrot.Mandelbrot;
begin
  for iters:=0 to 1000 do begin
    mandel:= uMandelbrot.Mandelbrot.create(1024, 1024,200, cinit(0.5, 0.0), cinit(-2.0, 2.0));
    write(iters, ',');
    mandel.free; // must free mandel object
  end;
end;

/////
begin
  DoMandelbrot;
  // testMem;
end.