unit uvec3;

{$mode objfpc}{$H+}
{$modeSwitch ISOUnaryMinus}

interface

uses
  Classes, SysUtils, Math;

type
  TVec3 = array [0..2] of single;

operator +(a, b: TVec3) r: TVec3;
operator -(a, b: TVec3) r: TVec3;
operator -(a: TVec3) r: TVec3;
operator / (a: TVec3; f: single) r: TVec3;
operator * (a: TVec3; f: single) r: TVec3;
operator := (f: single) r: TVec3;
operator = (const a, b: TVec3) r: boolean;

function equal(const a, b: TVec3): boolean;
procedure zero(var v: TVec3);
function dot(a, b: TVec3): single;
function cross(a, b: TVec3): TVec3;
function normal(v0, v1, v2: TVec3): TVec3;
function distSq(v: TVec3): single;
function dist(v: TVec3): single;
function unitv(v: TVec3): TVec3;
function normalize(v: TVec3): TVec3;
function maxAbs(v: TVec3): single;
function mid(a, b: TVec3): TVec3;
function tween(a, b: TVec3; t: single): TVec3;
function oneThird(a, b: TVec3): TVec3;
function midpoint(a, b: TVec3): TVec3;

const
  zerov: TVec3 = (0, 0, 0);

implementation

// TVec3

operator = (const a, b: TVec3) r: boolean;
begin
  r := (a[0] = b[0]) and (a[1] = b[1]) and (a[2] = b[2]);
end;

function equal(const a, b: TVec3): boolean;
begin
  Result := (a = b);
end;

operator +(a, b: TVec3) r: TVec3;
begin
  r[0] := a[0] + b[0];
  r[1] := a[1] + b[1];
  r[2] := a[2] + b[2];
end;

operator -(a, b: TVec3) r: TVec3;
begin
  r[0] := a[0] - b[0];
  r[1] := a[1] - b[1];
  r[2] := a[2] - b[2];
end;

operator -(a: TVec3) r: TVec3;
begin
  r[0] := -a[0];
  r[1] := -a[1];
  r[2] := -a[2];
end;

operator / (a: TVec3; f: single) r: TVec3;
begin
  r[0] := a[0] / f;
  r[1] := a[1] / f;
  r[2] := a[2] / f;
end;

operator * (a: TVec3; f: single) r: TVec3;
begin
  r[0] := a[0] * f;
  r[1] := a[1] * f;
  r[2] := a[2] * f;
end;

operator := (f: single) r: TVec3;
begin
  r[0] := f;
  r[1] := f;
  r[2] := f;
end;

procedure zero(var v: TVec3);
begin
  v := 0;
end;

function dot(a, b: TVec3): single;
begin
  Result := a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
end;

function cross(a, b: TVec3): TVec3;
begin
  Result[0] := (a[1] * b[2] - a[2] * b[1]);
  Result[1] := (a[2] * b[0] - a[0] * b[2]);
  Result[2] := (a[0] * b[1] - a[1] * b[0]);
end;

function normal(v0, v1, v2: TVec3): TVec3;
begin
  Result := cross(v1 - v0, v2 - v1);
end;

function distSq(v: TVec3): single;   inline;
begin
  Result := dot(v, v);
end;

function dist(v: TVec3): single; inline;
begin
  Result := sqrt(distSq(v));
end;

function unitv(v: TVec3): TVec3; inline;
begin
  if v = zerov then Result := v
  else
    Result := v / dist(v);
end;

function normalize(v: TVec3): TVec3;
begin
  Result := unitv(v);
end;

function maxAbs(v: TVec3): single;
begin
  Result := max(abs(v[0]), max(abs(v[1]), abs(v[2])));
end;

function mid(a, b: TVec3): TVec3;
begin
  Result := (a + b) / 2;
end;

function tween(a, b: TVec3; t: single): TVec3;
begin
  Result := (a * (1 - t)) + (b * t);
end;

function oneThird(a, b: TVec3): TVec3;
begin
  Result := tween(a, b, 1 / 3);
end;

function midpoint(a, b: TVec3): TVec3;
begin
  Result := (a + b) / 2;
end;

end.
