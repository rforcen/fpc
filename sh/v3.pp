unit v3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  vec3 = array[0..2] of single;
  vec3u = array [0..2] of uint32;

function mkvec3(a, b, c: single): vec3;
function mkvec3u(a, b, c: uint32): vec3u;
operator +(a, b: vec3): vec3;
operator -(a, b: vec3): vec3;
operator -(a: vec3; b: single): vec3;
operator * (a, b: vec3): vec3;
operator * (a: vec3; b: single): vec3;
operator / (a, b: vec3): vec3;
operator / (a: vec3; b: single): vec3;
operator = (a, b: vec3): boolean;
function hypot(v: vec3): single;
operator ** (a, b: vec3): vec3;   // cross prod
function normalize(v: vec3): vec3;
function normal(v0, v1, v2: vec3): vec3;
function unormal(v0, v1, v2: vec3): vec3;
operator ** (a: single; b: integer): single;

const
  vec3Zero: vec3 = (0, 0, 0);

implementation

// vec3

function mkvec3(a, b, c: single): vec3;
begin
  Result[0] := a;
  Result[1] := b;
  Result[2] := c;
end;

operator +(a, b: vec3): vec3;
begin
  Result := mkvec3(a[0] + b[0], a[1] + b[1], a[2] + b[2]);
end;

operator -(a, b: vec3): vec3;
begin
  Result := mkvec3(a[0] - b[0], a[1] - b[1], a[2] - b[2]);
end;

operator -(a: vec3; b: single): vec3;
begin
  Result := mkvec3(a[0] - b, a[1] - b, a[2] - b);
end;

operator * (a, b: vec3): vec3;
begin
  Result := mkvec3(a[0] * b[0], a[1] * b[1], a[2] * b[2]);
end;

operator * (a: vec3; b: single): vec3;
begin
  Result := mkvec3(a[0] * b, a[1] * b, a[2] * b);
end;

operator / (a, b: vec3): vec3;
begin
  Result := mkvec3(a[0] / b[0], a[1] / b[1], a[2] / b[2]);
end;

operator / (a: vec3; b: single): vec3;
begin
  Result := mkvec3(a[0] / b, a[1] / b, a[2] / b);
end;

operator = (a, b: vec3): boolean;
begin
  Result := (a[0] = b[0]) and (a[1] = b[1]) and (a[2] = b[2]);
end;

function hypot(v: vec3): single;
begin
  Result := sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
end;

operator ** (a, b: vec3): vec3;   // cross prod
begin
  Result := mkvec3(a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] *
    b[2], a[0] * b[1] - a[1] * b[0]);
end;

function normalize(v: vec3): vec3;
begin
  Result := v / hypot(v);
end;

function normal(v0, v1, v2: vec3): vec3;
var
  n: vec3;
begin
  n := (v2 - v0) ** (v1 - v0);
  if n = mkvec3(0, 0, 0) then
    Result := n
  else
    Result := normalize(n);
end;

function unormal(v0, v1, v2: vec3): vec3;
begin
  Result := (v2 - v0) ** (v1 - v0);
end;

operator ** (a: single; b: integer): single;
begin
  Result := IntPower(a, b);
end;



function mkvec3u(a, b, c: uint32): vec3u;
begin
  Result[0] := a;
  Result[1] := b;
  Result[2] := c;
end;

end.
