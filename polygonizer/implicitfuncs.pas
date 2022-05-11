unit implicitFuncs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  vec3Func = function(x, y, z: single): single;

function Sphere(x, y, z: single): single;
function Blob(x, y, z: single): single;
function NordstarndWeird(x, y, z: single): single;
function DecoCube(x, y, z: single): single;
function Cassini(x, y, z: single): single;
function Orth(x, y, z: single): single;
function Orth3(x, y, z: single): single;
function Pretzel(x, y, z: single): single;
function Tooth(x, y, z: single): single;
function Pilz(x, y, z: single): single;
function Bretzel(x, y, z: single): single;
function BarthDecic(x, y, z: single): single;
function Clebsch0(x, y, z: single): single;
function Clebsch(x, y, z: single): single;
function Chubs(x, y, z: single): single;
function Chair(x, y, z: single): single;
function Roman(x, y, z: single): single;
function TangleCube(x, y, z: single): single;
function Goursat(x, y, z: single): single;
function Sinxyz(x, y, z: single): single;


const
  ImplicitFunctions: array of vec3Func = (
    @Sphere, @Blob, @NordstarndWeird, @DecoCube, @Cassini, @Orth, @Orth3,
    @Pretzel, @Tooth, @Pilz, @Bretzel, @BarthDecic, @Clebsch0, @Clebsch,
    @Chubs, @Chair, @Roman, @TangleCube, @Goursat, @Sinxyz);



implementation

function sqr(x: single): single; inline;
begin
  Result := x * x;
end;

function cube(x: single): single; inline;
begin
  Result := x * x * x;
end;

function sqr3(x: single): single; inline;
begin
  Result := x * x * x;
end;

function sqr4(x: single): single; inline;
begin
  Result := x * x * x * x;
end;

function _sphere(x, y, z: single): single; inline;
var
  rsq: single;
begin
  rsq := x * x + y * y + z * z;
  if rsq < 1e-5 then rsq := 1e-5;
  Result := 1.0 / rsq;
end;


function DecoCube(x, y, z: single): single; inline;
const
  a: single = 0.95;
  b: single = 0.01;
begin
  Result := (sqr(x * x + y * y - a * a) + sqr(z * z - 1)) *
    (sqr(y * y + z * z - a * a) + sqr(x * x - 1)) *
    (sqr(z * z + x * x - a * a) + sqr(y * y - 1)) - b;
end;


function NordstarndWeird(x, y, z: single): single; inline;
begin
  Result :=
    25 * (x * x * x * (y + z) + y * y * y * (x + z) + z * z * z * (x + y)) +
    50 * (x * x * y * y + x * x * z * z + y * y * z * z) - 125 *
    (x * x * y * z + y * y * x * z + z * z * x * y) + 60 * x * y *
    z - 4 * (x * y + y * z + z * x);
end;


function Cassini(x, y, z: single): single;
const
  a: single = 0.3;
begin
  Result :=
    (sqr((x - a)) + z * z) * (sqr((x + a)) + z * z) - power(y, 4);
  // ( (x-a)^2 + y^2) ((x+a)^2 + y^2) = z^4 a = 0.5
end;

function Orth(x, y, z: single): single;
const
  a: single = 0.06;
  b: single = 2.0;
begin
  Result := (sqr(x * x + y * y - 1) + z * z) * (sqr(y * y + z * z - 1) + x * x) *
    (sqr(z * z + x * x - 1) + y * y) - a * a * (1 + b * (x * x + y * y + z * z));
end;

function Orthogonal(x, y, z: single): single; inline;
begin
  // let (a,b) = (0.06, 2)
  Result := Orth(x, y, z);
end;

function Orth3(x, y, z: single): single;
begin
  Result := 4.0 - Orth(x + 0.5, y - 0.5, z - 0.5) - Orth(x - 0.5, y + 0.5, z - 0.5) -
    Orth(x - 0.5, y - 0.5, z + 0.5);
end;

function Pretzel(x, y, z: single): single;
const
  aa: single = 1.6;
begin
  Result := sqr(((x - 1) * (x - 1) + y * y - aa * aa) *
    ((x + 1) * (x + 1) + y * y - aa * aa)) + z * z * 10 - 1;
end;

function Tooth(x, y, z: single): single;
begin
  Result := sqr4(x) + sqr4(y) + sqr4(z) - sqr(x) - sqr(y) - sqr(z);
end;

function Pilz(x, y, z: single): single;
const
  a: single = 0.05;
  b: single = -0.1;
begin
  Result := sqr(sqr(x * x + y * y - 1) + sqr(z - 0.5)) *
    (sqr(y * y / a * a + sqr(z + b) - 1.0) + x * x) - a * (1.0 + a * sqr(z - 0.5));
end;

function Bretzel(x, y, z: single): single;
const
  a: single = 0.003;
  b: single = 0.7;
begin
  Result := sqr(x * x * (1 - x * x) - y * y) + 0.5 * z * z - a *
    (1 + b * (x * x + y * y + z * z));
end;

function BarthDecic(x, y, z: single): single;
const
  GR: single = 1.6180339887; // Golden ratio
  //GR2: single = 1.6180339887 * 1.6180339887; //GR * GR
  GR4: single = 1.6180339887 * 1.6180339887 * 1.6180339887 * 1.6180339887; // gr2 *gr2
  w: single = 0.3;
begin
  Result := 8 * (x * x - GR4 * y * y) * (y * y - GR4 * z * z) *
    (z * z - GR4 * x * x) * (x * x * x * x + y * y * y * y + z * z *
    z * z - 2 * x * x * y * y - 2 * x * x * z * z - 2 * y * y * z * z) +
    (3 + 5 * GR) * sqr((x * x + y * y + z * z - w * w)) * sqr(
    (x * x + y * y + z * z - (2 - GR) * w * w)) * w * w;
end;

function Clebsch0(x, y, z: single): single;
begin
  Result := 81 * (cube(x) + cube(y) + cube(z)) - 189 *
    (sqr(x) * y + sqr(x) * z + sqr(y) * x + sqr(y) * z + sqr(z) * x + sqr(z) * y) +
    54 * (x * y * z) + 126 * (x * y + x * z + y * z) - 9 *
    (sqr(x) + sqr(y) + sqr(z)) - 9 * (x + y + z) + 1;
end;


function Clebsch(x, y, z: single): single;
begin
  Result := 16 * cube(x) + 16 * cube(y) - 31 * cube(z) + 24 * sqr(x) *
    z - 48 * sqr(x) * y - 48 * x * sqr(y) + 24 * sqr(y) * z - 54 *
    sqrt(3.0) * sqr(z) - 72 * z;
end;

function Chubs(x, y, z: single): single;
begin
  Result := power(x, 4) + power(y, 4) + power(z, 4) - sqr(x) - sqr(y) - sqr(z) + 0.5;
  // x^4 + y^4 + z^4 - x^2 - y^2 - z^2 + 0.5 = 0
end;

function Chair(x, y, z: single): single;
const
  k: single = 5.0;
  a: single = 0.95;
  b: single = 0.8;
begin
  Result := sqr(sqr(x) + sqr(y) + sqr(z) - a * sqr(k)) - b *
    ((sqr((z - k)) - 2 * sqr(x)) * (sqr((z + k)) - 2 * sqr(y)));
  // (x^2+y^2+z^2-a*k^2)^2-b*((z-k)^2-2*x^2)*((z+k)^2-2*y^2)=0,     with k=5, a=0.95 and b=0.8.
end;

function Roman(x, y, z: single): single;
const
  r: single = 2.0;
begin
  Result := sqr(x) * sqr(y) + sqr(y) * sqr(z) + sqr(z) * sqr(x) - r * x * y * z;
end;

function Sinxyz(x, y, z: single): single;
begin
  Result := sin(x) * sin(y) * sin(z);
end;

function F001(x, y, z: single): single;
begin
  Result := sqr3(x) + sqr3(y) + sqr4(z) - 10; // x^3 + y^3 + z^4 -10 = 0
end;

function TangleCube(x, y, z: single): single;
begin
  Result := sqr4(x) - 5 * sqr(x) + sqr4(y) - 5 * sqr(y) + sqr4(z) - 5 * sqr(z) + 11.8;
end;

function Goursat(x, y, z: single): single;
  // (x^4 + y^4 + z^4) + a * (x^2 + y^2 + z^2)^2 + b * (x^2 + y^2 + z^2) + c = 0
const
  a: single = 0;
  b: single = 0;
  c: single = -1.0;
begin
  Result := sqr4(x) + sqr4(y) + sqr4(z) + a * sqr(sqr(x) + sqr(y) + sqr(z)) +
    b * (sqr(x) + sqr(y) + sqr(z)) + c;
end;

function Blob(x, y, z: single): single;
begin
  Result :=
    4 - _sphere(x + 0.5, y - 0.5, z - 0.5) - _sphere(x - 0.5, y + 0.5, z - 0.5) -
    _sphere(x - 0.5, y - 0.5, z + 0.5);
end;

function Sphere(x, y, z: single): single;
begin
  Result := _sphere(x, y, z) - 1;
end;


end.
