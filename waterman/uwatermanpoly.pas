unit uWatermanPoly;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, v3, gcontnrs, Math;

type
  TVvec3 = specialize TGenVector<vec3>;

function watermanPoly(radius: double): TVvec3;

implementation

function watermanPoly(radius: double): TVvec3;
var
  x, y, z, a, b, c, xra, xrb, yra, yrb, zra, zrb, R, Ry, s, radius2: single;

begin

  Result := TVvec3.Create;

  a := 0;
  b := 0;
  c := 0; // center

  xra := 0;
  xrb := 0;
  yra := 0;
  yrb := 0;
  zra := 0;
  zrb := 0;


  s := radius;
  radius2 := radius; // * radius;
  xra := ceil(a - s);
  xrb := floor(a + s);

  x := xra;
  while x <= xrb do
  begin

    R := radius2 - (x - a) * (x - a);
    if R < 0 then
    begin
      x += 1;
      continue;
    end;
    s := sqrt(R);
    yra := ceil(b - s);
    yrb := floor(b + s);

    y := yra;
    while y <= yrb do
    begin

      Ry := R - (y - b) * (y - b);
      if Ry < 0 then
      begin
        y += 1;
        continue; //case Ry < 0
      end;
      if (Ry = 0) and (c = floor(c)) then
      begin
        //case Ry=0
        if ((x + y + c) mod 2) <> 0 then
        begin
          y += 1;
          continue; //case Ry < 0
        end
        else
        begin
          zra := c;
          zrb := c;
        end;
      end
      else
      begin
        // case Ry > 0
        s := sqrt(Ry);
        zra := ceil(c - s);
        zrb := floor(c + s);
        if ((x + y) mod 2) = 0 then
        begin
          if (zra mod 2) <> 0 then
          begin
            if zra <= c then
              zra := zra + 1
            else
              zra := zra - 1;
          end;
        end
        else
        begin
          if (zra mod 2) = 0 then
          begin
            if zra <= c then
              zra := zra + 1
            else
              zra := zra - 1;
          end;
        end;
      end;

      y += 1;
    end;


    z := zra;
    while z <= zrb do
    begin
      // save vertex x,y,z
      Result.append(mkvec3(x, y, z));
      z += 2;
    end;

    x += 1;
  end;
end;

end.
