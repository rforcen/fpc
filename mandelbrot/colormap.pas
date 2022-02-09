// vec3 map
// gcc -O3 -c vec3Map.c
unit colorMap;

interface

type
  vec3 = array[0..2] of single;

function colorMap(v, vmin, vmax: single; _type: integer): vec3;

implementation

function colorMap(v, vmin, vmax: single; _type: integer): vec3;
const
  ones: vec3 = (1, 1, 1);
var
  ratio, dv, vmid: single;
  c, c1, c2, c3: vec3;
begin

  c := ones;

  if (vmax < vmin) then
  begin
    dv := vmin;
    vmin := vmax;
    vmax := dv;
  end;
  if (vmax - vmin < 0.000001) then
  begin
    vmin := vmin - 1;
    vmax := vmax + 1;
  end;
  if (v < vmin) then
    v := vmin;
  if (v > vmax) then
    v := vmax;
  dv := vmax - vmin;

  case (_type) of

    0..1:
    begin
      if (v < (vmin + 0.25 * dv)) then
      begin
        c[0] := 0;
        c[1] := 4 * (v - vmin) / dv;
        c[2] := 1;
      end
      else if (v < (vmin + 0.5 * dv)) then
      begin
        c[0] := 0;
        c[1] := 1;
        c[2] := 1 + 4 * (vmin + 0.25 * dv - v) / dv;
      end
      else if (v < (vmin + 0.75 * dv)) then
      begin
        c[0] := 4 * (v - vmin - 0.5 * dv) / dv;
        c[1] := 1;
        c[2] := 0;
      end
      else
      begin
        c[0] := 1;
        c[1] := 1 + 4 * (vmin + 0.75 * dv - v) / dv;
        c[2] := 0;
      end;
    end;
    2:
    begin
      c[0] := (v - vmin) / dv;
      c[1] := 0;
      c[2] := (vmax - v) / dv;
    end;
    3:
    begin
      c[0] := (v - vmin) / dv;
      c[2] := c[0];
      c[1] := c[0];
    end;
    4:
    begin
      if (v < (vmin + dv / 6.0)) then
      begin
        c[0] := 1;
        c[1] := 6 * (v - vmin) / dv;
        c[2] := 0;
      end
      else if (v < (vmin + 2.0 * dv / 6.0)) then
      begin
        c[0] := 1 + 6 * (vmin + dv / 6.0 - v) / dv;
        c[1] := 1;
        c[2] := 0;
      end
      else if (v < (vmin + 3.0 * dv / 6.0)) then
      begin
        c[0] := 0;
        c[1] := 1;
        c[2] := 6 * (v - vmin - 2.0 * dv / 6.0) / dv;
      end
      else if (v < (vmin + 4.0 * dv / 6.0)) then
      begin
        c[0] := 0;
        c[1] := 1 + 6 * (vmin + 3.0 * dv / 6.0 - v) / dv;
        c[2] := 1;
      end
      else if (v < (vmin + 5.0 * dv / 6.0)) then
      begin
        c[0] := 6 * (v - vmin - 4.0 * dv / 6.0) / dv;
        c[1] := 0;
        c[2] := 1;
      end
      else
      begin
        c[0] := 1;
        c[1] := 0;
        c[2] := 1 + 6 * (vmin + 5.0 * dv / 6.0 - v) / dv;
      end;
    end;
    5:
    begin
      c[0] := (v - vmin) / (vmax - vmin);
      c[1] := 1;
      c[2] := 0;
    end;
    6:
    begin
      c[0] := (v - vmin) / (vmax - vmin);
      c[1] := (vmax - v) / (vmax - vmin);
      c[2] := c[0];
    end;
    7:
    begin
      if (v < (vmin + 0.25 * dv)) then
      begin
        c[0] := 0;
        c[1] := 4 * (v - vmin) / dv;
        c[2] := 1 - c[1];
      end
      else if (v < (vmin + 0.5 * dv)) then
      begin
        c[0] := 4 * (v - vmin - 0.25 * dv) / dv;
        c[1] := 1 - c[0];
        c[2] := 0;
      end
      else if (v < (vmin + 0.75 * dv)) then
      begin
        c[1] := 4 * (v - vmin - 0.5 * dv) / dv;
        c[0] := 1 - c[1];
        c[2] := 0;
      end
      else
      begin
        c[0] := 0;
        c[2] := 4 * (v - vmin - 0.75 * dv) / dv;
        c[1] := 1 - c[2];
      end;
    end;
    8:
    begin
      if (v < (vmin + 0.5 * dv)) then
      begin
        c[0] := 2 * (v - vmin) / dv;
        c[1] := c[0];
        c[2] := c[0];
      end
      else
      begin
        c[0] := 1 - 2 * (v - vmin - 0.5 * dv) / dv;
        c[1] := c[0];
        c[2] := c[0];
      end;
    end;
    9:
    begin
      if (v < (vmin + dv / 3)) then
      begin
        c[2] := 3 * (v - vmin) / dv;
        c[1] := 0;
        c[0] := 1 - c[2];
      end
      else if (v < (vmin + 2 * dv / 3)) then
      begin
        c[0] := 0;
        c[1] := 3 * (v - vmin - dv / 3) / dv;
        c[2] := 1;
      end
      else
      begin
        c[0] := 3 * (v - vmin - 2 * dv / 3) / dv;
        c[1] := 1 - c[0];
        c[2] := 1;
      end;
    end;
    10:
    begin
      if (v < (vmin + 0.2 * dv)) then
      begin
        c[0] := 0;
        c[1] := 5 * (v - vmin) / dv;
        c[2] := 1;
      end
      else if (v < (vmin + 0.4 * dv)) then
      begin
        c[0] := 0;
        c[1] := 1;
        c[2] := 1 + 5 * (vmin + 0.2 * dv - v) / dv;
      end
      else if (v < (vmin + 0.6 * dv)) then
      begin
        c[0] := 5 * (v - vmin - 0.4 * dv) / dv;
        c[1] := 1;
        c[2] := 0;
      end
      else if (v < (vmin + 0.8 * dv)) then
      begin
        c[0] := 1;
        c[1] := 1 - 5 * (v - vmin - 0.6 * dv) / dv;
        c[2] := 0;
      end
      else
      begin
        c[0] := 1;
        c[1] := 5 * (v - vmin - 0.8 * dv) / dv;
        c[2] := 5 * (v - vmin - 0.8 * dv) / dv;
      end;
    end;
    11:
    begin
      c1[0] := 200 / 255.0;
      c1[1] := 60 / 255.0;
      c1[2] := 0 / 255.0;
      c2[0] := 250 / 255.0;
      c2[1] := 160 / 255.0;
      c2[2] := 110 / 255.0;
      c[0] := (c2[0] - c1[0]) * (v - vmin) / dv + c1[0];
      c[1] := (c2[1] - c1[1]) * (v - vmin) / dv + c1[1];
      c[2] := (c2[2] - c1[2]) * (v - vmin) / dv + c1[2];
    end;
    12:
    begin
      c1[0] := 55 / 255.0;
      c1[1] := 55 / 255.0;
      c1[2] := 45 / 255.0;
      (* c2[0] = 200 / 255.0; c2[1] =  60 / 255.0; c2[2] =   0 / 255.0; *)
      c2[0] := 235 / 255.0;
      c2[1] := 90 / 255.0;
      c2[2] := 30 / 255.0;
      c3[0] := 250 / 255.0;
      c3[1] := 160 / 255.0;
      c3[2] := 110 / 255.0;
      ratio := 0.4;
      vmid := vmin + ratio * dv;
      if (v < vmid) then
      begin
        c[0] := (c2[0] - c1[0]) * (v - vmin) / (ratio * dv) + c1[0];
        c[1] := (c2[1] - c1[1]) * (v - vmin) / (ratio * dv) + c1[1];
        c[2] := (c2[2] - c1[2]) * (v - vmin) / (ratio * dv) + c1[2];
      end
      else
      begin
        c[0] := (c3[0] - c2[0]) * (v - vmid) / ((1 - ratio) * dv) + c2[0];
        c[1] := (c3[1] - c2[1]) * (v - vmid) / ((1 - ratio) * dv) + c2[1];
        c[2] := (c3[2] - c2[2]) * (v - vmid) / ((1 - ratio) * dv) + c2[2];
      end;
    end;
    13:
    begin
      c1[0] := 0 / 255.0;
      c1[1] := 255 / 255.0;
      c1[2] := 0 / 255.0;
      c2[0] := 255 / 255.0;
      c2[1] := 150 / 255.0;
      c2[2] := 0 / 255.0;
      c3[0] := 255 / 255.0;
      c3[1] := 250 / 255.0;
      c3[2] := 240 / 255.0;
      ratio := 0.3;
      vmid := vmin + ratio * dv;
      if (v < vmid) then
      begin
        c[0] := (c2[0] - c1[0]) * (v - vmin) / (ratio * dv) + c1[0];
        c[1] := (c2[1] - c1[1]) * (v - vmin) / (ratio * dv) + c1[1];
        c[2] := (c2[2] - c1[2]) * (v - vmin) / (ratio * dv) + c1[2];
      end
      else
      begin
        c[0] := (c3[0] - c2[0]) * (v - vmid) / ((1 - ratio) * dv) + c2[0];
        c[1] := (c3[1] - c2[1]) * (v - vmid) / ((1 - ratio) * dv) + c2[1];
        c[2] := (c3[2] - c2[2]) * (v - vmid) / ((1 - ratio) * dv) + c2[2];
      end;
    end;
    14:
    begin
      c[0] := 1;
      c[1] := 1 - (v - vmin) / dv;
      c[2] := 0;
    end;
    15:
    begin
      if (v < (vmin + 0.25 * dv)) then
      begin
        c[0] := 0;
        c[1] := 4 * (v - vmin) / dv;
        c[2] := 1;
      end
      else if (v < (vmin + 0.5 * dv)) then
      begin
        c[0] := 0;
        c[1] := 1;
        c[2] := 1 - 4 * (v - vmin - 0.25 * dv) / dv;
      end
      else if (v < (vmin + 0.75 * dv)) then
      begin
        c[0] := 4 * (v - vmin - 0.5 * dv) / dv;
        c[1] := 1;
        c[2] := 0;
      end
      else
      begin
        c[0] := 1;
        c[1] := 1;
        c[2] := 4 * (v - vmin - 0.75 * dv) / dv;
      end;
    end;
    16:
    begin
      if (v < (vmin + 0.5 * dv)) then
      begin
        c[0] := 0.0;
        c[1] := 2 * (v - vmin) / dv;
        c[2] := 1 - 2 * (v - vmin) / dv;
      end
      else
      begin
        c[0] := 2 * (v - vmin - 0.5 * dv) / dv;
        c[1] := 1 - 2 * (v - vmin - 0.5 * dv) / dv;
        c[2] := 0.0;
      end;
    end;
    17:
    begin
      if (v < (vmin + 0.5 * dv)) then
      begin
        c[0] := 1.0;
        c[1] := 1 - 2 * (v - vmin) / dv;
        c[2] := 2 * (v - vmin) / dv;
      end
      else
      begin
        c[0] := 1 - 2 * (v - vmin - 0.5 * dv) / dv;
        c[1] := 2 * (v - vmin - 0.5 * dv) / dv;
        c[2] := 1.0;
      end;
    end;
    18:
    begin
      c[0] := 0;
      c[1] := (v - vmin) / (vmax - vmin);
      c[2] := 1;
    end;
    19:
    begin
      c[0] := (v - vmin) / (vmax - vmin);
      c[1] := c[0];
      c[2] := 1;
    end;
    20:
    begin
      c1[0] := 0 / 255.0;
      c1[1] := 160 / 255.0;
      c1[2] := 0 / 255.0;
      c2[0] := 180 / 255.0;
      c2[1] := 220 / 255.0;
      c2[2] := 0 / 255.0;
      c3[0] := 250 / 255.0;
      c3[1] := 220 / 255.0;
      c3[2] := 170 / 255.0;
      ratio := 0.3;
      vmid := vmin + ratio * dv;
      if (v < vmid) then
      begin
        c[0] := (c2[0] - c1[0]) * (v - vmin) / (ratio * dv) + c1[0];
        c[1] := (c2[1] - c1[1]) * (v - vmin) / (ratio * dv) + c1[1];
        c[2] := (c2[2] - c1[2]) * (v - vmin) / (ratio * dv) + c1[2];
      end
      else
      begin
        c[0] := (c3[0] - c2[0]) * (v - vmid) / ((1 - ratio) * dv) + c2[0];
        c[1] := (c3[1] - c2[1]) * (v - vmid) / ((1 - ratio) * dv) + c2[1];
        c[2] := (c3[2] - c2[2]) * (v - vmid) / ((1 - ratio) * dv) + c2[2];
      end;
    end;
    21:
    begin
      c1[0] := 255 / 255.0;
      c1[1] := 255 / 255.0;
      c1[2] := 200 / 255.0;
      c2[0] := 150 / 255.0;
      c2[1] := 150 / 255.0;
      c2[2] := 255 / 255.0;
      c[0] := (c2[0] - c1[0]) * (v - vmin) / dv + c1[0];
      c[1] := (c2[1] - c1[1]) * (v - vmin) / dv + c1[1];
      c[2] := (c2[2] - c1[2]) * (v - vmin) / dv + c1[2];
    end;
    22:
    begin
      c[0] := 1 - (v - vmin) / dv;
      c[1] := 1 - (v - vmin) / dv;
      c[2] := (v - vmin) / dv;
    end;
    23:
    begin
      if (v < (vmin + 0.5 * dv)) then
      begin
        c[0] := 1;
        c[1] := 2 * (v - vmin) / dv;
        c[2] := c[1];
      end
      else
      begin
        c[0] := 1 - 2 * (v - vmin - 0.5 * dv) / dv;
        c[1] := c[0];
        c[2] := 1;
      end;
    end;
    24:
    begin
      if (v < (vmin + 0.5 * dv)) then
      begin
        c[0] := 2 * (v - vmin) / dv;
        c[1] := c[0];
        c[2] := 1 - c[0];
      end
      else
      begin
        c[0] := 1;
        c[1] := 1 - 2 * (v - vmin - 0.5 * dv) / dv;
        c[2] := 0;
      end;
    end;
    25:
    begin
      if (v < (vmin + dv / 3)) then
      begin
        c[0] := 0;
        c[1] := 3 * (v - vmin) / dv;
        c[2] := 1;
      end
      else if (v < (vmin + 2 * dv / 3)) then
      begin
        c[0] := 3 * (v - vmin - dv / 3) / dv;
        c[1] := 1 - c[0];
        c[2] := 1;
      end
      else
      begin
        c[0] := 1;
        c[1] := 0;
        c[2] := 1 - 3 * (v - vmin - 2 * dv / 3) / dv;
      end;
    end;
    else
      Result := c;
  end;
  Result := c;
end;

end.
