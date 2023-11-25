unit colorRef;

{$mode ObjFPC}{$H+}
{$WARN 5057 off : Local variable "$1" does not seem to be initialized}
interface
// ColorRef
uses
  Classes, SysUtils;

type
  TColorRef = int32;

procedure RGBtoHLS(const rgb: TColorRef; var H, L, S: single);
function HueToRGB(m1, m2, h: single): single;
function HLStoRGB(const H, L, S: single): TColorRef;
function ColorScaleHSL(const Col1, Col2: TColorRef; const Ratio: single): TColorRef;
function MakeRGB(r, g, b: int32): TColorRef;


implementation

function min3(a, b, c: single): single;
begin
  if a <= b then
  begin
    if a <= c then Result := a
    else
      Result := c;
  end
  else
  begin
    if b <= c then Result := b
    else
      Result := c;
  end;
end;

function max3(a, b, c: single): single;
begin
  if a >= b then
  begin
    if a >= c then Result := a
    else
      Result := c;
  end
  else
  begin
    if b >= c then Result := b
    else
      Result := c;
  end;
end;

procedure RGBtoHLS(const rgb: TColorRef; var H, L, S: single);
var
  delta, r, g, b, cmax, cmin: single;
begin

  r := ((rgb >> 16) and $FF) / 255;
  g := ((rgb >> 8) and $FF) / 255;
  b := (rgb and $FF) / 255;
  cmax := max3(r, g, b);
  cmin := min3(r, g, b);
  L := (cmax + cmin) / 2;
  if cmax = cmin then
  begin
    S := 0;
    H := 0;
  end // it's really undefined
  else
  begin
    if L < 0.5 then  S := (cmax - cmin) / (cmax + cmin)
    else
      S := (cmax - cmin) / (2.0 - cmax - cmin);
    delta := cmax - cmin;
    if r = cmax then H := (g - b) / delta
    else
    if g = cmax then H := 2.0 + (b - r) / delta
    else
      H := 4.0 + (r - g) / delta;
    H := H / 6.0;
    if H < 0 then H := H + 1;
  end;
end;


function HueToRGB(m1, m2, h: single): single;
begin
  if h < 0 then h := h + 1;
  if h > 1 then h := h - 1;
  if 6.0 * h < 1 then Result := (m1 + (m2 - m1) * h * 6.0)
  else
  if 2.0 * h < 1 then Result := m2
  else
  if 3.0 * h < 2.0 then Result := (m1 + (m2 - m1) * ((2.0 / 3.0) - h) * 6.0)
  else
    Result := m1;
end;

function MakeRGB(r, g, b: int32): TColorRef;
begin
  Result := (r << 16) or (g << 8) or b;
end;

function HLStoRGB(const H, L, S: single): TColorRef;
var
  r, g, b, m1, m2: single;
begin
  if S = 0 then
  begin
    r := L;
    g := L;
    b := L;
  end
  else
  begin

    if L <= 0.5 then m2 := L * (1 + S)
    else
      m2 := L + S - L * S;

    m1 := 2 * L - m2;
    r := HueToRGB(m1, m2, H + 1 / 3);
    g := HueToRGB(m1, m2, H);
    b := HueToRGB(m1, m2, H - 1 / 3);
  end;
  Result := MakeRGB(round(r * 255), round(g * 255), round(b * 255));
end;

{$WARN 5057 OFF}


function ColorScaleHSL(const Col1, Col2: TColorRef; const Ratio: single): TColorRef;
var
  H1, H2, S1, S2, L1, L2: single;
begin

  if Ratio <= 0 then Result := Col1  // Ratio parameter must be between 0 and 1
  else if Ratio >= 1 then Result := Col2;
  begin

    RGBtoHLS(Col1, H1, L1, S1);
    RGBtoHLS(Col2, H2, L2, S2);

    Result := HLStoRGB(H1 + (H2 - H1) * Ratio, L1 + (L2 - L1) * Ratio,
      S1 + (S2 - S1) * Ratio);
  end;
end;

{$WARN 5057 ON}

end.
