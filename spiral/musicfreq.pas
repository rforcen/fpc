unit musicFreq;

{$mode ObjFPC}{$H+}

interface

uses Graphics;

function Freq2Color(freq: single): TColor;


implementation

uses
  Classes, SysUtils, math, colorRef;


const
  MUSICAL_INC = 1.0594630943593; // 2^(1/12)
  // LOG_MUSICAL_INC = 0.0577622650466;
  baseC0 = 261.62556530061;  // 440 * MUSICAL_INC^(-9)
  LOG_baseC0 = 5.5669143414923;
  LOG2 = 0.6931471805599;


// NoteOct2Freq, convert Note/oct to freq
function NoteOct2Freq(note, oct: integer): single;
begin
  Result := baseC0 * power(MUSICAL_INC, note + 12 * oct);
end;

// Hz 2 octave
function Freq2Oct(freq: single): integer;
begin
  if freq <= 0 then Result := -999
  else
    Result := floor((ln(freq) - LOG_baseC0) / LOG2);
end;

function Freq2Color(freq: single): TColor;
var
  oct: integer;
  f0, fz, ratio: single;
begin
  oct := Freq2Oct(freq); // get note and freq. err
  f0 := NoteOct2Freq(0, oct);
  fz := NoteOct2Freq(0, oct + 1);
  ratio := (freq - f0) / (fz - f0);


  // ratio between RED, VIOLET
  Result := ColorScaleHSL(MakeRGB($ff, 0, 0), MakeRGB($ff, 0, $ff), ratio);
end;
end.

