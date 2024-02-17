unit uStereograph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, Math, GraphType, uImgHelper;

function GenerateSIRD(depthMap: TImage; color1, color2, color3: TColor;
  colorIntensity: double; Width, Height: integer;
  observationDistanceInches: double = 14.0; eyeSeparationInches: double = 2.5;
  maxDepthInches: double = 12.0; minDepthInches: double = 0;
  horizontalPPI: integer = 72): TImage;

function generateTexturedSIRD(const depthMap, texturePattern: TImage;
  Width, Height: integer; observationDistanceInches: double = 14.0;
  eyeSeparationInches: double = 2.5; maxDepthInches: double = 12.0;
  minDepthInches: double = 0; horizontalPPI: integer = 72;
  verticalPPI: integer = 72): TImage;

implementation


function MinDepth(separationFactor: single;
  maxdepth, observationDistance, suppliedMinDepth: integer): integer;
var
  computedMinDepth: integer;
begin
  computedMinDepth := Trunc((separationFactor * maxdepth * observationDistance) /
    (((1 - separationFactor) * maxdepth) + observationDistance));
  Result := Min(Max(computedMinDepth, suppliedMinDepth), maxdepth);
end;

function MaxDepth(suppliedMaxDepth, observationDistance: integer): integer;
begin
  Result := Max(Min(suppliedMaxDepth, observationDistance), 0);
end;

function ToPixels(valueInches: double; ppi: integer): integer;
begin
  Result := Trunc(valueInches * ppi);
end;

function GetRed(color: TColor): integer;
begin
  Result := (uint32(color) and $00ff0000) >> 16;
end;

function Depth(_depth: TColor; maxDepth, minDepth: integer): integer;
begin
  Result := maxDepth - (GetRed(_depth) * (maxDepth - minDepth) div 255);
end;

function Separation(observationDistance, eyeSeparation, depth: integer): integer;
begin
  Result := (eyeSeparation * depth) div (depth + observationDistance);
end;

type
  TArrInt = array of integer;

function getSeq(n: integer): TArrInt;
begin
  Result := nil;
  setLength(Result, n);

  while n > 0 do
  begin
    Dec(n);
    Result[n] := n;
  end;
end;

function GenerateSIRD(depthMap: TImage; color1, color2, color3: TColor;
  colorIntensity: double; Width, Height: integer;
  observationDistanceInches: double = 14.0; eyeSeparationInches: double = 2.5;
  maxDepthInches: double = 12.0; minDepthInches: double = 0;
  horizontalPPI: integer = 72): TImage;
var
  rdepthMap: TImage = nil;
  _stereogram: TImageBuffer = nil;
  _rdepthMap: TImageBuffer = nil;

  observationDistance, eyeSeparation, _maxdepth, _minDepth: integer;
  colors: TArrInt;
  linksL, linksR: TArrInt;
  l, c, _depth, _separation, left, right: integer;
  Visible: boolean;

  function ChooseColor(_color1, _color2, _color3: TColor;
    colorIntensity: double): TColor;
  begin
    if _color3 = 0 then
    begin
      if Random < colorIntensity then
        Result := _color1
      else
        Result := _color2;
    end
    else
      Result := colors[Random(3)]; // Assuming colors is an array of TColor
  end;

  function index(x, y: integer): integer; inline;
  begin
    Result := y * Width + x;
  end;

begin
  setlength(_stereogram, Width * Height); { working TColor array }

  rdepthMap := depthMap.resize(Width, Height); {resize to w,h -> array TColor}
  _rdepthMap := rdepthMap.toRGBA;
  rdepthMap.Free;

  observationDistance := ToPixels(observationDistanceInches, horizontalPPI);
  eyeSeparation := ToPixels(eyeSeparationInches, horizontalPPI);
  _maxdepth := maxdepth(ToPixels(maxDepthInches, horizontalPPI), observationDistance);
  _minDepth := minDepth(0.55, _maxdepth, observationDistance,
    ToPixels(minDepthInches, horizontalPPI));
  colors := [color1, color2, color3];


  for l := 0 to pred(Height) do
  begin
    linksL := getSeq(Width);
    linksR := getSeq(Width);

    for c := 0 to pred(Width) do
    begin
      _depth := depth(_rdepthMap[index(c, l)], _maxdepth, _minDepth);
      _separation := Separation(observationDistance, eyeSeparation, _depth);
      left := c - (_separation div 2);
      right := left + _separation;

      if (left >= 0) and (right < Width) then
      begin
        Visible := True;

        if linksL[right] <> right then
        begin
          if linksL[right] < left then
          begin
            linksR[linksL[right]] := linksL[right];
            linksL[right] := right;
          end
          else
            Visible := False;
        end;

        if linksR[left] <> left then
        begin
          if linksR[left] > right then
          begin
            linksL[linksR[left]] := linksR[left];
            linksR[left] := left;
          end
          else
            Visible := False;
        end;

        if Visible then
        begin
          linksL[right] := left;
          linksR[left] := right;
        end;
      end;
    end;

    for c := 0 to pred(Width) do
    begin
      if linksL[c] = c then
        _stereogram[index(c, l)] := ChooseColor(color1, color2, color3, colorIntensity)
      else
        _stereogram[index(c, l)] := _stereogram[index(linksL[c], l)];
    end;
  end;

  Result := TImage.Create(Width, Height).fromRGBA(_stereogram);
end;

// Textured Stereogram Generator

function generateTexturedSIRD(const depthMap, texturePattern: TImage;
  Width, Height: integer; observationDistanceInches, eyeSeparationInches,
  maxDepthInches, minDepthInches: double; horizontalPPI, verticalPPI: integer): TImage;
var
  _stereogram: TImageBuffer = nil;
  _wtexturePattern: TImageBuffer = nil;
  _rdepthMap: TImageBuffer = nil;
  rdepthMap: TImage;
  observationDistance, eyeSeparation, _maxDepth, _minDepth: integer;
  verticalShift, maxSeparation, lastLinked: integer;
  wtexturePattern: TImage;
  linksL, linksR: TArrInt;
  l, c, _depth, _separation, left, right: integer;
  wWidth, wHeight: integer;
  Visible: boolean;

  function index(x, y: integer): integer; inline; // _stereogram
  begin
    Result := y * Width + x;
  end;

  function indexw(x, y: integer): integer; inline; // wtexturePattern
  begin
    Result := y * wWidth + x;
  end;

begin
  setLength(_stereogram, Width * Height);   { working array }

  rdepthMap := depthMap.resize(Width, Height); { depthMap to working array }
  _rdepthMap := rdepthMap.toRGBA;
  rdepthMap.Free;

  observationDistance := topixels(observationDistanceInches, horizontalPPI);
  eyeSeparation := topixels(eyeSeparationInches, horizontalPPI);
  _maxDepth := maxdepth(topixels(maxDepthInches, horizontalPPI), observationDistance);
  _minDepth := mindepth(0.55, _maxDepth, observationDistance,
    topixels(minDepthInches, horizontalPPI));
  verticalShift := verticalPPI div 16;
  maxSeparation := separation(observationDistance, eyeSeparation, _maxDepth);

  { texture work array }
  wtexturePattern := texturePattern.resize(maxSeparation, maxSeparation);
  wWidth := wtexturePattern.Width;
  wHeight := wtexturePattern.Height;
  _wtexturePattern := wtexturePattern.toRGBA;
  wtexturePattern.Free;

  for l := 0 to pred(Height) do
  begin
    linksL := getSeq(Width);
    linksR := getSeq(Width);

    for c := 0 to pred(Width) do
    begin
      _depth := depth(_rdepthMap[index(c, l)], _maxDepth, _minDepth);
      _separation := separation(observationDistance, eyeSeparation, _depth);
      left := c - (_separation div 2);
      right := left + _separation;

      if (left >= 0) and (right < Width) then
      begin
        Visible := True;

        if linksL[right] <> right then
        begin
          if linksL[right] < left then
          begin
            linksR[linksL[right]] := linksL[right];
            linksL[right] := right;
          end
          else
            Visible := False;
        end;

        if linksR[left] <> left then
        begin
          if linksR[left] > right then
          begin
            linksL[linksR[left]] := linksR[left];
            linksR[left] := left;
          end
          else
            Visible := False;
        end;

        if Visible then
        begin
          linksL[right] := left;
          linksR[left] := right;
        end;
      end;
    end;

    lastLinked := -10;
    for c := 0 to pred(Width) do
    begin
      if linksL[c] = c then
      begin
        if lastLinked = c - 1 then
          _stereogram[index(c, l)] := _stereogram[index(c - 1, l)]
        else
          _stereogram[index(c, l)] :=
            _wtexturePattern[{%H-}indexw(c mod maxSeparation,
            (l + ((c div maxSeparation) * verticalShift)) mod wHeight)];
      end
      else
      begin
        _stereogram[index(c, l)] := _stereogram[index(linksL[c], l)];
        lastLinked := c;
      end;
    end;
  end;

  Result := TImage.Create(Width, Height).fromRGBA(_stereogram);
end;



end.
