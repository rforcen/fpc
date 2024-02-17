{ TImage helper
}
unit uImgHelper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, GraphType;

type

  TImageBuffer = array of TColor;
  PTColor = ^TColor;

  { TImageHelper }

  TImageHelper = class helper for TImage
    constructor Create(w, h: integer); overload;
    function resize(w, h: integer): TImage;
    function toRGBA: TImageBuffer;
    function fromRGBA(const a: TImageBuffer): TImage;
  end;


implementation

{ TImageHelper }

constructor TImageHelper.Create(w, h: integer);
begin
  inherited Create(nil);
  Width := w;
  Height := h;
end;

function TImageHelper.resize(w, h: integer): TImage;
begin
  Result := TImage.Create(nil);
  Result.Width := w;
  Result.Height := h;

  Result.Canvas.StretchDraw(Result.ClientRect, Picture.Graphic);
end;

function TImageHelper.toRGBA: TImageBuffer;
var
  l, c, w, h: integer;
  pb: pbyte;
  rawImg: TRawImage;
begin
  Result := nil;
  rawImg := Picture.Bitmap.rawimage;

  w := rawImg.Description.Width;
  h := rawImg.Description.Height;

  setLength(Result, w * h);

  for l := 0 to pred(h) do
  begin
    pb := rawImg.getLineStart(l);
    for c := 0 to pred(w) do
      move(pb[c * 3], Result[c + l * w], 3);
  end;

end;

function TImageHelper.fromRGBA(const a: TImageBuffer): TImage;
var
  rawImg: TRawImage;
  bmp: TBitmap;
begin
  { Create a rawimage with RGBA color format and link data array to it }
  with rawImg do
  begin
    Init;
    Description.Init_BPP32_R8G8B8A8_BIO_TTB(Width, Height); // RGBA
    DataSize := length(a) * sizeof(a[0]); // size of the image array, in bytes
    Data := @a[0];  // rawImg.Data points to first byte of the FRawData array
  end;

  // Create a bitmap from the rawImg and display it in the Image component
  bmp := TBitmap.Create;
  try
    bmp.LoadFromRawImage(rawImg, False);  // false = bmp does not "own" the image data.
    Picture.Assign(bmp);
  finally
    bmp.Free;
  end;
  Result := self;
end;

function createRawImage(w, h: integer): TRawImage;   // RGBA
begin
  with Result do
  begin
    Init;
    Description.Init_BPP32_R8G8B8A8_BIO_TTB(w, h); // RGBA
    DataSize := w * h * 4; // size of the image array, in bytes
    getmem(Data, w * h * 4);  // rawImg.Data points to first byte of the FRawData array
  end;
end;


end.
