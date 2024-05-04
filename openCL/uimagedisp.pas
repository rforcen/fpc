unit uImageDisp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Graphics, GraphType;

type
  TArru32 = array of uint32;

procedure ImageDisp(var timg: TImage; const img: TArru32);

implementation

// TImage <- img:array of u32
procedure ImageDisp(var timg: TImage; const img: TArru32);
var
  rawImg: TRawImage;
  bmp: TBitmap;
begin
  with rawImg do
  begin
    Init;
    Description.Init_BPP32_R8G8B8A8_BIO_TTB(timg.Width, timg.Height); // RGBA
    DataSize := length(img) * sizeof(img[0]);
    Data := @img[0];
  end;
  // Create a bitmap from the rawImg and display it in the Image component
  bmp := TBitmap.Create;
  try
    bmp.LoadFromRawImage(rawImg, False);  // false = bmp does not "own" the image data.
    timg.Picture.Assign(bmp);
  finally
    bmp.Free;
  end;
end;

end.
