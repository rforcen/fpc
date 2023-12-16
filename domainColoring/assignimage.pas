{
  TImage <- dc.image  thanks to @wp
}

unit assignImage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, GraphType;

procedure AssignImage(var timg: TImage; var img: array of uint32);

implementation


// TImage <- dc.image  thanks to @wp
procedure AssignImage(var timg: TImage; var img: array of uint32);
var
  rawImg: TRawImage;
  bmp: TBitmap;
begin
  { Create a rawimage with RGBA color format and link data array to it }

  rawImg.Init;
  rawImg.Description.Init_BPP32_R8G8B8A8_BIO_TTB(timg.Width, timg.Height); // RGBA
  rawImg.DataSize := length(img) * sizeof(img[0]); // size of the image array, in bytes
  rawImg.Data := @img[0];  // rawImg.Data points to first byte of the FRawData array

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
