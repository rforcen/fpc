{
  TImage <- dc.image  thanks to @wp
}

unit utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, GraphType;

type

  { TSessionDC }

  TSessionDC = class   // manages expressions used in DC session
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure writeExpr(expr: string);
    public
      fileName:string;
  private
    tf: TextFile;
  end;

procedure AssignImage(var timg: TImage; var img: array of uint32);
procedure CreatePNG(fileName: string; w, h: integer; var img: array of uint32);
function genBMP(w, h: integer; var img: array of uint32): TBitmap;


implementation

function CreateRawImage(w, h: integer; var image: array of uint32): TRawImage;
  // u32 - RGBA
begin
  with Result do
  begin
    Init;
    Description.Init_BPP32_R8G8B8A8_BIO_TTB(w, h);
    DataSize := w * h * sizeof(image[0]);
    Data := @image[0];
  end;
end;


// TImage <- dc.image  thanks to @wp
procedure AssignImage(var timg: TImage; var img: array of uint32);
var
  bmp: TBitMap;
begin
  // Create a bitmap from the rawImg and display it in the Image component
  bmp := TBitmap.Create;
  try
    bmp.LoadFromRawImage(CreateRawImage(timg.Width, timg.Height, img), False);
    // false = bmp does not "own" the image data.
    timg.Picture.Assign(bmp);
  finally
    bmp.Free;
  end;
end;

procedure CreatePNG(fileName: string; w, h: integer; var img: array of uint32);
var
  bmp: TBitmap;
begin

  bmp := TBitmap.Create;
  try
    bmp.LoadFromRawImage(CreateRawImage(w, h, img), False);
    // now owned so no need to rawImg.FreeData
    with  TPicture.Create do
    begin
      Assign(bmp);
      SaveToFile(fileName, 'png');
      Free;
    end;
  finally
    bmp.Free;
  end;

end;

function genBMP(w, h: integer; var img: array of uint32): TBitmap;
begin
  Result := TBitmap.Create;
  Result.LoadFromRawImage(CreateRawImage(w, h, img), False);
  // now owned so no need to rawImg.FreeData
end;

{ TSessionDC }

constructor TSessionDC.Create;
var
  i: integer = 0;

begin
  repeat // find latest domaincoloring file
    fileName := 'DomainColoring' + format('%d.dc', [i]);
    Inc(i);
  until not FileExists(fileName);
  Assign(tf, fileName);
  rewrite(tf);
end;

destructor TSessionDC.Destroy;
begin
  Close(tf);
  inherited Destroy;
end;

procedure TSessionDC.writeExpr(expr: string);
begin
  writeln(tf, expr);
  flush(tf);
end;

end.
