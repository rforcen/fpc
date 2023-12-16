{
 DomainCOloring cpp/dll wrapper
}
unit uDC;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, GraphType, IntfGraphics,
  Forms, Controls, Graphics, Dialogs, ComCtrls;

// c++ interface
{$ifdef linux}
  {$link cpp/DomainColoring.o}

  {$linklib c}
  {$linklib stdc++}
  {$linklib gcc_s}
  {$linklib m}
        const dcLib='';
{$endif}


// assign an array of int to TImage
procedure AssignImage(var timg: TImage; var img: array of uint32);

type

  Puint32 = ^uint32;
  f32 = single;
  Pf32 = ^f32;

  { TDC }

  TDC = class
  private
    dc: pointer;
  public

    constructor Create(image: Puint32; w, h: integer; expr: PChar);
    constructor Create(imagef32: Pf32; w, h: integer; expr: PChar);  overload;
    destructor Destroy; override;
    procedure generateImage;
    procedure generateImageMT;
    procedure generateImageMTf32;
    function getError: boolean;
  end;

implementation


// interface to dc.dll
{$ifdef windows}
        const dcLib='dc.dll';
{$endif}

function dcCreate(image: Puint32; w, h: integer; expr: PChar): pointer;
  cdecl; external dcLib;
function dcCreatef32(image: Pf32; w, h: integer; expr: PChar): pointer;
  cdecl; external dcLib;
function dcGetError(dc: pointer): boolean; cdecl; external dcLib;
procedure dcGenImage(dc: pointer); cdecl; external dcLib;
procedure dcGenImageMT(dc: pointer); cdecl; external dcLib;
procedure dcGenImageMTf32(dc: pointer); cdecl; external dcLib;
procedure dcFree(dc: pointer); cdecl; external dcLib;

{ TDC }

constructor TDC.Create(image: Puint32; w, h: integer; expr: PChar);
begin
   dc := dcCreate(image, w, h, expr);
end;

constructor TDC.Create(imagef32: Pf32; w, h: integer; expr: PChar);
begin
     dc := dcCreatef32(imagef32, w, h, expr);
end;

destructor TDC.Destroy;
begin
  dcFree(dc);
  inherited;
end;

procedure TDC.generateImage;
begin
  dcGenImage(dc); // image in image pointer passed on create
end;

procedure TDC.generateImageMT;
begin
  dcGenImageMT(dc); // image in image pointer passed on create
end;

procedure TDC.generateImageMTf32;
begin
    dcGenImageMTf32(dc); // image in image pointer passed on create
end;

function TDC.getError: boolean;
begin
  Result := dcGetError(dc);
end;

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
