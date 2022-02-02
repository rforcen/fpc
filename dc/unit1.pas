unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, DateUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    cbPresets: TComboBox;
    eExpr: TEdit;
    Image1: TImage;
    Label2: TLabel;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    procedure cbPresetsSelect(Sender: TObject);
    procedure eExprEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);

  private
    image: array of uint32;
    dc: pointer;
    w, h: integer;
    expr: string;
    t0: TDateTime;
    lap: int64;

    procedure CreateImage;
    procedure AssignImage(var timg: TImage; var img: array of uint32);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  GraphType, IntfGraphics;

// c++ interface
{$link cpp/DomainColoring.o}

{$linklib c}
{$linklib stdc++}
{$linklib gcc_s}
{$linklib m}
function dcCreate(image: pointer; w, h: integer; expr: PChar): pointer; cdecl; external;
function dcGetError(dc: pointer): boolean; cdecl; external;
procedure dcGenImage(dc: pointer); cdecl; external;
procedure dcGenImageMT(dc: pointer); cdecl; external;

{ TForm1 }

procedure TForm1.CreateImage;
begin
  expr := eExpr.Text;

  w := Image1.Width;
  h := Image1.Height;

  setLength(image, w * h);
  dc := dcCreate(@image[0], w, h, @expr[1]);

  if dcGetError(dc) then
    StatusBar1.SimpleText := 'syntax error in expression'
  else
  begin
    t0 := now;
    dcGenImageMT(dc);
    lap := MilliSecondsBetween(now, t0);
    StatusBar1.SimpleText := format('lap:%0dms | w:%1d, h:%2d', [lap, w, h]);

    AssignImage(Image1, image);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  eExpr.Text :=  cbPresets.Text;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  CreateImage;
end;


procedure TForm1.eExprEditingDone(Sender: TObject);
begin
  CreateImage;
end;

procedure TForm1.cbPresetsSelect(Sender: TObject);
begin
  eExpr.Text :=  cbPresets.Text;
  CreateImage;
end;



// Image1 <- mandel.image  thanks to @wp
procedure TForm1.AssignImage(var timg: TImage; var img: array of uint32);
var
  rawImg: TRawImage;
  bmp: TBitmap;
begin
  { Create a rawimage with RGBA color format and link data array to it }

  rawImg.Init;
  //rawImg.Description.Init_BPP32_A8R8G8B8_BIO_TTB(IMG_WIDTH, IMG_HEIGHT);  // ARGB
  rawImg.Description.Init_BPP32_R8G8B8A8_BIO_TTB(Image1.Width, Image1.Height); // RGBA
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
