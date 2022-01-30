unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, uMandelbrot, uComplex;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure Image1Click(Sender: TObject);

  private
    mandel: Mandelbrot;
    center, range: complex;

    procedure genMandel;
    procedure AssignImage;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  GraphType, IntfGraphics;


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  center := cinit(0.5, 0.0);
  range := cinit(-2.0, 2.0);

  genMandel;

end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    $1b:
    begin
      Close();
    end;
    $20:
    begin
      center := cinit(0.5, 0.0);
      range := cinit(-2.0, 2.0);
      genMandel;
    end;
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  genMandel;
end;

procedure TForm1.Image1Click(Sender: TObject);
var
  p: TPoint;
begin
  p := Image1.ScreenToClient(Mouse.CursorPos);
  StatusBar1.SimpleText := format('mouse:(%0:d, %1:d)', [p.x, p.y]);
  // re center fractal
  center := cinit(center.re + (Image1.Width / 2 - p.x) / image1.Width,
    center.im + (image1.Height / 2 - p.y) / image1.Height);
  range := cinit(range.re * 0.7, range.im * 0.7);
  genMandel;

  image1.refresh;
end;

procedure TForm1.genMandel;
begin
  mandel.Free;
  mandel := Mandelbrot.Create(Image1.Width, Image1.Height, 200, center, range);
  //mandel.genImage;
  mandel.genImageMT; // in multi thread mode

  StatusBar1.SimpleText := format('w:%0d, h:%1d, center:(%2f,%3f), range:(%4f,%5f)',
    [Image1.Width, Image1.Height, center.re, center.im, range.re, range.im]);

  AssignImage;
end;


procedure TForm1.AssignImage;   // image1 <- mandel.image  thanks to @wp
var
  rawImg: TRawImage;
  bmp: TBitmap;
begin
  { Create a rawimage with RGBA color format and link data array to it }

  rawImg.Init;
  //rawImg.Description.Init_BPP32_A8R8G8B8_BIO_TTB(IMG_WIDTH, IMG_HEIGHT);
  rawImg.Description.Init_BPP32_R8G8B8A8_BIO_TTB(Image1.Width, Image1.Height);
  rawImg.DataSize := Image1.Width * Image1.Height * SizeOf(mandel.image[0]);
  // size of the image array, in bytes
  rawImg.Data := @mandel.image[0];
  // rawImg.Data points to first byte of the FRawData array

  { Create a bitmap from the rawImg and display it in the Image component }
  bmp := TBitmap.Create;
  try
    bmp.LoadFromRawImage(rawImg, False);  // false = bmp does not "own" the image data.
    Image1.Picture.Assign(bmp);
  finally
    bmp.Free;
  end;
end;

end.