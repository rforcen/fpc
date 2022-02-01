unit uVoronoi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, iVoronoi, DateUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    NPoints: TEdit;
    Image1: TImage;
    Panel1: TPanel;
    StatusBar1: TStatusBar;

    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure NPointsEditingDone(Sender: TObject);


  private
    vrn: Voronoi;

    t0: TDateTime;
    lap: int64;
    procedure AssignImage(var timg: TImage; var img: array of uint32);
    procedure genVoronoi;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  GraphType, IntfGraphics;

{ TForm1 }

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    $1b:
    begin
      Close();
    end;
    $20:
    begin
      genVoronoi;
    end;
  end;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
   NPoints.Text := IntToStr(Image1.Width div 4);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  genVoronoi;
end;

procedure TForm1.Image1Click(Sender: TObject);
begin
  genVoronoi;
end;


procedure TForm1.NPointsEditingDone(Sender: TObject);
begin
    genVoronoi;
end;


procedure TForm1.genVoronoi;
begin

  vrn.Free;
  vrn := Voronoi.Create(Image1.Width, Image1.Height, StrToInt(NPoints.Text));

  t0 := now;

  //vrn.genImageMTT;
  vrn.genMTCPP;

  lap := MilliSecondsBetween(now, t0);

  StatusBar1.Panels[0].Text := format('lap:%0dms', [lap]);
  StatusBar1.Panels[1].Text :=
    format('w:%0d, h:%1d, #points:%2d', [Image1.Width, Image1.Height,
    length(vrn.points)]);
  AssignImage(Image1, vrn.image);

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
