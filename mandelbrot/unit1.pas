unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, DateUtils,
  ComCtrls, SpinEx, uMandelbrot, uComplex, Types, LCLType;

const
  BookMarkFile = 'mandelbrot.bm';

type

  MandelState = record
    center, range: Complex;
    ratio: double;
    iters, colorSet: integer;
  end;
  pMandelState = ^MandelState;


  { TForm1 }

  TForm1 = class(TForm)
    btdelBookMark: TButton;
    btSave: TButton;
    btInit: TButton;
    btBookMark: TButton;
    btSaveExt: TButton;
    cbEngines: TComboBox;
    Image1: TImage;
    Label1: TLabel;
    lbBookMark: TListBox;
    Panel1: TPanel;
    seIters: TSpinEditEx;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    procedure btdelBookMarkClick(Sender: TObject);
    procedure btBookMarkClick(Sender: TObject);
    procedure btInitClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btSaveExtClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Image1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure lbBookMarkClick(Sender: TObject);
    procedure seItersClick(Sender: TObject);
    procedure seItersEditingDone(Sender: TObject);

  private
    mandel: Mandelbrot;
    ms: MandelState;

    t0: TDateTime;
    lap: int64;
    working: boolean;

    procedure genMandel;
    procedure AssignImage(var timg: TImage; var img: array of uint32);
    procedure freeBookMarks;
    procedure writeBookMarks;
    procedure readBookMarks;
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
  readBookMarks;

  with ms do
  begin
    center := cinit(0.5, 0.0);
    range := cinit(-2.0, 2.0);
    iters := seIters.Value;
    ratio := 1;
  end;

  working := False;
  genMandel;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  writeBookMarks;
  freeBookMarks;
  mandel.Free;
end;

procedure TForm1.readBookMarks;
var
  tf: TextFile;
begin
  if FileExists(BookMarkFile) then
  begin
    AssignFile(tf, BookMarkFile);
    Reset(tf);
    while not EOF(tf) do
      with ms do
      begin
        readln(tf, ms.center.re, ms.center.im, ms.range.re, ms.range.im, iters, ratio);
        btBookMarkClick(nil);
      end;
    CloseFile(tf);
  end;
end;

procedure TForm1.writeBookMarks;
var
  i: integer;
  tf: TextFile;
begin
  AssignFile(tf, BookMarkFile);
  Rewrite(tf);
  for i := 0 to lbBookMark.Items.Count - 1 do
    with pMandelState(lbBookMark.Items.Objects[i])^ do
      writeln(tf, center.re, ' ', center.im, ' ', range.re, ' ',
        range.im, ' ', iters, ' ', ratio);
  CloseFile(tf);
end;

procedure TForm1.freeBookMarks;
var
  i: integer;
begin
  for i := 0 to lbBookMark.Items.Count - 1 do
    Dispose(pMandelState(lbBookMark.Items.Objects[i]));
end;

procedure TForm1.btSaveClick(Sender: TObject);
var
  i: integer;
  fn: string;
begin
  i := 0;
  repeat // find latest fracatl file
    fn := format('fractal%0d.jpg', [i]);
    Inc(i);
  until not FileExists(fn);
  Image1.Picture.SaveToFile(fn, 'jpg');
  StatusBar1.SimpleText := 'saved fractal file ' + fn;
end;

procedure TForm1.btSaveExtClick(Sender: TObject);

  function CreateRawImage(w, h: integer): TRawImage; // u32 - RGBA
  begin
    Result.Init;
    Result.Description.Init_BPP32_R8G8B8A8_BIO_TTB(w, w);
    Result.DataSize := w * w * sizeof(uint32);
  end;

var
  w, i: integer;
  s, fn: string;
  pic: TPicture;
  rawImg: TRawImage;
  bmp: TBitmap;
  m: Mandelbrot;
begin

  s := IntToStr(mandel.w);
  if InputQuery('save extended fractal', 'width:', False, s) then
  begin
    if TryStrToInt(s, w) then
    begin
      m := Mandelbrot.Create(w, w, ms.iters, 0, ms.center, ms.range);
      m.genImageMTT;

      rawImg := CreateRawImage(w, w);
      rawImg.Data := @m.image[0];

      bmp := TBitmap.Create;
      try
        bmp.LoadFromRawImage(rawImg, False);
        pic := TPicture.Create;
        pic.Assign(bmp);

        i := 0;
        repeat // find latest fractal_ext file
          fn := format('fractal_ext%0d.png', [i]);
          Inc(i);
        until not FileExists(fn);

        pic.SaveToFile(fn, 'png');
      finally
        bmp.Free;
        pic.Free;
      end;
      m.Free;

      StatusBar1.SimpleText := 'saved extended fractal file ' + fn;
    end
    else
      Application.MessageBox('error', 'bad width integer');
  end;
end;

procedure TForm1.btInitClick(Sender: TObject);
begin
  if Application.MessageBox('also reset bookmarks', 'Mandelbrot',
    MB_ICONQUESTION + MB_YESNO) = idYes then
  begin
    freeBookMarks;
    lbBookMark.Clear();
  end;

  with ms do
  begin
    center := cinit(0.5, 0.0);
    range := cinit(-2.0, 2.0);
    ratio := 1;
  end;

  genMandel;
end;

procedure TForm1.btBookMarkClick(Sender: TObject);
var
  pms: pMandelState;
begin
  // add bookmark
  new(pms);
  pms^ := ms;

  with ms do
    lbBookMark.AddItem(format('(%0f,%1f),(%2f,%3f)',
      [center.re, center.im, range.re, range.im]), TObject(pms));
end;

procedure TForm1.btdelBookMarkClick(Sender: TObject);
begin
  if lbBookMark.ItemIndex <> -1 then
    lbBookMark.Items.Delete(lbBookMark.ItemIndex);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  genMandel;
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  p: TPoint;
  w, h: integer;
  scale: double = 0.9;
begin
  p := Image1.ScreenToClient(Mouse.CursorPos);
  w := Image1.Width;
  h := image1.Height;
  with ms do
  begin
    ratio := (range.im - range.re) / 2;
    // re center fractal
    center := cinit(center.re + ratio * (w / 2 - p.x) / w, center.im +
      ratio * (h / 2 - p.y) / h);

    if Button = mbRight then
      scale := 1 / scale;
    range := cinit(range.re * scale, range.im * scale);
  end;
  genMandel;

  image1.refresh;
end;

procedure TForm1.Image1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
var
  p: TPoint;
  w, h: integer;
  scale: double = 0.9;
begin
  p := Image1.ScreenToClient(Mouse.CursorPos);
  w := Image1.Width;
  h := image1.Height;
  with ms do
  begin
    ratio := (range.im - range.re) / 10;
    // re center fractal
    center := cinit(center.re + ratio * (w / 2 - p.x) / w, center.im +
      ratio * (h / 2 - p.y) / h);

    if WheelDelta < 0 then
      scale := 1 / scale;
    range := cinit(range.re * scale, range.im * scale);
  end;
  genMandel;

  image1.refresh;
end;

procedure TForm1.lbBookMarkClick(Sender: TObject);
begin
  ms := pMandelState(lbBookMark.Items.Objects[lbBookMark.ItemIndex])^;
  seIters.Value := ms.iters;

  genMandel;
end;

procedure TForm1.seItersClick(Sender: TObject);
begin
  ms.iters := seIters.Value;
  genMandel;
end;

procedure TForm1.seItersEditingDone(Sender: TObject);
begin
  ms.iters := seIters.Value;
  genMandel;
end;

procedure TForm1.genMandel;
begin
  if not working then
  begin

    working := True;

    with ms do
    begin

      mandel.Free;
      mandel := Mandelbrot.Create(Image1.Width, Image1.Height, iters,
        colorSet, center, range);

      t0 := now;

      case cbEngines.ItemIndex of
        0: mandel.genMTCPPf32;
        1: mandel.genMTCPPf64;
        2: mandel.genMTCPPf128;
        3: mandel.genImageMT;
        4: mandel.genImageMTT;
        5: mandel.genMPFR;
      end;

      lap := MilliSecondsBetween(now, t0);
      StatusBar1.SimpleText :=
        format('lap:%0dms | w:%1d, h:%2d, center:(%3f,%4f), range:(%5f,%6f), ratio:%7.4f',
        [lap, Image1.Width, Image1.Height, center.re, center.im,
        range.re, range.im, ratio]);
    end;
    AssignImage(Image1, mandel.image);

    working := False;

  end;
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
