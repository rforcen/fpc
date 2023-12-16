unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Spin, DateUtils, LCLType,
  { local units} domainColoring, assignImage;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnReset: TButton;
    btnSave: TButton;
    btnLoadFormulas: TButton;
    cbPresets: TComboBox;
    eExpr: TEdit;
    Image1: TImage;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlTools: TPanel;
    seWidth: TSpinEdit;
    StatusBar1: TStatusBar;
    procedure btnLoadFormulasClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cbPresetsSelect(Sender: TObject);
    procedure eExprEditingDone(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; {%H-}Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);

  private
    w, h: integer;
    t0: TDateTime;
    lap: int64;

    domCol: TDomainColoring;
    presets: TStrings;
    FOrigBounds: TRect;

    procedure CreateImage;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  GraphType, IntfGraphics;

{ TForm1 }

procedure TForm1.CreateImage;
begin
  w := Image1.Width;
  h := Image1.Height;

  domCol := TDomainColoring.Create(rt_u32, w, h, eExpr.Text);

  if domCol.getError <> '' then
    StatusBar1.SimpleText := 'syntax error in expression:' + domCol.getError
  else
  begin

    t0 := now;

    domCol.genImageMT;

    lap := MilliSecondsBetween(now, t0);

    StatusBar1.SimpleText := format('lap:%0dms | w:%1d, h:%2d', [lap, w, h]);

    assignImage.AssignImage(Image1, domCol.image);
  end;

  domCol.Free;
end;



procedure TForm1.FormResize(Sender: TObject);
begin
  CreateImage;
end;


procedure TForm1.Image1DblClick(Sender: TObject);
begin
  if WindowState = wsNormal then
  begin
    FOrigBounds := BoundsRect;
    WindowState := wsFullScreen;
    BorderStyle := bsNone;
    pnlTools.Hide;
    StatusBar1.Hide;
  end
  else
  begin
    WindowState := wsNormal;
    BoundsRect := FOrigBounds;
    BorderStyle := bsSizeable;
    pnlTools.Show;
    StatusBar1.Show;
  end;
end;


procedure TForm1.eExprEditingDone(Sender: TObject);
begin
  CreateImage;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  eExpr.Text := cbPresets.Text;
  presets := cbPresets.Items;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

procedure TForm1.cbPresetsSelect(Sender: TObject);
begin
  eExpr.Text := cbPresets.Text;
  CreateImage;
end;

procedure TForm1.btnSaveClick(Sender: TObject);

  function CreateRawImage(w, h: integer): TRawImage; // u32 - RGBA
  begin
    Result.Init;
    Result.Description.Init_BPP32_R8G8B8A8_BIO_TTB(w, h);
    Result.DataSize := w * h * sizeof(uint32);
  end;

var
  i: integer;
  fn: string;
  pic: TPicture;
  rawImg: TRawImage;
  bmp: TBitmap;
begin

  w := seWidth.Value * 1024;

  domCol := TDomainColoring.Create(rt_u32, w, w, eExpr.Text);
  domCol.genImageMT;

  rawImg := CreateRawImage(w, w);
  rawImg.Data := @domCol.image[0];

  bmp := TBitmap.Create;
  try
    bmp.LoadFromRawImage(rawImg, False);  // now owned so no need to rawImg.FreeData
    pic := TPicture.Create;
    pic.Assign(bmp);

    i := 0;
    repeat // find latest fractal_ext file
      fn := 'DomainColoring' + format('%0d.png', [i]);
      Inc(i);
    until not FileExists(fn);

    pic.SaveToFile(fn, 'png');
  finally
    bmp.Free;
    pic.Free;
  end;

  domCol.Free;

  StatusBar1.SimpleText := 'saved extended Domain Coloring file: ' + fn;

end;

procedure TForm1.btnLoadFormulasClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    cbPresets.Items.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm1.btnResetClick(Sender: TObject);
begin
  if MessageDlg('load preset values?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    cbPresets.Items := presets;
end;

end.
