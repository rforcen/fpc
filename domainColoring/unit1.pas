unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Spin, DateUtils, LCLType,
  { local units} domainColoring, utils;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnReset: TButton;
    btnRandom: TButton;
    btnSave: TButton;
    btnLoadFormulas: TButton;
    cbPresets: TComboBox;
    cbShowSource: TCheckBox;
    eExpr: TEdit;
    Image1: TImage;
    ImageList1: TImageList;
    Label2: TLabel;
    lbSrccode: TListBox;
    lvThumbs: TListView;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    pnlTools: TPanel;
    seWidth: TSpinEdit;
    StatusBar1: TStatusBar;
    procedure btnLoadFormulasClick(Sender: TObject);
    procedure btnRandomClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cbPresetsSelect(Sender: TObject);
    procedure cbShowSourceClick(Sender: TObject);
    procedure eExprEditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; {%H-}Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure lvThumbsClick(Sender: TObject);

  private
    w, h: integer;
    t0: TDateTime;
    lap: int64;

    domCol: TDomainColoring;
    presets: TStrings;
    FOrigBounds: TRect;
    sess: TSessionDC;

    procedure CreateImage;
    procedure populateImageList;
    procedure freeImageList;
    procedure randomFormula;


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

  t0 := now;

  domCol.genImageMT(rt_u32, w, h, eExpr.Text);

  lap := MilliSecondsBetween(now, t0);

  if domCol.getError <> '' then
    StatusBar1.SimpleText := 'syntax error in expression:' + domCol.getError
  else
  begin
    StatusBar1.SimpleText := format('lap:%0dms | w:%1d, h:%2d, session:%s',
      [lap, w, h, sess.fileName]);

    AssignImage(Image1, domCol.image);

    // de compile -> source code
    domCol.zComp.deAssemble;
    eExpr.Text := domCol.zComp.deCompile;  // decompile generated code

    lbSrcCode.Items := domCol.zComp.srcCode;
  end;
end;

procedure TForm1.populateImageList;
const
  TN_SIZE = 256;
var
  expr: string;
  dc: TDomainColoring;
  bmp: TBitmap;
begin

  freeImageList;

  // fill imageList1 w/thumbnails of all cbPresets
  for expr in cbPresets.Items do
  begin
    dc := TDomainColoring.Create;
    dc.genImageMT(rt_u32, TN_SIZE, TN_SIZE, expr);

    if dc.ok then
    begin
      with lvThumbs.Items.Add do
      begin
        Caption := expr;
        bmp := genBMP(TN_SIZE, TN_SIZE, dc.image);
        ImageIndex := imageList1.Add(bmp, nil);
        bmp.Free; // no longer needed as it's copied to imageList
      end;
    end;
    dc.Free;
  end;
end;

procedure TForm1.freeImageList;
begin
  imageList1.Clear;
  lvThumbs.Clear;
end;

procedure TForm1.randomFormula;
begin

  domCol.zComp.generateRandomExpression;

  t0 := now;

  domCol.genImageMT(rt_u32, Image1.Width, Image1.Height);
  // no expression use generated code

  lap := MilliSecondsBetween(now, t0);

  StatusBar1.SimpleText := format('lap:%0dms | w:%1d, h:%2d, session:%s',
    [lap, w, h, sess.fileName]);

  AssignImage(Image1, domCol.image);

  // de compile -> source code
  domCol.zComp.deAssemble;
  lbSrcCode.Items := domCol.zComp.srcCode;

  eExpr.Text := domCol.zComp.deCompile;  // decompile generated code

  sess.writeExpr(eExpr.Text);
end;



procedure TForm1.FormResize(Sender: TObject);
begin
  CreateImage;
end;

procedure TForm1.Image1Click(Sender: TObject);
begin
  randomFormula;
end;


procedure TForm1.Image1DblClick(Sender: TObject);
begin
  if WindowState = wsNormal then
  begin
    FOrigBounds := BoundsRect;
    WindowState := wsFullScreen;
    BorderStyle := bsNone;
    pnlTools.Hide;
    Panel3.Hide;
    StatusBar1.Hide;
  end
  else
  begin
    WindowState := wsNormal;
    BoundsRect := FOrigBounds;
    BorderStyle := bsSizeable;
    pnlTools.Show;
    Panel3.Show;
    StatusBar1.Show;
  end;
end;

procedure TForm1.lvThumbsClick(Sender: TObject);
begin
  eExpr.Text := cbPresets.Items[lvThumbs.ItemIndex];
  CreateImage;
end;

procedure TForm1.eExprEditingDone(Sender: TObject);
begin
  sess.writeExpr(eExpr.Text);
  CreateImage;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  freeImageList;
  domCol.Free;

  sess.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  randomize;

  domCol := TDomainColoring.Create;

  cbPresets.ItemIndex := random(cbPresets.Items.Count);
  eExpr.Text := cbPresets.Text;
  presets := cbPresets.Items;

  populateImageList;

  sess := TSessionDC.Create;
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


procedure TForm1.cbShowSourceClick(Sender: TObject);
begin
  lbSrcCode.Visible := cbShowSource.Checked;
end;

procedure TForm1.btnSaveClick(Sender: TObject);

var
  i: integer;
  fn: string;
begin

  w := seWidth.Value * 1024;

  domCol.genImageMT(rt_u32, w, w); // generate current compiled image scaled de w

  i := 0;
  repeat // find latest domaincoloring file
    fn := 'DomainColoring' + format('%0d.png', [i]);
    Inc(i);
  until not FileExists(fn);

  CreatePNG(fn, w, w, domCol.image);

  StatusBar1.SimpleText := 'saved extended Domain Coloring file: ' + fn;
end;

procedure TForm1.btnLoadFormulasClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    sess.Free; // close current session
    begin
      cbPresets.Items.LoadFromFile(OpenDialog1.FileName);
      populateImageList;
    end;
    sess := TsessionDC.Create; // open a new session
  end;
end;

procedure TForm1.btnRandomClick(Sender: TObject);
begin
  randomFormula;
end;

procedure TForm1.btnResetClick(Sender: TObject);
begin
  if MessageDlg('load preset values & start new session?', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then
  begin
    cbPresets.Items := presets;
    populateImageList;

    sess.Free; // new session
    sess := TSessionDC.Create;
  end;
end;

end.
