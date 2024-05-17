unit uNPxplorer;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, Spin, ComCtrls, SpinEx,
  Types, ClipBrd, Math, f80,
  uNumPas, uTNPBrowser, NPInterpreter, uTCCSpinEdit;

type
  TReal = fp80;
  NP = TNumPas<TReal>;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    Button2: TButton;
    Button20: TButton;
    Button21: TButton;
    Button22: TButton;
    Button23: TButton;
    Button24: TButton;
    Button25: TButton;
    Button26: TButton;
    Button27: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    cbQuad: TCheckBox;
    cbMT: TCheckBox;
    edCommand: TEdit;
    edFileName: TEdit;
    edN: TEdit;
    edShape: TEdit;
    FlowPanel1: TFlowPanel;
    gb: TGroupBox;
    lbDims: TListBox;
    panelHead: TPanel;
    panelBody: TPanel;
    panelButtons: TPanel;
    seDims: TSpinEdit;
    seDecs: TSpinEdit;
    stBar: TStatusBar;
    procedure btSaveClick(Sender: TObject);
    procedure AnyButtonClick(Sender: TObject);
    procedure edCommandEditingDone(Sender: TObject);
    procedure edShapeEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure gbResize(Sender: TObject);
    procedure lbDimsDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure seDecsChange(Sender: TObject);
    procedure seDimsChange(Sender: TObject);
  private
    procedure reDisp(Sender: TObject = nil);
    procedure dimsListBox;
  private
    NPbrow: TNPBrowser<TReal>;
    a, b, slc: NP;
    npInt: TNPInterpreter;
    dims: TArrInt;
  public

  end;


var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  NPBrow := TNPBrowser<TReal>.Create(gb, reDisp);

  seDimsChange(nil);
  dimsListBox;
end;

procedure TForm1.btSaveClick(Sender: TObject);
begin
  a.toFile(edFileName.Text + '.bin');
  stBar.SimpleText := format(
    'array saved to:%s.bin, python instruction to read it copied to clipboard',
    [edFileName.Text]);

  ClipBoard.AsText := format('%s=np.fromfile(''%s.bin'', dtype=np.double).reshape(%s)',
    [edFileName.Text, edFileName.Text, a.dimsStr]);
end;

procedure TForm1.AnyButtonClick(Sender: TObject);
var
  s, sres: string;
  t0, fsAnt: int64;
  res: TReal;
begin

  s := (Sender as TButton).Caption;
  sres := '';
  res := 0;

  t0 := getTickCount64;

  if a.size > 0 then
  begin
    if cbMT.Checked then
    begin
      if s = 'inv' then if a.isQuadratic then  a := a.invMT;
      if s = 'det' then  if a.isQuadratic then a := a.detMT;
      if s = 'dot' then  if a.isQuadratic or ((a.ndims = 1) and (b.ndims = 1)) then
          a := a.dotMT(b);
      if s = 'rnd' then  a.randMT;
      if s = 'sort' then a := a.sortMT;
      if s = 'T' then  a := a.transposeMT;
    end
    else
    begin
      if s = 'inv' then if a.isQuadratic then  a := a.inv;
      if s = 'det' then  if a.isQuadratic then a := a.det;
      if s = 'dot' then
        if a.isQuadratic or ((a.ndims = 1) and (b.ndims = 1)) then
          a := a.dot(b);
      if s = 'rnd' then  a.rand;
      if s = 'sort' then a := a.sort;
      if s = 'T' then  a := a.transpose;
    end;

    if s = '==' then if a = b then sres := 'true'
      else
        sres := 'false';

    if s = 'sum' then sres := floatToStr(a.sum);
    if s = 'std' then sres := floatToStr(a.std);
    if s = 'mean' then sres := floatToStr(a.mean);


    if s = 'save' then a.save(edFileName.Text);
    if s = 'load' then a := a.load(edFileName.Text);

    if s = 'linsp' then a := a.linspace(0, a.size, a.size);
    if s = 'seq' then a.seq;

    if s = '+' then a += b;
    if s = '-' then a -= b;
    if s = '*' then a *= b;
    if s = '/' then a /= b;

    if s = '0' then a.zero;
    if s = '1' then a.one;
    if s = 'ident' then a := NP.identity(a.dim(0));
    if s = 'rng' then a := NP.arange(a.size).reshape(a.dims);
    if s = 'b=a' then  b := a.copy;
    if s = 'solve' then a := a.solve(b);
    if s = 'vander' then a := NP.vander(StrToInt(edN.Text));

    if s = 'cmp' then
    begin
      fsAnt := a.fSizeBytes;
      a.compress;
      stBar.SimpleText := format('lap: %d ms, compressed ratio %.1f',
        [getTickCount64 - t0, 1.0 * fsAnt / a.fCmpSize]);
    end;
  end;
  if s = 'dcp' then a.deCompress;

  if s <> 'cmp' then
    stBar.SimpleText := format('lap:%d ms %s: %s', [getTickCount64 - t0, s, sres]);

  dimsListBox;
end;

procedure TForm1.edCommandEditingDone(Sender: TObject);
begin
  //  a := npInt.Execute(edCommand.Text);
  dimsListBox;
end;

procedure TForm1.edShapeEditingDone(Sender: TObject);
begin
  a := NP.Create(strToDims(edShape.Text));
  dimsListBox;
end;


procedure TForm1.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to pred(lbDims.Items.Count) do  // all cc in listbox
    lbDims.items.objects[i].Free;
end;

procedure TForm1.gbResize(Sender: TObject);
begin
  reDisp;
end;

procedure TForm1.lbDimsDrawItem(Control: TWinControl; Index: integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  ((Control as TListBox).Items.Objects[Index] as TCCSpinEdit).SetBounds(ARect, State);
end;

procedure TForm1.seDecsChange(Sender: TObject);
begin
  reDisp;
end;

procedure TForm1.seDimsChange(Sender: TObject);
var
  dims: TArrInt = nil;
  i: integer;
begin
  setLength(dims, seDims.Value);
  repeat
    for i := low(dims) to high(dims) do dims[i] := 1 + random(10);
    if cbQuad.Checked and (length(dims) > 1) then
      dims[high(dims)] := dims[high(dims) - 1];
  until a.prod(dims) < $7fffffff;

  a := NP.rand(dims);
  b := a.copy;

  dimsListBox;
end;

procedure TForm1.reDisp(Sender: TObject);
var
  sd: string;
  se: TSpinEdit;

  procedure getSLC;
  var
    i, nit: integer;
  begin
    nit := lbDims.Items.Count;
    if a.fNDims >= 2 then // >= 2 dims
    begin
      if nit > 0 then
      begin
        setLength(dims, nit);
        for i := 0 to pred(nit) do
        begin // get values from spin edit's
          se := (lbDims.items.objects[i] as TCCSpinEdit).se;

          if (se.Value > se.MaxValue) or (se.Value < 0) then  // in range?
          begin
            dims[i] := se.MaxValue;
            se.Value := se.MaxValue;
          end
          else
            dims[i] := se.Value;

        end;
        slc := a.slice(dims);
      end
      else
        slc := a;

      sd := format('%d x', [a.dim(1)]);
    end
    else
    begin
      sd := '';
      slc := a;
    end;
  end;

begin
  if a.size = 0 then exit;

  getSLC;

  gb.Caption := format('dims:%s - [%s %d] of %s, items:%d',
    [unumpas.toString(dims), sd, a.dim(0), unumpas.toString(a.fDims), a.fNItems]);

  edShape.Text := unumpas.toString(a.fDims);

  NPBrow.refresh(slc, seDecs.Value);
end;

procedure TForm1.dimsListBox;
var
  i: integer;
begin
  for i := 0 to pred(lbDims.Items.Count) do  // remove lb items
    lbDims.items.objects[i].Free;
  lbDims.Clear;

  for i := 0 to pred(a.fNDims - 2) do // leave last 2 form sg
    lbDims.AddItem('', TCCSpinEdit.Create(lbDims, reDisp, a.fDims[i]));

  reDisp(nil);
end;

end.
