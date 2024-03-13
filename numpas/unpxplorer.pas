unit uNPxplorer;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, Spin, ComCtrls, SpinEx,
  Types, ClipBrd, EditBtn,
  uNumPas, NPInterpreter;

type
  { TForm1 }

  TForm1 = class(TForm)
    btSave: TButton;
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
    ebDims: TEditButton;
    edFileName: TEdit;
    edShape: TEdit;
    FlowPanel1: TFlowPanel;
    gb: TGroupBox;
    Label1: TLabel;
    lbDims: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    seDims: TSpinEdit;
    sg: TStringGrid;
    seDecs: TSpinEdit;
    stBar: TStatusBar;
    procedure btSaveClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ebDimsClick(Sender: TObject);
    procedure edCommandEditingDone(Sender: TObject);
    procedure edShapeEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbDimsDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure seDecsChange(Sender: TObject);
    procedure seDimsChange(Sender: TObject);
  private
    procedure reDisp(Sender: TObject);
    procedure DimsListBox;
  public

  end;

  { TCC - custom control derived }

  TCC = class(TCustomControl)
  public
    constructor Create(_parent: TWinControl; _notEvent: TNotifyEvent;
      _dim: integer); overload;
    destructor Destroy; override;

    procedure setBounds(const rect: TRect; _State: TOwnerDrawState); overload;
  private
    se: TSpinEdit;
    panel: TPanel;
  end;

  TReal = double;
  NP = TNumPas<TReal>;

var
  Form1: TForm1;
  a, b, slc: NP;
  npInt: TNPInterpreter;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  seDimsChange(nil);

  DimsListBox;
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

procedure TForm1.Button1Click(Sender: TObject);
var
  s: string;
  t0, fsAnt: int64;
begin
  s := (Sender as TButton).Caption;

  t0 := getTickCount64;

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

  if s = 'save' then a.save('a');
  if s = 'load' then a := a.load('a');

  if s = '+' then a += b;
  if s = '-' then a -= b;
  if s = '*' then a *= b;

  if s = '0' then a.zero;
  if s = '1' then a.one;
  if s = 'ident' then a := NP.identity(a.dim(0));
  if s = 'rng' then a := NP.arange(a.size).reshape(a.dims);
  if s = 'b=a' then  b := a.copy;
  if s = 'solve' then a := a.solve(b);

  if s = 'cmp' then
  begin
    fsAnt := a.fSizeBytes;
    a.compress;
    stBar.SimpleText := format('lap: %d ms, compressed ratio %.1f',
      [getTickCount64 - t0, 1.0 * fsAnt / a.fCmpSize]);
  end;
  if s = 'dcp' then a.deCompress;

  if s <> 'cmp' then
    stBar.SimpleText := format('lap:%d ms', [getTickCount64 - t0]);

  DimsListBox;
end;

procedure TForm1.ebDimsClick(Sender: TObject);
begin
  a := NP.Create(strToDims(ebDims.Text));
  a.fromFile(edFileName.Text);
  DimsListBox;
end;

procedure TForm1.edCommandEditingDone(Sender: TObject);
begin
  a := npInt.Execute(edCommand.Text);
  DimsListBox;
end;

procedure TForm1.edShapeEditingDone(Sender: TObject);
begin
  a := NP.Create(strToDims(edShape.Text));
  DimsListBox;
end;


procedure TForm1.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to pred(lbDims.Items.Count) do
    lbDims.items.objects[i].Free;
end;

procedure TForm1.lbDimsDrawItem(Control: TWinControl; Index: integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  ((Control as TListBox).Items.Objects[Index] as TCC).SetBounds(ARect, State);
end;

procedure TForm1.seDecsChange(Sender: TObject);
begin
  reDisp(Sender);
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
  DimsListBox;
end;

procedure TForm1.reDisp(Sender: TObject);
var
  i, r, c, d0, d1, nit: integer;
  dims: TArrInt = nil;
  sd: string;
  se: TSpinEdit;
begin
  sg.Clear;
  if a.size = 0 then exit;

  nit := lbDims.Items.Count;
  if a.fNDims >= 2 then // >= 2 dims
  begin
    if nit > 0 then
    begin
      setLength(dims, nit);
      for i := 0 to pred(nit) do
      begin // get values from spin edit's
        se := (lbDims.items.objects[i] as TCC).se;

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

    d0 := slc.dim(0);
    d1 := slc.dim(1);

    sd := format('%d x', [a.dim(1)]);

    sg.ColCount := slc.dim(0) + 1;
    sg.RowCount := slc.dim(1) + 1;
  end
  else
  begin
    sd := '';
    slc := a;

    d0 := a.dim(0);
    d1 := 1;

    sg.ColCount := slc.dim(0) + 1;
    sg.RowCount := 1 + 1;
  end;

  for c := 1 to d0 do sg.Cells[c, 0] := IntToStr(c); // col titles
  for r := 1 to d1 do sg.Cells[0, r] := IntToStr(r);

  for r := 0 to pred(d1) do
    for c := 0 to pred(d0) do
      sg.Cells[c + 1, r + 1] := format('%.*f', [seDecs.Value, slc[r, c]]);

  gb.Caption := format('dims:%s - [%s %d] of %s, items:%d',
    [unumpas.toString(dims), sd, a.dim(0), unumpas.toString(a.fDims), a.fNItems]);

  edShape.Text := unumpas.toString(a.fDims);
end;

procedure TForm1.DimsListBox;
var
  i: integer;
begin
  for i := 0 to pred(lbDims.Items.Count) do  // remove lb items
    lbDims.items.objects[i].Free;
  lbDims.Clear;

  for i := 0 to pred(a.fNDims - 2) do // leave last 2 form sg
    lbDims.AddItem('', TCC.Create(lbDims, reDisp, a.fDims[i]));

  reDisp(nil);
end;

{ TCC }

constructor TCC.Create(_parent: TWinControl; _notEvent: TNotifyEvent; _dim: integer);
begin
  inherited Create(nil);

  Parent := _parent;

  panel := TPanel.Create(nil);
  with panel do
  begin
    Parent := Self;
    ChildSizing.HorizontalSpacing := 4;
    ChildSizing.VerticalSpacing := 4;
    Align := alClient;
  end;

  se := TSpinEdit.Create(self);
  with se do
  begin
    Parent := panel;
    MinValue := 0;
    MaxValue := _dim - 1;
    Align := alClient;
    OnChange := _notEvent; // event to trigger on se change
  end;
end;

destructor TCC.Destroy;
begin
  se.Free;
  panel.Free;
  inherited Destroy;
end;

procedure TCC.setBounds(const rect: TRect; _State: TOwnerDrawState);
begin
  SetBounds(rect.Left, rect.Top, rect.Width, rect.Height);
  panel.SetBounds(rect.Left, rect.Top, rect.Width, rect.Height);
end;

end.
