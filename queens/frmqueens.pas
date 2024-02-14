unit frmQueens;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Spin, StdCtrls,
  ExtCtrls, ComCtrls, ClipBrd,
  uQueen, uBoard, Types;

type

  { TfrmQueen }

  TfrmQueen = class(TForm)
    cbMT: TComboBox;
    cbAnimate: TCheckBox;
    currSolution: TSpinEdit;
    Label1: TLabel;
    saveBtn: TButton;
    maxSols: TSpinEdit;
    goBtn: TButton;
    edNum: TSpinEdit;
    pbBoard: TPaintBox;
    panelControl: TPanel;
    panelDisp: TPanel;
    stcBtn: TButton;
    trBtn: TButton;
    sbar: TStatusBar;
    procedure currSolutionChange(Sender: TObject);
    procedure edNumChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
      WheelDelta: integer; {%H-}MousePos: TPoint; var {%H-}Handled: boolean);
    procedure goBtnClick(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure pbBoardPaint(Sender: TObject);
    procedure saveBtnClick(Sender: TObject);
    procedure stcBtnClick(Sender: TObject);
    procedure trBtnClick(Sender: TObject);
  private
    procedure asyncProc(Data: PtrInt);
    procedure showResults;

    procedure disableAll;
    procedure enableAll;
  private
    active: boolean; static;
  public
  end;

  { TFormThread }

  TFormThread = class(TThread)
    constructor Create;
    destructor Destroy; override;
  protected
    procedure Execute; override;
  end;

var
  frmQueen: TfrmQueen;
  queen: TQueen;
  t0: int64;
  th: TFormThread;



implementation

{$R *.lfm}

{ TfrmQueen }

procedure TfrmQueen.FormCreate(Sender: TObject);
begin
  queen := TQueen.Create(edNum.Value);
  th := nil;
  active := True;
  Application.QueueAsyncCall(asyncProc, 0); // used to update sbar when scanning
end;

procedure TfrmQueen.asyncProc(Data: PtrInt); // called on async update
var
  q: TQueen absolute Data;
begin
  if Data <> 0 then // final msg?
  begin
    if q.validated then // q ok?
    begin
      if cbAnimate.Checked then q.Paint(pbBoard.canvas);

      sbar.SimpleText := format('evaluated %s boards, solutions found:%d',
        [q.fmtEvalSuffix, q.countSolutions]);
    end;
  end
  else  // final message -> update
    showResults;
end;

procedure TfrmQueen.showResults;
begin
  if active then
  begin
    enableAll;

    queen.setSolution(0);

    sbar.SimpleText := Format(
      'board size: %d, lap:%d ms, evaluated %s boards, %d solutions found',
      [queen.n, GetTickCount64 - t0, queen.fmtEvalSuffix, queen.nSolutions]);

    currSolution.Value := 1;
    currSolution.MaxValue := queen.nSolutions;

    pbBoard.Refresh;
    goBtn.Caption := 'go!';
    th := nil;

  end;
end;

procedure TfrmQueen.disableAll; // except go button
var
  i: integer;
begin
  for i := 0 to panelControl.ControlCount - 1 do
    if panelControl.Controls[i].Name <> 'goBtn' then
      panelControl.Controls[i].Enabled := False;
  panelDisp.Enabled := False;
end;

procedure TfrmQueen.enableAll;
var
  i: integer;
begin
  for i := 0 to panelControl.ControlCount - 1 do
    panelControl.Controls[i].Enabled := True;
  panelDisp.Enabled := True;
end;

procedure TfrmQueen.FormDestroy(Sender: TObject);
begin
  active := False;
  if th <> nil then
  begin
    queen.running := False;
    sleep(100);
    th := nil;
  end;

  queen.Free;
end;

procedure TfrmQueen.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean); // browse sols
var
  cs: integer;
begin
  cs := currSolution.Value;
  Inc(cs, WheelDelta div abs(WheelDelta)); // +1 -1
  if (cs >= 0) and (cs <= queen.nSolutions) then
    currSolution.Value := cs;
end;

procedure TfrmQueen.goBtnClick(Sender: TObject); // scan solutions
begin

  t0 := GetTickCount64;

  case cbMT.ItemIndex of
    0: begin
      queen.Free;
      queen := TQueen.Create(edNum.Value, maxSols.Value);
      queen.setThMode(cbMT.ItemIndex);

      queen.findSolutions;

      showResults;
    end;
    1: begin
      if th = nil then  // not running
      begin
        disableAll;

        queen.Free;
        queen := TQueen.Create(edNum.Value, maxSols.Value);
        queen.setThMode(cbMT.ItemIndex);
        goBtn.Caption := 'stop';

        th := TFormThread.Create;
        th.Start;
      end
      else
      begin
        queen.running := False;
        sleep(100);
        th := nil;
      end;
    end;
    2: ; // queen.findSolutionsRecMT;
    else
      exit;
  end;
end;

procedure TfrmQueen.Label1Click(Sender: TObject);
begin
  if queen.isValid then sbar.simpletext := 'is valid'
  else
    sbar.simpletext := 'NOT valid';
end;

procedure TfrmQueen.edNumChange(Sender: TObject); // board size changed
begin
  queen.Free;
  queen := TQueen.Create(edNum.Value);

  currSolution.Value := 1;

  pbBoard.refresh;
end;

procedure TfrmQueen.currSolutionChange(Sender: TObject); // draw other solution
begin
  queen.setSolution(currSolution.Value - 1); // 1..nsol -> 0..nsol-1
  pbBoard.refresh;
end;

procedure TfrmQueen.pbBoardPaint(Sender: TObject);  // draw board
begin
  queen.Paint(Sender as TPaintBox);
end;

procedure TfrmQueen.saveBtnClick(Sender: TObject);  // save 4k image of current board
const
  s4k = 4096;
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.setSize(s4k, s4k);

  queen.Paint(bmp.canvas);

  with TPortableNetworkGraphic.Create do
  try
    Assign(bmp);
    SaveToFile('queen.png');
  finally
    Free;
  end;

  bmp.Free;
end;

procedure TfrmQueen.stcBtnClick(Sender: TObject);
begin
  Clipboard.AsText:=queen.solutionsAsTExt;
end;

procedure TfrmQueen.trBtnClick(Sender: TObject);
begin
  queen.transformations;

  queen.setSolution(0);

  currSolution.Value := 1;
  currSolution.MaxValue := queen.nSolutions;

  sbar.SimpleText := Format('%d transformations', [queen.nSolutions]);

  pbBoard.refresh;
end;


{ TFormThread }

constructor TFormThread.Create;
begin
  freeOnTerminate := True;
  inherited Create(True);
end;

destructor TFormThread.Destroy;
begin
  inherited Destroy;
end;


procedure TFormThread.Execute;
begin
  queen.setAsyncCall(frmQueen.asyncProc);
  queen.findSolutionsMT;
end;

{------------}


end.
