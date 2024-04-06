unit frm4il;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Spin, Grids, uFourInLine;

type

  { TForm1 }

  TForm1 = class(TForm)
    btNewGame: TButton;
    bodyPanel: TPanel;
    btUndo: TButton;
    cbMT: TCheckBox;
    pb: TPaintBox;
    sb: TStatusBar;
    seLevel: TSpinEdit;
    sg: TStringGrid;
    topPanel: TPanel;
    procedure btEvalClick(Sender: TObject);
    procedure btUndoClick(Sender: TObject);
    procedure btNewGameClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbMouseDown(Sender: TObject; Button: TMouseButton;
    {%H-}Shift: TShiftState; X, {%H-}Y: integer);
    procedure pbPaint(Sender: TObject);
  private
    board: TBoard;
    human, machine: TBoard.TChip;
    t0: int64;
    isPlaying: boolean;
    moves: array of byte;
  private
    procedure addMove(col: byte; chip: TBoard.TChip; val: integer);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  board.init;
  isPlaying := True;
  machine := TBoard.TChip.chBlack;
  human := TBoard.TChip.chWhite;
  moves := nil;
end;

procedure TForm1.btNewGameClick(Sender: TObject);
begin
  if (length(moves) <> 0) and (QuestionDlg('4 in line', 'new game?',
    mtConfirmation, [mrYes, mrNo], '') = mrYes) then
  begin
    board.init;
    pb.refresh;
    sg.Clean([gzNormal, gzFixedRows]);
    sg.RowCount := 1;
    isPlaying := True;
    moves := nil;
  end;
end;

procedure TForm1.btEvalClick(Sender: TObject);
begin
  sb.simpleText := format('val:%d', [board.evaluateBoard]);
end;

procedure TForm1.btUndoClick(Sender: TObject);
var
  numRemove: integer = 2;
begin
  if length(moves) <> 0 then
  begin
    board.unMoveChip(moves[high(moves)]);

    if odd(length(moves)) then  numRemove := 1 // odd -> after win condition
    else
      board.unMoveChip(moves[high(moves) - 1]);

    Delete(moves, length(moves) - numRemove, numRemove);

    sg.Clean(0, sg.RowCount - numRemove, 3, sg.RowCount, [gzNormal]);
    sg.RowCount := sg.RowCount - numRemove;
    pb.refresh;
    isPlaying := True;
  end;
end;


procedure TForm1.pbMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  val: integer;
begin
  if isPlaying then
  begin
    case Button of
      mbRight: board.Clear;
      mbLeft: begin
        if board.moveChipGeo(human, X) then
        begin

          addMove(board.lastC, human, 0);

          if board.isWinner(human) then
            sb.SimpleText := 'you win!'
          else
          begin
            t0 := getTickCount64;

            if cbMT.Checked then val := board.playMT(machine, seLevel.Value)
            else
              val := board.play(machine, seLevel.Value);

            board.moveBest;

            addMove(board.bestMove, machine, board.bestVal);

            if board.isWinner(machine) then
            begin
              sb.simpletext := 'i win!';
              isPlaying := False;
            end
            else if board.isDraw then
            begin
              sb.simpletext := 'draw';
              isPlaying := False;
            end
            else
              sb.simpleText :=
                format('i move %d, val:%d, #evals:%d/%d : %.1f%% prune, lap:%d ms',
                [board.getBestMove + 1, val, board.countEval,
                board.possibleMoves, 100.0 * (1.0 - board.countEval /
                board.possibleMoves), getTickCount64 - t0]);
          end;
        end
        else
          sb.SimpleText := 'invalid move';
      end;
    end;
    pb.refresh;
  end;
end;

procedure TForm1.pbPaint(Sender: TObject);
begin
  board.paint(pb);
end;

procedure TForm1.addMove(col: byte; chip: TBoard.TChip; val: integer);
const
  sval: array[-1..1] of string = ('Loose', ' ', 'Win');
var
  row: integer;
  s: string;
begin
  row := sg.RowCount;
  sg.RowCount := sg.RowCount + 1;

  if chip = human then s := 'human'
  else
    s := 'machine';
  sg.Cells[0, row] := IntToStr(row);
  sg.Cells[1, row] := IntToStr(col + 1);
  sg.Cells[2, row] := s;
  sg.Cells[3, row] := sval[val];

  sg.Row := sg.RowCount;

  insert(col, moves, length(moves));
end;

end.
