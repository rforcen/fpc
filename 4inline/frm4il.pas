unit frm4il;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Spin, board;

type

  { TForm1 }

  TForm1 = class(TForm)
    btNewGame: TButton;
    pb: TPaintBox;
    sb: TStatusBar;
    seLevel: TSpinEdit;
    topPanel: TPanel;
    procedure btEvalClick(Sender: TObject);
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
end;

procedure TForm1.btNewGameClick(Sender: TObject);
begin
  board.init;
  pb.refresh;
  isPlaying := True;
end;

procedure TForm1.btEvalClick(Sender: TObject);
begin
  sb.simpleText := format('val:%d', [board.evaluateBoard]);
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
          if board.isWinner(human) then
            sb.SimpleText := 'you win!'
          else
          begin
            t0 := getTickCount64;
            val := board.play(machine, seLevel.Value);
            board.moveBest;

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
                format('i move %d, val:%d, #evals:%d/%d : %.0f%% prune, lap:%d',
                [board.getBestMove + 1, val, board.countEval,
                board.possibleMoves, 100.0 * (1.0 - board.countEval / board.possibleMoves),
                getTickCount64 - t0]);
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

end.
