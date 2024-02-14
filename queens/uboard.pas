unit uBoard;

{$mode objfpc}// so we can use operator in TObject = class

interface

uses
  Classes, SysUtils, ExtCtrls, Graphics, Math;

const
  emptyChip: int8 = 0;
  MagicNum = 3141516;

type

  TChip = int8;
  TChipBoard = array of TChip;
  PTChipBoard = ^TChipBoard;
  { TBoard }

  TBoard = class

    constructor Create(_n: integer); overload;
    constructor Create(const b: TBoard); overload;
  public
    magic: integer;
    n: integer;
    board: TChipBoard;

    ld, rd, cl: array of boolean;

    procedure Paint(const pb: TPaintBox); overload;
    procedure Paint(const cnv: TCanvas); overload;

    procedure rnd;
    function btoString: string;

    procedure MoveChip(col, val: integer); inline;
    procedure UnmoveChip(col, val: integer); inline;
    function isValidMove(col, val: integer): boolean;
    procedure setZero;
    function validated: boolean;
    function toString(b: TChipBoard): string; overload;

    function equal(const a, b: TChipBoard): boolean;
  public

  end;

  PTBoard = ^TBoard;

operator <(const a, b: TBoard): boolean;

implementation

operator <(const a, b: TBoard): boolean;
var
  i: integer;
begin
  Result := False;
  for i := low(a.board) to high(a.board) do
    if a.board[i] >= b.board[i] then exit; // false
  Result := True;
end;

{ TBoard }

constructor TBoard.Create(_n: integer);
begin
  n := _n;

  setLength(ld, n * 2); // assume all false
  setLength(rd, n * 2);
  setLength(cl, n * 2);

  setLength(board, n);
  setZero;

  magic := MagicNum;
end;

constructor TBoard.Create(const b: TBoard);
begin
  n := b.n;
  board := copy(b.board);

  ld := copy(b.ld);
  rd := copy(b.rd);
  cl := copy(b.cl);

  magic := MagicNum;
end;


procedure TBoard.Paint(const pb: TPaintBox);
begin
  Paint(pb.canvas);
end;


procedure TBoard.Paint(const cnv: TCanvas);
var
  i, j, w, h: integer;
  dx, dxs, offx, offy: double;
begin
  w := cnv.ClipRect.Width; // get canvas geo
  h := cnv.ClipRect.Height;

  if w * h > 0 then
    with cnv do
    begin
      dx := min(w / n, h / n);
      dxs := dx * 0.09; {scaled dx}

      offx := (w - dx * n) / 2;  { center in box }
      offy := (h - dx * n) / 2;

      pen.color := clBlue; {grid}
      brush.color := clNone;
      pen.Width := 3;
      rectangle(round(offx), round(offy), round(n * dx + offx), round(n * dx));

      pen.Width := 1;
      for i := 0 to pred(n) do
      begin
        line(round(i * dx + offx), round(offy), round(i * dx + offx), round(n * dx));
        line(round(offx), round(i * dx), round(n * dx + offx), round(i * dx));
      end;


      pen.Width := 1;    { checkers }
      for i := 0 to pred(n) do for j := 0 to pred(n) do
        begin
          if (i and 1) = (j and 1) then // simplified by chatGPT
            brush.Color := clNone
          else
            brush.Color := clLtGray;

          rectangle(round(offx + j * dx), round(offy + i * dx),
            round(offx + (j + 1) * dx), round(offy + (i + 1) * dx));
        end;


      Brush.Color := clYellow; { queens / board }
      pen.color := clRed;
      pen.Width := 7;

      for i := 0 to pred(n) do
      begin
        begin { inv top/down }
          j := n - board[i] - 1;
          Ellipse(
            round(i * dx + offx + dxs), round(j * dx + offy + dxs),
            round(i * dx + offx + dx - dxs), round(offy + dx + j * dx - dxs)
            );
        end;
      end;
    end;
end;


procedure TBoard.rnd;
var
  i: integer;
begin
  for i := 0 to pred(n) do
    board[i] := random(n);
end;

function TBoard.btoString: string;
var
  b: TChip;
begin
  Result := '';
  for b in board do Result += IntToStr(b) + ' ';
end;

procedure TBoard.MoveChip(col, val: integer);
begin
  board[col] := val;

  ld[val - col + n - 1] := True;
  rd[val + col] := True;
  cl[val] := True;
end;

procedure TBoard.UnmoveChip(col, val: integer);
begin
  board[col] := emptyChip;

  ld[val - col + n - 1] := False;
  rd[val + col] := False;
  cl[val] := False;
end;

function TBoard.isValidMove(col, val: integer): boolean;
begin
  Result := not (ld[val - col + n - 1] or rd[val + col] or cl[val]);
end;

procedure TBoard.setZero;
var
  i: integer;
begin
  for i := 0 to pred(n) do
    board[i] := 0;
  for i := 0 to pred(n * 2) do
  begin
    ld[i] := False;
    rd[i] := False;
    cl[i] := False;
  end;
end;

function TBoard.validated: boolean;
begin
  Result := magic = MagicNum;
end;

function TBoard.toString(b: TChipBoard): string;
var
  c: TChip;
begin
  Result := '';
  for c in b do Result += IntToStr(c) + ' ';
end;

function TBoard.equal(const a, b: TChipBoard): boolean;
var
  i: integer;
begin
  if length(a) <> length(b) then exit(False);
  for i := low(a) to high(a) do
    if a[i] <> b[i] then exit(False);
  Result := True;
end;


end.
