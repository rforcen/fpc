unit board;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Graphics, Math;

type

  { TBoard }

  TBoard = record

  public
  const
    ROWS = 6;
    COLS = 7;
    inLineNum = 4; // 4 in line
    TOTALCHIPS = ROWS * COLS;

    MaxVal = 128;
    DrawVal = -127;

    maxInt8: int8 = $7e;

  type
    TChip = (chEmpty = 0, chBlack = 1, chWhite = 2, chFull = 3 {b11});
    TPlayer = (plHuman, plMachine); // player type

    TheBoard = packed record // a 7x6 board is packed in 2 bit x 42 = 84bit struct
      b0, b1, b2: uint32; // 3 x 4 = 12bytes = 96 bits < 84 required
    end;

    TArrCols = array[0..COLS - 1] of byte;
    TMoves = type TArrCols;
    TArrBytes = array of byte;

  const
    allChips = [chBlack, chWhite];

  private
    b: TheBoard; // 4 x 3 = 12 bytes
    nChips: TArrCols; // nchips in a col
    solutions: array [chBlack..chWhite] of array of TheBoard;
    lastR, lastC, lastIx: byte; // last index used
    lastChip: TChip;

    bestVal: integer;
    bestMove: byte;
    bestChip: TChip;

    xLinePos: array of integer; // position of vertical lines
    level: integer;

  public
    procedure init;
    procedure Clear;
    procedure moveChip(chip: TChip; r, c: byte); overload;
    procedure moveChip(chip: TChip; c: byte); overload;
    function moveChipGeo(chip: TChip; X: integer): boolean;

    procedure clearChip(r, c: byte); overload;
    procedure unMoveChip(c: byte); overload;
    procedure clearChip(const _moves: TMoves; c: byte); overload;

    function getChip(r, c: byte): TChip;
    function getChipByIndex(ix: byte): TChip;
    procedure Next(var r, c: byte);
    procedure prev(var r, c: byte);
    function isValid(r, c: byte): boolean;
    function toString(const brd: TheBoard): string; overload;
    function toString: string; overload;
    procedure copyBoard(_b: TheBoard);
    function isWinner(chip: TChip): boolean;
    function isDraw: boolean;
    function match(const brd: TheBoard): boolean;
    function genMoves(out _moves: TMoves): byte;
    function nMoves: byte;
    function play(chip: TChip; maxLevel: integer): integer;
    function evalWinner(chip: TChip; player: TPlayer): int8;
    function play02(chip: TChip): integer;
    function play01(chip: TChip): boolean;
    procedure strToBoard(_b: array of string);
    procedure moveBest;
    procedure initBest;
    function getBest: integer;
    function getBestMove: byte;
    function machineWins: boolean;
    function possibleMoves: integer;
  public
    procedure genSolutions;
    function rc2index(r, c: byte): byte; inline;
    function chip2Str(chip: TChip): char;
    function char2Chip(ch: char): TChip;
    function switchChip(chip: TChip): TChip;
    function switchPlayer(player: TPlayer): TPlayer;
    function evaluateBoard: integer;
    procedure setBestMove(chip: TChip; mv: byte; val: integer);
    procedure setMove(chip: TChip; col: byte; val: integer);

    // drawing
    procedure Paint(const pb: TPaintBox); overload;
    procedure Paint(const cnv: TCanvas); overload;
    procedure testBoard;
  public
    countEval: integer;
  end;

implementation

{ TBoard }

procedure TBoard.init;
begin
  Clear;
  genSolutions;
  Clear;

  initBest;
end;

procedure TBoard.Clear;
begin
  b.b0 := 0;
  b.b1 := 0;
  b.b2 := 0;

  fillChar(nChips, sizeof(nChips), 0);
end;

procedure TBoard.moveChip(chip: TChip; r, c: byte);
begin
  lastIx := {%H-}rc2index(r, c);
  lastChip := chip;
  lastR := r;
  lastC := c;

  case lastIx of
    00..30: b.b0 := (b.b0 and (uint32(3 << lastIx) xor $ffffffff)) or
        (uint32(chip) << lastIx);
    32..62: b.b1 := (b.b1 and (uint32(3 << lastIx) xor $ffffffff)) or
        (uint32(chip) << lastIx);
    64..84: b.b2 := (b.b2 and (uint32(3 << lastIx) xor $ffffffff)) or
        (uint32(chip) << lastIx);
  end;
end;

procedure TBoard.moveChip(chip: TChip; c: byte);   // based on moves
begin
  assert(nChips[c] < ROWS);

  moveChip(chip, nChips[c], c);
  Inc(nChips[c]);
end;

procedure TBoard.clearChip(r, c: byte); // moveChip(chEmpty,r,c);
var
  ix: byte;
begin
  ix := {%H-}rc2index(r, c);

  case ix of
    00..30: b.b0 := (b.b0 and (uint32(3 << ix) xor $ffffffff));
    32..62: b.b1 := (b.b1 and (uint32(3 << ix) xor $ffffffff));
    64..84: b.b2 := (b.b2 and (uint32(3 << ix) xor $ffffffff));
  end;
end;

procedure TBoard.unMoveChip(c: byte); // based on moves
begin
  Dec(nChips[c]);
  moveChip(chEmpty, nChips[c], c);
end;

procedure TBoard.clearChip(const _moves: TMoves; c: byte);
begin
  c := _moves[c];
  Dec(nChips[c]);
  moveChip(chEmpty, nChips[c], c);
end;


function TBoard.getChip(r, c: byte): TChip;
var
  ix: byte;
begin
  ix := {%H-}rc2index(r, c);

  case ix of
    00..30: Result := TChip((b.b0 and (uint32(3 << ix)) >> ix) and 3);
    32..62: Result := TChip((b.b1 and (uint32(3 << ix)) >> ix) and 3);
    64..84: Result := TChip((b.b2 and (uint32(3 << ix)) >> ix) and 3);
    else
      Result := chEmpty;
  end;
end;

function TBoard.getChipByIndex(ix: byte): TChip;
begin
  case ix of
    00..30: Result := TChip((b.b0 and (uint32(3 << ix)) >> ix) and 3);
    32..62: Result := TChip((b.b1 and (uint32(3 << ix)) >> ix) and 3);
    64..84: Result := TChip((b.b2 and (uint32(3 << ix)) >> ix) and 3);
    else
      Result := chEmpty;
  end;
end;

procedure TBoard.Next(var r, c: byte);
begin
  Inc(c);
  if c >= COLS then
  begin
    c := 0;
    Inc(r);
    if r >= ROWS then r := 0;
  end;
end;

procedure TBoard.prev(var r, c: byte);
begin
  if c > 0 then Dec(c)
  else
  begin
    if r > 0 then
    begin
      Dec(r);
      c := pred(COLS);
    end
    else
    begin
      r := pred(ROWS);
      c := pred(COLS);
    end;
  end;
end;

function TBoard.isValid(r, c: byte): boolean;
begin
  Result := (r < ROWS) and (c < COLS);
end;

function TBoard.toString(const brd: TheBoard): string;
var
  r, c: byte;
  saveBrd: TheBoard;
begin
  Result := '';
  saveBrd := b; // save /restore board

  b := brd;
  for r := pred(ROWS) downto 0 do
  begin
    Result += IntToStr(r) + ':';
    for c := 0 to pred(COLS) do Result += chip2Str(getChip(r, c));
    Result += #$0a;
  end;

  b := saveBrd;
end;

function TBoard.toString: string;
begin
  Result := toString(b);
end;

procedure TBoard.copyBoard(_b: TheBoard);
begin
  b := _b;
end;

function TBoard.isWinner(chip: TChip): boolean;
var
  brd: TheBoard;
begin
  for brd in solutions[chip] do
    if match(brd) then
      exit(True);
  Result := False;
end;

function TBoard.isDraw: boolean;
var
  moves: TMoves;
begin
  Result := genMoves(moves) = 0;
end;

function TBoard.match(const brd: TheBoard): boolean;   // brd is a solution chFull b(11)
begin
  if b.b0 = 0 then Result := brd.b0 = 0
  else
    Result := ((b.b0 and brd.b0) = brd.b0);
  if b.b1 = 0 then Result := Result and (brd.b1 = 0)
  else
    Result := Result and ((b.b1 and brd.b1) = brd.b1);
  if b.b2 = 0 then Result := Result and (brd.b2 = 0)
  else
    Result := Result and ((b.b2 and brd.b2) = brd.b2);
end;

function TBoard.genMoves(out _moves: TMoves): byte;
const
  centerMoves: array [0..COLS - 1] of byte = (3, 4, 2, 5, 1, 6, 0);
var
  cm: byte;
begin
  Result := 0;

  for cm in centerMoves do
    if nChips[cm] < ROWS then
    begin
      _moves[Result] := cm;
      Inc(Result);
    end;
end;

function TBoard.nMoves: byte;
var
  i: byte;
begin
  Result := 0;
  for i in nChips do
    if i < ROWS then Inc(Result);
end;

{
 end node eval,
 follows a radical win or nothing approach this allows full prunning
}

function TBoard.evalWinner(chip: TChip; player: TPlayer): int8;
begin

  if player = plHuman then chip := switchChip(chip);
  // board is evaluated from machine interest

  if isWinner(switchChip(chip)) then Result := -1  // opponent wins? -> -1
  else if isWinner(chip) then Result := +1 // if not i win? -> +1
  else
    Result := 0; // still no winner found

  Inc(countEval, 1);
end;

{ plays quite well at level 5 }
function TBoard.play(chip: TChip; maxLevel: integer): integer;

  function _play(chip: TChip; player: TPlayer;
    level, maxLevel, minResult, maxResult: int8): int8;
  var
    nmoves, m: byte;
    moves: TMoves;
  begin
    Result := 0;

    if level = maxLevel then
      Result := evalWinner(chip, player)
    else
    begin
      nmoves := genMoves(moves);
      if nmoves = 0 then Result := evalWinner(chip, player)
      else
      begin
        case player of
          plMachine: begin // machine eval -> max
            for m := 0 to pred(nmoves) do
            begin
              moveChip(chip, moves[m]);

              Result := _play(switchChip(chip), switchPlayer(player),
                level + 1, maxLevel, minResult, maxResult);

              if Result > maxResult then
              begin
                maxResult := Result;
                if level = 0 then setMove(chip, moves[m], Result);
              end;

              unMoveChip(moves[m]);

              if (maxResult = 1) or (minResult <= maxResult) then break; // alpha prune
            end;
            Result := maxResult;
          end;
          plHuman: begin // human eval -> min
            for m := 0 to pred(nmoves) do
            begin
              moveChip(chip, moves[m]);

              Result := _play(switchChip(chip), switchPlayer(player),
                level + 1, maxLevel, minResult, maxResult);

              minResult := min(minResult, Result);

              unMoveChip(moves[m]);

              if (minResult = -1) or (minResult <= maxResult) then break; // beta prune
            end;
            Result := minResult;
          end;
        end;
      end;
    end;
  end;

begin
  initBest;
  Result := 0;
  level := maxLevel;

  if nMoves = 0 then Result := DrawVal
  else
  if not play01(chip) then  // winning move?
    Result := _play(chip, plMachine, 0, maxLevel, +maxInt8, -maxInt8);
  // play even levels
end;

function TBoard.play02(chip: TChip): integer;     // 2 level play w/beta prune
var
  mm, mh, nmMachine, nmHuman, valHuman: integer;
  mvMachine, mvHuman: TMoves;
begin
  initBest;

  nmMachine := genMoves(mvMachine);
  Result := -maxInt;

  if nmMachine = 0 then Result := DrawVal
  else
    for mm := 0 to pred(nmMachine) do // find max of this level
    begin
      moveChip(chip, mvMachine[mm]);

      // valHuman -> min of human moves
      nmHuman := genMoves(mvHuman);
      valHuman := maxInt;

      for mh := 0 to pred(nmHuman) do
      begin
        moveChip(switchChip(chip), mvHuman[mh]);

        valHuman := min(-evaluateBoard, valHuman);
        // best for human is worst for machine

        unMoveChip(mvHuman[mh]);

        if valHuman <= Result then break; // beta prune
      end;

      // max of min's
      if valHuman > Result then
      begin
        Result := valHuman;
        setMove(chip, mvMachine[mm], valHuman);
      end;

      unMoveChip(mvMachine[mm]);
    end;
end;

function TBoard.play01(chip: TChip): boolean; // 1 level play
var
  m, nm: integer;
  moves: TMoves;
begin
  nm := genMoves(moves);
  Result := False;

  for m := 0 to pred(nm) do // find winner
  begin
    moveChip(chip, moves[m]);
    Result := isWinner(chip);
    unMoveChip(moves[m]);

    if Result then // winner
    begin
      setMove(chip, moves[m], 1);
      break;
    end;
  end;
end;

procedure TBoard.strToBoard(_b: array of string);
var
  r, c: byte;
begin
  assert(length(_b) = ROWS);

  Clear;
  for r := 0 to pred(ROWS) do
    for c := 0 to pred(COLS) do
      moveChip(char2Chip(_b[ROWS - r - 1][c + 1]), r, c);
  // update nChips
end;

procedure TBoard.moveBest;
begin
  moveChip(bestChip, bestMove);
end;

procedure TBoard.initBest;
begin
  bestVal := -MaxVal;
  bestMove := COLS + 1;
  bestChip := chWhite;

  countEval := 0;
end;

function TBoard.getBest: integer;
begin
  Result := bestVal;
end;

function TBoard.getBestMove: byte;
begin
  Result := bestMove;
end;

function TBoard.machineWins: boolean;
begin
  Result := bestVal = MaxVal;
end;

function TBoard.possibleMoves: integer;
begin
  Result := round(power(COLS, level));
end;

procedure TBoard.genSolutions;
var
  r, c, i: byte;
  chip: TChip;
begin

  for chip in [chBlack, chWhite] do
  begin
    solutions[chip] := nil;

    // horizontals
    for r := 0 to pred(ROWS) do
    begin
      for c := 0 to COLS - inLineNum do
      begin
        Clear;
        for i in [0..inLineNum - 1] do moveChip(chip, r, c + i);
        solutions[chip] += [b];
      end;
    end;
    // verticals
    for c := 0 to pred(COLS) do
    begin
      for r := 0 to ROWS - inLineNum do
      begin
        Clear;
        for i in [0..inLineNum - 1] do moveChip(chip, r + i, c);
        solutions[chip] += [b];
      end;
    end;
    // diagonal right '/'
    for r := 0 to ROWS - inLineNum do
    begin
      for c := 0 to COLS - inLineNum do
      begin
        Clear;
        for i in [0..inLineNum - 1] do moveChip(chip, r + i, c + i);
        solutions[chip] += [b];
      end;
    end;
    // diagonal left '\'
    for r := 0 to ROWS - inLineNum do
    begin
      for c := pred(COLS) downto inLineNum - 1 do
      begin
        Clear;
        for i in [0..inLineNum - 1] do moveChip(chip, r + i, c - i);
        solutions[chip] += [b];
      end;
    end;
  end;
end;

function TBoard.rc2index(r, c: byte): byte;
begin
  Result := r * COLS + c;
  assert(Result < TOTALCHIPS, 'board index out of range');
  Result := Result shl 1; // allways even
end;

function TBoard.chip2Str(chip: TChip): char;
begin
  case chip of
    chBlack: Result := 'O';
    chWhite: Result := 'W';
    chEmpty: Result := '_';
    chFull: Result := 'X';
    else
      Result := #0;
  end;
end;

function TBoard.char2Chip(ch: char): TChip;
  {%H-}begin
  case ch of
    'O': Result := chBlack;
    'W': Result := chWhite;
    '_': Result := chEmpty;
    else
      assert(False, 'bad chip');
  end;
end;

function TBoard.switchChip(chip: TChip): TChip;
begin
  if chip = chWhite then Result := chBlack
  else
  if chip = chBlack then Result := chWhite
  else
    Result := chip;
end;

function TBoard.switchPlayer(player: TPlayer): TPlayer;
  {%H-}begin
  case player of
    plHuman: Result := plMachine;
    plMachine: Result := plHuman;
  end;
end;

function TBoard.evaluateBoard: integer;
var
  r, c, cnt: int8;

  function cnt2Points: integer;
  begin
    case cnt of
      0: Result := 0;
      1: Result := 2;
      2: Result := 16;
      3: Result := MaxVal;
      else
        Result := MaxVal;
    end;
  end;

begin
  // stating from 'lastIx' & 'lastChip' find downwards
  Inc(countEval);

  Result := 0;

  // vert
  cnt := 0;
  r := lastR - 1;
  while (r >= 0) and (lastChip = getChip(r, lastC)) do
  begin
    Inc(cnt);
    Dec(r);
  end;

  Result += cnt2Points;
  if Result >= maxVal then exit(maxVal);

  // horz. left -> same row
  cnt := 0;
  c := lastC - 1;
  while (c >= 0) and (lastChip = getChip(lastR, c)) do
  begin
    Inc(cnt);
    Dec(c);
  end;
  // horz rigth
  c := lastC + 1;
  while (c < COLS) and (lastChip = getChip(lastR, c)) do
  begin
    Inc(cnt);
    Inc(c);
  end;
  Result += cnt2Points; // we've counted lastR, lastC twice
  if Result >= maxVal then exit(maxVal);

  // diag /
  cnt := 0;
  r := lastR - 1;
  c := lastC - 1;
  while (c > 0) and (r >= 0) and (lastChip = getChip(r, c)) do
  begin
    Inc(cnt);
    Dec(r);
    Dec(c);
  end;
  Result += cnt2Points;
  if Result >= maxVal then exit(maxVal);

  // diag \
  cnt := 0;
  r := lastR - 1;
  c := lastC + 1;
  while (c < COLS) and (r >= 0) and (lastChip = getChip(r, c)) do
  begin
    Inc(cnt);
    Dec(r);
    Inc(c);
  end;
  Result += cnt2Points;
  if Result >= maxVal then exit(maxVal);
end;

procedure TBoard.setBestMove(chip: TChip; mv: byte; val: integer);
begin
  if val > bestVal then
    setMove(chip, mv, val);
end;

procedure TBoard.setMove(chip: TChip; col: byte; val: integer);
begin
  bestVal := val;
  bestMove := col;
  bestChip := chip;
end;

{ paint }
procedure TBoard.Paint(const pb: TPaintBox);
begin
  Paint(pb.Canvas);
end;

procedure TBoard.Paint(const cnv: TCanvas);
const
  margin = 60;
var
  r, c, w, h: integer;
  dx, dxs, offx, offy: double;
begin
  w := cnv.ClipRect.Width; // get canvas geo
  h := cnv.ClipRect.Height;

  xLinePos := nil;

  if w * h > 0 then
    with cnv do
    begin
      pen.color := clNone;
      brush.color := clNone;
      rectangle(0, 0, w, h);

      w -= margin;
      h -= margin;

      dx := min(w / COLS, h / ROWS);

      dxs := dx * 0.09; {scaled dx}

      offx := (w - dx * COLS) / 2 + margin / 2;  { center in box }
      offy := (h - dx * ROWS) / 2 + margin / 2;

      pen.color := clBlue; {grid}
      brush.color := TColor($e0e0e0);
      pen.Width := 3;
      rectangle(round(offx), round(offy), round(COLS * dx + offx),
        round(ROWS * dx + offy));

      pen.Width := 1;
      for r := 0 to pred(COLS) do
      begin // vert lines
        line(round(r * dx + offx), round(offy), round(r * dx + offx),
          round(offy + ROWS * dx));
        xLinePos += [round(r * dx + offx)]; // save x for moveGeo
      end;

      for r := 0 to pred(ROWS) do  // horz lines
        line(round(offx), round(offy + r * dx), round(COLS * dx + offx),
          round(offy + r * dx));

      // chips
      for r := 0 to pred(ROWS) do
        for c := 0 to pred(COLS) do
        begin { inv top/down }
          case getChip(r, c) of
            chBlack:
            begin
              Brush.Color := clBlack;
              pen.color := clRed;
            end;
            chWhite:
            begin
              Brush.Color := clYellow;
              pen.color := clRed;
            end;
            else
              continue;
          end;

          Ellipse(
            round(c * dx + offx + dxs), round((ROWS - 1 - r) * dx + offy + dxs),
            round(c * dx + offx + dx - dxs), round(offy + dx + (ROWS - 1 - r) * dx - dxs)
            );
        end;
    end;
end;

function TBoard.moveChipGeo(chip: TChip; X: integer): boolean;
var
  c: integer;
begin

  Result := True;
  for c := high(xLinePos) downto 0 do
    if X > xLinePos[c] then
    begin
      if nChips[c] < ROWS then
        moveChip(chip, c)
      else
        Result := False;
      break;
    end;
end;

procedure TBoard.testBoard;
var
  r, c: integer;
  chip: TChip = chBlack;
begin
  init;

  for r := 0 to pred(ROWS) do
    for c := 0 to pred(COLS) do
    begin
      moveChip(chip, c);
      chip := switchChip(chip);
    end;
end;

end.
