unit uQueen;

{$mode delphi}

interface

uses
  Classes, SysUtils, MTProcs, Math, Forms, uBoard;

type

  { TQueen }

  TQueen = class(TBoard)
  type TThreadMode = (tmSingle, tmMulti, tmRecTM);

    constructor Create(_n: integer; _maxSols: integer = 1); overload;
    constructor Create(const q: TQueen); overload;

    destructor Destroy; override;
  public
    countEvals: double; static;
    solutions: array of TChipBoard;
    running: boolean; static;
    maxSolutions: integer; static;
    countSolutions: integer; static;

    function isRunning: boolean;
    function isValid: boolean; overload;
    function isValid(const b: TChipBoard): boolean; overload;

    procedure saveSolution; overload;
    procedure saveSolution(const b: TChipBoard); overload;
    procedure setSolution(ns: integer);
    function nSolutions: integer; inline;
    procedure setThMode(_runMode: TThreadMode); overload;
    procedure setThMode(_runMode: integer); overload;

    procedure setMaxSolutions(ms: integer);

    procedure findSolutions(col: integer = 0);
    procedure findSolutionsMT;
    //procedure findSolutionsRecMT;

    procedure ClearSolutions;
    procedure transformations;
    procedure uniqueSolutions;
    function fmtEvalSuffix: string; {show resull in K,M,G,T units}
    function solutionsAsTExt: string;
  public
    procedure setAsyncCall(__asyncCall: TDataEvent);
  private
    procedure asyncCall(Data: PtrInt);

  private
    runMode: TThreadMode; static;
    fLock: TRTLCriticalSection; static;
    _asyncCall: TDataEvent; static;
    cnt: integer; static; // counter
  end;

  PTQueen = ^TQueen;

  ATQueen = array of TQueen;
  PATQueen = ^ATQueen;

implementation

{ TQueen }


constructor TQueen.Create(_n: integer; _maxSols: integer);
begin
  inherited Create(_n); // TBoard

  n := _n;

  running := True;
  countEvals := 0;
  solutions := nil;
  countSolutions := 0;

  maxSolutions := _maxSols;
end;

constructor TQueen.Create(const q: TQueen);
begin
  inherited Create;

  running := True;
  countEvals := 0;
  solutions := nil;
  maxSolutions := q.maxSolutions;
  countSolutions := 0;

  n := q.n; // copy inherited TBoard TBoard.Create(TBoard(self));
  board := copy(q.board);
  ld := copy(q.ld);
  rd := copy(q.rd);
  cl := copy(q.cl);

  magic := q.magic;
end;


destructor TQueen.Destroy;
begin
  inherited Destroy;
end;

function TQueen.isRunning: boolean;
begin
  Result := running;
end;

function TQueen.isValid: boolean;
begin
  Result := isValid(board);
end;

function TQueen.isValid(const b: TChipBoard): boolean;
var
  i, j: integer;
begin
  for i := 0 to pred(n - 1) do
    for j := i + 1 to pred(n) do
    begin
      if b[i] = b[j] then exit(False); // horizontal -> bi=bj
      if (i - b[i]) = (j - b[j]) then exit(False); // vertical  / i-bi = j-bj
      if abs(b[i] - b[j]) = abs(i - j) then exit(False); // vertical \ |bi-bj| = |i-j|
    end;
  Result := True;
end;


procedure TQueen.saveSolution;
begin
  if isValid then
    saveSolution(board);
end;

procedure TQueen.saveSolution(const b: TChipBoard);

  procedure addSolution;
  begin
    solutions += [copy(b)]; // cs!
    Inc(countSolutions);
  end;

begin
  if running and (runMode <> tmSingle) then
  begin
    EnterCriticalSection(fLock);
    addSolution;
    LeaveCriticalSection(fLock);
  end
  else
    addSolution;
end;

procedure TQueen.setSolution(ns: integer);
begin
  if (ns >= 0) and (ns < length(solutions)) then board := solutions[ns];
end;

function TQueen.nSolutions: integer;
begin
  Result := countSolutions;
end;

procedure TQueen.setThMode(_runMode: TThreadMode);
begin
  runMode := _runMode;
end;

procedure TQueen.setThMode(_runMode: integer);
begin
  runMode := TThreadMode(_runMode);
end;

procedure TQueen.setMaxSolutions(ms: integer);
begin
  maxSolutions := ms;
end;

procedure TQueen.findSolutions(col: integer);
var
  row: integer;
begin
  if running then
  begin
    if col >= n then
    begin
      saveSolution;
      if countSolutions >= maxSolutions then
        running := False; // stop all
    end
    else
    begin
      for row := 0 to pred(n) do
      begin
        if running and isValidMove(col, row) then
        begin
          MoveChip(col, row);
          findSolutions(col + 1);
          UnmoveChip(col, row);
        end;
      end;
      countEvals += n; // cs
      Inc(cnt);

      // update evals
      if running and (runMode <> tmSingle) then
        if cnt >= 5000000 then
        begin
          Application.QueueAsyncCall(asyncCall, {%H-}PtrInt(self));
          cnt := 0;
        end;
    end;
  end;
end;

procedure TQueen.findSolutionsMT;  // thread pool version

  procedure _scanFirst(ixq: PtrInt; pq: PATQueen; {%H-}Item: TMultiThreadProcItem);
  begin
    pq[ixq].findSolutions(2);
  end;

var
  qs: ATQueen = nil;
  q: TQueen;
  i, nth, row: integer;
begin
  nth := min(n, GetCPUCount);
  setLength(qs, nth);  // create nth copies of self
  setZero; // board:=0

  for i := low(qs) to high(qs) do
  begin
    qs[i] := TQueen.Create(self);

    qs[i].MoveChip(0, i); // board[0]=i

    row := ((n div 2) + i + 1) mod n;  // board[1]=beyond n/2
    if isValidMove(1, row) then
      qs[i].MoveChip(1, row);
  end;

  self.setThMode(tmMulti);

  InitCriticalSection(fLock); // cs

  ProcThreadPool.DoParallelLocalProc(@_scanFirst, 0, pred(nth), @qs);

  DoneCriticalSection(fLock);

  ClearSolutions; // agregate qs[].solutions -> solutions
  for q in qs do solutions += q.solutions;
  uniqueSolutions;
  countSolutions := length(solutions);

  for q in qs do q.Free; // release qs

  Application.QueueAsyncCall(asyncCall, 0); // send the final message -> update
end;

procedure TQueen.ClearSolutions;
begin
  solutions := nil;
  countSolutions := 0;
end;

procedure TQueen.transformations;

// transformations
  procedure translate_vert; // up
  var
    i: int8;
  begin
    for i := low(board) to high(board) do
      MoveChip(i, (board[i] + 1) mod n);
    saveSolution;
  end;


  procedure translate_horz;
  var
    i: integer; // right
    v: TChipBoard = nil;
  begin
    setLength(v, n);
    for i := 0 to pred(n - 1) do
      v[i + 1] := board[i];
    v[0] := board[n - 1];
    if isValid(v) then saveSolution(v);
  end;


  procedure rotate90;
  var
    i, j: integer;
    rot_queens: TChipBoard = nil;
  begin

    setLength(rot_queens, n);
    for i := 0 to pred(n) do
    begin
      rot_queens[i] := 0;
      for j := 0 to pred(n) do // find i
        if board[j] = i then
        begin
          rot_queens[i] := n - j - 1;
          break;
        end;
    end;
    if isValid(rot_queens) then saveSolution(rot_queens);
  end;


  procedure mirror_horz;
  var
    i: integer;
  begin
    for i := 0 to pred(n) do
      MoveChip(i, (n - 1) - board[i]);
    saveSolution;
  end;

  procedure mirror_vert;
  var
    i, tmp: integer;
  begin
    for i := 0 to n div 2 do
    begin
      tmp := board[i];
      MoveChip(i, board[n - 1 - i]);
      MoveChip(n - 1 - i, tmp);
    end;
    saveSolution;
  end;

var
  mv, mh, r90, tv, th: integer;
begin
  runMode := tmSingle;

  ClearSolutions;

  for mv := 0 to 1 do
  begin
    for mh := 0 to 1 do
    begin
      for r90 := 0 to 3 do
      begin
        for tv := 0 to pred(n) do
        begin
          for th := 0 to pred(n) do
            translate_vert;
          translate_horz;
        end;
        rotate90;
      end;
      mirror_horz;
    end;
    mirror_vert;
  end;


  uniqueSolutions;

  setSolution(0);
end;


procedure TQueen.uniqueSolutions;
var
  i, j: integer;
  usols: array of TChipBoard = nil;
begin
  if length(solutions) <= 1 then exit; // done

  for i := low(solutions) to high(solutions) do
  begin
    for j := low(usols) to high(usols) do // find in usols
      if equal(solutions[i], usols[j]) then break; // dupe

    if ({%H-}j >= high(usols)) and isValid(solutions[i]) then
      usols += [solutions[i]];
  end;

  solutions := usols;
  countSolutions := length(solutions);
end;

function TQueen.fmtEvalSuffix: string;
const
  suffix = ' KMGTP';
var
  m: double;
  i: integer = 1;
begin
  m := countEvals;
  while (m >= 1e3) and (i < high(suffix)) do
  begin
    m /= 1e3;
    Inc(i);
  end;
  Result := Format('%.1f%s', [m, suffix[i]]);
end;

function TQueen.solutionsAsTExt: string;
var
  b: TChipBoard;
begin
  Result := '';
  for b in solutions do Result += toString(b) + #13;
end;

procedure TQueen.setAsyncCall(__asyncCall: TDataEvent);
begin
  _asyncCall := __asyncCall;
end;

procedure TQueen.asyncCall(Data: PtrInt);
begin
  if @_asyncCall <> nil then _asyncCall(Data);
end;



end.
