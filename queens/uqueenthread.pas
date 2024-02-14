unit uQueenThread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, uQueen;

type

  { TQueenThread  -  recursive thread support }

  TQueenThread = class(TThread)
    constructor Create(const _q: TQueen); overload; // root call
    constructor Create(const _q: TQueen; col: integer); overload;
  protected
    procedure Execute; override;
  private
    procedure findSolutions(col: integer);
  private
    q: TQueen; static; // solution accumulated queen holds fLock semaphore
    cq: TQueen; // instance queen
    column: integer;
    nth: integer; static;
  end;

  TQueenThHelper = class helper for TQueen
    procedure findSolutionsRecMT;
  end;

implementation

{ TQuennThread }

constructor TQueenThread.Create(const _q: TQueen);
begin
  q := _q;

  cq := TQueen.Create(_q);

  column := 0;
  nth := 0;
  FreeOnTerminate := True;
  inherited Create(True);
end;

constructor TQueenThread.Create(const _q: TQueen; col: integer);
begin
  cq := _q;
  column := col;
  Inc(nth);

  FreeOnTerminate := True;
  inherited Create(True);
end;

procedure TQueenThread.Execute;
begin
  Self.findSolutions(column);

  EnterCriticalSection(q.fLock);

  Dec(nth);

  LeaveCriticalSection(q.fLock);

  cq.Free;
end;

procedure TQueenThread.findSolutions(col: integer);
var
  i: integer;
begin
  if q.running then
    with cq do
    begin
      if col >= n then
      begin
        q.saveSolution(cq.board);

        if q.countSolutions >= q.maxSolutions then
          q.running := False; // stop all
      end
      else
      begin
        for i := 0 to pred(n) do
        begin
          if not q.running then break;

          if isValidMove(col, i) then
          begin
            MoveChip(col, i);

            if nth < min(n, GetCPUCount - 2) then
              TQueenThread.Create(TQueen.Create(cq), col + 1).Start
            // must work on a new TQueen instance
            else
              Self.findSolutions(col + 1);

            UnmoveChip(col, i);
          end;
          countEvals += n; // cs
        end;
      end;
    end;
end;

procedure TQueenThHelper.findSolutionsRecMT;    // slow & unstable
var
  qth: TQueenThread;
begin
  InitCriticalSection(fLock);

  runMode := tmRecTM;

  qth := TQueenThread.Create(self);
  qth.Start;

  while qth.q.running do sleep(5); // stop all
  sleep(100);

  solutions := qth.q.solutions;
  uniqueSolutions;
  countSolutions := length(solutions);

  DoneCriticalSection(fLock);
end;

end.
