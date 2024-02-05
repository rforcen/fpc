unit uStack;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

const
  MAX_STACK = 1024;

type

  { TStack }

  TStack<T> = object
    st: array[0..MAX_STACK] of T;
    sp: integer;

    procedure push(o: T);
    function pop: T;
    procedure Clear;
  end;

implementation

{ TStack }

procedure TStack<T>.push(o: T);
begin
  if sp < MAX_STACK then
  begin
    st[sp] := o;
    Inc(sp);
  end;
end;

function TStack<T>.pop: TObject;
begin
  if sp > 0 then
  begin
    Result := st[sp];
    Dec(sp);
  end
  else
    Result := nil;
end;

procedure TStack<T>.Clear;
var
  i: integer;
begin
  for i := low(st) to high(st) do st[i] := nil;
  sp := 0;
end;


end.
