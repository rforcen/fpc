// eXpanded fgl list

unit uListX;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}
interface

uses
  Classes, SysUtils, fgl;

type

  generic TListX<T> = class(specialize TFPGList<T>)
  public
    procedure shrinkList(size: integer);
    procedure deduplicate;
    function binaryFind(const x: T; cmp: TCompareFunc): T;
    function lowerBound(const x: T; cmp: TCompareFunc): T;
    procedure sortUnique(cmp: TCompareFunc);
    function _high: integer;
  end;

implementation

procedure TListX.shrinkList(size: integer);
var
  vv: array of T = nil;
  i: integer;
begin
  // v <- vv
  setLength(vv, size);
  for i := 0 to high(vv) do vv[i] := self[i];
  Free;
  self := TListX.Create;
  self.Capacity := length(vv);
  for i := 0 to high(vv) do add(vv[i]);
end;

procedure TListX.deduplicate;

var
  i, j, n: integer;
  hasDupes: boolean = False;

begin
  i := 0;
  n := self.Count;

  for j := 1 to n - 1 do
  begin
    if self[j] <> self[i] then
    begin
      Inc(i);
      self[i] := self[j];
    end
    else
      hasDupes := True;
  end;

  if hasDupes then shrinkList(i + 1);
end;


function TListX.binaryFind(const x: T; cmp: TCompareFunc): T;
var
  r, l, mid: integer;
begin
  l := 0;
  r := Count - 1;

  while r >= l do
  begin
    mid := (r + l) div 2;
    if cmp(self[mid], x) = 0 then exit(self[mid]);
    if cmp(self[mid], x) > 0 then
      r := mid - 1
    else
      l := mid + 1;
  end;
  Result := Default(T);
end;

function TListX.lowerBound(const x: T; cmp: TCompareFunc): T;
var
  r, l, mid: integer;
begin
  l := 0;
  r := Count - 1;

  while r >= l do
  begin
    mid := (r + l) div 2;
    if cmp(self[mid], x) > 0 then r := mid - 1
    else
      l := mid + 1;
  end;
  if r < 0 then r := 0;
  Result := self[r]; // lower bound
end;


procedure TListX.sortUnique(cmp: TCompareFunc);
begin
  sort(cmp);
  deduplicate;
end;


function TListX._high: integer;
begin
  Result := Count - 1;
end;

end.
