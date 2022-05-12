unit colorMap;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, gcontnrs, common;

type
  TColorMap = class(specialize TGenHashMap<integer, TPoint3d>)
    function DefaultHashKey(const Key: integer): integer; override;
    function DefaultKeysEqual(const A, B: integer): boolean; override;
  end;

implementation

function TColorMap.DefaultHashKey(const Key: integer): integer;
begin
  Result := Key;
  if Odd(Result) then
    Result := Result * 3;
end;

function TColorMap.DefaultKeysEqual(const A, B: integer): boolean;
begin
  Result := A = B;
end;

end.
