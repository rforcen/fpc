{ Numpas - numpy inspired basic numerical multidimensional array
usage:

var a:TNumPas<double>
begin
     a:=TNumPas<double>.Create([3,3,3]); // 3 x 3 x 3 tensor
     a.arange(100);
end.

}
{$modeswitch nestedprocvars}

unit uNumPas;

{$mode delphi}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, Math, TypInfo, MTProcs, Randoms,
  Generics.Defaults, Generics.Collections, f128;

type

  { TIndexValue }
  TIndexValue<T> = record
    index: integer;
    Value: T;
  end;

  TArrInt = array of integer;
  TDataType = (dtUnknown, dtint, dtf32, dtf64, dtf128);

  { TNumPas }

  TNumPas<T> = record
  private
    {const  Eps = 1e-10;}
  type
    TArrArrInt = array of TArrInt;
    TArrT = array of T;
    PT = ^T;
    TFlatArrayHelper = class(TArrayHelper<T>);
    NP = TNumPas<T>;
    TNPFunction = function(v: T): T; //of object;

    { TValEnumerator }
    TValEnumerator = record
      function GetCurrent: T;
      function MoveNext: boolean;
      property Current: T read GetCurrent;
    private
      epdata: PT;
      en, eindex: integer;
    end;

  public  { -- constructors }
    constructor Create(const _dims: TArrInt); overload;
    constructor Create(const a: TNumPas<T>); overload;

    constructor rand(_dims: TArrInt); overload;
    constructor arange(nitems: integer); overload;
    constructor linspace(astart, aend: double; _nitems: integer); overload;
    constructor eye(nitems: integer); overload;
    constructor full(const _dims: TArrInt; avalue: T); overload;
    constructor ones(const _dims: TArrInt); overload;
    constructor zeros(const _dims: TArrInt); overload;
    constructor identity(n: integer); overload;
    constructor fromString(const s: string; typ: TDataType = dtf64); overload;
    constructor transpose(const a: TNumPas<T>);

  private
    fDims: TArrInt;
    fNitems, fNdims: integer;
    fMlt: TArrInt; // mlt factor for index calc
    fSizeBytes: integer;

    _typeInfo: PTypeInfo;
    fDataType: TDataType;

    fpint: pinteger;
    fpf32: psingle;
    fpf64: pdouble;
    fpf128: pfp128;
    fpdata: ^T;
    fData: array of T;


  private
    function rget(index: TArrInt): T; overload; inline;
    procedure rput(index: integer; Value: T); overload; inline;
    function rget(index: integer): T; overload; inline;
    procedure rput(index: TArrInt; AValue: T); overload; inline;
    function rget(r, c: integer): T; overload; inline;
    procedure rput(r, c: integer; AValue: T); overload; inline;

    function combinations(const inArr: TArrInt): TArrArrInt;
    function cmpData(constref a, b: T): integer;
    procedure swap(i, j, k, l: integer); inline;
    procedure swap(i, j: integer);
    function transposedCoord(c: integer): integer;
    function prod(a: TArrInt): integer;
  public
    function calcIndex(const index: TArrInt): integer; inline;
    procedure rangeSlice(const index: TArrInt; out aStart, aEnd: integer);
    function startSlice(const slc: TArrInt): integer;
    function endSlice(const slc: TArrInt): integer;
    function checkDims: boolean;

    {rand primitives}
    function TsRandInt(var seed: integer): integer; inline;
    function TsRandf32(var seed: integer): single; inline;
    function TsRandf64(var seed: integer): double; inline;
    { enumerator }
    function GetEnumerator: TValEnumerator;
  public
    property item[index: integer]: T read rget write rput; default;
    property m2item[r, c: integer]: T read rget write rput;
    property mitem[index: TArrInt]: T read rget write rput;

    function {%H-}toString: string; overload;
    function toPython(varName: string): string; overload;
    function slice(index: TArrInt): NP;
    function copy: NP; // deep copy
    procedure zero;
    procedure rand; overload; // fill w/random values
    procedure randMT;
    function hi(index: integer): integer;  // -1 to math for loops->high
    function dim(index: integer): integer;

    function size: integer;
    function Data(index: integer): T;
    function getData: TArrT;
    procedure setValue(v: T); overload;
    procedure setValue(const slc: TArrInt; v: T); overload;
    function sameDims(a: NP): boolean; // same fDims
    function sum: T;
    function max: T;
    function min: T;
    function mean: T;
    function shape: TArrInt;
    procedure Assign(const a: NP; const _dims: TArrInt);
    procedure toFile(Name: string);
    procedure fromFile(Name: string);
    function flatSort: NP;
    procedure _flatSort; // insite 1 x fNitems
    function sort: NP;
    function diagonal: NP;
    function dot1x1(a: NP): NP;
    function dot2x1(a: NP): NP;
    function dot1x2(a: NP): NP;
    function dotnx1(a: NP): NP;
    function dot2x2(a: NP): NP;
    function dot(a: NP): NP;
    function apply(foo: TNPFunction): NP;
    { algebra }
  private
    function det_nxn(const a: NP; ni: integer): T;
    function inv_nxn: NP;
  public
    function det: NP;
    function detBareiss: T;
    function inv: NP;
    function transpose2x2: NP;
    function cofactor: NP;
    function allEQ(const v: T): boolean;
    function dims: TArrInt;
  public
    class operator := (values: TArrT): NP; // create a numpas by assign fDims

    class operator +(const a, b: NP): NP;
    class operator +(const a: NP; b: integer): NP;
    class operator +(const a: NP; b: single): NP;
    class operator +(const a: NP; b: double): NP;
    class operator -(const a, b: NP): NP;
    class operator -(const a: NP; b: integer): NP;
    class operator -(const a: NP; b: single): NP;
    class operator -(const a: NP; b: double): NP;
    class operator *(const a, b: NP): NP;
    class operator *(const a: NP; b: integer): NP;
    class operator *(const a: NP; b: single): NP;
    class operator *(const a: NP; b: double): NP;
    class operator /(const a, b: NP): NP;
    class operator /(const a: NP; b: integer): NP;
    class operator /(const a: NP; b: single): NP;
    class operator /(const a: NP; b: double): NP;

    class operator =(const a, b: NP): boolean; // logical
    class operator <>(const a, b: NP): boolean;
    class operator >(const a, b: NP): boolean;
    class operator <(const a, b: NP): boolean;
    class operator >=(const a, b: NP): boolean;
    class operator <=(const a, b: NP): boolean;

  end;

function toString(a: TArrInt): string;
function genCombs(const a: TArrInt; n: integer): TArrInt;

implementation


{ TNumPas }


constructor TNumPas<T>.Create(const _dims: TArrInt);
var
  i, nn: integer;
begin
  assert(length(_dims) > 0, 'zero fDims are not allowed');

  fDims := system.copy(_dims);
  fNDims := length(fDims);

  fNItems := 1; // fNItems items
  for i in fDims do
    fNItems *= i;
  fSizeBytes := fNItems * sizeof(T);

  setLength(fData, fNItems);
  zero;

  nn := 1;
  setLength(fMlt, fNDims);  // mtl factor to improve indexing
  for i := 0 to high(fDims) do
  begin
    fMlt[high(fMlt) - i] := nn;
    nn *= fDims[high(fDims) - i];
  end;

  fpint := pinteger(@fData[0]); // pointers to data
  fpf32 := psingle(@fData[0]);
  fpf64 := pdouble(@fData[0]);
  fpf128 := pfp128(@fData[0]);
  fpdata := @fData[0];

  _typeInfo := PTypeInfo(TypeInfo(T)); // data type of T

  case _typeInfo^.Kind of
    tkInteger: fDataType := dtint;
    tkFloat: begin
      if _typeInfo^.Name = 'Single' then fDataType := dtf32;
      if _typeInfo^.Name = 'Double' then fDataType := dtf64;
      if _typeInfo^.Name = 'Real' then fDataType := dtf64;
    end;
    tkRecord:
      if _typeInfo^.Name = 'fp128' then fDataType := dtf128;
    else
      fDataType := dtUnknown;
  end;

  assert(fDataType <> dtUnknown, 'data type not supported');
end;

constructor TNumPas<T>.Create(const a: TNumPas<T>);
begin
  Create(a.fDims);

  fData := system.copy(a.fData);

  fpint := pinteger(@fData[0]);
  fpf32 := psingle(@fData[0]);
  fpf64 := pdouble(@fData[0]);
  fpf128 := pfp128(@fData[0]);
  fpdata := @fData[0];

  fDims := system.copy(a.fDims);
  fMlt := system.copy(a.fMlt);
end;

constructor TNumPas<T>.rand(_dims: TArrInt);
begin
  self := Create(_dims);
  self.rand;
end;

constructor TNumPas<T>.arange(nitems: integer);
var
  i: integer;
begin
  assert(nitems > 0, 'nitems must be > 0 in arange');

  self := TNumPas<T>.Create([nitems]);
  for i := 0 to pred(nItems) do fData[i] := i;
end;

constructor TNumPas<T>.linspace(astart, aend: double; _nitems: integer);
var
  i: integer;
  ainc: double;
begin
  assert((astart < aend) and (_nitems > 0), 'linspace start > end and nitems > 0');
  self := TNumPas<T>.Create([_nitems]);
  ainc := (aend - astart) / _nitems;
  for i := 0 to pred(_nitems) do fData[i] := i * ainc + astart;
end;

constructor TNumPas<T>.eye(nitems: integer);
var
  i: integer;
begin
  assert(nitems > 0, 'nitems must be > 0 in arange');

  self := TNumPas<T>.Create([nitems, nitems]);
  for i := 0 to pred(nItems) do {%H-}self[i, i] := 1;
end;

constructor TNumPas<T>.full(const _dims: TArrInt; avalue: T);
begin
  if length(_dims) > 0 then
  begin
    self := TNumPas<T>.Create(_dims);
    setValue(avalue);
  end;
end;

constructor TNumPas<T>.ones(const _dims: TArrInt);
begin
  self := TNumPas<T>.full(_dims, 1);
end;

constructor TNumPas<T>.zeros(const _dims: TArrInt);
begin
  self := TNumPas<T>.full(_dims, 0);
end;

constructor TNumPas<T>.identity(n: integer);
begin
  self := TNumPas<T>.eye(n);
end;

constructor TNumPas<T>.fromString(const s: string; typ: TDataType);
var
  nums: TStringArray;
  i: integer = 0;
  st: string;
begin
  nums := s.split([' ', ',', #9], TStringSplitOptions.ExcludeEmpty);
  if length(nums) > 0 then
  begin
    self := TNumPas<T>.Create([length(nums)]);
    case typ of
      dtf32, dtf64: for st in nums do
        begin
          {%H-}self[i] := strtofloat(st);
          Inc(i);
        end;
      dtint: for st in nums do
        begin
          {%H-}self[i] := StrToInt(st);
          Inc(i);
        end;
      else;
    end;
  end;
end;

constructor TNumPas<T>.transpose(const a: TNumPas<T>);
var
  i: integer;
begin
  self := a.copy;
  for i := 0 to pred(fNitems) do
    swap(i, transposedCoord(i));
end;

{ ------- }
procedure TNumPas<T>.swap(i, j: integer);
var
  tmp: T;
begin
  tmp := self[i];
  self[i] := self[j];
  self[j] := tmp;
end;


function TNumPas<T>.prod(a: TArrInt): integer;
var
  i: integer;
begin
  if a = nil then exit(1);

  Result := 1;
  for i in a do Result *= i;
end;

function TNumPas<T>.transposedCoord(c: integer): integer;

  function rev(var A: TArrInt): TArrInt;
  var
    i, j, imax: dword;
    Tmp: integer;
  begin
    if Length(A) = 0 then exit(nil);
    imax := high(A);
    Result := system.copy(A);

    for i := imax div 2 downto 0 do
    begin
      j := imax - i;
      Tmp := Result[i];
      Result[i] := Result[j];
      Result[j] := tmp;
    end;
  end;

var
  trDims, revDims: TArrInt;
  i, iv, cc: integer;
begin
  cc := c;
  revDims := rev(fDims);
  setLength(trDims, fNDims);

  Result := 0;
  for i := 0 to pred(fNDims) do
  begin
    iv := high(fDims) - i;
    trDims[iv] := cc mod fDims[iv];
    cc := cc div fDims[iv];
  end;
  //if fNDims >= 2 then
  //begin
  //  iv := high(trDims);
  //  i := trDims[iv];
  //  trDims[iv] := trDims[iv - 1];
  //  trDims[iv - 1] := i;
  //end;

  Result := calcIndex(rev(trDims));
  i := 0;
end;

function TNumPas<T>.rget(index: TArrInt): T;
begin
  Result := fData[{%H-}calcIndex(index)];
end;

procedure TNumPas<T>.rput(index: integer; Value: T);
begin
  fData[index] := Value;
end;

function TNumPas<T>.rget(index: integer): T;
begin
  Result := fData[index];
end;

procedure TNumPas<T>.rput(index: TArrInt; AValue: T);
begin
  fData[{%H-}calcIndex(index)] := AValue;
end;

function TNumPas<T>.rget(r, c: integer): T; // r,c
begin
  if r = 0 then exit(fData[c]);
  Result := fData[r * dim(0) + c];
end;

procedure TNumPas<T>.rput(r, c: integer; AValue: T);
begin
  fData[r * dim(0) + c] := Avalue;
end;

function TNumPas<T>.combinations(const inArr: TArrInt): TArrArrInt;
var
  currComb: TArrInt = nil;

  function genCombs(ix: integer): TArrArrInt;
  var
    i: integer;
  begin
    Result := nil;
    if ix = length(inArr) then
      Result := [system.copy(currComb)]
    else
      for i := 0 to pred(inArr[ix]) do
      begin
        currComb[ix] := i;
        Result += [genCombs(succ(ix))];
      end;
  end;

begin
  setLength(currComb, length(inArr));
  Result := genCombs(0);
end;

function TNumPas<T>.cmpData(constref a, b: T): integer;
begin
  if a > b then Result := 1
  else if a < b then Result := -1
  else
    Result := 0;
end;

procedure TNumPas<T>.swap(i, j, k, l: integer);
var
  tmp: T;
begin
  tmp := self[i, j];
  self[i, j] := self[k, l];
  self[k, l] := tmp;
end;

function TNumPas<T>.calcIndex(const index: TArrInt): integer;
var
  i: integer;
begin
  if index = nil then exit(0);

  case fNDims of
    1: Result := index[0];
    2: Result := index[0] * fMlt[0] + index[1];
    3: Result := index[0] * fMlt[0] + index[1] * fMlt[1] + index[2];
    4: Result := index[0] * fMlt[0] + index[1] * fMlt[1] + index[2] * fMlt[2] + index[3];
    else
    begin
      Result := 0;
      for i := low(fMlt) to high(fMlt) do
        Result += fMlt[i] * index[i];
    end;
  end;
  assert(Result < fNItems, format('index %d out of range %d', [Result, fNItems]));
end;

procedure TNumPas<T>.rangeSlice(const index: TArrInt; out aStart, aEnd: integer);
var
  i: integer;
  widx: TArrInt;
begin
  if index = nil then
  begin
    aStart := 0;
    aEnd := 0;
  end
  else
  begin
    widx := system.copy(index);
    while length(widx) < fNDims do widx += [0]; // fill w/0
    aStart := calcIndex(widx);

    widx := system.copy(index); // fill w/ fDims-1
    for i := length(index) to high(fDims) do widx += [fDims[i] - 1];
    aEnd := calcIndex(widx) + 1;
  end;
end;

function TNumPas<T>.startSlice(const slc: TArrInt): integer;
var
  widx: TArrInt;
  i: integer;
begin
  setLength(widx, fNDims);
  for i := 0 to high(widx) do
    if i <= high(slc) then widx[i] := slc[i]
    else
      widx[i] := 0;

  //widx := system.copy(slc);
  //while length(widx) < fNDims do widx += [0]; // fill w/0
  Result := calcIndex(widx);
end;

function TNumPas<T>.endSlice(const slc: TArrInt): integer;
var
  i: integer;
  widx: TArrInt;
begin
  widx := system.copy(slc); // fill w/ fDims-1
  for i := length(slc) to high(fDims) do widx += [fDims[i] - 1];
  Result := calcIndex(widx);
end;

function TNumPas<T>.checkDims: boolean;
var
  i: integer;
begin
  if fNDims = 0 then exit(False);
  for i in fDims do if i <= 0 then exit(False);
  Result := True;
end;

{ thread safe random funcs}
function TNumPas<T>.TsRandInt(var seed: integer): integer;
begin
  {$R-}
  {$overflowchecks off}
  seed := seed * 939993 + 12345;
  Result := (seed shr 16) mod $7fff;  // Using shr and mod for efficiency
  {$R+}
  {$overflowchecks on}
end;

function TNumPas<T>.TsRandf64(var seed: integer): double;
begin
  Result := TsRandInt(seed) / $7fff;
end;

function TNumPas<T>.GetEnumerator: TValEnumerator;
begin
  //result:=TValEnumerator.Create(fNItems, fpdata);
  Result.epdata := fpdata;
  Dec(Result.epdata);
  Result.en := fNItems;
  Result.eindex := -1; // first move & then get
end;


function TNumPas<T>.TsRandf32(var seed: integer): single;
begin
  Result := TsRandInt(seed) / $7fff;
end;

function TNumPas<T>.toString: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to pred(fNItems) do
  begin

    if i > 0 then
    begin
      if i mod fDims[high(fDims)] = 0 then Result += #10
      else
        Result += ' ';
    end;

    case fDataType of
      dtint: Result += format('%d', [fpint[i]]);
      dtf32: Result += format('%f', [fpf32[i]]);
      dtf64: Result += format('%f', [fpf64[i]]);
      dtf128: Result += format('%s', [f128tos(fpf128[i])]);
      else;
    end;
  end;
end;


function TNumPas<T>.toPython(varName: string): string;
var
  i: integer;
begin
  Result := varName + '=np.array([';
  for i := 0 to pred(fNItems) do
  begin
    case fDataType of
      dtint: Result += format('%d', [fpint[i]]);
      dtf32: Result += format('%e', [fpf32[i]]);
      dtf64: Result += format('%e', [fpf64[i]]);
      dtf128: Result += format('%s', [f128tos(fpf128[i])]);
      else;
    end;
    if i <> pred(fNItems) then Result += ', ';
  end;
  Result += ']).reshape(';
  for i := 0 to pred(fNDims) do
  begin
    Result += IntToStr(fDims[i]);
    if i <> pred(fNDims) then Result += ',';
  end;
  Result += ')';
end;

function TNumPas<T>.slice(index: TArrInt): NP; // small partial index
var
  i, istart, iend: integer;
  widx: TArrInt;
begin
  assert(length(index) > 0, 'slice with empty array not allowed');

  widx := system.copy(index);
  while length(widx) < fNDims do widx += [0]; // fill w/0
  istart := calcIndex(widx);

  widx := system.copy(index); // fill w/ fDims-1
  for i := length(index) to high(fDims) do widx += [fDims[i] - 1];
  iend := calcIndex(widx) + 1;

  widx := system.copy(fDims, length(index), length(fDims) - length(index));
  Result := TNumPas<T>.Create(widx); // create new instance & copy data
  move(fData[istart], Result.fData[0], (iend - istart) * sizeof(T));
end;

function TNumPas<T>.copy: NP;
begin
  Result := TNumPas<T>.Create(self);
end;

procedure TNumPas<T>.zero;
begin
  if fSizeBytes > 0 then
    fillbyte(fData[0], fSizeBytes, 0);
end;


procedure TNumPas<T>.rand;
var
  i: integer = 0;
  seed: integer;
begin
  seed := random($7fff);
  case fDataType of
    dtint: for i := 0 to pred(fNItems) do fpint[i] := random(10000);
    dtf32: for i := 0 to pred(fNItems) do fpf32[i] := random; // fp32
    dtf64: for i := 0 to pred(fNItems) do fpf64[i] := random; // TsRandf64(seed); // fp64
    dtf128: for i := 0 to pred(fNItems) do fpf128[i] := random;
    else;
  end;
end;

procedure TNumPas<T>.randMT;   // slower than ST mode as TsRnd is much slower than random

  procedure _randMT(ix: PtrInt; {%H-}pnt: pointer; Item: TMultiThreadProcItem);
  var
    delta: integer;
    seed: integer;
  begin
    delta := Item.Group.EndIndex;
    seed := random($7fff); // each thread has its own seed

    case fDataType of
      dtint: while ix < fNItems do
        begin
          fpint[ix] := TsRandInt(seed);
          Inc(ix, delta);
        end;
      dtf32: while ix < fNItems do
        begin
          fpf32[ix] := TsRandf32(seed);
          Inc(ix, delta);
        end;
      dtf64: while ix < fNItems do
        begin
          fpf64[ix] := TsRandf64(seed); // TsRnd.random;
          Inc(ix, delta);
        end;
      else;
    end;
  end;

begin
  ProcThreadPool.DoParallelLocalProc(@_randMT, 0, GetCPUCount - 1, nil);
end;


function TNumPas<T>.hi(index: integer): integer;
begin
  if index < 0 then index := -index - 1; // allow pythan negative # index
  Result := dim(index) - 1;
end;

function TNumPas<T>.dim(index: integer): integer;
begin
  assert(index <= high(fDims));
  Result := fDims[high(fDims) - index];
end;

function TNumPas<T>.size: integer;
begin
  Result := fNItems;
end;

function TNumPas<T>.Data(index: integer): T;
begin
  assert(index < fNItems, format('index %d out of range %d', [index, pred(fNItems)]));
  Result := fData[index];
end;

function TNumPas<T>.getData: TArrT;
begin
  Result := fData;
end;

procedure TNumPas<T>.setValue(v: T);
var
  i: integer;
begin
  for i := 0 to pred(fNItems) do fData[i] := v;
end;

procedure TNumPas<T>.setValue(const slc: TArrInt; v: T);
var
  i: integer;
begin
  for i := startSlice(slc) to endSlice(slc) do fData[i] := v;
end;

function TNumPas<T>.sameDims(a: TNumPas<T>): boolean;
begin
  if fNDims <> a.fNDims then exit(False);
  Result := compareMem(@a.fDims[0], @fDims[0], length(fDims) * sizeof(fDims[0]));
end;

function TNumPas<T>.sum: T;
var
  i: T;
begin
  Result := 0;
  for  i in self do
    Result += i;
end;

function TNumPas<T>.max: T;

  function max(a, b: T): T;
  begin
    if a > b then Result := a
    else
      Result := b;
  end;

var
  i: T;
begin
  Result := self[0];
  for i in self do Result := max(Result, i);
end;

function TNumPas<T>.min: T;

  function min(a, b: T): T;
  begin
    if a < b then Result := a
    else
      Result := b;
  end;

var
  i: T;
begin
  Result := self[0];
  for i in self do Result := min(Result, i);
end;

function TNumPas<T>.mean: T;
begin
  Result := sum / fNItems;
end;

function TNumPas<T>.shape: TArrInt;
begin
  Result := fDims;
end;

procedure TNumPas<T>.Assign(const a: TNumPas<T>; const _dims: TArrInt);
var
  i, aStart, aEnd: integer;
begin
  rangeSlice(_dims, aStart, aEnd);
  for i := 0 to pred(a.fNItems) do self[i + aStart] := a[i];
end;

procedure TNumPas<T>.toFile(Name: string);
var
  fh: THandle;
begin
  try
    fh := fileCreate(Name);
    fileWrite(fh, fData, fSizeBytes);
  finally
    fileClose(fh);
  end;
end;

procedure TNumPas<T>.fromFile(Name: string);
var
  fh: THandle;
begin
  try
    fh := fileOpen(Name, fmOpenRead);
    fileRead(fh, fData, fSizeBytes); // read only supported size
  finally
    fileClose(fh);
  end;
end;


function TNumPas<T>.flatSort: NP;
begin
  Result := copy;
  Result._flatSort;
end;

procedure TNumPas<T>._flatSort;  // insite sort
begin
  TFlatArrayHelper.Sort(fData, TComparer<T>.Construct(cmpData));
end;

function TNumPas<T>.sort: NP;
var
  tdim: TArrInt;
  a: TNumPas<T>;
begin
  if fNDims = 1 then Result := flatSort
  else
  begin
    Result := copy;

    for tdim in combinations(system.Copy(fDims, 0, fNDims - 1)) do
    begin
      a := slice(tdim);
      a._flatSort;
      Result.Assign(a, tdim);
    end;
  end;
end;

function TNumPas<T>.diagonal: TNumPas<T>;
var
  i: integer;
begin
  assert((fNDims = 2) and (hi(0) = hi(1)), 'non quadratic');
  Result := TNumPas<T>.zeros([dim(0)]);
  for i := 0 to hi(0) do Result[i] := self[i, i];
end;

function TNumPas<T>.apply(foo: TNPFunction): TNumPas<T>;
var
  i: integer;
begin
  Result := copy;
  for i := 0 to pred(fNItems) do Result[i] := foo(self[i]);
end;


function TNumPas<T>.dot(a: TNumPas<T>): TNumPas<T>;
var
  pivotPos, pivd, ppivd: integer;
  ixs, ixa, ixr, prs, pra, r, c: integer;
  sStart, aStart, adim0, ahi0, ahi1, aStride: integer; // ranges
  raDim, aDim, sDim, resDim: TArrInt;
  p: T;
begin
  if not (checkDims and a.checkDims) then exit(nil);

  pivotPos := Math.min(1, a.fNdims - 1); // position in a of pivot dim in  1|0
  pivd := a.dim(pivotPos); // pivot dimension, a.dim(1|0) = dim(0)

  assert(dim(0) = pivd, 'shapes not aligned'); // !!! = dim(0)

  sDim := system.copy(fDims, 0, fNDims - 1); // all but dim(0)
  aDim := system.copy(a.fDims); // remove 'pp' 0|1
  Delete(aDim, high(a.fDims) - pivotPos, 1);

  resDim := sDim + aDim;
  if resDim = nil then resDim := [1];  // 1x1

  if a.fNdims <= 2 then raDim := nil // last a.fndims - 2
  else
    raDim := system.copy(a.fDims, 0, a.fNdims - 2);

  Result := NP.Create(resDim);

  prs := prod(sDim); // mult all dims 1 if nil
  pra := prod(raDim);

  aStride := a.fNitems div pra; // a stride

  adim0 := a.dim(0);
  ahi0 := a.hi(0);
  if a.fNdims > 1 then ahi1 := a.hi(1)
  else
    ahi1 := 0;
  ppivd := pred(pivd);

  sStart := 0;
  ixr := 0;

  case pivotPos of
    0: begin   // 1x1 reduction of self
      for ixs := 0 to pred(prs) do
      begin
        p := 0;   // calc dot 1x1
        for r := 0 to ppivd do p += self[sStart + r] * a[r];

        Result[ixr] := p;
        Inc(ixr);

        Inc(sStart, pivd); // next self pivd slice position
      end;
    end;

    1: begin
      for ixs := 0 to pred(prs) do   // 1x2 reduction of self
      begin
        aStart := 0;

        for ixa := 0 to pred(pra) do
        begin

          for c := 0 to ahi0 do // 1x2 dot prod.
          begin
            p := 0;
            for r := 0 to ahi1 do
              p += self[sStart + r] * a[aStart + r * adim0 + c];

            Result[ixr] := p;
            Inc(ixr);
          end;

          Inc(aStart, aStride); // next a slice
        end;

        Inc(sStart, pivd); // next pivd slice of self
      end;
    end;
  end;
end;

function TNumPas<T>.dot1x1(a: TNumPas<T>): TNumPas<T>;
var
  p: T;
  i: integer;
begin
  assert(sameDims(a) and (fNDims = 1), 'dot 1x1 dims not feasible');
  Result := NP.Create([1]);
  p := 0;
  for i := 0 to pred(fNItems) do p += self.fData[i] * a.fData[i];
  Result[0] := p;
end;

function TNumPas<T>.dot1x2(a: TNumPas<T>): TNumPas<T>;
var
  r, c: integer;
  p: T;
begin
  assert((self.fNDims = 1) and (a.fNDims = 2) and (dim(0) = a.dim(1)),
    'dot 1x2 dims product not feasible');

  Result := NP.zeros([a.dim(0)]);
  for c := 0 to a.hi(0) do
  begin
    p := 0;
    for r := 0 to a.hi(1) do
      p += self[r] * a[r, c];
    Result[c] := p;
  end;
end;

function TNumPas<T>.dotnx1(a: TNumPas<T>): TNumPas<T>;
var
  i, pivd, xs, r: integer;
  p: T;
begin
  assert((self.fNDims > 2) and (a.fNDims = 1) and (dim(0) = a.dim(0)),
    'dot 1xn dims product not feasible');

  pivd := hi(0);
  Result := TNumPas<T>.zeros(system.copy(fDims, 0, high(fDims))); // all except last

  r := 0;
  xs := 0;

  for r := 0 to pred(Result.size) do
  begin
    p := 0;
    for i := 0 to pivd do p += self[xs + i] * a[i];
    Result[r] := p;
    Inc(xs, dim(0));
  end;
end;

function TNumPas<T>.dot2x1(a: TNumPas<T>): TNumPas<T>;
var
  r, c: integer;
  p: T;
begin
  assert((self.fNDims = 2) and (a.fNDims = 1) and (dim(0) = a.dim(0)),
    'dot 2x1 dims product not feasible');

  Result := TNumPas<T>.zeros([fDims[0]]); // [n, m] · [m] = [n] -> fDims[0]

  for r := 0 to hi(1) do
  begin
    p := 0;
    for c := 0 to hi(0) do
      p += self[r, c] * a[c];
    Result[r] := p;
  end;
end;

function TNumPas<T>.dot2x2(a: TNumPas<T>): TNumPas<T>; // n x n same dims
var
  r, c, k: integer;
  p: T;
begin
  assert((fNDims = 2) and (a.fNDims = 2) and (hi(0) = a.hi(1)),
    'dot product not feasible');

  Result := TNumPas<T>.zeros([dim(1), a.dim(0)]);
  for r := 0 to hi(1) do
    for c := 0 to a.hi(0) do
    begin
      p := 0;
      for k := 0 to hi(0) do p += self[r, k] * a[k, c];
      Result[r, c] := p;
    end;
end;

{ algebra functions }
function TNumPas<T>.det_nxn(const a: TNumPas<T>; ni: integer): T; // slow
var
  minor: TNumPas<T>;
  i, j, k, c1, c2: integer;
  O: T;
begin
  minor := a.copy;
  minor.zero;

  O := 1;
  Result := 0;

  case ni of  // ni is pred(dim)
    1: Result := a[0, 0] * a[1, 1] - a[0, 1] * a[1, 0]; {apply Sarrus rule 2,3}
    2: Result := (a[0, 0] * (a[1, 1] * a[2, 2] - a[1, 2] * a[2, 1])) -
        (a[0, 1] * (a[1, 0] * a[2, 2] - a[1, 2] * a[2, 0])) +
        (a[0, 2] * (a[1, 0] * a[2, 1] - a[1, 1] * a[2, 0]))
    else
    begin
      for i := 0 to ni do
      begin
        c1 := 0;
        c2 := 0;
        for j := 0 to ni do
          for k := 0 to ni do
            if (j <> 0) and (k <> i) then
            begin
              minor[c1, c2] := a[j, k];
              Inc(c2);
              if c2 >= ni then
              begin
                Inc(c1);
                c2 := 0;
              end;
            end;
        Result += O * (a[0, i] * det_nxn(minor, pred(ni)));
        O := -O;
      end;
    end;
  end;
end;

function TNumPas<T>.inv_nxn: NP;
var
  n, c, i, j, k: integer;
  tmp: T;
  a: TNumPas<T>;
begin
  n := hi(0);   // dimensions 'a' - 1 -> for

  Result := TNumPas<T>.identity(dim(0));   // b:=diag 1
  a := copy;

  for j := 0 to n do   // Perform Gauss-Jordan elimination
  begin
    for i := j to n do
    begin
      if a[i, j] <> 0 then
      begin
        for k := 0 to n do
        begin
          a.swap(j, k, i, k);
          Result.swap(j, k, i, k);
        end;

        tmp := 1 / a[j, j];
        for k := 0 to n do
        begin
          a[j, k] := tmp * a[j, k];
          Result[j, k] := tmp * Result[j, k];
        end;

        for k := 0 to n do
        begin
          if k <> j then
          begin
            tmp := -a[k, j];
            for c := 0 to n do
            begin
              a[k, c] := a[k, c] + tmp * a[j, c];
              Result[k, c] := Result[k, c] + tmp * Result[j, c];
            end;
          end;
        end;
        break;
      end;
    end;

    assert(a[i, j] <> 0, 'Singular matrix encountered -> zero determinant');
  end;
end;


function TNumPas<T>.det: NP;
var
  tdim: TArrInt;
  tdims: TArrArrInt;
begin
  assert((fNDims >= 2) and (hi(0) = hi(1)), 'not quadratic matrix');

  if fNDims = 2 then
  begin
    Result := TNumPas<T>.Create([1]);
    Result[0] := detBareiss;
  end
  else
  begin
    tdim := system.Copy(fDims, 0, fNDims - 2);

    tdims := combinations(tdim);
    Result := TNumPas<T>.Create(tdim);

    for tdim in tdims do
      {%H-}Result[tdim] := slice(tdim).detBareiss;
  end;
end;

function TNumPas<T>.detBareiss: T;  // 2 dims n x n works on copy
var
  factor: T;
  i, j, k: integer;
  a: TNumPas<T>;
begin
  assert((fNDims = 2) and (hi(0) = hi(1)), 'not quadratic matrix');

  Result := 1.0;
  a := copy;

  for i := 0 to hi(0) do
  begin
    for j := i + 1 to hi(0) do
    begin
      if a[i, i] <> 0 then
      begin
        factor := a[j, i] / a[i, i];
        for k := i to hi(0) do
          a[j, k] := a[j, k] - factor * a[i, k];
      end;
    end;
    Result *= a[i, i];
  end;
end;

function TNumPas<T>.inv: NP;
var
  tdim: TArrInt;
  tdims: TArrArrInt;
begin
  assert((fNDims >= 2) and (hi(0) = hi(1)), 'not quadratic matrix');

  Result := copy;

  if fNDims = 2 then
    Result := inv_nxn
  else
  begin
    tdim := system.Copy(fDims, 0, fNDims - 2);

    tdims := combinations(tdim);

    for tdim in tdims do
      Result.Assign(slice(tdim).inv_nxn, tdim);
  end;
end;

function TNumPas<T>.transpose2x2: NP;
var
  i, j: integer;
begin
  assert((fNDims = 2) and (hi(0) = hi(1)), 'not quadratic matrix');

  Result := self.copy;
  for i := 0 to hi(0) do
    for j := 0 to hi(0) do
      Result[j, i] := self[i, j];
end;

function TNumPas<T>.cofactor: NP;
var
  i, j, p, q, x, y: integer;
  subm: TNumPas<T>;
begin
  assert((fNDims = 2) and (dim(0) = dim(1)), 'not quadratic matrix');

  Result := copy;

  for i := 0 to hi(0) do
    for j := 0 to hi(0) do
    begin
      subm := TNumPas<T>.Create([hi(0), hi(0)]);
      p := 0;
      for x := 0 to hi(0) do
      begin
        if x = i then continue;
        q := 0;
        for y := 0 to hi(0) do
        begin
          if y = j then continue;
          subm[p, q] := self[x, y];
          Inc(q);
        end;
        Inc(p);
      end;
      Result[i, j] := power(-1.0, i + j) * subm.detBareiss;
    end;
end;

function TNumPas<T>.allEQ(const v: T): boolean;
var
  i: integer;
begin
  for i := 0 to pred(fNItems) do if fData[i] <> v then exit(False);
  Result := True;
end;

function TNumPas<T>.dims: TArrInt;
begin
  Result := fDims;
end;

{ operators }

class operator TNumPas<T>.:=(values: TArrT): TNumPas<T>;
var
  i: integer;
begin
  Result := TNumPas<T>.Create([length(values)]);
  for i := 0 to high(values) do Result.fData[i] := values[i];
end;

class operator TNumPas<T>.+(const a, b: TNumPas<T>): TNumPas<T>;
var
  i: integer;
begin
  assert(a.sameDims(b), 'can''t add with different simensions');
  Result := a.copy;
  for i := 0 to pred(a.fNItems) do Result.fData[i] += b.fData[i];
end;

class operator TNumPas<T>.+(const a: TNumPas<T>; b: integer): TNumPas<T>;
var
  i: integer;
begin
  Result := a.copy;
  for i := 0 to pred(a.fNItems) do Result.fData[i] += b;
end;

class operator TNumPas<T>.+(const a: TNumPas<T>; b: single): TNumPas<T>;
var
  i, rb: integer;
begin
  Result := a.copy;
  rb := round(b);
  case a.fDataType of
    dtint: for i := 0 to pred(a.fNItems) do Result.fpint[i] += rb;
    dtf32: for i := 0 to pred(a.fNItems) do Result.fpf32[i] += b;
    dtf64: for i := 0 to pred(a.fNItems) do Result.fpf64[i] += b;
    dtf128: for i := 0 to pred(a.fNItems) do Result.fpf128[i] += b;
    else;
  end;
end;

class operator TNumPas<T>.+(const a: TNumPas<T>; b: double): TNumPas<T>;
var
  i, rb: integer;
begin
  Result := a.copy;
  rb := round(b);
  case a.fDataType of
    dtint: for i := 0 to pred(a.fNItems) do Result.fpint[i] += rb;
    dtf32: for i := 0 to pred(a.fNItems) do Result.fpf32[i] += b;
    dtf64: for i := 0 to pred(a.fNItems) do Result.fpf64[i] += b;
    dtf128: for i := 0 to pred(a.fNItems) do Result.fpf128[i] += b;
    else;
  end;
end;

class operator TNumPas<T>.-(const a, b: TNumPas<T>): TNumPas<T>;
var
  i: integer;
begin
  assert(a.sameDims(b), 'can''t sub with different simensions');
  Result := a.copy;
  for i := 0 to pred(a.fNItems) do Result.fData[i] -= b.fData[i];
end;

class operator TNumPas<T>.-(const a: TNumPas<T>; b: integer): TNumPas<T>;
var
  i: integer;
begin
  Result := a.copy;
  for i := 0 to pred(a.fNItems) do Result.fData[i] -= b;
end;

class operator TNumPas<T>.-(const a: TNumPas<T>; b: single): TNumPas<T>;
var
  i, rb: integer;
begin
  Result := a.copy;
  rb := round(b);
  case a.fDataType of
    dtint: for i := 0 to pred(a.fNItems) do Result.fpint[i] -= rb;
    dtf32: for i := 0 to pred(a.fNItems) do Result.fpf32[i] -= b;
    dtf64: for i := 0 to pred(a.fNItems) do Result.fpf64[i] -= b;
    dtf128: for i := 0 to pred(a.fNItems) do Result.fpf128[i] -= b;
  end;
end;

class operator TNumPas<T>.-(const a: TNumPas<T>; b: double): TNumPas<T>;
var
  i, rb: integer;
begin
  Result := a.copy;
  rb := round(b);
  case a.fDataType of
    dtint: for i := 0 to pred(a.fNItems) do Result.fpint[i] -= rb;
    dtf32: for i := 0 to pred(a.fNItems) do Result.fpf32[i] -= b;
    dtf64: for i := 0 to pred(a.fNItems) do Result.fpf64[i] -= b;
    dtf128: for i := 0 to pred(a.fNItems) do Result.fpf128[i] -= b;
  end;
end;

class operator TNumPas<T>.*(const a, b: TNumPas<T>): TNumPas<T>;
var
  i, j: integer;
begin

  Result := a.copy;

  i := 0;
  while i < a.fNItems do
  begin
    for j := 0 to pred(b.fnitems) do
    begin
      if i < a.fnitems then
        Result[i] := Result[i] * b[j]
      else
        break;
      Inc(i);
    end;
  end;
end;


class operator TNumPas<T>.*(const a: TNumPas<T>; b: integer): TNumPas<T>;
var
  i: integer;
begin
  Result := a.copy;
  for i := 0 to pred(a.fNItems) do Result.fData[i] *= b;
end;

class operator TNumPas<T>.*(const a: TNumPas<T>; b: single): TNumPas<T>;
var
  i, rb: integer;
begin
  Result := a.copy;
  rb := round(b);
  case a.fDataType of
    dtint: for i := 0 to pred(a.fNItems) do Result.fpint[i] *= rb;
    dtf32: for i := 0 to pred(a.fNItems) do Result.fpf32[i] *= b;
    dtf64: for i := 0 to pred(a.fNItems) do Result.fpf64[i] *= b;
    dtf128: for i := 0 to pred(a.fNItems) do Result.fpf128[i] *= b;
  end;
end;

class operator TNumPas<T>.*(const a: TNumPas<T>; b: double): TNumPas<T>;
var
  i, rb: integer;
begin
  Result := a.copy;
  rb := round(b);
  case a.fDataType of
    dtint: for i := 0 to pred(a.fNItems) do Result.fpint[i] *= rb;
    dtf32: for i := 0 to pred(a.fNItems) do Result.fpf32[i] *= b;
    dtf64: for i := 0 to pred(a.fNItems) do Result.fpf64[i] *= b;
    dtf128: for i := 0 to pred(a.fNItems) do Result.fpf128[i] *= b;
  end;
end;

class operator TNumPas<T>./(const a, b: TNumPas<T>): TNumPas<T>;
var
  i: integer;
begin
  assert(a.sameDims(b), 'can''t / with different simensions');
  Result := a.copy;
  case a.fDataType of
    dtint: for i := 0 to pred(a.fNItems) do Result.fpint[i] := a.fpint[i] div b.fpint[i];
    dtf32: for i := 0 to pred(a.fNItems) do Result.fpf32[i] /= b.fpf32[i];
    dtf64: for i := 0 to pred(a.fNItems) do Result.fpf64[i] /= b.fpf64[i];
    dtf128: for i := 0 to pred(a.fNItems) do Result.fpf128[i] /= b.fpf128[i];
  end;
end;

class operator TNumPas<T>./(const a: TNumPas<T>; b: integer): TNumPas<T>;
var
  i: integer;
begin
  Result := a.copy;

  case a.fDataType of
    dtint: for i := 0 to pred(a.fNItems) do Result.fpint[i] := a.fpint[i] div b;
    dtf32: for i := 0 to pred(a.fNItems) do Result.fpf32[i] /= b;
    dtf64: for i := 0 to pred(a.fNItems) do Result.fpf64[i] /= b;
    dtf128: for i := 0 to pred(a.fNItems) do Result.fpf128[i] /= b;
  end;
end;

class operator TNumPas<T>./(const a: TNumPas<T>; b: single): TNumPas<T>;
var
  i, rb: integer;
begin
  Result := a.copy;
  rb := round(b);
  case a.fDataType of
    dtint: for i := 0 to pred(a.fNItems) do Result.fpint[i] := a.fpint[i] div rb;
    dtf32: for i := 0 to pred(a.fNItems) do Result.fpf32[i] /= b;
    dtf64: for i := 0 to pred(a.fNItems) do Result.fpf64[i] /= b;
    dtf128: for i := 0 to pred(a.fNItems) do Result.fpf128[i] /= b;
  end;
end;

class operator TNumPas<T>./(const a: TNumPas<T>; b: double): TNumPas<T>;
var
  i, rb: integer;
begin
  Result := a.copy;
  rb := round(b);
  case a.fDataType of
    dtint: for i := 0 to pred(a.fNItems) do Result.fpint[i] := a.fpint[i] div rb;
    dtf32: for i := 0 to pred(a.fNItems) do Result.fpf32[i] /= b;
    dtf64: for i := 0 to pred(a.fNItems) do Result.fpf64[i] /= b;
    dtf128: for i := 0 to pred(a.fNItems) do Result.fpf128[i] /= b;
  end;
end;


class operator TNumPas<T>.=(const a, b: TNumPas<T>): boolean;
var
  i: integer;
begin
  assert(a.sameDims(b), 'can''t compare with different simensions');

  for i := 0 to pred(a.fNItems) do if a.fData[i] <> b.fData[i] then exit(False);
  Result := True;
end;

class operator TNumPas<T>.<>(const a, b: TNumPas<T>): boolean;
begin
  Result := not (a = b);
end;

class operator TNumPas<T>.>(const a, b: TNumPas<T>): boolean;
var
  i: integer;
begin
  assert(a.sameDims(b), 'can''t compare with different simensions');

  for i := 0 to pred(a.fNItems) do if a.fData[i] <= b.fData[i] then exit(False);
  Result := True;
end;

class operator TNumPas<T>.<(const a, b: TNumPas<T>): boolean;
var
  i: integer;
begin
  assert(a.sameDims(b), 'can''t compare with different simensions');

  for i := 0 to pred(a.fNItems) do if a.fData[i] >= b.fData[i] then exit(False);
  Result := True;
end;

class operator TNumPas<T>.>=(const a, b: TNumPas<T>): boolean;
begin
  Result := not (a < b);
end;

class operator TNumPas<T>.<=(const a, b: TNumPas<T>): boolean;
begin
  Result := not (a < b);
end;

{ TNumPas.TValEnumerator }

function TNumPas<T>.TValEnumerator.GetCurrent: T;
begin
  Result := epdata^;
end;

function TNumPas<T>.TValEnumerator.MoveNext: boolean;
begin
  Inc(epdata);
  Inc(eindex);
  Result := eindex < en;
end;

{ aux funcs }
function toString(a: TArrInt): string;
var
  i: integer;
begin
  Result := '[';
  for i := 0 to high(a) do
  begin
    Result += format('%d', [a[i]]);
    if i <> high(a) then Result += ',';
  end;
  Result += ']';
end;

function genCombs(const a: TArrInt; n: integer): TArrInt;
var
  i: integer;
begin
  Result := system.copy(a);

  for i := high(a) downto 0 do
  begin
    if a[i] <> 0 then
    begin
      Result[i] := n mod a[i];
      n := n div a[i];
    end
    else
      Result[i] := 0;
  end;
end;

initialization
  randomize;

finalization

end.
