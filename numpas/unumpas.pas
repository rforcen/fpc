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

uses // add required packages: multithreadprocslaz
  Classes, SysUtils, Math, TypInfo, MTProcs, Randoms,
  UTF8Process,
  Generics.Defaults, Generics.Collections, f128, f80, zstream;

type

  { TIndexValue }
  TIndexValue<T> = record
    index: integer;
    Value: T;
  end;

  TArrInt = array of integer;
  TDataType = (dtUnknown, dtint, dtf32, dtf64, dtf80, dtf128);

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
    constructor Create(const _dims: TArrInt; _pt: PT); overload;
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
    constructor vander(n: integer);

  private
    fDims: TArrInt;
    hitems, fNitems, fNdims: int64;
    fMlt: TArrInt; // mlt factor for index calc
    fSizeBytes, fCmpSize: int64;

    _typeInfo: PTypeInfo;
    fDataType: TDataType;

    fpint: pinteger;
    fpf32: psingle;
    fpf64: pdouble;
    fpf80: pfp80;
    fpf128: pfp128;

    fpdata: PT;
    fData: array of T;


  private
    function rget(index: TArrInt): T; overload; inline;
    procedure rput(index: integer; Value: T); overload; inline;
    function rget(index: integer): T; overload; inline;
    procedure rput(index: TArrInt; AValue: T); overload; inline;
    function rget(r, c: integer): T; overload; inline;
    procedure rput(r, c: integer; AValue: T); overload; inline;

    function nDims: integer;
    function combinations(const inArr: TArrInt = nil): TArrArrInt;
    function cmpData(constref a, b: T): integer;
    procedure swap(i, j, k, l: integer); inline;
    procedure swap(i, j: integer);
    function prod(a: TArrInt): int64;
    procedure setPointers;
    function index2Dims(index: integer): TArrInt;
    function transposedCoord(index: integer): integer;
    function sqr(x: T): T; inline;
    function getNThreads: integer;
  public
    function calcIndex(const index: TArrInt): integer; inline;
    procedure rangeSlice(const index: TArrInt; out aStart, aEnd: integer);
    function startSlice(const slc: TArrInt): integer;
    function endSlice(const slc: TArrInt): integer;
    function checkDims: boolean; overload;
    function checkDims(_dims: TArrInt): boolean; overload;

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
    function npToStr(r, c, nDecs: integer): string;
    function toPython(varName: string): string; overload;
    function slice(index: TArrInt): NP;
    function copy: NP; // deep copy
    procedure zero;
    procedure one;
    procedure rand; overload; // fill w/random values
    procedure randMT;
    function hi(index: integer): integer;  // -1 to math for loops->high
    function dim(index: integer): integer;

    function size: integer;
    function Data(index: integer): T;
    function getData: TArrT;
    procedure setValue(v: T); overload;
    procedure fromBuffer(v: PT);
    procedure toBuffer(v: pointer);
    procedure setValue(const slc: TArrInt; v: T); overload;
    function sameDims(a: NP): boolean; // same fDims
  public
    function sum: T;
    function max: T;
    function min: T;
    function mean: T;
    function std: T;
    procedure seq; // fill with seq # 0..pred(size)
    function shape: TArrInt;
    procedure Assign(const a: NP; const _dims: TArrInt);
    procedure toFile(Name: string);
    procedure fromFile(Name: string);
    procedure save(Name: string);
    function load(Name: string): NP;
    function flatSort: NP;
    procedure _flatSort; // insite 1 x fNitems
    function sort: NP;
    function sortMT: NP;
    function diagonal: NP;
    function dot1x1(a: NP): NP;
    function dot2x1(a: NP): NP;
    function dot1x2(a: NP): NP;
    function dotnx1(a: NP): NP;
    function dot2x2(a: NP): NP;
    function dot(a: NP): NP;
    function dotMT(a: NP): NP;
    function apply(foo: TNPFunction): NP;
    function isQuadratic: boolean;
    function reshape(ndim: TArrInt): NP;
  public
    procedure compress;
    procedure deCompress;

    { algebra }
  private
    function det_nxn(const a: NP; ni: integer): T;
    function inv_nxn: NP;
  public
    function det: NP;
    function detMT: NP;
    function detBareiss: T;
    function inv: NP;
    function invMT: NP;
    function solve(b: NP): NP;
    function transpose2x2: NP;
    function transpose: NP;
    function transposeMT: NP;
    function cofactor: NP;
    function allEQ(const v: T): boolean;
    function allNE(const v: T): boolean;
    function dims: TArrInt;
    function dimsStr: string;
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

    class operator =(const a, b: NP): boolean; overload; // logical
    class operator <>(const a, b: NP): boolean;
    class operator >(const a, b: NP): boolean;
    class operator <(const a, b: NP): boolean;
    class operator >=(const a, b: NP): boolean;
    class operator <=(const a, b: NP): boolean;

  end;

  TFileNPY = packed record // used in save/load
  const
    _id = #$93 + 'NUMPY';
  public
    id: array[0..5] of char;
    mayVer, minVer: byte;
    headLen: uint16;
  end;

{ aux's }
function toString(a: TArrInt): string;
function genCombs(const a: TArrInt; n: integer): TArrInt;
function revArrInt(const a: TArrInt): TArrInt;
function strToDims(sd: string): TarrInt;


implementation


{ TNumPas }


constructor TNumPas<T>.Create(const _dims: TArrInt);
var
  i, nn: integer;
begin
  assert(length(_dims) > 0, 'zero dims are not allowed');

  fDims := system.copy(_dims);
  fNDims := length(fDims);

  fNItems := 1;
  for i in fDims do
    fNItems *= i;
  hitems := fNItems - 1;

  fSizeBytes := fNItems * sizeof(T);
  fCmpSize := 0;

  assert(fNItems > 0, 'zero dims not supported');

  setLength(fData, fNItems);
  // zero;

  nn := 1;
  setLength(fMlt, fNDims);  // mtl factor to improve indexing
  for i := 0 to high(fDims) do
  begin
    fMlt[high(fMlt) - i] := nn;
    nn *= fDims[high(fDims) - i];
  end;

  setPointers;

  _typeInfo := PTypeInfo(TypeInfo(T)); // data type of T
  fDataType := dtUnknown;

  case _typeInfo^.Kind of
    tkInteger: fDataType := dtint;
    tkFloat: begin
      if CompareStr(_typeInfo^.Name, 'Single') = 0 then fDataType := dtf32
      else
      if CompareStr(_typeInfo^.Name, 'Double') = 0 then fDataType := dtf64
      else
      if CompareStr(_typeInfo^.Name, 'Real') = 0 then fDataType := dtf64;
    end;
    tkRecord: begin
      if CompareStr(_typeInfo^.Name, 'fp80') = 0 then fDataType := dtf80
      else
      if CompareStr(_typeInfo^.Name, 'fp128') = 0 then fDataType := dtf128;
    end;
    else
      fDataType := dtUnknown;
  end;

  assert(fDataType <> dtUnknown, 'data type not supported');
end;

constructor TNumPas<T>.Create(const _dims: TArrInt; _pt: PT);
begin
  self := Create(_dims).copy;
  self.fromBuffer(_pt);
end;

constructor TNumPas<T>.Create(const a: TNumPas<T>);
begin
  self := Create(a.fDims);

  fData := system.copy(a.fData);
  setPointers;
end;

constructor TNumPas<T>.rand(_dims: TArrInt);
begin
  self := Create(_dims);
  rand;
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

constructor TNumPas<T>.vander(n: integer);
var
  r, c: integer;
begin
  assert(n > 0, 'vander matrix N<=0');
  self := NP.Create([n, n]);

  for r := 0 to self.hi(1) do
    for c := 0 to self.hi(0) do
      self[r, c] := power(r + 1, c);
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

procedure TNumPas<T>.swap(i, j: integer);
var
  tmp: T;
begin
  tmp := self[i];
  self[i] := self[j];
  self[j] := tmp;
end;

procedure TNumPas<T>.setPointers;
begin
  fpint := pinteger(@fData[0]); // pointers to data
  fpf32 := psingle(@fData[0]);
  fpf64 := pdouble(@fData[0]);
  fpf80 := pfp80(@fData[0]);
  fpf128 := pfp128(@fData[0]);
  fpdata := @fData[0];
end;

function TNumPas<T>.index2Dims(index: integer): TArrInt;
var
  i, ih, td: integer;
begin
  Result := nil;
  setLength(Result, fNDims);

  for i := 0 to high(fDims) do
  begin
    ih := high(fDims) - i;
    td := fDims[ih];
    Result[ih] := index mod td;
    index := index div td;
  end;
end;

function TNumPas<T>.sqr(x: T): T; inline;
begin
  Result := x * x;
end;

function TNumPas<T>.getNThreads: integer;
begin
  {$ifdef windows}
  result:=getCPUCount;
  {$else}
  Result := GetSystemThreadCount;
  {$endif}
end;

function TNumPas<T>.transposedCoord(index: integer): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to pred(fNDims) do
  begin
    if index = 0 then break;

    Result += (index mod fDims[i]) * fMlt[i];
    index := index div fDims[i];
  end;
end;

function TNumPas<T>.prod(a: TArrInt): int64;
var
  i: integer;
begin
  if a = nil then exit(1);

  Result := 1;
  for i in a do Result *= i;
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
  if r = 0 then Result := fData[c]
  else
    Result := fData[r * dim(0) + c];
end;

procedure TNumPas<T>.rput(r, c: integer; AValue: T);
begin
  if r = 0 then fData[c] := Avalue
  else
    fData[r * dim(0) + c] := Avalue;
end;

function TNumPas<T>.nDims: integer;
begin
  Result := fNDims;
end;

function TNumPas<T>.combinations(const inArr: TArrInt): TArrArrInt;
var
  currComb: TArrInt = nil;
  wArr: TArrInt = nil;

  function genCombs(ix: integer): TArrArrInt;
  var
    i: integer;
  begin
    Result := nil;
    if ix = length(wArr) then
      Result := [system.copy(currComb)]
    else
      for i := 0 to pred(wArr[ix]) do
      begin
        currComb[ix] := i;
        Result += [genCombs(succ(ix))];
      end;
  end;

begin
  if inArr = nil then wArr := fDims
  else
    wArr := inArr;

  setLength(currComb, length(wArr));
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
  widx: TArrInt = nil;
  i: integer;
begin
  setLength(widx, fNDims);
  for i := 0 to high(widx) do
    if i <= high(slc) then widx[i] := slc[i]
    else
      widx[i] := 0;

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
begin
  Result := checkDims(fDims);
end;

function TNumPas<T>.checkDims(_dims: TArrInt): boolean;
var
  i: integer;
begin
  if length(_dims) = 0 then exit(False);
  for i in _dims do if i <= 0 then exit(False);
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
  for i := 0 to hitems do
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
      dtf80: Result += fp80ToStr(fpf80[i]);
      dtf128: Result += f128tos(fpf128[i]);
      else;
    end;
  end;
end;

function TNumPas<T>.npToStr(r, c, nDecs: integer): string;
var
  index: integer;
begin
  Result := '';
  index := r * dim(0) + c;
  case fDataType of
    dtint: Result := format('%d', [fpint[index]]);
    dtf32: Result := format('%.*f', [nDecs, fpf32[index]]);
    dtf64: Result := format('%.*f', [nDecs, fpf64[index]]);
    dtf80: Result := fp80ToStr(fpf80[index], nDecs);
    dtf128: Result := f128tos(fpf128[index]);
    else;
  end;
end;


function TNumPas<T>.toPython(varName: string): string;
var
  i: integer;
begin
  Result := varName + '=np.array([';
  for i := 0 to hitems do
  begin
    case fDataType of
      dtint: Result += format('%d', [fpint[i]]);
      dtf32: Result += format('%e', [fpf32[i]]);
      dtf64: Result += format('%e', [fpf64[i]]);
      dtf80: Result += fp80toStr(fpf80[i]);
      dtf128: Result += f128tos(fpf128[i]);
      else;
    end;
    if i <> hitems then Result += ', ';
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

procedure TNumPas<T>.one;
begin
  setValue(1);
end;

procedure TNumPas<T>.rand;
var
  i: integer = 0;
begin
  case fDataType of
    dtint: for i := 0 to hitems do fpint[i] := random(10000);
    dtf32: for i := 0 to hitems do fpf32[i] := random; // fp32
    dtf64: for i := 0 to hitems do fpf64[i] := random; // TsRandf64(seed); // fp64
    dtf80: for i := 0 to hitems do fpf80[i] := random;
    dtf128: for i := 0 to hitems do fpf128[i] := random;
    else;
  end;
end;

procedure TNumPas<T>.randMT;   // slower than ST mode as TsRnd is much slower than random

  procedure _randMT(ix: PtrInt; {%H-}pnt: pointer; Item: TMultiThreadProcItem);
  var
    delta: integer;
    seed: integer;
  begin
    delta := Item.Group.EndIndex + 1;
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
      dtf80: while ix < fNItems do
        begin
          fpf80[ix] := TsRandf64(seed);
          Inc(ix, delta);
        end;
      else;
    end;
  end;

begin
  ProcThreadPool.DoParallelLocalProc(@_randMT, 0, getNThreads - 1, nil);
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
  assert((index < fNItems) or (index < 0), format('index %d out of range %d',
    [index, hitems]));
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
  for i := 0 to hitems do fData[i] := v;
end;

procedure TNumPas<T>.setValue(const slc: TArrInt; v: T);
var
  i: integer;
begin
  for i := startSlice(slc) to endSlice(slc) do fData[i] := v;
end;

procedure TNumPas<T>.toBuffer(v: pointer);
begin
  move(fData[0], v^, fSizeBytes);
end;

procedure TNumPas<T>.fromBuffer(v: PT);
begin
  move(v^, fData[0], fSizeBytes);
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

function TNumPas<T>.std: T;
var
  m, s: T;
  i: integer;
begin // sigma = sqrt(1/n * sum(sqr(x-mean)))
  m := mean;
  s := 0;
  for i := 0 to pred(fNItems) do s += sqr(self[i] - m);
  Result := sqrt(s / fNItems);
end;

procedure TNumPas<T>.seq; // fill with seq # 0..pred(size)
var
  i: integer;
begin
  for i := 0 to pred(size) do self[i] := i;
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
  for i := 0 to a.hitems do self[i + aStart] := a[i];
end;

{
 numpy binary read w/ np.fromfile

 a = np.fromfile("a.bin", dtype=np.double).reshape(fDims)
}
procedure TNumPas<T>.toFile(Name: string);
var
  fh: THandle;
  bw: integer;
begin
  try
    fh := fileCreate(Name);
    bw := fileWrite(fh, fData[0], fSizeBytes);
    assert(bw = fSizeBytes, 'error writing binary file');
  finally
    fileClose(fh);
  end;
end;

procedure TNumPas<T>.save(Name: string);

  function dupeChar(ch: char; n: integer): string;
  var
    i: integer;
  begin
    Result := '';
    setLength(Result, n);
    for i := 1 to n do Result[i] := ch;
  end;

var
  fh: THandle;
  hr: TFileNPY;
  desc: string;
  szHdr: integer;
begin
  try
    fh := FileCreate(Name + '.npy');

    // length(desc)+len(header)+#$0a must be divisible by 64 for alignment purposes.
    // 128-(6+1+1+2)=118
    desc := format('{''descr'':''<f%d'', ''fortran_order'':False, ''shape'':(%s),}',
      [sizeof(T), dimsStr]);

    szHdr := sizeof(TFileNPY) + length(desc) + 1; // header size + $0a
    desc += dupeChar(' ', 64 * succ(szHdr div 64) - szHdr) + #$0a;

    with hr do
    begin
      id := _id;
      mayVer := 1;
      minVer := 0;
      headLen := length(desc);
    end;

    FileWrite(fh, hr, sizeof(hr));        // header
    FileWrite(fh, desc[1], length(desc)); // desc
    FileWrite(fh, fData[0], fSizeBytes);  // data

  finally
    FileClose(fh);
  end;
end;

{ this is a limited reader as it asumes same format as current NumPas<T>, no fortran mode}
function TNumPas<T>.load(Name: string): NP;
var
  fh: THandle;
  hr: TFileNPY;
  desc: string = '';
  _dims: TArrInt;
begin
  try
    fh := fileOpen(Name + '.npy', fmOpenRead);
    fileRead(fh, hr, sizeof(hr)); // len(magic string) + 2 + len(length) + HEADER_LEN
    setLength(desc, hr.headLen);
    fileRead(fh, desc[1], length(desc));

    // just scan shape, TD: parse dict sentence, i.e.:
    // {'desc':'<f8', 'fortran_mode':False, 'shape':(100,),}

    // extract shape
    desc := rightStr(desc, length(desc) - pos('(', desc));
    desc := leftStr(desc, pos(')', desc) - 1);

    _dims := strToDims(desc);

    if checkDims(_dims) then
    begin
      Result := NP.Create(_dims);
      fileRead(fh, Result.fData[0], Result.fSizeBytes);
    end;
  finally
    FileClose(fh);
  end;
end;

procedure TNumPas<T>.fromFile(Name: string);
var
  fh: THandle;
  br: integer;
begin
  try
    fh := fileOpen(Name, fmOpenRead);
    br := fileRead(fh, fData[0], fSizeBytes);
    assert(br = fSizeBytes, 'error reading binary file');
    // read only supported size
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
  tdims: TArrArrInt;
  a: TNumPas<T>;
begin
  if fNDims = 1 then Result := flatSort // 1d sort
  else
  begin
    Result := copy;

    tdims := combinations(system.Copy(fDims, 0, fNDims - 1));
    for tdim in tdims do // sort last
    begin
      a := slice(tdim);
      a._flatSort;
      Result.Assign(a, tdim);
    end;
  end;
end;

function TNumPas<T>.sortMT: NP;
var
  tdims: TArrArrInt;

  procedure _sortMT(ix: PtrInt; {%H-}pnt: pointer; Item: TMultiThreadProcItem);
  var
    delta: integer;
    a: TNumPas<T>;
  begin
    delta := Item.Group.EndIndex + 1;

    while ix < length(tdims) do
    begin
      a := slice(tdims[ix]);
      a._flatSort;
      Result.Assign(a, tdims[ix]);

      Inc(ix, delta);
    end;
  end;

begin
  if fNDims = 1 then Result := flatSort // 1d sort
  else
  begin
    Result := copy;
    tdims := combinations(system.Copy(fDims, 0, fNDims - 1));

    ProcThreadPool.DoParallelLocalProc(@_sortMT, 0, getNThreads - 1, nil);
  end;
end;

function TNumPas<T>.diagonal: NP;
var
  i: integer;
begin
  assert((fNDims = 2) and (hi(0) = hi(1)), 'non quadratic');
  Result := TNumPas<T>.zeros([dim(0)]);
  for i := 0 to hi(0) do Result[i] := self[i, i];
end;

function TNumPas<T>.apply(foo: TNPFunction): NP;
var
  i: integer;
begin
  Result := copy;
  for i := 0 to hitems do Result[i] := foo(self[i]);
end;


function TNumPas<T>.isQuadratic: boolean;
begin
  Result := (fNDims >= 2) and (hi(0) = hi(1));
end;

// does not use any additional buffer for compression
// using only fpData -> fData[]

procedure TNumPas<T>.compress;
var
  inStream, compStream: TMemoryStream;
begin
  if fNitems = 0 then exit; // already compressed

  inStream := TMemoryStream.Create;    // populate inStream << fpData(fSizeBytes)
  inStream.SetSize(fSizeBytes);        // this dups fData
  inStream.Write(fpData^, fSizeBytes);
  inStream.Seek(0, soFromBeginning);

  compStream := TMemoryStream.Create;

  with TCompressionStream.Create(clMax, compStream) do
  begin // compress
    CopyFrom(inStream, 0);  { compress inStream }
    Free; // once free or flush, result is available
  end;
  inStream.Free;

  fCmpSize := compStream.Size; // fpData << compStream,  compressed < original!!
  setLength(fData, (fCmpSize div sizeof(T)) + 1);
  setPointers;
  compStream.Position := 0;
  compStream.ReadBuffer(fpData^, fCmpSize);

  compStream.Free;

  fNitems := 0; // fpData contains compressed data, not accesible!!
  fSizeBytes := 0;
end;

procedure TNumPas<T>.deCompress;
var
  zdc: TDecompressionStream;
  compStream: TMemoryStream;
begin
  if fNitems <> 0 then exit; // fpData is not compressed

  compStream := TMemoryStream.Create; // populate compStream w/fpData
  compStream.SetSize(fCmpSize);
  compStream.Write(fpData^, fCmpSize);
  compStream.Position := 0;

  with TMemoryStream.Create do
  begin

    zdc := TDecompressionStream.Create(compStream);
    CopyFrom(zdc, 0);

    zdc.Free;
    compStream.Free;

    fSizeBytes := Size;
    fNItems := fSizeBytes div sizeof(T);

    Position := 0;
    setLength(fData, fSizeBytes);
    setPointers;
    ReadBuffer(fpData^, fSizeBytes); // copy to fpData

    Free;
  end;
end;

function TNumPas<T>.reshape(ndim: TArrInt): NP;
begin
  assert(size = prod(ndim), 'incompatible shapes');

  Result := NP.Create(ndim);
  move(fData[0], Result.fData[0], fSizeBytes);
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

        Result[ixs] := p;
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
    else
      assert(False, 'internal error in dot');
  end;
end;

function TNumPas<T>.dotMT(a: TNumPas<T>): TNumPas<T>; // unfinished!
var
  pivotPos, pivd, ppivd: integer;
  prs, pra: integer;
  adim0, ahi0, ahi1, aStride: integer; // ranges
  raDim, aDim, sDim, resDim: TArrInt;

  procedure _dot1x1MT(ix: PtrInt; {%H-}pnt: pointer; Item: TMultiThreadProcItem);
  var
    delta, sStart, r: integer;
    p: T;
  begin
    delta := Item.Group.EndIndex + 1;


    while ix < prs do
    begin
      sStart := ix * pivd;

      p := 0;   // calc dot 1x1
      for r := 0 to ppivd do p += self[sStart + r] * a[r];
      Result[ix] := p;

      Inc(ix, delta);
    end;

  end;

  procedure _dot1x2MT(ix: PtrInt; {%H-}pnt: pointer; Item: TMultiThreadProcItem);
  var
    delta, sStart, aStart, r, c, ixa, ixr: integer;
    p: T;
  begin
    delta := Item.Group.EndIndex + 1;

    while ix < prs do
    begin

      sStart := ix * pivd;
      ixr := ix * (pra * adim0);

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

      Inc(ix, delta);
    end;
  end;

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

  case pivotPos of
    0: ProcThreadPool.DoParallelLocalProc(@_dot1x1MT, 0, getNThreads - 1, nil);
    1: ProcThreadPool.DoParallelLocalProc(@_dot1x2MT, 0, getNThreads - 1, nil);
    else
      assert(False, 'internal error in dot');
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
  for i := 0 to hitems do p += self.fData[i] * a.fData[i];
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
  assert(isQuadratic, 'not quadratic matrix');

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

function TNumPas<T>.detMT: NP;
var
  tdim: TArrInt;
  tdims: TArrArrInt;

  procedure _detMT(ix: PtrInt; {%H-}pnt: pointer; Item: TMultiThreadProcItem);
  var
    delta: integer;
  begin
    delta := Item.Group.EndIndex + 1;

    while ix < length(tdims) do
    begin
      {%H-}Result[tdims[ix]] := slice(tdims[ix]).detBareiss;
      Inc(ix, delta);
    end;
  end;

begin
  assert(isQuadratic, 'not quadratic matrix');

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

    ProcThreadPool.DoParallelLocalProc(@_detMT, 0, getNThreads - 1, nil);
  end;
end;

function TNumPas<T>.detBareiss: T;  // 2 dims n x n works on copy
var
  factor: T;
  i, j, k, hi0: integer;
  a: TNumPas<T>;
begin
  assert((fNDims = 2) and (hi(0) = hi(1)), 'not quadratic matrix');

  Result := 1.0;
  a := copy;

  hi0 := hi(0);

  for i := 0 to hi0 do
  begin
    for j := i + 1 to hi0 do
    begin
      if a[i, i] <> 0 then
      begin
        factor := a[j, i] / a[i, i];
        for k := i to hi0 do
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

function TNumPas<T>.invMT: NP;
var
  tdim: TArrInt;
  tdims: TArrArrInt;

  procedure _invMT(ix: PtrInt; {%H-}pnt: pointer; Item: TMultiThreadProcItem);
  var
    delta: integer;
  begin
    delta := Item.Group.EndIndex + 1;

    while ix < length(tdims) do
    begin
      Result.Assign(slice(tdims[ix]).inv_nxn, tdims[ix]);
      Inc(ix, delta);
    end;
  end;

begin
  assert((fNDims >= 2) and (hi(0) = hi(1)), 'not quadratic matrix');

  Result := copy;

  if fNDims = 2 then
    Result := inv_nxn
  else
  begin
    tdim := system.Copy(fDims, 0, fNDims - 2);
    tdims := combinations(tdim);

    ProcThreadPool.DoParallelLocalProc(@_invMT, 0, getNThreads - 1, nil);
  end;
end;

function TNumPas<T>.solve(b: TNumPas<T>): TNumPas<T>;
begin
  Result := inv.dot(b);
end;

function TNumPas<T>.transpose: NP;
var
  i: integer;
begin
  Result := NP.Create(revArrInt(fDims));   // shape = self.reversed dims

  for i := 1 to pred(size div 2) - 1 do
    Result[Result.transposedCoord(i)] := self[i];
end;

function TNumPas<T>.transposeMT: NP;

  procedure _transMT(ix: PtrInt; {%H-}pnt: pointer; Item: TMultiThreadProcItem);
  var
    delta: integer;
  begin
    delta := Item.Group.EndIndex + 1;

    while ix < fNItems do
    begin
      Result[Result.transposedCoord(ix)] := self[ix];
      Inc(ix, delta);
    end;
  end;

begin
  Result := NP.Create(revArrInt(fDims));   // shape = self.reversed dims

  ProcThreadPool.DoParallelLocalProc(@_transMT, 0, getNThreads - 1, nil);
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
  for i := 0 to hitems do if fData[i] <> v then exit(False);
  Result := True;
end;

function TNumPas<T>.allNE(const v: T): boolean;
var
  i: integer;
begin
  for i := 0 to hitems do if fData[i] = v then exit(False);
  Result := True;
end;

function TNumPas<T>.dims: TArrInt;
begin
  Result := fDims;
end;

function TNumPas<T>.dimsStr: string;
var
  i: integer;
begin
  Result := '';
  for i := low(fDims) to high(fDims) do
  begin
    Result += IntToStr(fDims[i]);
    if i < high(fDims) then Result += ',';
  end;
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
  for i := 0 to a.hitems do Result.fData[i] += b.fData[i];
end;

class operator TNumPas<T>.+(const a: TNumPas<T>; b: integer): TNumPas<T>;
var
  i: integer;
begin
  Result := a.copy;
  for i := 0 to a.hitems do Result.fData[i] += b;
end;

class operator TNumPas<T>.+(const a: TNumPas<T>; b: single): TNumPas<T>;
var
  i, rb: integer;
begin
  Result := a.copy;
  rb := round(b);
  case a.fDataType of
    dtint: for i := 0 to a.hitems do Result.fpint[i] += rb;
    dtf32: for i := 0 to a.hitems do Result.fpf32[i] += b;
    dtf64: for i := 0 to a.hitems do Result.fpf64[i] += b;
    dtf80: for i := 0 to a.hitems do Result.fpf80[i] += b;
    dtf128: for i := 0 to a.hitems do Result.fpf128[i] += b;
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
    dtint: for i := 0 to a.hitems do Result.fpint[i] += rb;
    dtf32: for i := 0 to a.hitems do Result.fpf32[i] += b;
    dtf64: for i := 0 to a.hitems do Result.fpf64[i] += b;
    dtf80: for i := 0 to a.hitems do Result.fpf80[i] += b;
    dtf128: for i := 0 to a.hitems do Result.fpf128[i] += b;
    else;
  end;
end;

class operator TNumPas<T>.-(const a, b: TNumPas<T>): TNumPas<T>;
var
  i: integer;
begin
  assert(a.sameDims(b), 'can''t sub with different simensions');
  Result := a.copy;
  for i := 0 to a.hitems do Result.fData[i] -= b.fData[i];
end;

class operator TNumPas<T>.-(const a: TNumPas<T>; b: integer): TNumPas<T>;
var
  i: integer;
begin
  Result := a.copy;
  for i := 0 to a.hitems do Result.fData[i] -= b;
end;

class operator TNumPas<T>.-(const a: TNumPas<T>; b: single): TNumPas<T>;
var
  i, rb: integer;
begin
  Result := a.copy;
  rb := round(b);
  case a.fDataType of
    dtint: for i := 0 to a.hitems do Result.fpint[i] -= rb;
    dtf32: for i := 0 to a.hitems do Result.fpf32[i] -= b;
    dtf64: for i := 0 to a.hitems do Result.fpf64[i] -= b;
    dtf80: for i := 0 to a.hitems do Result.fpf80[i] -= b;
    dtf128: for i := 0 to a.hitems do Result.fpf128[i] -= b;
  end;
end;

class operator TNumPas<T>.-(const a: TNumPas<T>; b: double): TNumPas<T>;
var
  i, rb: integer;
begin
  Result := a.copy;
  rb := round(b);
  case a.fDataType of
    dtint: for i := 0 to a.hitems do Result.fpint[i] -= rb;
    dtf32: for i := 0 to a.hitems do Result.fpf32[i] -= b;
    dtf64: for i := 0 to a.hitems do Result.fpf64[i] -= b;
    dtf80: for i := 0 to a.hitems do Result.fpf80[i] -= b;
    dtf128: for i := 0 to a.hitems do Result.fpf128[i] -= b;
  end;
end;

class operator TNumPas<T>.*(const a, b: TNumPas<T>): TNumPas<T>;
var
  i: integer;
begin
  assert(a.sameDims(b), 'can''t * with different simensions');

  Result := a.copy;
  for i := 0 to a.hitems do Result[i] := Result[i] * b[i];
end;


class operator TNumPas<T>.*(const a: TNumPas<T>; b: integer): TNumPas<T>;
var
  i: integer;
begin
  Result := a.copy;
  for i := 0 to a.hitems do Result.fData[i] *= b;
end;

class operator TNumPas<T>.*(const a: TNumPas<T>; b: single): TNumPas<T>;
var
  i, rb: integer;
begin
  Result := a.copy;
  rb := round(b);
  case a.fDataType of
    dtint: for i := 0 to a.hitems do Result.fpint[i] *= rb;
    dtf32: for i := 0 to a.hitems do Result.fpf32[i] *= b;
    dtf64: for i := 0 to a.hitems do Result.fpf64[i] *= b;
    dtf80: for i := 0 to a.hitems do Result.fpf80[i] *= b;
    dtf128: for i := 0 to a.hitems do Result.fpf128[i] *= b;
  end;
end;

class operator TNumPas<T>.*(const a: TNumPas<T>; b: double): TNumPas<T>;
var
  i, rb: integer;
begin
  Result := a.copy;
  rb := round(b);
  case a.fDataType of
    dtint: for i := 0 to a.hitems do Result.fpint[i] *= rb;
    dtf32: for i := 0 to a.hitems do Result.fpf32[i] *= b;
    dtf64: for i := 0 to a.hitems do Result.fpf64[i] *= b;
    dtf80: for i := 0 to a.hitems do Result.fpf80[i] *= b;
    dtf128: for i := 0 to a.hitems do Result.fpf128[i] *= b;
  end;
end;

class operator TNumPas<T>./(const a, b: TNumPas<T>): TNumPas<T>;
var
  i: integer;
begin
  assert(a.sameDims(b), 'can''t / with different dimensions');
  Result := a.copy;
  case a.fDataType of
    dtint: for i := 0 to a.hitems do Result.fpint[i] := a.fpint[i] div b.fpint[i];
    dtf32: for i := 0 to a.hitems do Result.fpf32[i] /= b.fpf32[i];
    dtf64: for i := 0 to a.hitems do Result.fpf64[i] /= b.fpf64[i];
    dtf80: for i := 0 to a.hitems do Result.fpf80[i] /= b.fpf80[i];
    dtf128: for i := 0 to a.hitems do Result.fpf128[i] /= b.fpf128[i];
  end;
end;

class operator TNumPas<T>./(const a: TNumPas<T>; b: integer): TNumPas<T>;
var
  i: integer;
begin
  Result := a.copy;

  case a.fDataType of
    dtint: for i := 0 to a.hitems do Result.fpint[i] := a.fpint[i] div b;
    dtf32: for i := 0 to a.hitems do Result.fpf32[i] /= b;
    dtf64: for i := 0 to a.hitems do Result.fpf64[i] /= b;
    dtf80: for i := 0 to a.hitems do Result.fpf80[i] /= b;
    dtf128: for i := 0 to a.hitems do Result.fpf128[i] /= b;
  end;
end;

class operator TNumPas<T>./(const a: TNumPas<T>; b: single): TNumPas<T>;
var
  i, rb: integer;
begin
  Result := a.copy;
  rb := round(b);
  case a.fDataType of
    dtint: for i := 0 to a.hitems do Result.fpint[i] := a.fpint[i] div rb;
    dtf32: for i := 0 to a.hitems do Result.fpf32[i] /= b;
    dtf64: for i := 0 to a.hitems do Result.fpf64[i] /= b;
    dtf80: for i := 0 to a.hitems do Result.fpf80[i] /= b;
    dtf128: for i := 0 to a.hitems do Result.fpf128[i] /= b;
  end;
end;

class operator TNumPas<T>./(const a: TNumPas<T>; b: double): TNumPas<T>;
var
  i, rb: integer;
begin
  Result := a.copy;
  rb := round(b);
  case a.fDataType of
    dtint: for i := 0 to a.hitems do Result.fpint[i] := a.fpint[i] div rb;
    dtf32: for i := 0 to a.hitems do Result.fpf32[i] /= b;
    dtf64: for i := 0 to a.hitems do Result.fpf64[i] /= b;
    dtf80: for i := 0 to a.hitems do Result.fpf80[i] /= b;
    dtf128: for i := 0 to a.hitems do Result.fpf128[i] /= b;
  end;
end;

// logical

class operator TNumPas<T>.=(const a, b: TNumPas<T>): boolean;
var
  i: integer;
begin
  assert(a.sameDims(b), 'can''t compare with different dimensions');

  for i := 0 to a.hitems do if a.fData[i] <> b.fData[i] then exit(False);
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
  assert(a.sameDims(b), 'can''t compare with different dimensions');

  for i := 0 to a.hitems do if a.fData[i] <= b.fData[i] then exit(False);
  Result := True;
end;

class operator TNumPas<T>.<(const a, b: TNumPas<T>): boolean;
var
  i: integer;
begin
  assert(a.sameDims(b), 'can''t compare with different dimensions');

  for i := 0 to a.hitems do if a.fData[i] >= b.fData[i] then exit(False);
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

function revArrInt(const a: TArrInt): TArrInt;
var
  i, j, imax: dword;
  Tmp: integer;
begin
  if Length(a) = 0 then exit(nil);
  imax := high(a);
  Result := system.copy(a);

  for i := imax div 2 downto 0 do
  begin
    j := imax - i;
    Tmp := Result[i];
    Result[i] := Result[j];
    Result[j] := tmp;
  end;
end;

function strToDims(sd: string): TarrInt;
var
  nums: TStringArray;
  s: string;
begin
  nums := sd.split([' ', ',', '(', ')', '[', ']'], TStringSplitOptions.ExcludeEmpty);
  Result := nil;
  if length(nums) > 0 then
    for s in nums do if StrToInt(s) > 0 then Result += [StrToInt(s)];
end;

initialization
  randomize;

finalization

end.
