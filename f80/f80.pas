{
 80 bit fp80 support
 this is the extended float type not supported in win64

 range: 3.65 × 10−4951 to 1.18 × 10+4932

 intel asm equivalence

 byte = uint8
 word = uint16
 dword = uint32
 qword = uint64
 tbyte = ten byte = 80 bits
 }

unit f80;

{$mode ObjFPC}{$H+}
{$asmmode intel}

interface

uses
  Classes, SysUtils, Math;

type
  float80 = packed array [0..9] of byte;  // 80 bits : 10 bytes
  fp80 = float80;


  TF80 = packed record   // float80 single
    case byte of
      0: (
        r: bitpacked record
          mant: uint64;        // 64-bit mantissa
          expo: 0..32768 - 1;  // 15-bit exponent
          sign: 0..1;          // 1-bit sign
          end;
      );
      1: (
        v: fp80;
      );
  end;

  fp128 = packed array[0..15] of byte;

  TF128 = bitpacked record
    mant: packed array[0..13] of byte;  // 112-bit mantissa
    expo: 0..32768 - 1;         // 15-bit exponent
    sign: 0..1;                 // 1-bit sign
  end;

const
  ten: fp80 = (0, 0, 0, 0, 0, 0, 0, 160, 2, 64);

operator := (const a: double): fp80;
operator := (const a: fp80): double;
operator := (const s: string): fp80;

operator +(const a, b: fp80): fp80; assembler;
operator -(const a, b: fp80): fp80; assembler;
operator -(const a: fp80): fp80; assembler;
operator *(const a, b: fp80): fp80; assembler;
operator /(const a, b: fp80): fp80;

operator =(const a, b: fp80): boolean; assembler;
operator <>(const a, b: fp80): boolean;
operator >(const a, b: fp80): boolean; assembler;
operator <(const a, b: fp80): boolean; assembler;
operator >=(const a, b: fp80): boolean;
operator <=(const a, b: fp80): boolean;

operator **(const a, b: fp80): fp80; assembler;

// trigs
function sin(x: fp80): fp80; assembler;
function cos(x: fp80): fp80; assembler;
function tan(x: fp80): fp80; assembler;
function atan(x, y: fp80): fp80; assembler;


// funcs
function zero: fp80; assembler;
function one: fp80; assembler;
function pi: fp80; assembler;
function log10_2: fp80; assembler;
function cpuid(code: integer): integer;
function rand: fp80;

function sqrt(x: fp80): fp80; assembler;
function abs(x: fp80): fp80; assembler;
function remainder(x, y: fp80): fp80; assembler; // mod
function exp(x: fp80): fp80; assembler;
function log(x: fp80): fp80; assembler;

function strToFP80(const s: string): fp80;
function fp80ToStr(x: fp80; decimals: integer = 0): string;

function IsNaN(x: fp80): boolean; assembler;
function IsInfinite(x: fp80): boolean; assembler;
procedure extractMantExp(const a: fp80; var mant: fp80; var expo: fp80); assembler;
function combineMantExp(const mant: fp80; const expo: fp80): fp80;
// mant * power(2, expo)

implementation

operator := (const a: double): fp80;
begin
  asm
           FLD     qword[a]
           MOV     R11,tbyte[result]       // tbyte = ten byte prefix
           FSTP    tbyte[R11]
  end;
end;

operator := (const a: fp80): double;
begin
  asm
           MOV     R11,[a]     // r11=adress a
           FLD     tbyte[R11]
           FSTP    [result]
  end;
end;

operator := (const s: string): fp80;
begin
  Result := strToFP80(s);
end;

operator +(const a, b: fp80): fp80; assembler; nostackframe;
asm
         FLD     tbyte[a]       // rdx
         FLD     tbyte[b]       // r8

         FADDP    ST(1),ST(0)

         FSTP    tbyte[result]  // rcx
end;

operator -(const a, b: fp80): fp80; assembler; nostackframe;
asm
         FLD     tbyte[a]
         FLD     tbyte[b]

         FSUBP    ST(1),ST(0)

         FSTP    tbyte[Result]
end;

operator -(const a: fp80): fp80; assembler;
asm
         FLD     tbyte[a]
         FCHS
         FSTP    tbyte[Result]
end;

operator *(const a, b: fp80): fp80; assembler; nostackframe;
asm
         FLD     tbyte[a]
         FLD     tbyte[b]

         FMULP    ST(1),ST(0)

         FSTP    tbyte[Result]
end;

operator /(const a, b: fp80): fp80; assembler; nostackframe;
asm
         FLD     tbyte[a]
         FLD     tbyte[b]

         FDIVP    ST(1),ST(0)

         FSTP    tbyte[Result]
end;

// logical

operator =(const a, b: fp80): boolean; assembler; nostackframe;
asm
         FLD     tbyte[a]
         FLD     tbyte[b]

         FCOMIP   ST(0),ST(1) // compare st0,st1 to cpu flags

         JZ      @0 // eq
         MOV     [result],0
         JMP     @1
         @0:
         MOV     [result],1
         @1:
         FSTP    ST(0) // pop
end;

operator <>(const a, b: fp80): boolean;
begin
  Result := not (a = b);
end;

operator >(const a, b: fp80): boolean; assembler; nostackframe;
asm
         FLD     tbyte[a]
         FLD     tbyte[b]

         FCOMIP   ST(0),ST(1) // compare st0,st1 to cpu flags

         JC      @0 // >
         MOV     [result],0
         JMP     @1
         @0:
         MOV     [result],1
         @1:
         FSTP    ST(0)
end;

operator <(const a, b: fp80): boolean; assembler; nostackframe;
asm
         FLD     tbyte [a]
         FLD     tbyte [b]

         FCOMIP   ST(0),ST(1) // compare st0,st1 to cpu flags

         JZ      @2 // = -> false
         JNC     @0 // <
         @2:
         MOV     [result],0
         JMP     @1
         @0:
         MOV     [result],1
         @1:
         FSTP    ST(0)
end;

operator >=(const a, b: fp80): boolean;
begin
  Result := not (a < b);
end;

operator <=(const a, b: fp80): boolean;
begin
  Result := not (a > b);
end;

operator **(const a, b: fp80): fp80; assembler; nostackframe;
asm
         FLD     tbyte [b]
         FLD     tbyte [a]

         FYL2X // y*log2(x)

         FLD1
         FLD     ST(1)
         FPREM
         F2XM1
         FADDP      ST(1), ST(0)
         FSCALE
         FSTP       ST(1)

         FSTP     tbyte [result]
end;

function sin(x: fp80): fp80; assembler; nostackframe;
asm
         FLD     tbyte[x]
         FSIN
         FSTP    tbyte[result]
end;

function cos(x: fp80): fp80; assembler; nostackframe;
asm
         FLD     [x]
         FCOS
         FSTP    tbyte[result]
end;

function tan(x: fp80): fp80; assembler; nostackframe;
asm
         FLD     tbyte[x]
         FPTAN
         FSTP    tbyte[result]
end;

function atan(x, y: fp80): fp80; assembler; nostackframe;
asm
         FLD     tbyte[y]
         FLD     tbyte[x]
         FPATAN
         FSTP    tbyte[result]
end;

function zero: fp80; assembler;
asm
         FLDZ
         FSTP    tbyte[result]
end;

function one: fp80; assembler;
asm
         FLD1
         FSTP    tbyte[result]
end;

function pi: fp80; assembler;
asm
         FLDPI
         FSTP    tbyte[result]
end;

function log10_2: fp80; assembler;
asm
         FLDLG2
         FSTP    tbyte[result]
end;

function cpuid(code: integer): integer;
begin
  asm
           MOV     EAX,[code]
           CPUID
           MOV     [result],EAX
  end;
end;

function rand: fp80;
var
  v: uint64;
begin
  asm
           RDRAND  R11

           MOV     [v],R11 // int64 rand
           FILD     [v]

           MOV     R12,$7fffffffffffffff  // maxint64
           MOV     [v],R12
           FILD    [v]

           FDIVP   ST(1),ST(0)  // div to 0..1 range
           FABS

           MOV     R11,tbyte[result] // store result
           FSTP    tbyte[R11]
  end;
end;

function sqrt(x: fp80): fp80; assembler; nostackframe;
asm
         FLD     tbyte[x]
         FSQRT
         FSTP    tbyte[result]
end;

function abs(x: fp80): fp80; assembler; nostackframe;
asm
         FLD     tbyte[x]
         FABS
         FSTP    tbyte[result]
end;

function remainder(x, y: fp80): fp80; assembler; nostackframe;
asm
         FLD     tbyte[y]
         FLD     tbyte[x]
         FPREM
         FSTP    tbyte[result]
end;

function exp(x: fp80): fp80; assembler;
asm
         // Calculate e^x using the relationship e^x = 2^(x * log2(e))
         FLD     tbyte[x]           // Load x
         FLDL2E              // Load log base 2 of e onto the stack
         FMUL    ST(0),ST(1) // Multiply st(0) by st(1) to get x * log2(e)
         FLD1                // Load the constant 1 onto the stack
         FLD     ST(1)       // Copy the result of x * log2(e) to the top of the stack
         FPREM               // Compute the partial remainder of st(0) / st(1)
         F2XM1               // Compute 2^(st(0)) - 1
         FADDP   ST(1), ST   // Add the result from F2XM1 to 1
         FSCALE              // Scale by power of 2, using the value in st(1)
         FSTP    tbyte[result]      // Store the result in 'result'

end;

function log(x: fp80): fp80; assembler;
asm
         //FINIT
         FLDLN2
         FLD     tbyte[x]
         FYL2X
         FSTP    tbyte[result]
end;

function strToFP80(const s: string): fp80;
var
  i: integer;
  Mantissa: int64;
  Exponent: integer;
  Divisor: int64;
  IsNegative, IsExponentNegative: boolean;
begin
  Mantissa := 0;
  Exponent := 0;
  Divisor := 1;
  IsNegative := False;
  IsExponentNegative := False;

  // Parse the mantissa
  i := 1;
  if S[i] = '-' then
  begin
    IsNegative := True;
    Inc(i);
  end
  else if S[i] = '+' then
    Inc(i);

  while (i <= Length(S)) and (S[i] <> 'e') and (S[i] <> 'E') do
  begin
    if S[i] = '.' then
      Divisor := 10
    else
    begin
      Mantissa := Mantissa * 10 + (Ord(S[i]) - Ord('0'));
      if Divisor > 1 then
        Divisor := Divisor * 10;
    end;
    Inc(i);
  end;

  // Parse the exponent
  if (i <= Length(S)) and ((S[i] = 'e') or (S[i] = 'E')) then
  begin
    Inc(i); // Skip 'e' or 'E'
    if S[i] = '-' then
    begin
      IsExponentNegative := True;
      Inc(i);
    end
    else if S[i] = '+' then
      Inc(i);

    while (i <= Length(S)) do
    begin
      Exponent := Exponent * 10 + (Ord(S[i]) - Ord('0'));
      Inc(i);
    end;
  end;

  // Calculate the result
  Result := Mantissa / Divisor;
  if IsNegative then
    Result := -Result;

  if IsExponentNegative then
    Result := Result / (ten ** Exponent)
  else
    Result := Result * (ten ** Exponent);
end;

procedure extractMantExp(const a: fp80; var mant: fp80; var expo: fp80); assembler;
// a = mant * power(2, expo), mant * power(10, expo * 0.30103)
asm
         FLD     tbyte[a]
         FXTRACT // expo:st(0), mant:st(1)
         FSTP    tbyte[mant]
         FSTP    tbyte[expo]
end;

function combineMantExp(const mant: fp80; const expo: fp80): fp80;
begin // mant * power(2, expo)
  Result := mant * (fp80(2) ** expo);
end;

function fp80ToStr(x: fp80; decimals: integer): string;
var
  m, e: fp80;
  exf, ex: double;
  exi: integer;
begin
  if x = 0 then exit('0');

  extractMantExp(x, m, e);

  // mant * power(10, expo * 0.30103), mant^
  ex := double(e) * log10_2; // 0.30103;
  exi := trunc(ex);
  exf := Frac(ex);
  m := m * power(10, exf);

  if abs(m - 10) < 1e-6 then
  begin
    m /= 10;
    Inc(exi);
  end;

  if exi <> 0 then
    Result := format('%ge%d', [double(m), exi])
  else
    Result := format('%g', [double(m)]);
end;

function __fp80ToStr(x: fp80; decimals: integer): string;
const
  sqTab: array of fp80 = ( // table of squares or 10: 1,10,100,10000...
    {1e0}(0, 0, 0, 0, 0, 0, 0, 128, 255, 63),
    {1e1}(0, 0, 0, 0, 0, 0, 0, 160, 2, 64),
    {1e2}(0, 0, 0, 0, 0, 0, 0, 200, 5, 64),
    {1e4}(0, 0, 0, 0, 0, 0, 64, 156, 12, 64),
    {1e8}(0, 0, 0, 0, 0, 32, 188, 190, 25, 64),
    {1e16}(0, 0, 0, 4, 191, 201, 27, 142, 52, 64),
    {1e32}(158, 181, 112, 43, 168, 173, 197, 157, 105, 64),
    {1e64}(213, 166, 207, 255, 73, 31, 120, 194, 211, 64),
    {1e128}(223, 140, 233, 128, 201, 71, 186, 147, 168, 65),
    {1e256}(140, 222, 249, 157, 251, 235, 126, 170, 81, 67),
    {1e512}(194, 145, 14, 166, 174, 160, 25, 227, 163, 70),
    {1e1024}(15, 12, 117, 129, 134, 117, 118, 201, 72, 77),
    {1e2048}(215, 93, 61, 197, 93, 59, 139, 158, 146, 90),
    {1e4096}(121, 151, 32, 138, 2, 82, 96, 196, 37, 117));
var
  i, ex2, exponent: integer;
  ds, xOrg, mant: fp80;
  sign: string;
begin

  if x < 0 then
  begin
    sign := '-';
    x := -x;
  end;

  // Handle special cases (NaN and Inf)
  if IsNaN(x) then  Exit(sign + 'NaN');
  if IsInfinite(x) then  Exit(sign + 'Inf');

  xOrg := x;
  exponent := 0;

  if abs(x - 10) < 1e-6 then    // 10..+inf
  begin
    i := high(sqTab);
    ds := sqTab[i];
    ex2 := 4096;

    while ex2 > 0 do
    begin
      if (x > ds) or (abs(x - ds) < 1e-6) then // x >= ds with 1e-6 tolerance
      begin
        exponent += ex2;
        x /= ds;
      end;

      ex2 := ex2 div 2;
      Dec(i);
      ds := sqTab[i];
    end;

    if x > 1 then
    begin
      x /= 10;
      Inc(exponent, 1);
    end;
  end
  else
  begin
    if (x > 0.0) and (x <= 1.0) then   // 0.00..1 .. 1
    begin
      i := high(sqTab);
      ds := 1 / sqTab[i];
      ex2 := 4096;

      while ex2 > 0 do
      begin
        if (x < ds) then
        begin
          exponent -= ex2;
          x /= ds;
        end;

        ex2 := ex2 div 2;
        Dec(i);
        ds := 1 / sqTab[i];
      end;
      if abs(x - 1) < 1e-6 then
        Inc(exponent);
    end;
  end;

  mant := xOrg / (f80.ten ** exponent);

  if exponent <> 0 then
    Result := format('%s%.5ge%d', [sign, double(x), exponent])
  else
    Result := format('%s%.5g', [sign, double(x)]);
end;


function IsNaN(x: fp80): boolean; assembler; nostackframe;
asm
         FLD     tbyte[x]
         FLDZ
         FCOMIP            // Compare st(0) with 0
         JP      @nan_detected // If the parity bit is set, the number is NaN
         MOV     [result],0 // Code execution continues here if the number is not NaN
         JMP     @_end
         @nan_detected:
         MOV     [result],1
         @_end:
         FSTP    ST(0)
end;

function IsInfinite(x: fp80): boolean; assembler; nostackframe;
asm
         FLD     tbyte[x]
         FLDZ            // Load 0 onto the stack
         FCOMIP    ST(1)      // Compare st(0) with the value to check
         JA      @infinite     // If the above flag is set, the number is infinite
         MOV     [result],0 // if the number is not infinite
         JMP     @_end
         @infinite:
         MOV     [result],1
         @_end:
         FSTP    ST(0)
end;


end.
