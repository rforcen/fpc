{
 80 bit fp80 support
 this is the extended float type not supported in win64
}

unit f80;

{$mode ObjFPC}{$H+}
{$asmmode intel}

interface

uses
  Classes, SysUtils, Math;

type
  float80 = packed array [0..9] of byte;
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
function sqrt(x: fp80): fp80; assembler;
function abs(x: fp80): fp80; assembler;
function remainder(x, y: fp80): fp80; assembler; // mod
function exp(x: fp80): fp80; assembler;
function log(x: fp80): fp80; assembler;

function strToFP80(const s: string): fp80;
function fp80ToStr(x: fp80; decimals: integer = 0): string;

function IsNaN(x: fp80): boolean; assembler;
function IsInfinite(x: fp80): boolean; assembler;

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
         FMUL                // Multiply st(0) by st(1) to get x * log2(e)
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
    Result := Result / Power(10, Exponent)
  else
    Result := Result * Power(10, Exponent);
end;

function __old_fp80ToStr(const a: fp80): string;
var
  mant: double;
  expo: int32;
begin
  mant := a;
  asm
           MOV     R11,[a]
           FLD     tbyte [R11]
           FXTRACT // expo:st(0), mant:st(1)
           FST     dword [expo]
           FSTP    ST(1)
           FST     qword [mant]
  end;
  Result := format('%fe%d', [mant, expo]);
end;

function fp80ToStr(x: fp80; decimals: integer): string;
var
  integral: int64;
  decimal: int64;
  exponent: double;
begin
  Result := '';
  if x < 0.0 then     // Handle negative numbers
  begin
    Result := '-';
    x := -x;
  end;

  // Handle special cases (NaN and Inf)
  if IsNaN(x) then  Exit(Result + 'NaN');
  if IsInfinite(x) or (x > high(int64)) then  Exit(Result + 'Inf');

  exponent := power(10.0, Min(decimals, 16)); // exponent of 10 for rounding
  x := x + 0.5 / exponent;     // Rounding

  integral := Trunc(x);     // Extract integral and decimal parts
  decimal := Trunc((x - integral) * exponent);

  Result := Result + IntToStr(integral);     // Construct the final string
  if decimals > 0 then
    Result := Result + '.' + IntToStr(decimal);
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
