unit uf16;

{$mode ObjFPC}{$H+}
{$WARN 5025 off : Local variable "$1" not used}

interface

uses
  Classes, SysUtils;

type
  _basef16 = uint16;
  f16 = _basef16;
  float16 = _basef16;

const
  half_tenth: f16 = 11878;
  half_fifth: f16 = 12902;
  half_third: f16 = 13653;
  half_half: f16 = 14336;
  half_one: f16 = 15360;
  half_two: f16 = 16384;
  half_three: f16 = 16896;
  half_five: f16 = 17664;
  half_ten: f16 = 18688;
  half_pi: f16 = 16968;
  half_half_pi: f16 = 15944;

type
  THalf = packed record     // float 16
    case integer of
      0: (
        r: bitpacked record
          mant: 0..1024 - 1; // 10-bit mantissa
          expo: 0..32 - 1;   // 5-bit exponent
          sign: 0..2 - 1;    // 1-bit sign
          end;
      );
      1: (
        v: f16; // 16-bit half-precision floating-point value
      );
  end;

  TSingle = packed record   // float32 single
    case integer of
      0: (
        r: bitpacked record
          mant: 0..8388608 - 1; // 23-bit mantissa
          expo: 0..256 - 1;     // 8-bit exponent
          sign: 0..1;           // 1-bit sign
          end;
      );
      1: (
        v: single; // 16-bit half-precision floating-point value
      );
  end;

  half = type THalf;

const
  hPI: half = (v: 16968);
  hOne: half = (v: 15360);

  { THalf }

operator +(a, b: half): half; // arithmetic
operator -(a, b: half): half;
operator *(a, b: half): half;
operator /(a, b: half): half;
operator -(a: half): half;

operator =(a, b: half): boolean; // logical
operator <>(a, b: half): boolean;
operator >(a, b: half): boolean;
operator <(a, b: half): boolean;
operator >=(a, b: half): boolean;
operator <=(a, b: half): boolean;

operator := (a: single): half; // assign single <-> half
operator := (a: half): single;
operator := (a: f16): half;

function toString(a: half): string;

function singleToHalf(s: single): f16;
function halfToSingle(f: f16): single;

{ ----- }

implementation

{ float16.c wrapper }
{
{%H-}{$l cpp\float16.o}

function singletoF16(v: single): f16; cdecl; external Name 'float32_to_float16';
function f16toSingle(f: f16): single; cdecl; external Name 'float16_to_float32';

function f16_2_f32(f: f16): single; cdecl; external Name 'f16_2_f32';
function f32_2_f16(s: single): f16; cdecl; external Name 'f32_2_f16';

{%H-}{
#define SIGN_MASK 0x8000
#define EXP_MASK 0x7C00
#define NAN_VALUE 0x7FFF
#define IS_ZERO(x) (((x) & 0x7FFF) == 0)
#define IS_INVALID(x) (((x) & EXP_MASK) == EXP_MASK)
#define IS_NAN(x) (((x) & 0x7FFF) > 0x7C00)
#define IS_INF(x) ( ((x) & 0x7FFF) == 0x7C00)
#define MANTISSA(x) (((x) & 1023) | (((x) & 0x7C00) == 0 ? 0 : 1024))
#define EXPONENT(x) (((x) & 0x7C00) >> 10)
#define SIGNED_INF_VALUE(x)  ((x & SIGN_MASK) | 0x7C00)
}


function f16add(a, b: f16): f16; cdecl; external Name 'f16_add';
function f16sub(a, b: f16): f16; cdecl; external Name 'f16_sub';
function f16mul(a, b: f16): f16; cdecl; external Name 'f16_mul';
function f16div(a, b: f16): f16; cdecl; external Name 'f16_div';
function f16neg(a: f16): f16; cdecl; external Name 'f16_neg';
function from_int(v: integer): f16; cdecl; external Name 'f16_from_int';
function f16int(v: f16): integer; cdecl; external Name 'f16_int';

function f16gte(a, b: f16): integer; cdecl; external Name 'f16_gte';
function f16gt(a, b: f16): integer; cdecl; external Name 'f16_gt';
function f16eq(a, b: f16): integer; cdecl; external Name 'f16_eq';
function f16lte(a, b: f16): integer; cdecl; external Name 'f16_lte';
function f16lt(a, b: f16): integer; cdecl; external Name 'f16_lt';
function f16neq(a, b: f16): integer; cdecl; external Name 'f16_neq';

}

function halfToSingle(f: f16): single;
var
  sf: TSingle;
  hf: half;
begin
  hf.v := f;

  sf.r.sign := hf.r.sign;
  sf.r.expo := hf.r.expo - 15 + 127; // 2^(5-1) 2^(8-1)
  sf.r.mant := hf.r.mant << (23 - 10);

  Result := sf.v;
end;

function singleToHalf(s: single): f16;
var
  sf: TSingle;
  hf: half;
begin
  sf.v := s;

  hf.r.sign := sf.r.sign;
  hf.r.expo := sf.r.expo + 15 - 127;
  hf.r.mant := sf.r.mant >> (23 - 10);

  Result := hf.v;
end;


{ native primitives }
const
  SIGN_MASK = $8000;
  EXP_MASK = $7C00;
  NAN_VALUE = $7FFF;


function IS_ZERO(x: f16): boolean; inline;
begin
  Result := (x and NAN_VALUE) = 0;
end;

function IS_INVALID(x: f16): boolean; inline;
begin
  Result := (x and EXP_MASK) = EXP_MASK;
end;

function IS_NAN(x: f16): boolean; inline;
begin
  Result := (x and $7FFF) > EXP_MASK;
end;

function IS_INF(x: f16): boolean; inline;
begin
  Result := (x and $7FFF) = EXP_MASK;
end;

function MANTISSA(x: f16): f16; inline;
begin
  Result := x and 1023;
  if (x and EXP_MASK) <> 0 then Result := Result or 1024;
end;

function EXPONENT(x: f16): integer; inline;
begin
  Result := (x and EXP_MASK) shr 10;
end;

function SIGNED_INF_VALUE(x: f16): f16; inline;
begin
  Result := (x and SIGN_MASK) or EXP_MASK;
end;

function f16_sub(ain, bin: f16): f16; forward;

function f16_add(a, b: f16): f16;
var
  sign, ax, bx, exp_diff, exp_part, r, am, new_m: word;
  shift: integer;
begin
  if ((a xor b) and SIGN_MASK) <> 0 then
    Exit(f16_sub(a, b xor SIGN_MASK));

  sign := a and SIGN_MASK;
  a := a and NAN_VALUE;
  b := b and NAN_VALUE;

  if a < b then
  begin
    a := a xor b;
    b := a xor b;
    a := a xor b;
  end;

  if (a >= EXP_MASK) or (b >= EXP_MASK) then
  begin
    if (a > EXP_MASK) or (b > EXP_MASK) then
      Exit($7FFF);
    Exit(EXP_MASK or sign);
  end;

  ax := a and EXP_MASK;
  bx := b and EXP_MASK;

  exp_diff := ax - bx;
  exp_part := ax;

  if exp_diff <> 0 then
  begin
    shift := exp_diff shr 10;
    if bx <> 0 then
      b := ((b and 1023) or 1024) shr shift
    else
      b := b shr (shift - 1);
  end
  else
  begin
    if bx = 0 then
      Exit((a + b) or sign)
    else
      b := (b and 1023) or 1024;
  end;

  r := a + b;

  if (r and EXP_MASK) <> exp_part then
  begin
    am := (a and 1023) or 1024;
    new_m := (am + b) shr 1;
    r := (exp_part + $400) or (1023 and new_m);
  end;

  if f16(r) >= EXP_MASK then
    Exit(sign or EXP_MASK);

  Exit(r or sign);
end;

function f16_sub(ain, bin: f16): f16;
var
  a, b, sign, ax, bx, res, exp_diff, exp_part, r, am, new_m: f16;
  shift: integer;
begin
  a := ain;
  b := bin;

  if ((a xor b) and SIGN_MASK) <> 0 then
    Exit(f16_add(a, b xor SIGN_MASK));

  sign := a and SIGN_MASK;
  a := a shl 1;
  b := b shl 1;

  if a < b then
  begin
    a := a xor b;
    b := a xor b;
    a := a xor b;
    sign := sign xor $8000;
  end;

  ax := a and $F800;
  bx := b and $F800;

  if (a >= $F800) or (b >= $F800) then
  begin
    if (a > $F800) or (b > $F800) or (a = b) then
      Exit($7FFF);

    res := sign or $7C00;
    if a = $F800 then
      Exit(res)
    else
      Exit(res xor SIGN_MASK);
  end;

  exp_diff := ax - bx;
  exp_part := ax;

  if exp_diff <> 0 then
  begin
    shift := exp_diff shr 11;
    if bx <> 0 then
      b := ((b and 2047) or 2048) shr shift
    else
      b := b shr (shift - 1);
  end
  else
  begin
    if bx = 0 then
    begin
      r := (a - b) shr 1;
      if r = 0 then
        Exit(r);
      Exit(r or sign);
    end
    else
      b := (b and 2047) or 2048;
  end;

  r := a - b;

  if (r and $F800) = exp_part then
    Exit((r shr 1) or sign);

  am := (a and 2047) or 2048;
  new_m := am - b;

  if new_m = 0 then
    Exit(0);

  while (exp_part <> 0) and (new_m and 2048 = 0) do
  begin
    exp_part := exp_part - $800;
    if exp_part <> 0 then
      new_m := new_m shl 1;
  end;

  Exit(((new_m and 2047) or exp_part) shr 1 or sign);
end;

function f16_mul(a, b: f16): f16;
var
  sign: integer;
  m1, m2: uint16;
  v: uint32;
  ax, bx, new_exp: integer;
begin
  sign := (a xor b) and SIGN_MASK;

  if IS_INVALID(a) or IS_INVALID(b) then
  begin
    if IS_NAN(a) or IS_NAN(b) or IS_ZERO(a) or IS_ZERO(b) then
      exit(NAN_VALUE);
    exit(sign or EXP_MASK);
  end;

  if IS_ZERO(a) or IS_ZERO(b) then exit(0);

  m1 := MANTISSA(a);
  m2 := MANTISSA(b);

  v := m1 * m2;
  ax := EXPONENT(a);
  bx := EXPONENT(b);

  Inc(ax, Ord(ax = 0));
  Inc(bx, Ord(bx = 0));
  new_exp := ax + bx - 15;

  if v and (1 shl 21) <> 0 then
  begin
    v := v shr 11;
    Inc(new_exp);
  end
  else if v and (1 shl 20) <> 0 then
    v := v shr 10
  else
  begin
    Dec(new_exp, 10);
    while v >= 2048 do
    begin
      v := v shr 1;
      Inc(new_exp);
    end;
  end;

  if new_exp <= 0 then
  begin
    v := v shr (-new_exp + 1);
    new_exp := 0;
  end
  else if new_exp >= 31 then
    Exit(SIGNED_INF_VALUE(sign));

  Exit(sign or (new_exp shl 10) or (v and 1023));
end;

function f16_div(a, b: f16): f16;
var
  sign: int16;
  m1, m2: uint16;
  m1_shifted, v: uint32;
  rem: uint16;
  ax, bx, new_exp: integer;
begin
  sign := (a xor b) and SIGN_MASK;

  if IS_NAN(a) or IS_NAN(b) or (IS_INVALID(a) and IS_INVALID(b)) or
    (IS_ZERO(a) and IS_ZERO(b)) then
    Exit(NAN_VALUE);

  if IS_INVALID(a) or IS_ZERO(b) then
    Exit(SIGNED_INF_VALUE(sign));

  if IS_INVALID(b) or IS_ZERO(a) then
    Exit(0);

  m1 := MANTISSA(a);
  m2 := MANTISSA(b);

  m1_shifted := m1 shl 10;
  v := m1_shifted div m2;
  rem := m1_shifted mod m2;

  ax := EXPONENT(a);
  bx := EXPONENT(b);
  Inc(ax, Ord(ax = 0));
  Inc(bx, Ord(bx = 0));
  new_exp := ax - bx + 15;

  if (v = 0) and (rem = 0) then
    Exit(0);

  while (v < 1024) and (new_exp > 0) do
  begin
    v := v shl 1;
    rem := rem shl 1;
    if rem >= m2 then
    begin
      Inc(v);
      Dec(rem, m2);
    end;
    Dec(new_exp);
  end;

  while v >= 2048 do
  begin
    v := v shr 1;
    Inc(new_exp);
  end;

  if new_exp <= 0 then
  begin
    v := v shr (-new_exp + 1);
    new_exp := 0;
  end
  else if new_exp >= 31 then
    Exit(SIGNED_INF_VALUE(sign));

  Exit(sign or (v and 1023) or (new_exp shl 10));
end;

function f16_neg(v: f16): f16;
begin
  Result := SIGN_MASK xor v;
end;

// logical
function f16_gte(a, b: f16): boolean;
begin
  if IS_ZERO(a) and IS_ZERO(b) then
    Exit(True);
  if IS_NAN(a) or IS_NAN(b) then
    Exit(False);
  if (a and $8000 = 0) then
  begin
    if (b and $8000 = $8000) then
      Exit(True);
    Exit(a >= b);
  end
  else
  begin
    if (b and $8000 = 0) then
      Exit(False);
    Exit((a and $7FFF) <= (b and $7FFF));
  end;
end;

function f16_gt(a, b: f16): boolean;
begin
  if (IS_NAN(a) or IS_NAN(b)) then
    Exit(False);
  if IS_ZERO(a) and IS_ZERO(b) then
    Exit(False);
  if (a and $8000 = 0) then
  begin
    if (b and $8000 = $8000) then
      Exit(True);
    Exit(a > b);
  end
  else
  begin
    if (b and $8000 = 0) then
      Exit(False);
    Exit((a and $7FFF) < (b and $7FFF));
  end;
end;

function f16_eq(a, b: f16): boolean;
begin
  if IS_NAN(a) or IS_NAN(b) then
    Exit(False);
  if IS_ZERO(a) and IS_ZERO(b) then
    Exit(True);
  Exit(a = b);
end;

function f16_lte(a, b: f16): boolean;
begin
  if IS_NAN(a) or IS_NAN(b) then
    Exit(False);
  Exit(f16_gte(b, a));
end;

function f16_lt(a, b: f16): boolean;
begin
  if (IS_NAN(a) or IS_NAN(b)) then
    Exit(False);
  Exit(f16_gt(b, a));
end;

function f16_neq(a, b: f16): boolean;
begin
  Exit(not f16_eq(a, b));
end;



{ half operators }

operator +(a, b: half): half;
begin
  Result.v := f16_add(a.v, b.v);
end;

operator -(a, b: half): half;
begin
  Result.v := f16_sub(a.v, b.v);
end;

operator *(a, b: half): half;
begin
  Result.v := f16_mul(a.v, b.v);
end;

operator /(a, b: half): half;
begin
  Result.v := f16_div(a.v, b.v);
end;

operator =(a, b: half): boolean;
begin
  Result := f16_eq(a.v, b.v);
end;

operator <>(a, b: half): boolean;
begin
  Result := f16_neq(a.v, b.v);
end;

operator >(a, b: half): boolean;
begin
  Result := f16_gt(a.v, b.v);
end;

operator <(a, b: half): boolean;
begin
  Result := f16_lt(a.v, b.v);

end;

operator >=(a, b: half): boolean;
begin
  Result := f16_gte(a.v, b.v);

end;

operator <=(a, b: half): boolean;
begin
  Result := f16_lte(a.v, b.v);
end;

operator -(a: half): half;
begin
  Result.v := f16_neg(a.v);
end;

operator := (a: single): half;
begin
  Result.v := singleToHalf(a);
end;

operator := (a: half): single;
begin
  Result := halfToSingle(a.v);
end;

operator := (a: f16): half;
begin
  Result.v := a;
end;

function toString(a: half): string;
begin
  Result := FloatToStr(halfToSingle(a.v));
end;

end.
