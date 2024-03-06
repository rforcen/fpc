{
   f128 wrapper to c type long double
   usage:

   var
     i:f128;
   begin
         i:=100; // int64 assign
         i:='1234567890'; // or as a literal
         i:=i*i+3/i; // arithmetic
         i:=i>>10; // shifts
         if i>100 then ...
         writeln('i=', i128toa(i));// print
   end.
}

unit f128;

{$mode ObjFPC}{$H+}
{$WARN 5060 off : Function result variable does not seem to be initialized}

interface

uses ctypes, SysUtils;

type
  fp128 = packed record
    d: array[1..16] of byte;
  end;

  pfp128 = ^fp128;


// assign
operator := (a: double): fp128; overload; inline;
operator := (a: real): fp128; overload; inline;
operator := (a: integer): fp128; overload; inline;

operator := (s: string): fp128; inline;
// arith. oper.
operator +(const a, b: fp128): fp128; inline;
operator -(const a, b: fp128): fp128; inline;
operator *(const a, b: fp128): fp128; inline;
operator /(const a, b: fp128): fp128; inline;
operator -(const a: fp128): fp128; inline;
operator **(const a, b: fp128): fp128; inline;
// compare
operator >(const a, b: fp128): boolean; inline;
operator >=(const a, b: fp128): boolean; inline;
operator <(const a, b: fp128): boolean; inline;
operator <=(const a, b: fp128): boolean; inline;
operator =(const a, b: fp128): boolean; inline;
operator <>(const a, b: fp128): boolean; inline;
// print
function f128tos(a: fp128): string;
function sTof128(s: string): fp128;

// misc
procedure Incf128(var a: fp128); cdecl; external;
procedure Decf128(var a: fp128); cdecl; external;
function predf128(a: fp128): fp128; inline;
function succf128(a: fp128): fp128; inline;
function truncf128(a: fp128): fp128; inline;

implementation

{$linklib ..\units\fp128.dll}// c wrapper

procedure putf128(var r: fp128; a: double); cdecl; external;

// arithmetic
procedure addf128(var r: fp128; a, b: fp128); cdecl; external;
procedure subf128(var r: fp128; a, b: fp128); cdecl; external;
procedure mulf128(var r: fp128; a, b: fp128); cdecl; external;
procedure divf128(var r: fp128; a, b: fp128); cdecl; external;
procedure negf128(var r: fp128; a: fp128); cdecl; external;
procedure powf128(var r: fp128; a, b: fp128); cdecl; external;

// compare boolean is a byte type
procedure gtf128(var r: cint; a, b: fp128); cdecl; external;
procedure gef128(var r: cint; a, b: fp128); cdecl; external;
procedure ltf128(var r: cint; a, b: fp128); cdecl; external;
procedure lef128(var r: cint; a, b: fp128); cdecl; external;
procedure eqf128(var r: cint; a, b: fp128); cdecl; external;
procedure nef128(var r: cint; a, b: fp128); cdecl; external;

// to ascii
procedure f128toa(r: PChar; a: fp128); cdecl; external;
procedure atof128(var r: fp128; s: PChar); cdecl; external;

// funcs
procedure _truncf128(var f: fp128; a: fp128); cdecl; external;

///////////////////////////////////////////////////////////
// assign
operator := (a: double): fp128;
begin
  putf128(Result, a);
end;

operator := (a: real): fp128;
begin
  putf128(Result, double(a));
end;

operator := (a: integer): fp128;
begin
  putf128(Result, double(a));
end;

operator := (s: string): fp128;
begin
  atof128(Result, PChar(s));
end;


// aritmetics
operator +(const a, b: fp128): fp128;
begin
  addf128(Result, a, b);
end;

operator -(const a, b: fp128): fp128;
begin
  subf128(Result, a, b);
end;

operator *(const a, b: fp128): fp128;
begin
  mulf128(Result, a, b);
end;

operator /(const a, b: fp128): fp128;
begin
  divf128(Result, a, b);
end;

operator -(const a: fp128): fp128;
begin
  negf128(Result, a);
end;

operator **(const a, b: fp128): fp128;
begin
  powf128(Result, a, b);
end;

// comp's
operator >(const a, b: fp128): boolean;
var
  r: cint;
begin
  gtf128(r, a, b);
  Result := r = 1;
end;

operator >=(const a, b: fp128): boolean;
var
  r: cint;
begin
  gef128(r, a, b);
  Result := r = 1;
end;

operator <(const a, b: fp128): boolean;
var
  r: cint;
begin
  ltf128(r, a, b);
  Result := r = 1;
end;

operator <=(const a, b: fp128): boolean;
var
  r: cint;
begin
  lef128(r, a, b);
  Result := r = 1;
end;

operator =(const a, b: fp128): boolean;
var
  r: cint;
begin
  eqf128(r, a, b);
  Result := r = 1;
end;

operator <>(const a, b: fp128): boolean;
var
  r: cint;
begin
  nef128(r, a, b);
  Result := r = 1;
end;

// ascii <-> f128
function f128tos(a: fp128): string;
var
  s: array[0..256] of char;
begin
  f128toa(s, a);
  Result := s;
end;

function sTof128(s: string): fp128;
begin
  atof128(Result, PChar(s));
end;

function predf128(a: fp128): fp128;
begin
  subf128(Result, a, 1);
end;

function succf128(a: fp128): fp128;
begin
  addf128(Result, a, 1);
end;

function truncf128(a: fp128): fp128;
begin
  _truncf128(Result, a);
end;

// misc

end.
