{
  complex number expression compiler and virtual machine executor
}

unit zCompiler;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uComplex, character, strutils;


{ TZCompiler }
type

  TZCompiler = class
  const
    MAX_CONSTANTS = 255;
    MAX_CODE = 16 * 1024;
    MAX_STACK = 256;

  type
    Token = (tkSNULL, tkNUMBER, tkIDENT_i, tkIDENT_z, tkPLUS, tkMINUS,
      tkMULT, tkDIV, tkOPAREN, tkCPAREN, tkPOWER,
      tkPERIOD,

      // function names
      tkFSIN, tkFCOS, tkFTAN, tkFEXP, tkFLOG, tkFLOG10,
      tkFINT, tkFSQRT, tkFASIN, tkFACOS, tkFATAN, tkFABS,
      tkFC,
      tkSPI, tkSPHI, tkPUSHC, tkPUSHZ, tkPUSHI, tkPUSHCC, tkNEG);

  public
    function compile(expr: string): boolean;
    function execute(z: complex): complex;
    function getErrMessage: string;

  private
    expression: string; // expression to evaluate;
    ixExpr: integer;

    sym: Token;     // actual sym
    ch: char;     // actual ch
    nval: double; // actual numerical value
    id: string; // actual id,

    constants: array[0..MAX_CONSTANTS] of double; // constants table
    ixConst: integer;

    err: boolean;  // error suport
    errMessage: string;

    // compiler
    Code: array [0..MAX_CODE] of byte;
    ixCode, CodeSize: integer;

    // funcs & proc's
    function getch: char;
    function getsym: Token;
    procedure ungetch;

    procedure expr0;
    procedure expr1;
    procedure expr2;
    procedure expr3;

    procedure error(se: string);

    procedure generate(tk: Token; f: double);
    procedure generate(tk: Token; i: byte); overload;
    procedure generate(tk: Token); overload;

  end;

implementation

const
  fname: array of string = ('SIN', 'COS', 'TAN', 'EXP', 'LOG', 'LOG10', 'INT',
    'SQRT', 'ASIN', 'ACOS', 'ATAN', 'ABS', 'C', 'PI', 'PHI');

  PHI = 0.6180339887;

{ TZCompiler }

function TZCompiler.compile(expr: string): boolean;
begin
  ixCode := 0;
  ixConst := 0;
  ixExpr := 1;    // strings are indexed 1..length+1
  id := '';
  expression := expr;

  getch;
  err := False;

  getsym;  // compile
  expr0;

  CodeSize := ixCode;

  Result := err;
end;

function TZCompiler.execute(z: complex): complex;
var
  stack: array [0..MAX_STACK] of complex;
  // in MT runtime envirment MUST be local stack vars
  sp, pc: integer;

begin
  sp := 0;
  pc := 0;

  while pc < CodeSize do
  begin
    case Token(Code[pc]) of
      tkPUSHC: begin
        pc += 1;
        stack[sp] := cinit(constants[Code[pc]], 0);
        sp += 1;
        pc += 1;
      end;

      tkPUSHZ: begin
        pc += 1;
        stack[sp] := z;
        sp += 1;
      end;

      tkPUSHI: begin
        pc += 1;
        stack[sp] := uComplex.i;
        sp += 1;
      end;

      tkPLUS: begin
        sp -= 1;
        stack[sp - 1] += stack[sp];
        pc += 1;
      end;

      tkMINUS: begin
        sp -= 1;
        {%H-}stack[sp - 1] -= {%H-}stack[sp];
        pc += 1;
      end;

      tkMULT: begin
        sp -= 1;
        stack[sp - 1] *= stack[sp];
        pc += 1;
      end;

      tkDIV: begin
        sp -= 1;
        stack[sp - 1] /= stack[sp];
        pc += 1;
      end;

      tkPOWER: begin
        sp -= 1;
        stack[sp - 1] := stack[sp - 1] ** stack[sp];
        pc += 1;
      end;

      tkNEG: begin
        stack[sp - 1] := -stack[sp - 1];
        pc += 1;
      end;


      tkFSIN: begin
        stack[sp - 1] := csin(stack[sp - 1]);
        pc += 1;
      end;

      tkFCOS: begin
        stack[sp - 1] := ccos(stack[sp - 1]);
        pc += 1;
      end;

      tkFTAN: begin
        stack[sp - 1] := ctg(stack[sp - 1]);
        pc += 1;
      end;

      tkFASIN: begin
        stack[sp - 1] := carc_sin(stack[sp - 1]);
        ixCode += 1;
      end;

      tkFACOS: begin
        stack[sp - 1] := carc_cos(stack[sp - 1]);
        pc += 1;
      end;

      tkFATAN: begin
        stack[sp - 1] := carc_tg(stack[sp - 1]);
        pc += 1;
      end;

      tkFEXP: begin
        stack[sp - 1] := cexp(stack[sp - 1]);
        pc += 1;
      end;

      tkFLOG: begin
        stack[sp - 1] := cln(stack[sp - 1]);
        pc += 1;
      end;

      tkFSQRT: begin
        stack[sp - 1] := csqrt(stack[sp - 1]);
        pc += 1;
      end;

      tkFC: begin
        sp -= 1;
        pc += 1;
        stack[sp - 1] := cinit(stack[sp - 1].re, stack[sp].re);
      end

      else
      begin
        err := True;
        break;
      end;
    end;
  end;

  if sp <> 0 then
    Result := stack[sp - 1]
  else
    Result := uComplex._0;
end;

function TZCompiler.getErrMessage: string;
begin
  Result := errMessage;
end;

function TZCompiler.getch: char;
begin
  ch := #0;

  if ixExpr <= length(expression) then
  begin
    ch := expression[ixExpr];
    ixExpr += 1;
  end;
  Result := ch;
end;

procedure TZCompiler.ungetch;
begin
  if ixExpr > 1 then ixExpr -= 1;
end;

function TZCompiler.getsym: Token;
var
  ix: integer;
begin

  sym := tkSNULL;
  id := '';

  // skip blanks
  while (ch <> #0) and (ch <= ' ') do
    getch;

  if isLetter(ch) then   // detect symbol

  begin // ident ?
    id := '';
    while isLetterOrDigit(ch) or (ch = '_') do
    begin
      id += ch;
      getch;
    end;
    sym := tkIDENT_i;
    id := {%H-}toUpper(id{%H-}); // case insensitive

    case id of
      'Z': sym := tkIDENT_z;
      'I': sym := tkIDENT_i;
      else
      begin
        // is a func ?
        ix := AnsiIndexStr(id, fname);
        if ix <> -1 then
        begin
          sym := Token(ix + integer(tkFSIN)); // first symbol offset
        end
        else
        begin
          sym := tkSNULL;
          error('unknown symbol:' + id);
        end;
      end;
    end;
  end
  else
  begin // number ?
    if isDigit(ch) then
    begin // number (double) take care of dddd.ddde-dd
      while isDigit(ch) or (ch = '.') or (ch = 'e') or (ch = 'E') do
      begin
        id += ch;
        getch;
      end;
      sym := tkNUMBER;

      try // convert to number
        nval := strToFloat(id);
      except
        On E: Exception do
        begin
          nval := 0;
          error('malformed number:' + id);
        end;
      end;

    end
    else // special char or error, no more choices
    begin
      case ch of
        '+': sym := tkPLUS;
        '-': sym := tkMINUS;
        '*': sym := tkMULT;
        '/': sym := tkDIV;
        '(': sym := tkOPAREN;
        ')': sym := tkCPAREN;
        '^': sym := tkPOWER;
        ',': sym := tkPERIOD;
        #0: sym := tkSNULL;
        else
        begin
          sym := tkSNULL;
          error('character not recognized: ' + ch);
        end;

      end;

      getch; // next char
    end;
  end;

  Result := sym;
end;


procedure TZCompiler.generate(tk: Token; f: double);
begin // code Generation
  Code[ixCode] := byte(tk);
  ixCode += 1;
  Code[ixCode] := byte(ixConst);
  ixCode += 1;

  constants[ixConst] := f;
  ixConst += 1;
end;

procedure TZCompiler.generate(tk: Token; i: byte);
begin
  Code[ixCode] := byte(tk);
  ixCode += 1;
  Code[ixCode] := byte(i);
  ixCode += 1;
end;

procedure TZCompiler.generate(tk: Token);
begin
  Code[ixCode] := byte(tk);
  ixCode += 1;
end;

// expression parser

procedure TZCompiler.expr0;
var
  tsym: Token;
begin
  if not err then
  begin
    expr1;
    repeat
      case sym of
        tkPLUS, tkMINUS: begin
          tsym := sym;
          getsym;
          expr1;
          generate(tsym);
        end
        else;
      end;
    until (sym <> tkPLUS) and (sym <> tkMINUS);
  end;
end;

procedure TZCompiler.expr1;
var
  tsym: Token;
begin
  if not err then
  begin
    expr2;
    repeat
      case sym of
        tkMULT, tkDIV: begin
          tsym := sym;
          getsym;
          expr2;
          generate(tsym);
        end;
      end;
    until (sym <> tkMULT) and (sym <> tkDIV);
  end;
end;

procedure TZCompiler.expr2;
begin
  if not err then
  begin
    expr3;
    repeat
      case sym of
        tkPOWER: begin
          getsym;
          expr3;
          generate(tkPOWER);
        end;
      end;
    until sym <> tkPOWER;
  end;
end;

procedure TZCompiler.expr3;
var
  tsym: Token;
begin
  if not err then
  begin
    case sym of
      tkOPAREN: begin
        getsym;
        expr0;
        getsym;
      end;
      tkNUMBER: begin
        generate(tkPUSHC, nval);
        getsym;
      end;
      tkIDENT_i: begin
        generate(tkPUSHI);
        getsym;
      end;
      tkIDENT_z: begin
        generate(tkPUSHZ);
        getsym;
      end;
      tkMINUS: begin
        getsym;
        expr3;
        generate(tkNEG);
      end;
      tkPLUS: begin
        getsym;
        expr3;
      end;

      // funcs
      tkFSIN, tkFCOS, tkFTAN, tkFASIN, tkFACOS, tkFATAN, tkFEXP,
      tkFINT, tkFABS, tkFLOG, tkFLOG10, tkFSQRT:
      begin
        tsym := sym;
        getsym;
        expr3;
        generate(tsym);
      end;

      tkFC: begin
        getsym;
        getsym;
        expr3;
        getsym;
        expr3;
        getsym;
        generate(tkFC);
      end;

      tkSPI: begin
        getsym;
        generate(tkPUSHC, PI);
      end;
      tkSPHI: begin
        getsym;
        generate(tkPUSHC, PHI);
      end;

      tkSNULL: ;

      else
        error('unknown symbol: ' + id);
    end;
  end;
end;

procedure TZCompiler.error(se: string);
begin
  errMessage := se;
  err := True;
end;

end.
