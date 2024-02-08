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
    constructor Create;
    destructor Destroy; override;

    function compile(expr: string): boolean;
    function Execute(z: complex): complex;
    function getErrMessage: string;
    function getErr: boolean;
    procedure deAssemble;
    function deCompile: string;
    procedure generateRandomExpression;

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

  public
    srcCode: TStringList;

  private

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
  functionNames: array of string = ('SIN', 'COS', 'TAN', 'EXP', 'LOG', 'LOG10', 'INT',
    'SQRT', 'ASIN', 'ACOS', 'ATAN', 'ABS', 'C', 'PI', 'PHI');

  PHI = 0.6180339887;

{ TZCompiler }

constructor TZCompiler.Create;
begin
  srcCode := TStringList.Create;
end;

destructor TZCompiler.Destroy;
begin
  srcCode.Free;
  inherited Destroy;
end;

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

function TZCompiler.Execute(z: complex): complex;
var
  stack: array [0..MAX_STACK] of complex;
  // in MT runtime envirment MUST be local stack vars
  sp: integer = 0;
  pc: integer = 0;

begin

  while pc < CodeSize do
  begin
    case Token(Code[pc]) of
      tkPUSHC: begin
        pc += 1;
        stack[sp] := cinit(constants[Code[pc]], 0);
        sp += 1;
      end;

      tkPUSHZ: begin
        stack[sp] := z;
        sp += 1;
      end;

      tkPUSHI: begin
        stack[sp] := uComplex.i;
        sp += 1;
      end;

      tkPLUS: begin
        sp -= 1;
        stack[sp - 1] += stack[sp];
      end;

      tkMINUS: begin
        sp -= 1;
        {%H-}stack[sp - 1] -= {%H-}stack[sp];
      end;

      tkMULT: begin
        sp -= 1;
        stack[sp - 1] *= stack[sp];
      end;

      tkDIV: begin
        sp -= 1;
        stack[sp - 1] /= stack[sp];
      end;

      tkPOWER: begin
        sp -= 1;
        stack[sp - 1] := stack[sp - 1] ** stack[sp];
      end;

      tkNEG: stack[sp - 1] := -stack[sp - 1];

      tkFSIN: stack[sp - 1] := csin(stack[sp - 1]);
      tkFCOS: stack[sp - 1] := ccos(stack[sp - 1]);
      tkFTAN: stack[sp - 1] := ctg(stack[sp - 1]);
      tkFASIN: stack[sp - 1] := carc_sin(stack[sp - 1]);
      tkFACOS: stack[sp - 1] := carc_cos(stack[sp - 1]);
      tkFATAN: stack[sp - 1] := carc_tg(stack[sp - 1]);

      tkFEXP: stack[sp - 1] := cexp(stack[sp - 1]);
      tkFLOG: stack[sp - 1] := cln(stack[sp - 1]);
      tkFLOG10: stack[sp - 1] := cln(stack[sp - 1]); // implement log10
      tkFSQRT: stack[sp - 1] := csqrt(stack[sp - 1]);

      tkFC: begin
        sp -= 1;
        stack[sp - 1] := cinit(stack[sp - 1].re, stack[sp].re);
      end;

      tkFINT: ;

      else
      begin
        err := True;
        break;
      end;
    end;

    Inc(pc);
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

function TZCompiler.getErr: boolean;
begin
  Result := err;
end;

procedure TZCompiler.deAssemble;
type
  tkStr = record
    k: Token;
    s: string;
  end;
var
  pc: integer = 0;
  td: array of tkStr = ((k: tkPUSHZ; s: 'pushz'; ), (k: tkPUSHI;
    s: 'pushi'; ), (k: tkPLUS; s: 'add'; ), (k: tkMINUS; s: 'sub';
    ), (k: tkMULT; s: 'mul'; ), (k: tkDIV; s: 'div'; ), (k: tkPOWER;
    s: 'pow'; ), (k: tkNEG; s: 'neg'; ), (k: tkFC; s: 'cplx'; ));

  function getString(tk: Token): string; // find a token in 'td'
  var
    t: tkStr;
  begin
    Result := '';
    for t in td do
      if t.k = tk then
      begin
        Result := t.s;
        break;
      end;
  end;

begin

  srcCode.Clear;

  while pc < CodeSize do
  begin
    case Token(Code[pc]) of
      tkPUSHC: begin
        Inc(pc);
        srcCode.Add(Format('pushc (%f, %f)', [constants[Code[pc]], 0.0]));
      end;

      tkPUSHZ,
      tkPUSHI,
      tkPLUS,
      tkMINUS,
      tkMULT,
      tkDIV,
      tkPOWER,
      tkNEG,
      tkFC {to complex}: srcCode.Add(getString(Token(Code[pc])));

      tkFSIN, tkFCOS, tkFTAN, tkFASIN, tkFACOS, tkFATAN, tkFEXP, tkFLOG, tkFSQRT:
        srcCode.Add(toLower(functionNames[Code[pc] - integer(tkFSIN)]{%H-}){%H-});

      else
      begin
        err := True;
        break;
      end;
    end;
    Inc(pc);
  end;
end;

function TZCompiler.deCompile: string;

  function opPrecedence(tk: Token): integer;
  begin
    case tk of
      tkPLUS, tkMINUS: Result := 0;
      tkMULT, tkDIV: Result := 1;
      tkPOWER: Result := 2;
      else
        Result := -1;
    end;
  end;

  function op2Char(tk: Token): char;
  begin
    case tk of
      tkPLUS: Result := '+';
      tkMINUS: Result := '-';
      tkMULT: Result := '*';
      tkDIV: Result := '/';
      tkPOWER: Result := '^';
      else
        Result := ' ';
    end;
  end;

type
  TStack = record
    val: string;
    prec: integer;
  end;

var
  pc: integer = 0;
  sp: integer = 0;
  tk: Token;
  stack: array [0..MAX_STACK] of TStack;
  _prec: integer = 999;

begin
  while pc < CodeSize do
  begin
    tk := Token(Code[pc]);
    case tk of
      tkPUSHC: begin
        Inc(pc);

        stack[sp].val := Format('%.1f', [constants[Code[pc]]]);
        stack[sp].prec := 999;

        Inc(sp);
      end;

      tkPUSHZ: begin
        stack[sp].val := 'z';
        stack[sp].prec := 999;
        Inc(sp);
      end;
      tkPUSHI: begin
        stack[sp].val := 'i';
        stack[sp].prec := 999;
        Inc(sp);
      end;

      tkPLUS,
      tkMINUS,
      tkMULT,
      tkDIV,
      tkPOWER: begin
        Dec(sp);

        _prec := opPrecedence(tk);

        with {%H-}stack[sp - 1] do
        begin
          if prec < _prec then  // left <op> | ( left ) <op>
            val := '(' + val + ')';

          val += op2Char(tk);

          if stack[sp].prec < _prec then  //  right | (right)
            val += '(' + stack[sp].val + ')'
          else
            val += stack[sp].val;

          prec := _prec;
        end;
      end;

      tkNEG: ;
      tkFC {to complex}: begin
        Dec(sp);
        stack[sp - 1].val := 'c(' + stack[sp - 1].val + ',' + stack[sp].val + ')';
      end;

      tkFSIN, tkFCOS, tkFTAN, tkFASIN, tkFACOS, tkFATAN, tkFEXP, tkFLOG, tkFSQRT:
        stack[sp - 1].val := toLower(functionNames[Code[pc] - integer(tkFSIN)]{%H-}) +
          '(' + stack{%H-}[sp - 1].val {%H-}+ ')';
      else
      begin
        err := True;
        break;
      end;
    end;
    Inc(pc);
  end;

  if sp <> 0 then
    Result := stack[sp - 1].val
  else
    Result := '';
end;

procedure TZCompiler.generateRandomExpression;

var
  i: integer;

  procedure generateRndConstants;
  var
    i: integer;
  begin
    ixConst := random(30);
    for i := 0 to ixConst - 1 do constants[i] := random * 100.0;
  end;

  procedure genRndPush;
  begin

    case random(10) of
      0..6: generate(tkPUSHZ);
      else
        generate(tkPUSHC, byte(random(ixConst)));
    end;
  end;

  procedure genRndOper;
  begin
    case random(5) of
      0: generate(tkPLUS);
      1: generate(tkMINUS);
      2: generate(tkMULT);
      3: generate(tkDIV);

      4: generate(tkPOWER);
    end;
  end;

  function getRndFunc: Token;
  begin
    Result := Token(integer(tkFSIN) + random(integer(tkFATAN) - integer(tkFSIN)));
  end;

begin
  ixCode := 0;

  randomize;

  generateRndConstants;

  genRndPush;

  for i := 0 to random(10) + 5 do
  begin
    genRndPush;
    genRndOper;
    if random(5) >= 3 then generate(getRndFunc);
  end;

  CodeSize := ixCode;
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
        ix := AnsiIndexStr(id, functionNames);
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
