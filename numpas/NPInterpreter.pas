{
  NP interpreter
}

unit NPInterpreter;

{$mode delphi}

interface

uses
  Classes, SysUtils, character, strutils, uNumPas;

type

  TReal = double;
  NP = TNumPas<TReal>;

  { NPInterpreter }

  { TNPInterpreter }

  TNPInterpreter = record

  type
    Token = (tkSNULL, tkNUMBER, tkPLUS, tkMINUS,
      tkMULT, tkDIV, tkOPAREN, tkCPAREN, tkOSQBRACK, tkCSQBRACK, tkPOWER,
      tkPERIOD,
      // function names
      tkFSIN, tkFCOS, tkFTAN, tkFEXP, tkFLOG, tkFLOG10,
      tkFINT, tkFSQRT, tkFASIN, tkFACOS, tkFATAN, tkFABS, tkNP);

  public
    function Execute(expr: string): NP;
    function getErrMessage: string;
    function getErr: boolean;

  private
    expression: string; // expression to evaluate;
    ixExpr: integer;

    sym: Token;     // actual sym
    ch: char;     // actual ch
    nval: double; // actual numerical value
    id: string; // actual id,

    ixConst: integer;

    err: boolean;  // error suport
    errMessage: string;

    dims: TArrInt;
    npa: NP;

  private

    // funcs & proc's
    function getch: char;
    function getsym: Token;
    procedure ungetch;

    procedure expr0;
    procedure expr1;
    procedure expr2;
    procedure expr3;

    procedure exec(s: Token);

    procedure error(se: string);
  end;

implementation

const
  functionNames: TArray<string> = ['SIN', 'COS', 'TAN', 'EXP', 'LOG', 'LOG10', 'INT',
    'SQRT', 'ASIN', 'ACOS', 'ATAN', 'ABS', 'NP'];

  { TNPInterpreter }

function TNPInterpreter.Execute(expr: string): NP;
begin
  ixConst := 0;
  ixExpr := 1;    // strings are indexed 1..length+1
  id := '';
  expression := expr;

  getch;
  err := False;

  dims := nil;

  getsym;  // compile
  expr0;

  Result := npa;
end;

function TNPInterpreter.getErrMessage: string;
begin
  Result := errMessage;
end;

function TNPInterpreter.getErr: boolean;
begin
  Result := err;
end;

function TNPInterpreter.getch: char;
begin
  ch := #0;

  if ixExpr <= length(expression) then
  begin
    ch := expression[ixExpr];
    ixExpr += 1;
  end;
  Result := ch;
end;

procedure TNPInterpreter.ungetch;
begin
  if ixExpr > 1 then ixExpr -= 1;
end;

function TNPInterpreter.getsym: Token;
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
    id := {%H-}toUpper(id{%H-}); // case insensitive

    // is a func ?
    ix := AnsiIndexStr(id, functionNames);
    if ix <> -1 then
      sym := Token(ix + integer(tkFSIN)) // first symbol offset
    else
    begin
      sym := tkSNULL;
      error('unknown symbol:' + id);
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
        '[': sym := tkOSQBRACK;
        ']': sym := tkCSQBRACK;

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


// expression parser

procedure TNPInterpreter.expr0;
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
          exec(tsym);
        end
        else;
      end;
    until (sym <> tkPLUS) and (sym <> tkMINUS);
  end;
end;

procedure TNPInterpreter.expr1;
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
          exec(tsym);
        end;
      end;
    until (sym <> tkMULT) and (sym <> tkDIV);
  end;
end;

procedure TNPInterpreter.expr2;
begin
  if not err then
  begin
    expr3;
    repeat
      case sym of
        tkPOWER: begin
          getsym;
          expr3;
          exec(tkPOWER);
        end;
      end;
    until sym <> tkPOWER;
  end;
end;

procedure TNPInterpreter.expr3;
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


      // funcs
      tkNP: begin
        tsym := sym;
        getsym; // (
        dims := nil;
        repeat
          getsym;
          if sym = tkNUMBER then
          begin
            dims += [round(nval)];
            getsym;
          end
          else
          begin
            error('error in dimension formation');
            break;
          end;
        until sym = tkCPAREN;
        npa := NP.Create(dims);
      end;

      tkFSIN, tkFCOS, tkFTAN, tkFASIN, tkFACOS, tkFATAN, tkFEXP,
      tkFINT, tkFABS, tkFLOG, tkFLOG10, tkFSQRT:
      begin
        tsym := sym;
        getsym;
        expr3;
        exec(tsym);
      end;

      tkSNULL: ;
      else
        error('unknown symbol: ' + id);
    end;
  end;
end;

procedure TNPInterpreter.exec(s: Token);
begin

end;

procedure TNPInterpreter.error(se: string);
begin
  errMessage := se;
  err := True;
end;

end.
