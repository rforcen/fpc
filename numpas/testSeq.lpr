{$mode delphi}
{$modeswitch nestedprocvars}
program testSeq;

uses
  SysUtils,
  uNumPas,
  randoms,
  f128;

type
  FP = double;
  NP = TNumPas<FP>;

var
  a, b, d: TNumPas<FP>;   // extended maps to double in windows
  det: FP;
  tx: Text;


  procedure testf128;
  var
    a, b: fp128;
  begin
    a := 1;
    b := 2;
    a += b;
    writeln(f128tos(a));
    a *= b;
    writeln(f128tos(a));
    a /= 4.5;
    writeln('a:', f128tos(a), ' b:', f128tos(b), ' a>b:',
      a > b, ' a<b:', a < b, '  a=a', a = a, ' a<>b: ', a <> b,
      ' a>=b: ', a >= b, ' a<=b: ', a <= b);
    a := sTof128('1.23456e123');
    writeln('a:', f128tos(a), 'b:', f128tos(b), ' a**b:', f128tos(a ** b));
  end;

  procedure testMult;
  begin

    a := NP.rand([3, 3]);
    b := NP.rand([3]);
    d := a * b;
    writeln(a.toPython('a'));
    writeln(b.toPython('b'));
    writeln(d.toPython('axb'));
  end;


  procedure testDot;

    procedure prShape;
    begin
      writeln('a:', toString(a.shape), ' b:', toString(b.shape),
        ' d:', toString(d.shape));
    end;

  begin

    a := NP.rand([7]);
    b := NP.rand([7]);
    d := a.dot(b);
    prShape;

    a := NP.rand([7, 3]);
    b := NP.rand([3]);
    d := a.dot(b);
    prShape;

    a := NP.rand([7]);
    b := NP.rand([7, 3]);
    d := a.dot(b);
    prShape;

    a := NP.rand([5, 6]);
    b := NP.rand([6, 5]);
    d := a.dot(b);
    prShape;

    a := NP.rand([5, 6, 7]);
    b := NP.rand([2, 7, 6]);
    d := a.dot(b);
    prShape;


    a := NP.rand([3, 4, 16]);
    b := NP.rand([16, 2]);
    d := a.dot(b);
    prShape;

    a := NP.rand([3, 5, 4, 6]);
    b := NP.rand([6, 7]);
    d := a.dot(b);
    prShape;

    Assign(tx, 'python.py');
    try
      rewrite(tx);
      writeln(tx, a.toPython('a'));
      writeln(tx, b.toPython('b'));
      writeln(tx, d.toPython('d'));

      a := NP.rand([5, 5]);
      writeln(tx, a.toPython('ai'));
      writeln(tx, (a.inv).toPython('ainv'));
      writeln(tx, 'dd=dot(a,b)');

    finally
      closefile(tx);
    end;

  end;


  procedure testInv;

    function fpTrunc(v: FP): FP;
    begin
      Result := trunc(v);
    end;

  const
    n = 40;  // > 500 more will generate overflows in det calc
  begin
    a := NP.rand([n, n]);
    b := a.inv;
    //det := a.detBareiss;


    Assign(tx, 'python.py');
    try
      rewrite(tx);

      writeln(tx, 'import numpy as np');
      writeln(tx, 'det=np.linalg.det');
      writeln(tx, 'inv=np.linalg.inv');

      writeln(tx);
      writeln(tx);

      writeln(tx, a.toPython('a'));
      writeln(tx, 'print("shape a:", a.shape)');

      writeln(tx, 'deta=', a.det.toPython('deta'));

      writeln(tx, b.toPython('ainv'));

      d := (a.dot2x2(b)).apply(@fpTrunc);
      writeln(tx, d.toPython('adotb'));

      writeln(tx, 'print("det(a):", det(a), "deta:", deta)');
      writeln(tx, 'print("inv error:", abs(np.mean(inv(a)-ainv)))');
      writeln(tx, 'r,r=adotb.shape');
      writeln(tx, 'print("a * inv(a).dot error:",np.mean(adotb-np.eye(r,r)))');
    finally
      closefile(tx);
    end;
  end;

  procedure testall;
  begin
    a := NP.linspace(0, 1, 100);
    writeln(a.toPython('a'));

    a := NP.eye(10);
    writeln(a.toPython('a'));

    a := TNumPas<FP>.ones([3, 3]);
    writeln(a.toPython('a'));

    a := [1.0, 2, 3, 4, 5, 6, 7, 8, 9];
    writeln(a.topython('a'));

    a := NP.fromString('1.0, 2, 3, 4, 5, 6, 7, 8, 9');
    writeln(a.topython('a'));

    a := NP.Create([10, 10, 100, 100, 50]);
    a.setValue([0, 0, 0, 0], 111);


    b := a.slice([0, 0, 0, 0]);
    b.setValue(123);
    b := NP.arange(10);
    b += b;
    writeln(b.toPython('a'));

    repeat
      b := NP.rand([3, 3, 3]);
      d := b.det;
      if not d.allEQ(0) then
      begin
        writeln('inv:', b.inv.toPython('a'));
        break;
      end
      else
        Write('.');
    until False;

    writeln('b shape:', toString(b.shape));
    writeln('----b:', b.toPython('b'));
    writeln('----det b:', b.det.toString);
    //writeln(format('sum:%.2f, max:%.2f min:%.2f, avg:%.2f',     [b.sum, b.max, b.min, b.mean]));

    writeln('b.sort:', b.sort.toPython('bsort'));

    readln;

  end;

begin
  //testf128;
  //testInv;
  testDot;


  writeln('# --- done');
  readln;
end.
