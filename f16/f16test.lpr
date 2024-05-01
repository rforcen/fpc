program f16test;

uses
  SysUtils,
  uf16;

  procedure halfTest;
  const
    n = 100000000;
  var
    a, b, c, d: half;
    s, sa, sb, sc, sd: single;
    i: integer;
    t0: int64;
  begin
    a := half_pi;
    //a := 3.141592;
    s := a;

    b := a * 2;
    c := a / b;
    writeln('a:', toString(a), ', b:', toString(b), ', c:', toString(c), ', s:', s);

    c := a - b;
    writeln('a:', toString(a), ', b:', toString(b), ', c:', toString(c), ', s:', s);

    if a = b then writeln('a=b')
    else
      writeln('a<>b');

    if a > b then writeln('a>b')
    else
      writeln('a<=b');

    // half
    t0 := getTickCount64;
    for i := 0 to n do
    begin
      c := a + b;
      d := c / b;
    end;
    writeln('lap half arithmetics ', n, ':', getTickCount64 - t0, ' ms');

    // single
    t0 := getTickCount64;
    sa := a;
    sb := b;
    sc := c;
    sd := d;

    for i := 0 to n do  // half is about 100 times slower than single
    begin
      sc := sa + sb;
      sd := sc / sb;
    end;
    writeln('lap single           ', n, ':', getTickCount64 - t0, ' ms');

    // conversion
    t0 := getTickCount64;
    for i := 0 to n do
    begin
      c := half(s);
      s := single(c);
    end;
    writeln('lap conversion       ', n, ':', getTickCount64 - t0, ' ms');

  end;

begin
  halfTest;

  readln;
end.
