{$mode delphi}
program testNumPas;

uses
  SysUtils,
  uNumPas;

const
  d1: TArrInt = [10000];
  d2: TArrInt = [200, 300];
  d2sq: TArrInt = [100, 100];
  dn: TArrInt = [10, 20, 40, 60];
  dnb: TArrInt = [20, 60, 40];
  da1: TArrInt = [4, 6, 20, 20];

type
  TReal = double;
  NP = TNumPas<TReal>;
var
  a, b, d: NP;
  lap: int64;

  procedure initMsg(m: string);
  begin
    lap := getTickCOunt64;
    Write(m);
  end;

  procedure endMsg;
  begin
    writeln(' -- ok, lap:', getTickCOunt64 - lap, ' ms');
  end;

  procedure testIndexing;
  var
    i, j: integer;
    v: TArrInt;
  begin
    initMsg('testing Indexing...1d.');

    // 1d indexing
    a := NP.Create(d1);
    assert(a.checkDims);

    for i := 0 to pred(a.size) do a[i] := i;
    for i := 0 to pred(a.size) do assert(a[i] = i, format('error in index %d', [i]));

    // 1d indexing d2 dim
    a := NP.Create(d2);

    for i := 0 to pred(a.size) do a[i] := i;
    for i := 0 to pred(a.size) do assert(a[i] = i, format('error in index %d', [i]));

    // 2d indexing
    Write('2d.');

    a := NP.Create(d2);
    assert(a.checkDims);
    for i := 0 to a.hi(1) do
      for j := 0 to a.hi(0) do a[i, j] := i * a.hi(1) + j;

    for i := 0 to a.hi(1) do
      for j := 0 to a.hi(0) do
        assert(a[i, j] = i * a.hi(1) + j, format('error in index [%d,%d]', [i, j]));

    // n dim indexing
    Write('nd.');
    a := NP.Create(dn);
    i := 0;
    for v in a.combinations(a.fDims) do
    begin
      a[v] := i;
      Inc(i);
    end;
    for v in a.combinations(a.fDims) do
    begin
      assert(a[v] = i, format('error in n dim indexing %d', [i]));
      Inc(i);
    end;

    endMsg;
  end;

  procedure testFuncs1;
  var
    t, p: TReal;
  begin
    initMsg('testing funcs.');

    a := NP.rand(dn);
    b := a.copy;

    assert(a = b, 'error comparing = copy NP');
    assert(not (a <> b), 'error comparing <> copy NP');
    assert(not (a > b), 'error comparing > copy NP');
    assert(not (a < b), 'error comparing < copy NP');
    assert((a <= b), 'error comparing <= copy NP');
    assert((a >= b), 'error comparing >= copy NP');

    assert(a.sum = b.sum, 'error on sum NP');
    assert(a.max = b.max, 'error on max NP');
    assert(a.min = b.min, 'error on min NP');
    assert(a.mean = b.mean, 'error on mean NP');

    a.toFile('test.bin');
    b.fromFile('test.bin');
    assert(a = b, 'error to/from file');

    deletefile('test.bin');

    p := -1; // sort
    for t in a.sort do
    begin
      assert(p < t, 'error not sorted');
      p := t;
    end;

    assert(a.sort.sum = a.sum, 'error in multidimensional sorting');

    a.setValue(1);  // setValue
    assert(a.sum = a.size, 'error in setValue');

    a := NP.rand(dn); // dot prod
    b := NP.rand(dnb);

    d := a.dot(b);

    a.toFile('ad.bin');
    b.toFile('bd.bin');
    d.toFile('dd.bin');

    writeln;
    writeln('testing dot prod.');
    writeln(format('ad=np.fromfile("ad.bin", dtype=np.double).reshape(%s)',
      [toString(a.fDims)]));
    writeln(format('bd=np.fromfile("bd.bin", dtype=np.double).reshape(%s)',
      [toString(b.fDims)]));
    writeln(format('dd=np.fromfile("dd.bin", dtype=np.double).reshape(%s)',
      [toString(d.fDims)]));
    writeln('np.mean(np.dot(ad,bd)-dd) # sould be near 0');

    a := NP.rand(da1); // det/inv
    b := a.det;

    if a.allNE(0) then
    begin
      b := a.inv;

      a.toFile('a2.bin');
      b.toFile('ainv.bin');

      writeln(format('a2=np.fromfile("a2.bin", dtype=np.double).reshape(%s)',
        [toString(a.fDims)]));
      writeln(format('ainv=np.fromfile("ainv.bin", dtype=np.double).reshape(%s)',
        [toString(b.fDims)]));
      writeln('np.mean(np.dot(a2,ainv) - np.dot(a2,np.linalg.inv(a2))) # near 0');
      writeln;

      d := a.dot(a.inv);
      Write(format('inv matrix acc. error:%.1g', [abs(d.sum - a.dim(0)) / d.size]));
    end
    else
      Write('det=0, no inv matrix');

    a := NP.rand(dn);  // transpose
    b := a.transpose;

    a.toFile('a.bin');
    writeln;
    writeln('# numpy read binary data ');
    writeln(format('a = np.fromfile("a.bin", dtype=np.double).reshape(%s)',
      [toString(a.fDims)]));
    b.toFile('b.bin');
    writeln(format('b = np.fromfile("b.bin", dtype=np.double).reshape(%s)',
      [toString(b.fDims)]));
    writeln('np.sum(a.T - b) # should be 0');

    endMsg;
  end;

begin

  testIndexing;
  testFuncs1;

  writeln('--- end of test --- press <enter>');
  readln;
end.
