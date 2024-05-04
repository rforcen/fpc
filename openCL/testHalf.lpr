{
  half float16 opencl / native pascal ST/MT comparision on complex arithmetics:

  a[i] * b[i] + a[i] - b[i] / a[i];

  this is for a ryzen 7 5700G w/ embedded radeon graph  2GB RAM

  lap half opencl:    31 ms
  lap half pas   :  1078 ms
  lap half MT    :   172 ms

  ratio ST/CL: 34.7
  ratio MT/CL: 5.5
}

{$mode delphi}

program testHalf;

uses
  SysUtils,
  uOpenCL,
  uf16,
  mtprocs;

const
  n = 10 * 1000 * 1000;

var
  a, b: array of half;
  t0: int64;

  procedure write_a;
  const nitems=20;
  var
    i: integer;
  begin
    for i := 0 to nitems do
      Write(format('%.3f ', [single(a[i])]));
    writeln;
    for i := high(a) - nitems to high(a) do
      Write(format('%.3f ', [single(a[i])]));
    writeln;
    writeln;
  end;

  procedure load_ab;
  const
    va = 1.1;
    vb = 2.2;
  var
    i: integer;
  begin
    a := nil;
    b := nil;
    setLength(a, n);
    setLength(b, n);

    for i := 0 to high(a) do
    begin
      a[i] := va;
      b[i] := vb;
    end;
  end;

  procedure testHalfOCL;
  var
    oclH: TOpenCL;
    ba, bb: TCLBuffer;
  begin

    oclH.AMDdevice; // init amd device

    oclH.compileFile('cl/half.cl', 'halfArith');

    load_ab;

    ba := oclH.buffer<half>(a); // a,b buffers
    bb := oclH.buffer<half>(b);

    oclH.setArgs([ba, bb]);    // args a,b

    t0 := gettickCount64;

    oclH.Write<half>(a, ba); // write ba=a, bb=b buffers
    oclH.Write<half>(b, bb);
    oclH.run(n);    // run
    oclH.Read<half>(a, ba);  // read buffer a=ba

    t0 := gettickCount64 - t0;

    write_a;

    oclH.Free; // release

    writeln(format('lap half opencl         : %5d ms', [t0]));
  end;

  procedure testIter;
  var
    i: integer;
  begin
    load_ab;

    t0 := gettickCount64;
    for i := 0 to high(a) do
      a[i] := a[i] * b[i] + a[i] - b[i] / a[i];
    t0 := gettickCount64 - t0;

    writeln(format('lap half pas            : %5d ms', [t0]));
    write_a;
  end;

  procedure testMT;

    function getNThreads: integer;
    begin
      {$ifdef windows}
      result:=getCPUCount;
      {$else}
      Result := GetSystemThreadCount;
      {$endif}
    end;

    procedure _gen_ab(i: PtrInt; {%H-}pnt: pointer; Item: TMultiThreadProcItem);
    var
      delta: integer;
    begin
      delta := Item.Group.EndIndex + 1;
      while i < n do
      begin
        a[i] := a[i] * b[i] + a[i] - b[i] / a[i];
        Inc(i, delta);
      end;
    end;

  begin

    load_ab;

    t0 := gettickCount64;
    ProcThreadPool.DoParallelLocalProc(@_gen_ab, 0, getNThreads - 2, nil);
    t0 := gettickCount64 - t0;

    writeln(format('lap half MT, %d threads : %5d ms', [getNThreads, t0]));
    write_a;

  end;

begin
  writeln(format('opengl native pas bench, %d iters', [n]));

  testHalfOCL;
  testIter;
  testMT;

  writeln('end');
  readln;
end.
