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

type
  TSingle4 = array[0..3] of single; // 128 bits xmm#
  THalf8 = array[0..7] of half;
  THalf4 = array[0..3] of half;
  pTSingle4 = ^ TSingle4;
  pTHalf8 = ^THalf8;
  pTHalf4 = ^THalf4;


var
  a, b: array of half;
  t0: int64;

  procedure write_a;
  const
    nitems = 19;
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

  {$asmmode intel}
  procedure testSingleConv; // even slower
  var
    sa, sb: single;
    i, j: integer;
    ha, hb: half;

    f4a, f4b: TSingle4;
    h8, h8a: THalf8;
    pf4: pTSingle4;
    ph8: pTHalf8;
  begin

    load_ab;

    t0 := gettickCount64;

    i := 0;
    while i < length(a) do
    begin
      // h8a:=a[i]
      ph8 := pTHalf8(@a[i]);
      asm // convert THalf8 -> TSingle4
               MOV     R11,[ph8]
               MOVUPS  XMM0,[R11]
               DB      $c4, $e2, $79, $13, $c0 // VCVTPH2PS XMM0,XMM0  //  c4 e2 79 13 c0
               MOVUPS  [f4a],XMM0
      end;

      // h8b:=b[i]
      ph8 := pTHalf8(@b[i]);
      asm // convert THalf8 -> TSingle4
               MOV     R11,[ph8]
               MOVUPS  XMM0,[R11]
               DB      $c4, $e2, $79, $13, $c0 // VCVTPH2PS XMM0,XMM0  //  c4 e2 79 13 c0
               MOVUPS  [f4b],XMM0
      end;

      // evaluate: sa * sb + sa - sb / sa;
      for j := 0 to 3 do f4a[j] := f4a[j] * f4b[j] + f4a[j] - f4b[j] / f4a[j];

      asm  // convert single to half f4a -> half(f4a)
               MOVUPS  XMM0,[f4a]
               DB      $c4, $e3,  $79, $1d, $c0, $00 // vcvtps2ph xmm0,xmm0,0x0
               MOVUPS   [f4a],XMM0
      end;

      pTHalf4(@a[i])^ := pTHalf4(@f4a)^;

      Inc(i, 4);
    end;

    t0 := gettickCount64 - t0;

    writeln(format('lap half pas simd intrinsic w/single   : %5d ms', [t0]));
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

    procedure _gen_ab_simd(i: PtrInt; {%H-}pnt: pointer; Item: TMultiThreadProcItem);
    begin

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

  testSingleConv;
  testHalfOCL;
  testIter;
  testMT;


  writeln('end');
  readln;
end.
