program testBT;

uses
  SysUtils,
  btree;

const
  n = 100;
var
  bt: TBtree;
  k: string;
  recno: integer = 0;
  i: integer;

  procedure testOpenFind;
  begin
    writeln('test open/find');
    bt := TBtree.Open('test.ndx');

    if not bt.ok then writeln('error opening ndx file')
    else
    begin
      for i := 0 to pred(n) do
      begin
        k := format('%0.5d', [i]);
        if i mod 1000 = 0 then Write(#13, 'key:', k);

        if bt.find(k, recno) then
          assert(i = recno)
        else
          assert(False, 'key not found:' + k);

        //if bt.Next(k, recno) then
        //  writeln('key:', k, ' recno:', recno);
      end;

      bt.Close;

      writeln('ok');

    end;
  end;

  procedure testCreate;
  var
    recno: integer = 0;
  begin
    writeln('test create/add keys');
    bt := TBtree.Create('test.ndx', 5, True); // keylen, unique

    for recno := 0 to pred(n) do
    begin
      k := format('%0.5d', [recno]);
      if recno mod 1000 = 0 then Write(#13, 'key:', k);

      if not bt.Add(k, recno) then assert(False, 'error adding key');
    end;

    bt.Close;

    writeln('ok, added ', n, ' keys');
  end;

  procedure testDeleteAll;
  begin
    writeln('test delete all');
    bt := TBtree.Open('test.ndx');

    if not bt.ok then writeln('error opening ndx file')
    else
    begin
      for i := 0 to pred(n) do
      begin
        k := format('%05d', [i]);
        if i mod 1000 = 0 then Write(#13, 'key:', k);

        if not bt.eraseEQ(k) then
          assert(False);
      end;

      bt.Close;

      writeln('ok');

    end;
  end;

  procedure testPopulate;
  var
    recno: integer = 0;
  begin
    writeln('test populate all');
    bt := TBtree.Open('test.ndx');

    if not bt.ok then writeln('error opening ndx file')
    else
    begin
      for recno := 0 to pred(n) do
      begin
        bt.add(format('%05d', [recno]), recno);
        if recno mod 1000 = 0 then Write(#13, 'key:', recno);
      end;
    end;
    bt.Close;
  end;

  procedure testTraverse;
  var
    kr: TKeyRecno;
  begin
    writeln('test traverse all');
    bt := TBtree.Open('test.ndx');

    if not bt.ok then writeln('error opening ndx file')
    else
    begin
      for kr in bt do
        Write('-', kr.key, '-', ' ', kr.recno, ',', #9);
      writeln;

      bt.Close;
    end;
  end;

begin
  //testCreate;
  //testOpenFind;
  //testDeleteAll;
  //testPopulate;

  testTraverse;

  printSizes;

  writeln('end.');
  readln;
end.
