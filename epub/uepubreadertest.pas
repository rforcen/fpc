{ epub reader test }
unit uepubReaderTest;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Grids, ExtCtrls, epubReader;

type

  { TForm1 }

  TForm1 = class(TForm)
    btOpen: TButton;
    CheckBox1: TCheckBox;
    cbOrder: TComboBox;
    edFind: TEdit;
    gb: TGroupBox;
    od: TOpenDialog;
    Panel1: TPanel;
    sb: TStatusBar;
    sg: TStringGrid;
    sgcw: TStringGrid;
    procedure btOpenClick(Sender: TObject);
    procedure cbOrderChange(Sender: TObject);
    procedure edFindEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sgClick(Sender: TObject);
    procedure sgcwClick(Sender: TObject);
  private
    procedure populateSG;
  end;



var
  Form1: TForm1;
  epub: TEpubReader;
  epubIndex: TEpubIndexer;
  t0: int64;

implementation

{$R *.lfm}


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  epubIndex := nil;
end;

procedure TForm1.cbOrderChange(Sender: TObject);
begin
  populateSG;
end;

procedure TForm1.edFindEditingDone(Sender: TObject);
var
  ix: integer = 0;
  k: string;
begin
  k := edFind.Text;
  if epubIndex.find(k, ix) then
    if cbOrder.ItemIndex = 0 then  sg.Row := ix + 1 // word
    else
    if epubIndex.findIndex(ix) then
      sg.Row := ix + 1;
end;


procedure TForm1.btOpenClick(Sender: TObject);
begin
  if od.Execute then
  begin
    t0 := getTickCount64;

    if fileexists(od.filename) then
    begin
      epub := TEpubReader.Create(od.filename);

      epubIndex.Free;
      epubIndex := TEpubIndexer.Create;

      epubIndex.indexEpub(epub);

      epub.Free;

      populateSG;
    end;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  epubIndex.Free;
end;

procedure TForm1.sgClick(Sender: TObject);
const
  nw = 15;
var
  i, ix: integer;
  clWrds: TArrMinPos; // closest words pos and min distance
begin
  if cbOrder.ItemIndex = 0 then ix := sg.row - 1
  else
    ix := epubIndex.countMap.Data[sg.row - 1];

  gb.Caption := 'closest words to:' + string(epubIndex.word(ix));

  sgcw.rowcount := nw + 1;
  sgcw.Clean([gzNormal, gzFixedRows]);

  clWrds := epubIndex.closestIndex(ix, nw); // calcDistances(ix, nw);

  for i := 0 to pred(nw) do
  begin
    sgcw.Cells[0, i + 1] := IntToStr(clWrds[i].min);
    sgcw.Cells[1, i + 1] := string(epubIndex.wordOrg(clWrds[i].pmin));
    sgcw.Cells[2, i + 1] := intToStr(length(epubIndex.position(clWrds[i].pmin)));
  end;
end;

procedure TForm1.sgcwClick(Sender: TObject);
var
  ix: integer = 0;
begin
  if epubIndex.find(sgcw.Cells[1, sgcw.Row], ix) then
    if cbOrder.ItemIndex = 0 then  sg.Row := ix + 1
    else
    begin
      epubIndex.findIndex(ix);
      sg.Row := ix + 1;
    end;
end;

procedure TForm1.populateSG;
var
  ix, index: integer;
begin
  sg.clean([gzNormal, gzFixedRows]);
  Form1.Caption := 'epub indexer';
  sb.SimpleText := '';


  if epubIndex.nWords > 0 then
  begin
    sg.RowCount := epubIndex.nWords + 1;

    for ix := 1 to epubIndex.nWords do
    begin
      if cbOrder.ItemIndex = 0 then  index := ix - 1 // word
      else
        index := epubIndex.countMap.Data[ix - 1]; // count reverse order

      sg.Cells[0, ix] := IntToStr(ix);
      sg.Cells[1, ix] := string(epubIndex.wordOrg(index));
      sg.Cells[2, ix] := IntToStr(epubIndex.wordIndex(index));

      if cbOrder.ItemIndex = 0 then // word
        sg.Cells[3, ix] := IntToStr(length(epubIndex.position(index)))
      else
        sg.Cells[3, ix] := IntToStr(-epubIndex.countMap.Keys[ix - 1]);
    end;
    Form1.Caption := 'epub indexer: ' + od.filename;
    sb.SimpleText := format('lap:%d ms, # unique words: %d',
      [getTickCount64 - t0, epubIndex.nWords]);
  end;
end;

end.
