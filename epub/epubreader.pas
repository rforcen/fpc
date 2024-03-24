unit epubReader;

{$mode delphi}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, Math, zipper, sax_html, dom_html, dom, fgl,
  Generics.Defaults, Generics.Collections, strUtils;

const
  emptyWordsSpanish: array of string =
    ['', 'a', 'acá', 'ahí', 'al', 'algo', 'algún', 'alguna',
    'alguno', 'algunos', 'allá', 'allí', 'ambos', 'ante', 'antes',
    'aquel', 'aquella', 'aquello', 'aquellas', 'aquí', 'arriba',
    'así', 'atrás', 'aun', 'aunque', 'bien', 'cada', 'casi', 'como',
    'con', 'cual', 'cuales', 'cualquier', 'cuan', 'cuando',
    'cuanto', 'de', 'del', 'demás', 'desde', 'donde', 'dos', 'el', 'él',
    'ella', 'ello', 'ellos', 'en', 'eres', 'esa', 'ese', 'eso', 'esos', 'esta', 'estas',
    'este', 'esto', 'estos', 'etc', 'ha',
    'hasta', 'la', 'lo', 'las', 'los', 'me', 'mi', 'mis', 'mía', 'mías',
    'mientras', 'muy', 'ni',
    'nosotras', 'nosotros', 'nuestra', 'nuestro', 'nuestras', 'nuestros', 'os',
    'otra', 'otro', 'otros', 'para', 'pero',
    'pues', 'que', 'qué', 'si', 'sí', 'siempre', 'siendo', 'sin', 'sino', 'so',
    'sobre', 'sr', 'sra', 'sres', 'sta', 'su', 'sus', 'te', 'tu', 'tus',
    'un', 'una', 'uno', 'unos',
    'usted', 'ustedes', 'vosotras', 'vosotros', 'vuestra', 'vuestro',
    'vuestras', 'vuestros',
    'y', 'ya', 'yo',

    'por', 'porque', 'porqué', 'no', 'según', 'tras', 'se',
    'más', 'le', 'es', 'hacia', 'todo', 'entre', 'sólo',
    'través', 'está', 'tan', 'o', 'u', 'e',
    'nos', 'así', 'hay', 'aún', 'ti', 'había', 'era', 'fue', 'entonces',
    'fuera', 'quien', 'tenia', 'todos'];

const
  specialCharEquivalence: array of
    unicodestring =
    ['a', 'áàâä', 'e', 'èéêë', 'i', 'ìíîï', 'o', 'óòôö',
    'u', 'ùúûü', 'n', 'ñ'];

type

  TArrInt = array of integer;

  TMinPos = record
    pmin, min: integer;
  end;
  TArrMinPos = array of TMinPos;

  TWordInfo = record
    word: unicodestring;
    index, Count: integer;
  end;

  TArrWordInfo = array of TWordInfo;

  { TEpubReader }

  TEpubReader = class
  public
  type
    TArrString = array of unicodestring;

    TIndex = record
      wordIndex: integer;
      word: unicodestring;
      position: array of integer;
    end;
    pTIndex = ^TIndex;

    TItem = record
      word: unicodestring;
      wordAscii: string;
      position: integer;
    end;

    { TWordEnumerator }

    TWordEnumerator = record
      function GetCurrent: TItem;
      function MoveNext: boolean;
      property Current: TItem read GetCurrent;
    public
      item: TItem;
      pallContent: punicodestring;
    end;

  public
    constructor Create(fileName: string);
    destructor Destroy; override;

    function getContent: TArrString;
    function getFilesNames: TStringList;
    function getHtml: TArrString;
    function getAll: unicodestring;

    function GetEnumerator: TWordEnumerator; // traverse allContent in word tokens
  private
    unZip: TUnZipper;
    filesNames: TStringList;
    content, html: TArrString; // per file content:text in html, html:original html code
    allContent: unicodestring; // all the epub file plain text
  private
    procedure DoCreateOutZipStream(Sender: TObject; var AStream: TStream;
    {%H-}AItem: TFullZipFileEntry);
    procedure DoDoneOutZipStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
  end;

  { TDistMatrix }

  TDistMatrix = record // lower triangular matrix excluding diagonal
    m: array of array of integer;

    procedure init(nw: integer);

    function rget(i, j: integer): integer; overload; inline;
    procedure rput(i, j: integer; AValue: integer); overload; inline;
    property item[i, j: integer]: integer read rget write rput; default;
  end;

  { TEpubIndexer }

  TEpubIndexer = class
  public
    constructor Create;
    destructor Destroy; override;

    function isEmptyWord(const word: string): boolean;
    procedure indexEpub(const epub: TEpubReader);

    function nWords: integer;
    function word(ix: integer): string;
    function wordOrg(ix: integer): unicodestring;
    function wordIndex(ix: integer): integer;
    function position(ix: integer): TArrInt;
    function find(k: string; var ix: integer): boolean;
    function findIndex(k: string; var ix: integer): boolean; overload;
    function findIndex(var ix: integer): boolean; overload;
    // find key return countMap index
    procedure createDistMatrix;
    function calcDistances(wix, nwords: integer): TArrMinPos;
    function closestIndex(wix, nwords: integer): TArrMinPos;
    function minDistanceWord(wix1, wix2: integer): integer;
      overload;// distance between words
    function minDistanceWord(wix: integer): integer; overload; // closest word to wix
    function minDistance(wix: integer): integer;
    function closestWord(ix: integer): unicodestring;

    //private
  public
    wordDict: TDictionary<string, TEpubReader.TIndex>;
    wordMap: TFPGMap<string, TEpubReader.TIndex>;
    countMap: TFPGMap<integer, integer>; // count, word ix
    emptyWordsDict: TDictionary<string, integer>;
    spCharEq: TFPGMap<unicodechar, unicodestring>;

    globalPos: integer;

    distMatrix: TDistMatrix;// triangular matrix of word pos w/min distance
  private
    procedure emptyWords;
  end;

function StrtoAscii(const inStr: string): string;
function extractParagraphsFromHTML(const htmlIn: string): string;

implementation

{ convert string w/accenteed vowels + ñÑ, etc. to plain ascii, ñÑ->nN }
function StrtoAscii(const inStr: string): string;

  function asciiStr(ch1, ch2: char): char;
  const
    VowelsNn: string = 'nNAAEIOUAEIOUaeiouaeiou';
    AccentedVowelsNn: string = 'ñÑÆÁÉÍÓÚÄËÏÖÜáéíóúäëïöü';
  var
    i: integer = 1;
  begin
    Result := #0;
    while i < length(AccentedVowelsNn) do
    begin
      if (AccentedVowelsNn[i] = ch1) and (AccentedVowelsNn[i + 1] = ch2) then
        exit(VowelsNn[1 + (i div 2)]);
      Inc(i, 2);
    end;
  end;

var
  ch: char;
  i: integer;
begin
  Result := '';
  i := 1;

  while i <= length(inStr) do
  begin
    ch := inStr[i];

    case ch of
      #$20..#$7e: ; // ascii

      #$E2: begin
        if inStr[i + 1] = #$80 then ch := '"' {“}
        else
          ch := ' ';
        Inc(i, 2);
      end;
      #$C2: begin
        Inc(i);
        case inStr[i] of
          #$A1: ch := '!'; {¡}
          #$Ab, #$bb: ch := '"';
          #$b5: ch := 'u';
          else
            ch := ' ';
        end;
      end;
      #$C3: begin
        Inc(i);
        ch := asciiStr(ch, inStr[i]);
      end;

      else
        ch := ' '; // seperator
    end;

    Result += ch;

    Inc(i);
  end;
end;

// very simplified version to extract paragraphs from html <p...>text</p>
function extractParagraphsFromHTML(const htmlIn: string): string;
var
  i, f, t: integer;
  txt: string;

  function extractTag(const html, startTag, endTag: string): string;
  var
    lst: integer;
  begin
    Result := '';
    lst := length(startTag);
    f := pos(startTag, html);
    if f > 0 then
    begin
      t := pos(endTag, html, f + lst);
      Result := copy(html, f + lst, t - f - lst) + ' ';
    end;
  end;

  procedure scan(const tkStart, tkEnd: string);
  begin
    f := pos(tkStart, txt);
    if f > 0 then
    begin
      f := pos('>', txt, f + 2);
      t := pos(tkEnd, txt, f + 1);
      txt := copy(txt, f + 1, t - f - 1);
    end;
  end;

  procedure scanDel(const tkStart: string);
  begin
    f := pos(tkStart, txt);
    if f > 0 then
    begin
      t := pos('/>', txt, f + 3);
      Delete(txt, f, t - f + 2);
    end;
  end;

  function scanParagraph(const html: string; var i: integer): string;
  begin
    Result := '';
    f := pos('<p', html, i);
    if f > 0 then   // <p...>.....</p>
    begin
      f := pos('>', html, f + 2);
      t := pos('</p>', html, f + 1);
      i := t + 4;

      Result := copy(html, f + 1, t - f - 1);
    end;
  end;

begin
  Result := extractTag(htmlIn, '<title>', '</title>');

  i := 1;

  repeat
    txt := scanParagraph(htmlIn, i);

    if length(txt) > 0 then    // clear txt
    begin
      scan('<b', '</b>');
      scan('<u', '</u>');
      scan('<i', '</i>');
      scan('<span', '</span>');    // etc...

      scanDel('<br');
      scanDel('<img');

      Result += txt + #$0a;
    end

    else
      break;
  until i >= length(htmlIn);
end;


{ TEpubReader }

constructor TEpubReader.Create(fileName: string);
begin
  filesNames := TStringList.Create;
  content := nil;
  allContent := '';

  unZip := TUnZipper.Create;

  unZip.OnCreateStream := DoCreateOutZipStream;
  unZip.OnDoneStream := DoDoneOutZipStream;

  unZip.UnZipAllFiles(fileName);
end;

destructor TEpubReader.Destroy;
begin
  unZip.Free;
  filesNames.Free;
  content := nil;
  html := nil;

  inherited Destroy;
end;

function TEpubReader.getContent: TArrString;
begin
  Result := content;
end;

function TEpubReader.getFilesNames: TStringList;
begin
  Result := filesNames;
end;

function TEpubReader.getHtml: TArrString;
begin
  Result := html;
end;

function TEpubReader.getAll: unicodestring;
begin
  Result := allContent;
end;

{ TEpubReader.TWordEnumerator }

function TEpubReader.TWordEnumerator.GetCurrent: TItem;
begin
  Result := item;
end;

function TEpubReader.TWordEnumerator.MoveNext: boolean;  // set item w/next word

  // extended ascii table character
  function isExtendedLetter(ch: unicodechar): boolean;

    function rng(f, t: unicodechar): boolean; inline;
    begin
      Result := (ch >= f) and (ch <= t);
    end;

  begin  // https://en.wikipedia.org/wiki/Latin-1_Supplement
    Result := (ch in ['A'..'Z', 'a'..'z']) or rng(#$C0, #$D6) or
      rng(#$D8, #$F6) or rng(#$F8, #$FF);
  end;

var
  ch: unicodechar;
begin
  if item.position >= length(pallContent^) then exit(False);
  item.word := '';

  repeat

    ch := pallContent^[item.position];

    Inc(item.position);

    if isExtendedLetter(ch) then  item.word += ch
    else
      break;

  until item.position = length(pallContent^);

  item.word := LowerCase(item.word);
  item.wordAscii := LowerCase(StrToAscii(string(item.word)));

  Result := True;
end;

{ TDistMatrix }

procedure TDistMatrix.init(nw: integer);
var
  i: integer;
begin
  setLength(m, nw - 1);
  for i := 0 to pred(nw - 1) do
    setLength(m[i], i + 1);
end;

function TDistMatrix.rget(i, j: integer): integer;
begin
  if i = j then Result := 0
  else
  if i < j then Result := m[j - 1, i]
  else
    Result := m[i - 1, j];
end;

procedure TDistMatrix.rput(i, j: integer; AValue: integer);
begin
  if i <> j then
    if i < j then m[j - 1, i] := AValue
    else
      m[i - 1, j] := AValue;
end;

{ TEpubIndexer }

constructor TEpubIndexer.Create;
begin
  wordMap := TFPGMap<string, TEpubReader.TIndex>.Create; // map words/[index]
  //wordMap.Sorted := True;

  wordDict := TDictionary<string, TEpubReader.TIndex>.Create;
  countMap := TFPGMap<integer, integer>.Create;

  emptyWords;
  globalPos := 0;
end;

destructor TEpubIndexer.Destroy;
begin
  wordMap.Free;
  wordDict.Free;
  emptyWordsDict.Free;
  countMap.Free;
end;

function TEpubIndexer.isEmptyWord(const word: string): boolean; inline;
begin
  Result := emptyWordsDict.ContainsKey(StrToAscii(word));
end;

procedure TEpubIndexer.indexEpub(const epub: TEpubReader);
var
  wordIndex: integer = 0;
  item: TEpubReader.TItem;
  ix: integer;
  index: TEpubReader.TIndex;
  glPos: integer;
  kv: TPair<string, TEpubReader.TIndex>;
begin
  for item in epub do  // traverse all words/position in epub
    if not isEmptyWord(item.wordAscii) then // empty word?
    begin
      glPos := item.position + globalPos; // global position

      if wordDict.ContainsKey(item.wordAscii) then // exists
      begin
        index := wordDict[item.wordAscii];  // yes, append position to array
        insert(glPos, index.position, length(index.position));
        wordDict.AddOrSetValue(item.wordAscii, index);
      end
      else
      begin // not exists -> wordIndex, first position
        index.position := [glPos];
        index.wordIndex := wordIndex;
        index.word := item.word;
        wordDict.Add(item.wordAscii, index);
        Inc(wordIndex);
      end;
    end;

  // post process
  for kv in wordDict do wordMap.Add(kv.Key, kv.Value);
  wordMap.sort;
  wordMap.sorted := True;

  Inc(globalPos, item.position);

  // add to count map <-nWords, wix>
  for ix := 0 to pred(wordMap.Count) do
    countMap.Add(-length(wordMap.Data[ix].position), ix);
  countMap.sort;
end;

function TEpubIndexer.nWords: integer;
begin
  Result := wordMap.Count;
end;

function TEpubIndexer.word(ix: integer): string;
begin
  Result := wordMap.Keys[ix];
end;

function TEpubIndexer.wordOrg(ix: integer): unicodestring;
begin
  Result := wordMap.Data[ix].word;
end;

function TEpubIndexer.wordIndex(ix: integer): integer;
begin
  Result := wordMap.Data[ix].wordIndex;
end;

function TEpubIndexer.position(ix: integer): TArrInt;
begin
  Result := wordMap.Data[ix].position;
end;

function TEpubIndexer.find(k: string; var ix: integer): boolean;
begin
  Result := wordMap.Find(strToAscii(k), ix);
end;

function TEpubIndexer.findIndex(k: string; var ix: integer): boolean;
var
  i: integer;
begin
  if find(strToAscii(k), ix) then
  begin
    i := countMap.IndexOfData(ix);
    if i <> -1 then
    begin
      ix := i;
      exit(True);
    end;
  end;
  Result := False;
end;

function TEpubIndexer.findIndex(var ix: integer): boolean;
var
  i: integer = 0;
begin
  i := countMap.IndexOfData(ix);
  if i <> -1 then
  begin
    ix := i;
    exit(True);
  end;
  Result := False;
end;

procedure TEpubIndexer.emptyWords; // create a dict
var
  s: string;
begin
  emptyWordsDict := TDictionary<string, integer>.Create;  // create dict

  for s in emptyWordsSpanish do
    if not emptyWordsDict.ContainsKey(StrToAscii(s)) then
      emptyWordsDict.Add(StrToAscii(s), 0);
end;

procedure TEpubIndexer.createDistMatrix;
var
  i, j, ip1, ip2, _min, nw: integer;
  p1, p2: TArrInt;
begin
  nw := wordMap.Count;
  distMatrix.init(nw);

  for i := 0 to pred(nw) do
  begin
    p1 := wordMap.Data[i].position;
    for j := 0 to pred(nw) do
      if i < j then   // lower triangular i<j
      begin
        p2 := wordMap.Data[j].position;
        _min := maxInt;

        for ip1 in p1 do for ip2 in p2 do
            _min := min(_min, abs(ip1 - ip2));

        distMatrix[i, j] := _min;
      end;
  end;
end;

function cmpMinPos(constref a, b: TMinPos): integer;
begin
  if a.min > b.min then Result := 1
  else if a.min < b.min then Result := -1
  else
    Result := 0;
end;

function TEpubIndexer.calcDistances(wix, nwords: integer): TArrMinPos;
var
  j, ip1, ip2, _min, pmin, nw, adiff: integer;
  p1, p2: TArrInt;

  pres: array of TMinPos;
begin
  nw := wordMap.Count;

  pres := nil;
  setLength(pres, nw);

  p1 := wordMap.Data[wix].position;

  for j := 0 to pred(nw) do
  begin
    p2 := wordMap.Data[j].position;

    _min := maxInt;
    pmin := 0;

    for ip1 in p1 do
      for ip2 in p2 do
      begin
        adiff := abs(ip1 - ip2);
        if (adiff > 0) and (adiff < _min) then
        begin
          _min := adiff;
          pmin := j;
        end;
      end;

    pres[j].min := _min;
    pres[j].pmin := pmin;
  end;

  TArrayHelper<TMinPos>.Sort(pres, TComparer<TMinPos>.Construct(cmpMinPos));
  Result := nil;
  setlength(Result, nwords);
  for j := 0 to pred(nwords) do Result[j] := pres[j];
end;

function TEpubIndexer.closestIndex(wix, nwords: integer): TArrMinPos;
var
  w1, j, nw: integer;
begin
  nw := wordMap.Count;

  Result := nil;
  setLength(Result, nw);

  w1 := wordMap.Data[wix].wordIndex;

  for j := 0 to pred(nw) do
    if j <> wix then
      with Result[j] do
      begin
        min := abs(w1 - wordMap.Data[j].wordIndex);
        pmin := j;
      end
    else
      with Result[j] do
      begin
        min := maxInt;
        pmin := 0;
      end;

  TArrayHelper<TMinPos>.Sort(Result, TComparer<TMinPos>.Construct(cmpMinPos));
  Result := Copy(Result, 0, nwords);
end;

function TEpubIndexer.minDistanceWord(wix1, wix2: integer): integer;
begin
  Result := distMatrix[wix1, wix2];
end;

function TEpubIndexer.minDistanceWord(wix: integer): integer;
var
  j, pm, _min: integer;
begin
  Result := wix;

  _min := maxInt;
  for j := 0 to pred(wordMap.Count) do
  begin
    pm := distMatrix[wix, j];
    if (pm <> 0) and (pm < _min) then
    begin
      _min := pm;
      Result := j;
    end;
  end;
end;

function TEpubIndexer.minDistance(wix: integer): integer;
var
  j, pm: integer;
begin
  Result := wix;

  Result := maxInt;
  for j := 0 to pred(wordMap.Count) do
  begin
    pm := distMatrix[wix, j];
    if (pm <> 0) and (pm < Result) then
      Result := pm;
  end;
end;

function TEpubIndexer.closestWord(ix: integer): unicodestring;
begin
  Result := wordMap.Data[minDistanceWord(ix)].word;
end;

{ TEpubReader }
function TEpubReader.GetEnumerator: TWordEnumerator;
begin
  with Result{%H-}.item do
  begin
    word := '';
    position := 1;
  end;
  Result.pallContent := @self.allContent;
end;

procedure TEpubReader.DoCreateOutZipStream(Sender: TObject;
  var AStream: TStream; AItem: TFullZipFileEntry);
begin
  AStream := TMemorystream.Create;
end;

procedure TEpubReader.DoDoneOutZipStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);

  function isHTML(const fn: string): boolean;
  const
    htmlExtensions: array of string = ['.html', '.htm', '.xhtml', '.shtml', '.shtml'];
  var
    s: string;
  begin
    for s in htmlExtensions do if pos(s, fn) <> 0 then exit(True);
    Result := False;
  end;

const

  useTHTML = True;
var
  fileContent: TStringList;
  doc: THTMLDocument;
  els: tdomnodelist;
  body: unicodestring = '';
  title: unicodestring = '';
  ptit: integer;
  strStream: TStringStream;
  strText: string;
begin
  if isHTML(AItem.ArchiveFileName) then
  begin
    filesNames.Append(AItem.ArchiveFileName);

    AStream.Position := 0;
    fileContent := TStringList.Create;
    fileContent.LoadFromStream(Astream); // got content here
    strText := fileContent.Text; // to string

    html += [strText];


    if pos('<?xml', fileContent[0]) > 0 then
      fileContent.Delete(0); // skip  <?xml version="1.0" encoding="utf-8"?>

    strStream := TStringStream.Create(fileContent.Text);
    ReadHTMLFile(doc, strStream);
    strStream.Free;

    els := doc.GetElementsByTagName('title');
    if els.Count > 0 then
      title := tdomelement(els[0]).textcontent;
    els.Free;

    els := doc.GetElementsByTagName('body');
    if els.Count > 0 then
      body := tdomelement(els[0]).textcontent;
    els.Free;
    doc.Free;

    ptit := pos(title, body);
    if ptit > 0 then insert(' ', body, ptit + length(title));

    content += [body];
    allContent += body + ' ';

    fileContent.Free;

  end;
  Astream.Free;
end;



end.
