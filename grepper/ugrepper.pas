unit uGrepper;

{$mode delphi}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, ExtCtrls,
  StdCtrls, ComCtrls, LazFileUtils, StrUtils, Process, IniFiles, LMessages, uStack;

const
  pathDelimiter = {$ifdef windows}'\'{$else}'/'{$endif};
  WildCard = {$ifdef windows}'*.*'{$else}'*'{$endif};

  GrepperIniFile = 'grepper.ini';

  WM_ADD_TREE_ITEM = LM_USER + 2004;

type

  { TParamMsg }

  TParamMsg = record
    path, Name: string;
    node: TTreeNode;

    procedure msgSet(_path, _name: string; _node: TTreeNode);
  end;
  PTParamMsg = ^TParamMsg;

  { TFindThread }

  TFindThread = class(TThread)
  type
    TThrreadType = (ttSingleThread = 0, ttMultiThread = 1);

  public
    constructor Create(_pathName, _fileCard, _textSearch: string;
      _ignoreCase, _recurseDir: boolean; _thty: TThrreadType;
      _critSect: TRTLCriticalSection); overload;

    constructor Create(_pathName, _fileCard, _textSearch: string;
      _ignoreCase, _recurseDir: boolean); overload;

    function getNumNodes: integer;
    procedure initThread(_formHandle: THandle; OnEndEvent: TNotifyEvent);
    procedure finishAllThreads;

  protected
    procedure Execute; override;
    procedure FindFiles(_pathName, _fileCard, _textSearch: string;
      _ignoreCase, _recurseDir: boolean);
    procedure OnThreadFinished(Sender: TObject);


  private
    pathName, fileCard, textSearch: string;
    ignoreCase, recurseDir: boolean;
    stack: TStack<TFindThread>;
    running: boolean; static;

  public
    numNodes: integer; static;
    nth: integer; static; // num threads started
    notifyEnd: TNotifyEvent; static;
    thtype: TThrreadType; static;
    critSect: TRTLCriticalSection; static; // critical section
    formHandle: THandle; static;
  end;



  { TForm1 }

  TForm1 = class(TForm)
    cbIgnoreCase: TCheckBox;
    cbRecDir: TCheckBox;
    cbThread: TComboBox;
    dirEntry: TDirectoryEdit;
    edFileFilter: TEdit;
    edTextSearch: TEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    sbar: TStatusBar;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    ToolBar1: TToolBar;
    tbGo: TToolButton;
    tvFiles: TTreeView;
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tbGoClick(Sender: TObject);
    procedure tvFilesClick(Sender: TObject);
    procedure tvFilesDblClick(Sender: TObject);

  private
    procedure doFind(Sender: TObject);
    procedure FindFiles(tv: TTreeView; pathName, fileCard, textSearch: string;
      ignoreCase, recurseDir: boolean; var numNodes: integer);
    procedure openAssociatedApp(fn: string);

    procedure readIni;
    procedure writeIni;
    procedure clearFileTree;
    procedure StopThread;

    procedure OnThreadDone(Sender: TObject);
    procedure iconFind;
    procedure iconStop;
    function thrdType: TFindThread.TThrreadType;

    procedure addTreeItem(var Msg: TLMessage); message WM_ADD_TREE_ITEM;
  private
    IniFile: TIniFile;
    th: TFindThread;
    t0: cardinal;
    critSect: TRTLCriticalSection; // critical section
  public

  end;



var
  Form1: TForm1;


implementation

{$R *.lfm}

function FileContains(const fn, _text: string; _ignoreCase: boolean): boolean;
var
  s: string;
  sl: TStringList;
begin

  if _text = '' then
  begin
    Result := True;
    exit;
  end;

  Result := False;

  if FileIsText(fn) then
  begin
    sl := TStringList.Create;
    sl.LoadFromFile(fn);

    for s in sl do
    begin
      if Result then break;

      if _ignoreCase then
      begin
        if AnsiContainsText(s, _text) then  Result := True; // found
      end
      else if pos(_text, s) > 0 then  Result := True; // found
    end;
    sl.Free;
  end;
end;

{ TParamMsg }

procedure TParamMsg.msgSet(_path, _name: string; _node: TTreeNode);
begin
  path := _path;
  Name := _name;
  node := _node;
end;


{ TFindThread }

procedure TFindThread.Execute;
begin
  if running then
    FindFiles(pathName, fileCard, textSearch, ignoreCase, recurseDir);
end;

procedure TFindThread.FindFiles(_pathName, _fileCard, _textSearch: string;
  _ignoreCase, _recurseDir: boolean);
var
  sr: TSearchRec;
  path: string;
  Node: TTreeNode = nil;
  msg: TParamMsg;

begin

  path := IncludeTrailingBackslash(_pathName);

  // find files in current path applying fileCard
  if FindFirst(path + _fileCard, faAnyFile - faDirectory, sr) = 0 then
  begin
    repeat
      if not running then break;

      if FileContains(path + sr.Name, _textSearch, _ignoreCase) then
      begin
        if (thtype = ttMultiThread) then EnterCriticalSection(critSect); { -> start CS }

        begin // send msg to man form with node, path, name parameters
          msg.msgSet(path, sr.Name, Node);
          SendMessage(formHandle, WM_ADD_TREE_ITEM, {%H-}WPARAM(@msg), 0);
          Node := msg.node;   // modified node

          Inc(numNodes);
        end;

        if (thtype = ttMultiThread) then LeaveCriticalSection(critSect); { -> end CS }
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;

  // directory traverse ?
  if recurseDir and (FindFirst(path + WildCard, faDirectory, sr) = 0) then
  begin
    repeat
      if not running then break;

      if (sr.Name <> '.') and (sr.Name <> '..') and // is dir?
        ((sr.Attr and faDirectory) = faDirectory) then
      begin
        if (thtype = ttMultiThread) and (nth < GetCPUCount - 2) then
          // use limited # of threads
          TFindThread.Create(path + sr.Name + pathDelimiter,
            _fileCard, _textSearch, _ignoreCase, _recurseDir).Start
        else
          FindFiles(path + sr.Name + pathDelimiter, _fileCard, _textSearch,
            _ignoreCase, _recurseDir);
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

procedure TFindThread.OnThreadFinished(Sender: TObject);
begin
  Dec(nth);
  stack.pop;

  if (nth <= 0) and running then
  begin
    if (@notifyEnd <> nil) then // al threads started are finished
      notifyEnd(Self);
  end;
end;

constructor TFindThread.Create(_pathName, _fileCard, _textSearch: string;
  _ignoreCase, _recurseDir: boolean; _thty: TThrreadType;
  _critSect: TRTLCriticalSection);
begin
  pathName := _pathName;
  fileCard := _fileCard;
  textSearch := _textSearch;
  ignoreCase := _ignoreCase;
  recurseDir := _recurseDir;
  thtype := _thty;
  critSect := _critSect;

  FreeOnTerminate := True;
  OnTerminate := OnThreadFinished;

  Inc(nth);

  stack.push(Self);

  inherited Create(True); // start suspended ?
end;

constructor TFindThread.Create(_pathName, _fileCard, _textSearch: string;
  _ignoreCase, _recurseDir: boolean);
begin
  pathName := _pathName;
  fileCard := _fileCard;
  textSearch := _textSearch;
  ignoreCase := _ignoreCase;
  recurseDir := _recurseDir;

  FreeOnTerminate := True;
  OnTerminate := OnThreadFinished;

  Inc(nth);

  EnterCriticalSection(critSect); { -> start CS }
  stack.push(Self);
  LeaveCriticalSection(critSect); { -> end CS }

  inherited Create(True); // start suspended ?
end;

function TFindThread.getNumNodes: integer;
begin
  Result := numNodes;
end;

procedure TFindThread.initThread(_formHandle: THandle; OnEndEvent: TNotifyEvent);
begin
  numNodes := 0;
  nth := 0;
  notifyEnd := nil;
  formHandle := _formHandle;
  notifyEnd := OnEndEvent;
  running := True;
  stack.Clear;
end;

procedure TFindThread.finishAllThreads;
begin
  running := False;
  sleep(100);
  stack.Clear;
end;


procedure TForm1.StopThread;
begin
  if th <> nil then
    th.finishAllThreads;
  th := nil;
  sbar.SimpleText := 'ready';
end;

{ TForm1 }

procedure TForm1.FindFiles(tv: TTreeView; pathName, fileCard, textSearch: string;
  ignoreCase, recurseDir: boolean; var numNodes: integer);
var
  Node: TTreeNode = nil;
  sr: TSearchRec;
  path: string;

begin

  //DispatchMsg;

  path := IncludeTrailingBackslash(pathName);

  tv.TopItem := tv.Items.GetLastNode;

  // find files in current dir applying fileCard
  if FindFirst(path + fileCard, faAnyFile - faDirectory, sr) = 0 then
  begin
    repeat
      if FileContains(path + sr.Name, textSearch, ignoreCase) then
      begin
        if Node = nil then
          Node := tv.Items.Add(nil, path); // add node item only if file found
        tv.Items.AddChild(Node, path + sr.Name); // add file
        Inc(numNodes);
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;

  // directory traverse ?
  if recurseDir and (FindFirst(path + WildCard, faDirectory, sr) = 0) then
  begin
    repeat
      if (sr.Name = '.') or (sr.Name = '..') then
      else
      begin
        if ((sr.Attr and faDirectory) = faDirectory) then
          FindFiles(tv, path + sr.Name + pathDelimiter, fileCard,
            textSearch, ignoreCase, recurseDir, numNodes);
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

procedure TForm1.openAssociatedApp(fn: string);
begin
  with TProcess.Create(nil) do
  begin
    Executable := fn;
    Execute;
    Free;
  end;
end;

procedure TForm1.readIni;
begin
  IniFile := TIniFile.Create(GrepperIniFile);

  dirEntry.Directory := IniFile.ReadString('Section', 'dirEntry', GetCurrentDir);
  edTextSearch.Text := IniFile.ReadString('Section', 'textSearch', '');
  edFileFilter.Text := IniFile.ReadString('Section', 'fileFilter', '');
  cbIgnoreCase.Checked := IniFile.ReadBool('Section', 'ignoreCase', True);
  cbRecDir.Checked := IniFile.ReadBool('Section', 'recurse', True);
  cbThread.ItemIndex := IniFile.ReadInteger('Section', 'ThreadType', 0);
end;

procedure TForm1.writeIni;
begin
  IniFile.WriteString('Section', 'dirEntry', dirEntry.Directory);
  IniFile.WriteString('Section', 'textSearch', edTextSearch.Text);
  IniFile.WriteString('Section', 'fileFilter', edFileFilter.Text);
  IniFile.WriteBool('Section', 'ignoreCase', cbIgnoreCase.Checked);
  IniFile.WriteBool('Section', 'recurse', cbRecDir.Checked);
  IniFile.WriteInteger('Section', 'ThreadType', cbThread.ItemIndex);

  IniFile.Free;
end;

procedure TForm1.clearFileTree;
begin
  tvFiles.Items.Clear;
end;

procedure TForm1.OnThreadDone(Sender: TObject);
begin
  if th <> nil then
  begin
    sbar.SimpleText := Format('found %d files, lap:%d ms',
      [th.getNumNodes, GetTickCount64 - t0]);
    th := nil;
    iconFind;
  end;
end;

procedure TForm1.iconFind;
begin
  tbGo.ImageIndex := 0;
end;

procedure TForm1.iconStop;
begin
  tbGo.ImageIndex := 1;
end;

function TForm1.thrdType: TFindThread.TThrreadType;
begin
  Result := TFindThread.TThrreadType(cbThread.ItemIndex);
end;

procedure TForm1.addTreeItem(var Msg: TLMessage); {update tree}
begin
  with {%H-}PTParamMsg(Msg.wParam)^ do
  begin
    if Node = nil then
      Node := tvFiles.Items.Add(nil, path); // add node item only if file found

    tvFiles.Items.AddChild(Node, path + Name); // add file
    tvFiles.TopItem := tvFiles.Items.GetLastNode;
  end;
end;


procedure TForm1.doFind(Sender: TObject);
begin
  clearFileTree;
  sbar.SimpleText := 'searching...';

  t0 := GetTickCount64;

  if th <> nil then
  begin
    StopThread; // stop th & rest of threads
    iconFind;
  end
  else
  begin
    iconStop;

    th := TFindThread.Create(dirEntry.Directory + pathDelimiter,
      edFileFilter.Text, edTextSearch.Text, cbIgnoreCase.Checked,
      cbRecDir.Checked, thrdType, critSect);

    with th do
    begin
      initThread(Self.Handle, Self.OnThreadDone);

      Start;
    end;
  end;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  readIni;

  dirEntry.RootDir := dirEntry.Directory;
  th := nil;

  InitCriticalSection(critSect{%H-});
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  writeIni;
  StopThread;

  DoneCriticalSection(critSect);
end;

procedure TForm1.tbGoClick(Sender: TObject);
begin
  doFind(Sender);
end;

procedure TForm1.tvFilesClick(Sender: TObject);
var
  fn: string;
begin
  if tvFiles.Selected <> nil then
  begin
    fn := tvFiles.Selected.Text;
    if FileIsText(fn) then
    begin
      Memo1.Lines.LoadFromFile(fn);
      sbar.SimpleText := Format('file size: %d bytes', [FileSizeUTF8(fn)]);
    end
    else
    if FileIsReadable(fn) then sbar.SimpleText := 'not a text file ' + fn;
  end;
end;

procedure TForm1.tvFilesDblClick(Sender: TObject);
var
  fn: string;
begin
  if tvFiles.Selected <> nil then
  begin
    fn := tvFiles.Selected.Text;
    openAssociatedApp(fn);
  end;
end;

end.
