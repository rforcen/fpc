unit uThread;

{$mode delphi}{$H+}

interface

uses
  Windows, Classes, SysUtils, Controls, LMessages, ComCtrls, LazFileUtils, StrUtils;

const
  WM_ADD_TREE_ITEM = LM_USER + 2004;     // msg id to add an item to tree

  pathDelimiter = {$ifdef windows}'\'{$else}'/'{$endif};
  WildCard = {$ifdef windows}'*.*'{$else}'*'{$endif};

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
    running: boolean; static;

  public
    numNodes: integer; static;
    nth: integer; static; // num threads started
    notifyEnd: TNotifyEvent; static;
    thtype: TThrreadType; static;
    critSect: TRTLCriticalSection; static; // critical section
    formHandle: THandle; static;
  end;



implementation

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
end;

procedure TFindThread.finishAllThreads;
begin
  running := False;
  sleep(100);
end;


end.
