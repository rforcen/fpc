unit uGrepper;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, ExtCtrls,
  StdCtrls, ComCtrls, Process, IniFiles, LazFileUtils, LMessages,
  uThread;

const
  GrepperIniFile = 'grepper.ini';

type

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

procedure TForm1.StopThread;
begin
  if th <> nil then
    th.finishAllThreads;
  th := nil;
  sbar.SimpleText := 'ready';
end;

{ TForm1 }

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
