{ fpc wrapper to btree.dll}
unit btree;


{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  cbool = int8; // change for different c++ compiler
  cint = integer;

  BtreeDX = pointer;

  TKeyRecno = record
    key: string;
    recno: integer;
  end;

  { TkrEnumerator }
  TkrEnumerator = record
  public
    function GetCurrent: TKeyRecno;
    function MoveNext: boolean;
    property Current: TKeyRecno read GetCurrent;
  private
    _kr: TKeyRecno;
    bt: pointer;
  end;


  { TBtree }

  TBtree = record

  public
    constructor Create(fileName: string; __keylen: integer; unique: boolean);
    constructor Open(fileName: string);

    procedure Close;
    function add(const key: string; const recno: integer): boolean;
    function find(const key: string; var recno: integer): boolean;
    function findEQ(const key: string; var recno: integer): boolean;
    function Next(var key: string; var recno: integer): boolean;
    function eraseEQ(const key: string): boolean;
    function eraseMatch(const key: string): integer;

  private
    bt: BtreeDX;
    _ok: boolean;
    _keyLen: integer;
  private
    function getKey: string;
    function getRecno: integer;
    function getNnodes: integer;
    function getKeyLen: integer;
  public
    property key: string read getKey;
    property recno: integer read getRecno;
    property nNodes: integer read getNnodes;
    property ok: boolean read _ok;
    property keyLen: integer read _keyLen;

    // iterator
  public
    function GetEnumerator: TkrEnumerator;
  end;

  PTBtree = ^TBtree;

procedure printSizes; cdecl; external;

implementation

// btree.dll wrapper

{$linklib btree.dll}

function btCreate(fileName: PChar; keyLen, unique: cint): BtreeDX; cdecl; external;
function btOpen(fileName: PChar): BtreeDX; cdecl; external;
procedure btClose(bt: BTreeDX); cdecl; external;
function btFind(bt: BTreeDX; const key: PChar; var recno: cint): cbool; cdecl; external;
function btFindEQ(bt: BTreeDX; const key: PChar; var recno: cint): cbool;
  cdecl; external;
function btNext(bt: BTreeDX; key: PChar; var recno: cint): cbool; cdecl; external;
function btAdd(bt: BTreeDX; const key: PChar; const recno: cint): cbool; cdecl; external;
function btEraseEQ(bt: BTreeDX; const key: PChar): cbool; cdecl; external;
function btEraseMatch(bt: BtreeDX; const key: PChar): cint; cdecl; external;
function btGetKey(bt: BtreeDX): PChar; cdecl; external;
function btGetRecNo(bt: BtreeDX): cint; cdecl; external;
function btGetNnodes(bt: BtreeDX): cint; cdecl; external;
function btGetKeyLength(bt: BtreeDX): cint; cdecl; external;

{ TkrEnumerator }

function TkrEnumerator.GetCurrent: TKeyRecno;
begin
  Result := _kr;
end;

function TkrEnumerator.MoveNext: boolean;
begin
  if _kr.recno = -1 then // first time
  begin
    Result := PTBtree(bt)^.find('', _kr.recno);
    _kr.key := PTBtree(bt)^.key;
  end
  else // next
    Result := PTBtree(bt)^.Next(_kr.key, _kr.recno);
end;

{ TBtree }

constructor TBtree.Create(fileName: string; __keylen: integer; unique: boolean);
var
  u: integer = 0;
begin
  _keyLen := __keyLen;
  if unique then u := 1;
  bt := btCreate(PChar(filename), _keyLen, u);
  _ok := bt <> nil;
end;

constructor TBtree.Open(fileName: string);
begin
  bt := btOpen(PChar(fileName));
  _keyLen := getKeyLen;
  _ok := bt <> nil;
end;

procedure TBtree.Close;
begin
  btClose(bt);
end;

function TBtree.add(const key: string; const recno: integer): boolean;
begin
  Result := btAdd(bt, PChar(key), cint(recno)) = 1;
end;

function TBtree.find(const key: string; var recno: integer): boolean;
begin
  Result := btFind(bt, PChar(key), recno) = 1;
end;

function TBtree.findEQ(const key: string; var recno: integer): boolean;
begin
  Result := btFindEQ(bt, PChar(key), recno) = 1;
end;

function TBtree.Next(var key: string; var recno: integer): boolean;
begin
  setLength(key, getKeyLen + 1);
  Result := btNext(bt, @key[1], recno) = 1;
end;

function TBtree.eraseEQ(const key: string): boolean;
begin
  Result := btEraseEQ(bt, PChar(key)) = 1;
end;

function TBtree.eraseMatch(const key: string): integer;
begin
  Result := btEraseMatch(bt, PChar(key));
end;

function TBtree.getKey: string;
begin
  Result := string(btGetKey(bt));
end;

function TBtree.getRecno: integer;
begin
  Result := btGetRecno(bt);
end;

function TBtree.getNnodes: integer;
begin
  Result := btGetNnodes(bt);
end;

function TBtree.getKeyLen: integer;
begin
  Result := btGetKeyLength(bt);
end;

function TBtree.GetEnumerator: TkrEnumerator;
begin
  Result._kr.key := '';
  Result._kr.recno := -1;
  Result.bt := @self;
end;

end.
