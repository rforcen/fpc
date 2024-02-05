unit uTree;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type

  TNode<T> = class
  type PNode = ^TNode<T>;
  private
    Data: T;
    Children: PNode;
    Sibling: PNode;
  end;

  { TTree }

  TTree<T> = class(TNode<T>)
  private
    Root: PNode;
  public
    constructor Create;
    procedure Add(Data: T); // addnode, addnode(node,data)
    procedure Delete(Data: T);
    procedure TraversePreOrder(Node: PNode);
    procedure TraverseInOrder(Node: PNode);
    procedure TraversePostOrder(Node: PNode);
  end;

implementation

constructor TTree<T>.Create;
begin
  Root := nil;

  inherited Create;
end;

procedure TTree<T>.Add(Data: T);
var
  Node, Parent: PNode;
begin
  New(Node);
  Node^.Data := Data;
  Node^.Children := nil;
  Node^.Sibling := nil;
  if Root = nil then
    Root := Node
  else
  begin
    Parent := Root;
    while Parent^.Sibling <> nil do
      Parent := Parent^.Sibling;
    Parent^.Sibling := Node;
  end;
end;

procedure TTree<T>.Delete(Data: T);
var
  Node, Parent: PNode;
begin
  Node := Root;
  Parent := nil;
  while (Node <> nil) and (Node^.Data <> Data) do
  begin
    Parent := Node;
    Node := Node^.Sibling;
  end;
  if Node = nil then
    Exit;
  if Node = Root then
    Root := Node^.Sibling
  else
    Parent^.Sibling := Node^.Sibling;
  Dispose(Node);
end;

procedure TTree<T>.TraversePreOrder(Node: PNode);
begin
  while Node <> nil do
  begin
    // WriteLn(Node^.Data);
    TraversePreOrder(Node^.Children);
    Node := Node^.Sibling;
  end;
end;

procedure TTree<T>.TraverseInOrder(Node: PNode);
begin
  if Node <> nil then
  begin
    TraverseInOrder(Node^.Children);
    // WriteLn(Node^.Data);
    TraverseInOrder(Node^.Sibling);
  end;
end;

procedure TTree<T>.TraversePostOrder(Node: PNode);
begin
  if Node <> nil then
  begin
    TraversePostOrder(Node^.Children);
    TraversePostOrder(Node^.Sibling);
    //WriteLn(Node^.Data);
  end;
end;


end.
