unit VSEQuadTree;

interface

uses
  AvL, avlUtils, avlClasses, avlMath, avlVectors, VSEGameObject;

type
  TQTNode=class;
  TQTNodeItem=class(TDLCListItem)
  private
    FNode: TQTNode;
    FObject: TGameObject;
  public
    constructor Create(PrevItem: TDLCListItem; Node: TQTNode; Obj: TGameObject);
    destructor Destroy; override;
    property Obj: TGameObject read FObject;
  end;
  TQTNode=class
  private
    FParent, FLUChild, FLDChild, FRUChild, FRDChild: TQTNode;
    FItems: TQTNodeItem;
    function FindObjectCheck(Item: TDLCListItem; Data: Integer): Boolean;
  public
    constructor Create(Parent: TQTNode);
    destructor Destroy; override;
    procedure AddObject(Obj: TGameObject);
    procedure DeleteObject(Obj: TGameObject);
    property Parent: TQTNode read FParent;
    property LUChild: TQTNode read FLUChild;
    property LDChild: TQTNode read FLDChild;
    property RUChild: TQTNode read FRUChild;
    property RDChild: TQTNode read FRDChild;
    property Items: TQTNodeItem read FItems;
  end;
  TQuadTree=class
  private
    FRoot: TQTNode;
  public
    constructor Create;
    destructor Destroy; override;
    property Root: TQTNode read FRoot;
  end;

implementation

{ TQTNodeItem }

constructor TQTNodeItem.Create(PrevItem: TDLCListItem; Node: TQTNode; Obj: TGameObject);
begin
  inherited Create(PrevItem);
  FNode:=Node;
  FObject:=Obj;
end;

destructor TQTNodeItem.Destroy;
begin
  if FNode.FItems=Self then
    if Next<>Self
      then FNode.FItems:=TQTNodeItem(Next)
      else FNode.FItems:=nil;
  inherited;
end;

{ TQTNode }

procedure TQTNode.AddObject(Obj: TGameObject);
begin
  if Assigned(FItems) then
  begin
    if not Assigned(FItems.FindItem(FindObjectCheck, Integer(Obj)))
      then TQTNodeItem.Create(FItems, Self, Obj);
  end
    else FItems:=TQTNodeItem.Create(nil, Self, Obj);
end;

constructor TQTNode.Create(Parent: TQTNode);
begin
  inherited Create;
  FParent:=Parent;
end;

procedure TQTNode.DeleteObject(Obj: TGameObject);
var
  Item: TQTNodeItem;
begin
  if not Assigned(FItems) then Exit;
  Item:=TQTNodeItem(FItems.FindItem(FindObjectCheck, Integer(Obj)));
  if Assigned(Item) then Item.Free;
end;

destructor TQTNode.Destroy;
begin
  if Assigned(FLUChild) then FAN(FLUChild);
  if Assigned(FLDChild) then FAN(FLDChild);
  if Assigned(FRUChild) then FAN(FRUChild);
  if Assigned(FRDChild) then FAN(FRDChild);
  if Assigned(FItems) then
  begin
    FItems.ClearList;
    FAN(FItems);
  end;
  inherited;
end;

function TQTNode.FindObjectCheck(Item: TDLCListItem; Data: Integer): Boolean;
begin
  Result:=TQTNodeItem(Item).FObject=TGameObject(Data);
end;

{ TQuadTree }

constructor TQuadTree.Create;
begin
  inherited Create;
end;

destructor TQuadTree.Destroy;
begin
  FAN(FRoot);
  inherited;
end;

end.