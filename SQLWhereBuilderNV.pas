unit SQLWhereBuilderNV;

interface

uses
  Classes, TypInfo;
type

  TUIComponent = class(TCollectionItem)
  private
    FComponent: TComponent;
    FPropertyName: string;
  published
    property Component: TComponent read FComponent write FComponent;
    property PropertyName: string read FPropertyName write FPropertyName;
  end;

  TUIComponents = class(TCollection)
  private
    FOwner: TComponent;
    function GetItems(Index: Integer): TUIComponent;
    procedure SetItems(Index: Integer; const Value: TUIComponent);
  public
    constructor Create(const AOwner: TComponent);
    function AddItem(AComponent: TComponent; PropertyName: string): TUIComponent;
    function GetOwner: TPersistent;
    property Items[Index: Integer]: TUIComponent read GetItems write SetItems;
        default;
  end;

  { TWhereClause }

  TWhereClause = class(TCollectionItem)
  private
    FClause: string;
    FField: string;
    FUIComponents: TUIComponents;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property Clause: string read FClause write FClause;
    property Field: string read FField write FField;
    property UIComponents: TUIComponents read FUIComponents write FUIComponents;
  end;

  TWhereClauses = class(TCollection)
  private
    FOwner : TComponent;
    function GetItem(Index: Integer): TWhereClause;
    function GetOwner: TComponent;
    procedure SetItem(Index: Integer; const Value: TWhereClause);
  public
    constructor Create(const AOwner: TComponent);
    function Additem(const Field: string; Clause: string; const UIList: array of
        const): TWhereClause; overload;
    property Item[Index: Integer]: TWhereClause read GetItem write SetItem; default;
  end;

  TuiType = ( uiText, uiList );

  TswOptype = ( swoEquals, swoGreater, swoLesser, swoStarts,
                swoContains, swoBetween, swoLike);

  TswConditionItem = class(TCollectionItem)
  private
    FField: string;
    FOperatorToken: TswOptype;
    FPropertyName: string;
    FText: string;
    FUIComponent: TComponent;
    FUIType: TuiType;
  public
    property Text: string read FText write FText;
  published
    property Field: string read FField write FField;
    property OperatorToken: TswOptype read FOperatorToken write FOperatorToken;
    property PropertyName: string read FPropertyName write FPropertyName;
    property UIComponent: TComponent read FUIComponent write FUIComponent;
    property UIType: TuiType read FUIType write FUIType;
  end;

  TswConditions = class(TCollection)
  private
    FOwner: TComponent;
    function GetItems(Index: Integer): TswConditionItem;
    procedure SetItems(Index: Integer; const Value: TswConditionItem);
  public
    constructor Create(AOwner: TComponent);
    function AddItem(Field: string; OperatorToken: TswOptype; UIComponent: TComponent;
        PropertyName: string; const UIType: TuiType = UIText): TswConditionItem;
    function GetOwner: TPersistent; override;
    property Items[Index: Integer]: TswConditionItem read GetItems write SetItems; default;
  end;

  { TSQLWhereBuilder }

  TSQLWhereBuilder = class(TComponent)
  private
    FConditions : TswConditions;
    FEmptyIsNull: Boolean;
    FWhereList : TStrings;
    FWhereClauses: TWhereClauses;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property WhereList : TStrings read FWhereList write FWhereList;
    property EmptyIsNull: Boolean read FEmptyIsNull write FEmptyIsNull;
    function AddCondition(Field: string; OperatorToken: TswOptype; UIComponent:
        TComponent; PropertyName: string; const UIType: TuiType = UIText):
        TswConditionItem;
    function AddWhereClauseAnd(const Clause: string;
      const UIList: array of const): TWhereClause; overload;
    procedure UpdateWhereClause;
    procedure UpdateWhereClauses;
  end;

  function GetOperator(OperatorToken: TswOptype): string;

implementation

uses
  //SysUtils, Clipper;
  sysutils
  ;

  function GetOperator(OperatorToken: TswOptype): string;
  begin
    case OperatorToken of
      swoEquals : Result := ' = ' ;
      swoGreater: Result := ' > ' ;
      swoLesser:  Result := ' < ' ;
      swoStarts:  Result := ' starting ';
      swoContains: Result := ' containing ';
      swoBetween:  Result := ' between ';
      swoLike:     Result := ' like ';
      else Result := '';
    end;
  end;

constructor TswConditions.Create(AOwner: TComponent);
begin
  inherited Create(TswConditionItem);
  FOwner := AOwner;
end;

function TswConditions.AddItem(Field: string; OperatorToken: TswOptype; UIComponent:
    TComponent; PropertyName: string; const UIType: TuiType = UIText):
    TswConditionItem;
var
  NewItem: TswConditionItem;
begin
  NewItem := TswConditionItem(Self.Add);
  NewItem.Field := Field;
  NewItem.OperatorToken := OperatorToken;
  NewItem.UIComponent := UIComponent;
  NewItem.PropertyName := PropertyName;
  NewItem.UIType := UIType;
  NewItem.Text := '';
  Result := NewItem;
end;

function TswConditions.GetItems(Index: Integer): TswConditionItem;
begin
  Result := TswConditionItem( inherited Items[Index] );
end;

function TswConditions.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TswConditions.SetItems(Index: Integer; const Value: TswConditionItem);
begin
  Items[Index] := Value;
end;

constructor TSQLWhereBuilder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEmptyIsNull:= False;
  FConditions := TswConditions.Create(Self);
  FWhereList := TStringList.Create;

  FWhereClauses := TWhereClauses.Create(Self);
end;

destructor TSQLWhereBuilder.Destroy;
begin
  FConditions.Free;
  FWhereList.Free;
  inherited;
end;

function TSQLWhereBuilder.AddCondition(Field: string; OperatorToken: TswOptype;
    UIComponent: TComponent; PropertyName: string; const UIType: TuiType =
    UIText): TswConditionItem;
begin
  Result := FConditions.AddItem(Field,OperatorToken,UIComponent,PropertyName,UIType);
end;

function TSQLWhereBuilder.AddWhereClauseAnd(const Clause: string; const
    UIList: array of const): TWhereClause;
// allows use of complex where condition, AddCondition allows only one
var
  Field: string;
begin
  Field:= '';
  Result := FWhereClauses.Additem(Field, Clause, UIList);
end;

procedure TSQLWhereBuilder.UpdateWhereClause;
// use this with AddCondition
var
  i: Integer;
  c: TComponent;
  v: string;
  firstEntry: Boolean;
begin
  firstEntry := True;
  FWhereList.Clear;
  for i := 0 to FConditions.Count -1 do
  begin
    c := Owner.FindComponent(FConditions[i].UIComponent.Name);
    if Assigned(c) then
      v := GetPropValue(c,FConditions[i].PropertyName);
    if v <> '' then
    begin
      with FConditions[i] do
      begin
        if not firstEntry then
          Text := ' and '
        else
          Text := '';
        Text := Text + '('+ Field + GetOperator(OperatorToken) + QuotedStr(v) +')' + #13#10;
        firstEntry := False;
        FWhereList.Add(Text);  
      end
    end
    else
      with FConditions[i] do
        Text := '';
  end;
end;

procedure TSQLWhereBuilder.UpdateWhereClauses;
// use this with AddWhereClauseAnd
var
  i: Integer;
  c: TComponent;
  v: string;
  firstEntry: Boolean;
  j: Integer;
  text: string;
  Aclause : string;
begin
  firstEntry := True;
  FWhereList.Clear;
  for i := 0 to FWhereClauses.Count -1 do
  begin
    Text := '';
    with FWhereClauses[i] do
    begin
      if not firstEntry then
        Text := ' and '
      else
        Text := '';
      Aclause := FWhereClauses[i].Clause;
      for j := 0 to FWhereClauses[i].UIComponents.Count-1 do
      begin
        c := FWhereClauses[i].UIComponents[j].Component;
        if Assigned(c) then
          v := GetPropValue(c,FWhereClauses[i].UIComponents[j].PropertyName);
        if v <> '' then
          Aclause := StringReplace(Aclause,'?',QuotedStr(v),[])
        else
          Aclause := '';
      end;
      if Aclause <> '' then begin
        Text := Text + '(' + Aclause +')' {+ #13#10} ;
        firstEntry := False;
        FWhereList.Add(Text);
      end;
    end
  end;
end;

constructor TUIComponents.Create(const AOwner: TComponent);
begin
  inherited Create(TUIComponent);
  FOwner := AOwner;
end;

function TUIComponents.AddItem(AComponent: TComponent; PropertyName: string):
    TUIComponent;
var
  NewItem : TUIComponent;
begin
  NewItem := TUIComponent.Create(Self);
  NewItem.Component := AComponent;
  NewItem.PropertyName := PropertyName;
  Result := NewItem;
end;

function TUIComponents.GetItems(Index: Integer): TUIComponent;
begin
  Result := TUIComponent( inherited Items[Index] );
end;

function TUIComponents.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TUIComponents.SetItems(Index: Integer; const Value: TUIComponent);
begin
  Items[Index] := Value;
end;

constructor TWhereClauses.Create(const AOwner: TComponent);
begin
  inherited Create(TWhereClause);
  FOwner := AOwner;
end;

function TWhereClauses.Additem(const Field: string; Clause: string; const
    UIList: array of const): TWhereClause;
var
  newItem: TWhereClause;
  i: Integer;
begin
  newItem := TWhereClause.Create(Self);
  newItem.Field := Field;
  newItem.Clause := Clause;
  newItem.UIComponents.AddItem(TComponent(uilist[0].vObject),uilist[1].VPChar);
  if High(UIList) > 1 then
    for i := 2 to High(UIList) do
    begin
      if (i mod 2) = 0 then
        newItem.UIComponents.AddItem(TComponent(uilist[i].vObject),uilist[i+1].VPChar);
    end;
  Result := newItem;
end;

function TWhereClauses.GetItem(Index: Integer): TWhereClause;
begin
  Result := TWhereClause( inherited Items[Index] );
end;

function TWhereClauses.GetOwner: TComponent;
begin
  Result := FOwner;
end;

procedure TWhereClauses.SetItem(Index: Integer; const Value: TWhereClause);
begin
  Items[Index] := Value;
end;

constructor TWhereClause.Create(ACollection: TCollection);
begin
  //inherited Create(Collection);
  inherited Create(ACollection);
  FUIComponents := TUIComponents.Create(TComponent(Self));
end;

destructor TWhereClause.Destroy;
begin
  FUIComponents.Free;
  inherited Destroy;
end;

end.
