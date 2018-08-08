unit ledger_bom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  ,tiObject
  ;

type
  { TReadPersonFilter }

  TPersonsFilter = class
  private
    FActive: Boolean;
    FCriteria: string;
  public
    property Active: Boolean read FActive write FActive;
    property Criteria: string read FCriteria write FCriteria;
  end;

  { TManualObject }

  TManualObject = class(TtiObject)
    public
      procedure SaveObject;
      procedure DeleteObject(FromList: TtiObjectList); //copied
  end;

  TPerson = class;
  TPersonList = class;

  TService = class;
  TServiceList = class;

  TPersonLedgerItem = class;
  TPersonLedger = class;

  { TPerson }

  TPerson = class(TManualObject)
  private
    FDateJoined: TDate;
    FName: string;
    function GetDateJoinedAsString: string;
    function GetID: string;
    procedure SetDateJoined(AValue: TDate);
    procedure SetName(AValue: string);
  protected
    function  GetOwner: TPersonList; reintroduce;
    procedure SetOwner(const Value: TPersonList); reintroduce;
  public
    property  Owner: TPersonList read GetOwner write SetOwner;
  published
    property ID: string read GetID;
    property Name: string read FName write SetName;
    property DateJoined: TDate read FDateJoined write SetDateJoined;
    property DateJoinedAsString: string read GetDateJoinedAsString;
  end;

  { TPersonList }

  TPersonList = class(TtiObjectList)
  private
    FPersonsFilter: TPersonsFilter;
  protected
    function  GetItems(i: integer): TPerson; reintroduce;
    procedure SetItems(i: integer; const Value: TPerson); reintroduce;
  public
    property PersonsFilter: TPersonsFilter read FPersonsFilter write FPersonsFilter;
    property  Items[i:integer]: TPerson read GetItems write SetItems;
    procedure Add(AObject:TPerson); reintroduce;
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TService }

  TService = class(TtiObject)
  private
    FInterestRate: Currency;
    FMaxAmount: Currency;
    FMinAmount: Currency;
    FName: string;
    FTerms: integer;
    procedure SetInterestRate(AValue: Currency);
    procedure SetMaxAmount(AValue: Currency);
    procedure SetMinAmount(AValue: Currency);
    procedure SetName(AValue: string);
    procedure SetTerms(AValue: integer);
  protected
    function  GetOwner: TServiceList; reintroduce;
    procedure SetOwner(const Value: TServiceList); reintroduce;
  public
    property  Owner: TServiceList read GetOwner write SetOwner;
  published
    property Name: string read FName write SetName;
    property MaxAmount: Currency read FMaxAmount write SetMaxAmount;
    property MinAmount: Currency read FMinAmount write SetMinAmount;
    property InterestRate: Currency read FInterestRate write SetInterestRate;
    property Terms: integer read FTerms write SetTerms;
  end;

  { TServiceList }

  TServiceList = class(TtiObjectList)
  private
  protected
    function  GetItems(i: integer): TService; reintroduce;
    procedure SetItems(i: integer; const Value: TService); reintroduce;
  public
    property  Items[i:integer]: TService read GetItems write SetItems;
    procedure Add(AObject:TService); reintroduce;
  published
  end;

  { TPersonLedgerItem }

  TPersonLedgerItem = class(TtiObject)
  private
    FCharges: Currency;
    FPayments: Currency;
    FPerson: TPerson;
    FReference: String;
    FService: TService;
    FTransDate: TDate;
    procedure SetCharges(AValue: Currency);
    procedure SetPayments(AValue: Currency);
    procedure SetPerson(AValue: TPerson);
    procedure SetReference(AValue: String);
    procedure SetService(AValue: TService);
    procedure SetTransDate(AValue: TDate);
  protected
    function  GetOwner: TPersonLedger; reintroduce;
    procedure SetOwner(const Value: TPersonLedger); reintroduce;
  public
    property  Owner: TPersonLedger read GetOwner write SetOwner;
  published
    property Person: TPerson read FPerson write SetPerson;
    property TransDate: TDate read FTransDate write SetTransDate;
    property Service: TService read FService write SetService;
    property Reference: String read FReference write SetReference;
    property Charges: Currency read FCharges write SetCharges;
    property Payments: Currency read FPayments write SetPayments;
  end;

  TPersonLedger = class(TtiObjectList)
  private
  protected
    function  GetItems(i: integer): TPersonLedgerItem; reintroduce;
    procedure SetItems(i: integer; const Value: TPersonLedgerItem); reintroduce;
  public
    property  Items[i:integer]: TPersonLedgerItem read GetItems write SetItems;
    procedure Add(AObject:TPersonLedgerItem); reintroduce;
  published
  end;

implementation

{ TManualObject }

procedure TManualObject.SaveObject;
begin
  Dirty:= True;
  NotifyObservers;
  Save;
end;

procedure TManualObject.DeleteObject( FromList: TtiObjectList);
begin
  Deleted:=True;
  Save;
  If Assigned(FromList) then
     FromList.FreeDeleted;
end;

{ TPersonLedger }

function TPersonLedger.GetItems(i: integer): TPersonLedgerItem;
begin
  result := TPersonLedgerItem(inherited GetItems(i));
end;

procedure TPersonLedger.SetItems(i: integer; const Value: TPersonLedgerItem);
begin
  inherited SetItems(i, Value);
end;

procedure TPersonLedger.Add(AObject: TPersonLedgerItem);
begin
  inherited Add(AObject);
end;

{ TPersonLedgerItem }

procedure TPersonLedgerItem.SetCharges(AValue: Currency);
begin
  if FCharges=AValue then Exit;
  FCharges:=AValue;
end;

procedure TPersonLedgerItem.SetPayments(AValue: Currency);
begin
  if FPayments=AValue then Exit;
  FPayments:=AValue;
end;

procedure TPersonLedgerItem.SetPerson(AValue: TPerson);
begin
  if FPerson=AValue then Exit;
  FPerson:=AValue;
end;

procedure TPersonLedgerItem.SetReference(AValue: String);
begin
  if FReference=AValue then Exit;
  FReference:=AValue;
end;

procedure TPersonLedgerItem.SetService(AValue: TService);
begin
  if FService=AValue then Exit;
  FService:=AValue;
end;

procedure TPersonLedgerItem.SetTransDate(AValue: TDate);
begin
  if FTransDate=AValue then Exit;
  FTransDate:=AValue;
end;

function TPersonLedgerItem.GetOwner: TPersonLedger;
begin
  result := TPersonLedger(inherited GetOwner);
end;

procedure TPersonLedgerItem.SetOwner(const Value: TPersonLedger);
begin
  inherited SetOwner(Value);
end;

{ TServiceList }

function TServiceList.GetItems(i: integer): TService;
begin
  result := TService(inherited GetItems(i));
end;

procedure TServiceList.SetItems(i: integer; const Value: TService);
begin
  inherited SetItems(i, Value);
end;

procedure TServiceList.Add(AObject: TService);
begin
  inherited Add(AObject);
end;

{ TService }

procedure TService.SetInterestRate(AValue: Currency);
begin
  if FInterestRate=AValue then Exit;
  FInterestRate:=AValue;
end;

procedure TService.SetMaxAmount(AValue: Currency);
begin
  if FMaxAmount=AValue then Exit;
  FMaxAmount:=AValue;
end;

procedure TService.SetMinAmount(AValue: Currency);
begin
  if FMinAmount=AValue then Exit;
  FMinAmount:=AValue;
end;

procedure TService.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TService.SetTerms(AValue: integer);
begin
  if FTerms=AValue then Exit;
  FTerms:=AValue;
end;

function TService.GetOwner: TServiceList;
begin
  result := TServiceList(inherited GetOwner);
end;

procedure TService.SetOwner(const Value: TServiceList);
begin
  inherited SetOwner(Value);
end;

{ TPersonList }

function TPersonList.GetItems(i: integer): TPerson;
begin
  result := TPerson(inherited GetItems(i));
end;

procedure TPersonList.SetItems(i: integer; const Value: TPerson);
begin
  inherited SetItems(i, Value);
end;

procedure TPersonList.Add(AObject: TPerson);
begin
  inherited Add(AObject);
end;

constructor TPersonList.Create;
begin
  inherited Create;
  FPersonsFilter := TPersonsFilter.Create;
  FPersonsFilter.Active:= False;
end;

destructor TPersonList.Destroy;
begin
  FPersonsFilter.Free;
  inherited Destroy;
end;

{ TPerson }

procedure TPerson.SetDateJoined(AValue: TDate);
begin
  if FDateJoined=AValue then Exit;
  FDateJoined:=AValue;
end;

function TPerson.GetDateJoinedAsString: string;
begin
  Result := DateToStr(DateJoined);
end;

function TPerson.GetID: string;
begin
  result := OID.AsString;
end;

procedure TPerson.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

function TPerson.GetOwner: TPersonList;
begin
  result := TPersonList(inherited GetOwner);
end;

procedure TPerson.SetOwner(const Value: TPersonList);
begin
  inherited SetOwner(Value);
end;

end.

