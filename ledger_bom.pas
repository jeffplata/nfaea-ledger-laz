unit ledger_bom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  ,tiObject
  ,ObjectListFilter
  ;

type

  { TObjectListFilter }



  TPersonsFilter = class(TObjectListFilter);

  { TManualObject }

  TManualObject = class(TtiObject)
    public
      procedure SaveObject;
      procedure DeleteObject(FromList: TtiObjectList; var s: string ); //copied somewhere
  end;

  TPersonBasic = class;
  TPerson = class;
  TPersonList = class;
  TPersonsLookup = class;

  TServiceBasic = class;
  TServiceBasicList = class;
  TService = class;
  TServiceList = class;

  TPersonLedgerItem = class;
  TPersonLedger = class;

  TLoan = class;
  TLoanList = class;

  TPayment = class;
  TPaymentList = class;

  { TPayment }

  TPayment = class(TManualObject)
  private
    FAmount: Currency;
    FDocDate: TDateTime;
    FDocNumber: string;
    FPerson: TPersonBasic;
    FRemarks: string;
    FService: TServiceBasic;
    procedure SetAmount(AValue: Currency);
    procedure SetDocDate(AValue: TDateTime);
    procedure SetDocNumber(AValue: string);
    procedure SetPerson(AValue: TPersonBasic);
    procedure SetRemarks(AValue: string);
    procedure SetService(AValue: TServiceBasic);
  protected
    function  GetOwner: TPaymentList; reintroduce;
    procedure SetOwner(const Value: TPaymentList); reintroduce;
  public
    property  Owner: TPaymentList read GetOwner write SetOwner;
    constructor Create; override;
    destructor Destroy; override;
    procedure AssignClassProps(ASource: TtiObject); override;
  published
    property Person: TPersonBasic read FPerson write SetPerson;
    property Service: TServiceBasic read FService write SetService;
    property DocDate: TDateTime read FDocDate write SetDocDate;
    property DocNumber: string read FDocNumber write SetDocNumber;
    property Amount: Currency read FAmount write SetAmount;
    property Remarks: string read FRemarks write SetRemarks;
  end;

  { TPaymentList }

  TPaymentList = class(TtiObjectList)
  private
  protected
    function  GetItems(i: integer): TPayment; reintroduce;
    procedure SetItems(i: integer; const Value: TPayment); reintroduce;
  public
    property  Items[i:integer]: TPayment read GetItems write SetItems;
    procedure Add(AObject:TPayment); reintroduce;
  published
  end;

  { TPersonBasic }

  TPersonBasic = class(TManualObject)
  private
    FName: string;
    procedure SetName(AValue: string);
  published
    property Name: string read FName write SetName;
  end;

  { TPersonsLookUp }

  TPersonsLookUp = class(TtiObjectList)
  private
    FListFilter: TObjectListFilter;
  protected
    function  GetItems(i: integer): TPersonBasic; reintroduce;
    procedure SetItems(i: integer; const Value: TPersonBasic); reintroduce;
  public
    property ListFilter: TObjectListFilter read FListFilter write FListFilter;
    property  Items[i:integer]: TPersonBasic read GetItems write SetItems;
    procedure Add(AObject:TPersonBasic); reintroduce;
    constructor Create; override;
    destructor Destroy; override;
  end;



  { TPerson }

  TPerson = class(TPersonBasic)
  private
    FDateJoined: TDate;
    //FName: string;
    function GetDateJoinedAsString: string;
    function GetID: string;
    procedure SetDateJoined(AValue: TDate);
    //procedure SetName(AValue: string);
  protected
    function  GetOwner: TPersonList; reintroduce;
    procedure SetOwner(const Value: TPersonList); reintroduce;
  public
    property  Owner: TPersonList read GetOwner write SetOwner;
  published
    property ID: string read GetID;
    //property Name: string read FName write SetName;
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

  { TServiceBasic }

  TServiceBasic = class(TManualObject)
  private
    FName: string;
    procedure SetName(AValue: string);
    function GetCaption: string;
  published
    property Caption: string read GetCaption;
    property Name: string read FName write SetName;
  end;

  { TServiceBasicList }

  TServiceBasicList = class(TtiObjectList)
  private
  protected
    function  GetItems(i: integer): TServiceBasic; reintroduce;
    procedure SetItems(i: integer; const Value: TServiceBasic); reintroduce;
  public
    property  Items[i:integer]: TServiceBasic read GetItems write SetItems;
    procedure Add(AObject:TServiceBasic); reintroduce;
  published
  end;

  { TService }

  TService = class(TManualObject)
  private
    FInterestRate: Currency;
    FMaxAmount: Currency;
    FMaxTerm: integer;
    FMinAmount: Currency;
    FMinTerm: integer;
    FName: string;
    FRebateRate: Currency;
    function GetCaption: string;
    procedure SetInterestRate(AValue: Currency);
    procedure SetMaxAmount(AValue: Currency);
    procedure SetMaxTerm(AValue: integer);
    procedure SetMinAmount(AValue: Currency);
    procedure SetMinTerm(AValue: integer);
    procedure SetName(AValue: string);
    procedure SetRebateRate(AValue: Currency);
  protected
    function  GetOwner: TServiceList; reintroduce;
    procedure SetOwner(const Value: TServiceList); reintroduce;
  public
    property  Owner: TServiceList read GetOwner write SetOwner;
  published
    property Caption: string read GetCaption;
    property Name: string read FName write SetName;
    property MaxAmount: Currency read FMaxAmount write SetMaxAmount;
    property MinAmount: Currency read FMinAmount write SetMinAmount;
    property InterestRate: Currency read FInterestRate write SetInterestRate;
    property RebateRate: Currency read FRebateRate write SetRebateRate;
    property MinTerm: integer read FMinTerm write SetMinTerm;
    property MaxTerm: integer read FMaxTerm write SetMaxTerm;
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

  { TLoan }

  TLoan = class(TManualObject)
  private
    FAmortization: Currency;
    FDocDate: TDate;
    FDocNumber: string;
    FInterest: Currency;
    FInterestRate: Currency;
    FNetProceeds: Currency;
    FNotes: string;
    FPaymentEnd: Tdate;
    FPaymentStart: Tdate;
    FPerson: TPersonBasic;
    FPreviousBalance: Currency;
    FPrincipal: Currency;
    FRebateRate: currency;
    FRebates: Currency;
    FRecomputeTotals: boolean;
    FService: TService;
    FTerms: Integer;
    FTotal: Currency;
    function GetPersonID: string;
    function GetServiceID: string;
    procedure SetAmortization(AValue: Currency);
    procedure SetDocDate(AValue: TDate);
    procedure SetDocNumber(AValue: string);
    procedure SetInterest(AValue: Currency);
    procedure SetInterestRate(AValue: Currency);
    procedure SetNetProceeds(AValue: Currency);
    procedure SetNotes(AValue: string);
    procedure SetPaymentEnd(AValue: Tdate);
    procedure SetPaymentStart(AValue: Tdate);
    procedure SetPerson(AValue: TPersonBasic);
    procedure SetPreviousBalance(AValue: Currency);
    procedure SetPrincipal(AValue: Currency);
    procedure SetRebateRate(AValue: currency);
    procedure SetRebates(AValue: Currency);
    procedure SetService(AValue: TService);
    procedure SetTerms(AValue: Integer);
    procedure SetTotal(AValue: Currency);
  protected
    function  GetOwner: TLoanList; reintroduce;
    procedure SetOwner(const Value: TLoanList); reintroduce;
  public
    property  Owner: TLoanList read GetOwner write SetOwner;
    property RecomputeTotals: boolean read FRecomputeTotals write FRecomputeTotals;
    constructor Create; override;
    destructor Destroy; override;
    function IsValid(const pErrors: TtiObjectErrors): Boolean; override;
    procedure AssignClassProps(ASource: TtiObject); override;
    procedure RecomputeTotal;
    procedure RecomputeNetProceeds;
  published
    property PersonID: string read GetPersonID;
    property ServiceID: string read GetServiceID;
    property Person: TPersonBasic read FPerson write SetPerson;
    property Service: TService read FService write SetService;
    property DocNumber: string read FDocNumber write SetDocNumber;
    property DocDate: TDate read FDocDate write SetDocDate;
    property Notes: string read FNotes write SetNotes;
    property Principal: Currency read FPrincipal write SetPrincipal;
    property Interest: Currency read FInterest write SetInterest;
    property Total: Currency read FTotal write SetTotal;
    Property PreviousBalance: Currency read FPreviousBalance write SetPreviousBalance;
    Property Rebates: Currency read FRebates write SetRebates;
    Property NetProceeds: Currency read FNetProceeds write SetNetProceeds;
    Property InterestRate: Currency read FInterestRate write SetInterestRate;
    property RebateRate: currency read FRebateRate write SetRebateRate;
    Property Terms: Integer read FTerms write SetTerms;
    Property Amortization: Currency read FAmortization write SetAmortization;
    Property PaymentStart: Tdate read FPaymentStart write SetPaymentStart;
    Property PaymentEnd: Tdate read FPaymentEnd write SetPaymentEnd;
  end;

  { TLoanList }

  TLoanList = class(TtiObjectList)
  private
  protected
    function  GetItems(i: integer): TLoan; reintroduce;
    procedure SetItems(i: integer; const Value: TLoan); reintroduce;
  public
    property  Items[i:integer]: TLoan read GetItems write SetItems;
    procedure Add(AObject:TLoan); reintroduce;
  published
  end;


implementation
uses
  variants
  , Model_View
  ;

const
  cNameMissing = 'Member name cannot be empty.';
  cLoanTypeMissing = 'Loan Type cannot be empty.';
  cValueNotAllowed = '%s value is not valid.''';

{ TServiceBasicList }

function TServiceBasicList.GetItems(i: integer): TServiceBasic;
begin
  result := TServiceBasic(inherited GetItems(i));
end;

procedure TServiceBasicList.SetItems(i: integer; const Value: TServiceBasic);
begin
  inherited SetItems(i, Value);
end;

procedure TServiceBasicList.Add(AObject: TServiceBasic);
begin
  inherited Add(AObject);
end;

{ TServiceBasic }

procedure TServiceBasic.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

function TServiceBasic.GetCaption: string;
begin
  result := Name;
end;

{ TPaymentList }

function TPaymentList.GetItems(i: integer): TPayment;
begin
  result := TPayment(inherited GetItems(i));
end;

procedure TPaymentList.SetItems(i: integer; const Value: TPayment);
begin
  inherited SetItems(i, Value);
end;

procedure TPaymentList.Add(AObject: TPayment);
begin
  inherited Add(AObject);
end;

{ TPayment }

procedure TPayment.SetPerson(AValue: TPersonBasic);
begin
  if FPerson=AValue then Exit;
  FPerson:=AValue;
end;

procedure TPayment.SetRemarks(AValue: string);
begin
  if FRemarks=AValue then Exit;
  FRemarks:=AValue;
end;

procedure TPayment.SetAmount(AValue: Currency);
begin
  if FAmount=AValue then Exit;
  FAmount:=AValue;
end;

procedure TPayment.SetDocDate(AValue: TDateTime);
begin
  if FDocDate=AValue then Exit;
  FDocDate:=AValue;
end;

procedure TPayment.SetDocNumber(AValue: string);
begin
  if FDocNumber=AValue then Exit;
  FDocNumber:=AValue;
end;

procedure TPayment.SetService(AValue: TServiceBasic);
begin
  if FService=AValue then Exit;
  FService:=AValue;
end;

function TPayment.GetOwner: TPaymentList;
begin
  result := TPaymentList(inherited GetOwner);
end;

procedure TPayment.SetOwner(const Value: TPaymentList);
begin
  inherited SetOwner(Value);
end;

constructor TPayment.Create;
begin
  inherited Create;
  FPerson := TPersonBasic.Create;
  FService := TServiceBasic.Create;
end;

destructor TPayment.Destroy;
begin
  FPerson := nil;
  FPerson.Free;

  FService := nil;
  FService.Free;

  inherited Destroy;
end;

procedure TPayment.AssignClassProps(ASource: TtiObject);
begin
  //FPerson.Assign(TPayment(ASource).Person);
  FPerson := TPayment(ASource).Person;
  //FService.Assign(TPayment(ASource).Service);
  FService := TPayment(ASource).Service;
end;

{ TPersonsLookUp }

function TPersonsLookUp.GetItems(i: integer): TPersonBasic;
begin
  result := TPersonBasic(inherited GetItems(i));
end;

procedure TPersonsLookUp.SetItems(i: integer; const Value: TPersonBasic);
begin
  inherited SetItems(i, Value);
end;

procedure TPersonsLookUp.Add(AObject: TPersonBasic);
begin
  inherited Add(AObject);
end;

constructor TPersonsLookUp.Create;
begin
  inherited Create;
  FListFilter := TObjectListFilter.Create;
  FListFilter.Active:= False;
end;

destructor TPersonsLookUp.Destroy;
begin
  FListFilter.Free;
  inherited Destroy;
end;

{ TPersonBasic }

procedure TPersonBasic.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

{ TLoanList }

function TLoanList.GetItems(i: integer): TLoan;
begin
  result := TLoan(inherited GetItems(i));
end;

procedure TLoanList.SetItems(i: integer; const Value: TLoan);
begin
  inherited SetItems(i, Value);
end;

procedure TLoanList.Add(AObject: TLoan);
begin
  inherited Add(AObject);
end;

{ TLoan }

procedure TLoan.SetService(AValue: TService);
begin
  if FService=AValue then Exit;
  FService:=AValue;
end;

procedure TLoan.SetTerms(AValue: Integer);
begin
  if FTerms=AValue then Exit;
  FTerms:=AValue;
  RecomputeTotal;
end;

procedure TLoan.SetTotal(AValue: Currency);
begin
  if FTotal=AValue then Exit;
  FTotal:=AValue;
end;

procedure TLoan.SetPerson(AValue: TPersonBasic);
begin
  if FPerson=AValue then Exit;
  FPerson:=AValue;
end;

procedure TLoan.SetAmortization(AValue: Currency);
begin
  if FAmortization=AValue then Exit;
  FAmortization:=AValue;
end;

function TLoan.GetPersonID: string;
begin
  result := Person.OID.AsString;
end;

function TLoan.GetServiceID: string;
begin
  result := Service.OID.AsString;
end;

procedure TLoan.SetDocDate(AValue: TDate);
begin
  if FDocDate=AValue then Exit;
  FDocDate:=AValue;
end;

procedure TLoan.SetDocNumber(AValue: string);
begin
  if FDocNumber=AValue then Exit;
  FDocNumber:=AValue;
end;

procedure TLoan.SetInterest(AValue: Currency);
begin
  if FInterest=AValue then Exit;
  FInterest:=AValue;
end;

procedure TLoan.SetInterestRate(AValue: Currency);
begin
  if FInterestRate=AValue then Exit;
  FInterestRate:=AValue;
end;

procedure TLoan.SetNetProceeds(AValue: Currency);
begin
  if FNetProceeds=AValue then Exit;
  FNetProceeds:=AValue;
end;

procedure TLoan.SetNotes(AValue: string);
begin
  if FNotes=AValue then Exit;
  FNotes:=AValue;
end;

procedure TLoan.SetPaymentEnd(AValue: Tdate);
begin
  if FPaymentEnd=AValue then Exit;
  FPaymentEnd:=AValue;
end;

procedure TLoan.SetPaymentStart(AValue: Tdate);
begin
  if FPaymentStart=AValue then Exit;
  FPaymentStart:=AValue;
end;

procedure TLoan.SetPreviousBalance(AValue: Currency);
begin
  if FPreviousBalance=AValue then Exit;
  FPreviousBalance:=AValue;
  RecomputeNetProceeds;
end;

procedure TLoan.SetPrincipal(AValue: Currency);
begin
  if FPrincipal=AValue then Exit;
  FPrincipal:=AValue;
  RecomputeTotal;
end;

procedure TLoan.SetRebateRate(AValue: currency);
begin
  if FRebateRate=AValue then Exit;
  FRebateRate:=AValue;
end;

procedure TLoan.SetRebates(AValue: Currency);
begin
  if FRebates=AValue then Exit;
  FRebates:=AValue;
  RecomputeNetProceeds;
end;

function TLoan.GetOwner: TLoanList;
begin
  result := TLoanList(inherited GetOwner);
end;

procedure TLoan.SetOwner(const Value: TLoanList);
begin
  inherited SetOwner(Value);
end;

constructor TLoan.Create;
begin
  inherited Create;
  FPerson := TPersonBasic.Create;
  FService := TService.Create;
  FRecomputeTotals:= False;
end;

destructor TLoan.Destroy;
begin
  FPerson := nil;
  FPerson.Free;
  FService := nil;
  FService.Free;
  inherited Destroy;
end;

function TLoan.IsValid(const pErrors: TtiObjectErrors): Boolean;
begin
  result := inherited IsValid(pErrors);
  if not result then exit; // <==

  if Person.Name = '' then
    pErrors.AddError('Name', cNameMissing);

  if Person.Name <> '' then
  begin
    if Service = nil then
      pErrors.AddError('Service.Name', cLoanTypeMissing)
    else
      if (Principal < service.MinAmount) or (Principal > service.MaxAmount) then
        pErrors.AddError('Principal', Format(cValueNotAllowed, ['Principal value']));
  end;

  Result := pErrors.Count = 0;
end;

procedure TLoan.AssignClassProps(ASource: TtiObject);
begin
  //FPerson.Assign(TLoan(ASource).Person);
  //FService.Assign(TLoan(ASource).Service );
  FPerson := TLoan(ASource).Person;
  FService := TLoan(ASource).Service
end;

procedure TLoan.RecomputeTotal;
begin
  if not RecomputeTotals then Exit;  // <===
  Interest:= Principal * InterestRate * 0.01 * (Terms/12);
  Total:= Principal + Interest;
  RecomputeNetProceeds;
end;

procedure TLoan.RecomputeNetProceeds;
begin
  NetProceeds:= Principal - PreviousBalance + Rebates;
  if Terms > 0 then
    Amortization:= Total / Terms
  else
    Amortization:= 0;
  NotifyObservers;
end;

{ TManualObject }

procedure TManualObject.SaveObject;
begin
  Dirty:= True;
  //NotifyObservers;
  Save;
end;

procedure TManualObject.DeleteObject(FromList: TtiObjectList; var s: string);
begin
  s := '';
  Deleted:=True;
  try
    Save;  // Most likely, errors will sprout from here
    if Owner <> nil then
      If Assigned(FromList) then
         FromList.FreeDeleted;
  except
    on e: Exception do
    begin
      s := e.Message;
      Deleted := False;
      ObjectState:= posClean;
    end;
  end;
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

function TService.GetCaption: string;
begin
  Result := Name;
end;

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

procedure TService.SetMaxTerm(AValue: integer);
begin
  if FMaxTerm=AValue then Exit;
  FMaxTerm:=AValue;
end;

procedure TService.SetMinAmount(AValue: Currency);
begin
  if FMinAmount=AValue then Exit;
  FMinAmount:=AValue;
end;

procedure TService.SetMinTerm(AValue: integer);
begin
  if FMinTerm=AValue then Exit;
  FMinTerm:=AValue;
end;

procedure TService.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TService.SetRebateRate(AValue: Currency);
begin
  if FRebateRate=AValue then Exit;
  FRebateRate:=AValue;
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


function TPerson.GetOwner: TPersonList;
begin
  result := TPersonList(inherited GetOwner);
end;

procedure TPerson.SetOwner(const Value: TPersonList);
begin
  inherited SetOwner(Value);
end;

end.

