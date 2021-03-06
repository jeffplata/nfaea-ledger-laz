unit ledger_bom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  ,tiObject
  ,ObjectListFilter
  ;

type

  TManualObjectClass = class of TManualObject;

  { TFilteredObjectList }

  TFilteredObjectList = class(TtiObjectList)
  private
    FListFilter: TObjectListFilter;
  public
    property ListFilter: TObjectListFilter read FListFilter write FListFilter;
    constructor Create; override;
    destructor Destroy; override;
  end;

  //TPersonsFilter = class(TObjectListFilter);

  { TManualObject }

  TManualObject = class(TtiObject)
    public
      procedure Mark;
      procedure SaveObject;
      procedure DeleteObject(FromList: TtiObjectList; var s: string; const isPropagageToDB:boolean = True ); //copied somewhere
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

  TLoanAdjustment = class;
  TLoanAdjustmentList = class;

  TPayment = class;
  TPaymentList = class;

  { TLoanAdjustment }

  TLoanAdjustment = class(TManualObject)
  private
    FAmount: Currency;
    FLoanID: string;
    FService: TService;
    function GetAmountAsString: string;
    procedure SetAmount(AValue: Currency);
    procedure SetLoanID(AValue: string);
    procedure SetService(AValue: TService);
  protected
    function  GetOwner: TLoanAdjustmentList; reintroduce;
    procedure SetOwner(const Value: TLoanAdjustmentList); reintroduce;
  public
    property  Owner: TLoanAdjustmentList read GetOwner write SetOwner;
    constructor Create; override;
    destructor Destroy; override;
    procedure AssignClassProps(ASource: TtiObject); override;
  published
    property LoanID: string read FLoanID write SetLoanID;
    property Service: TService read FService write SetService;
    property Amount: Currency read FAmount write SetAmount;
    property AmountAsString: string read GetAmountAsString;
  end;

  { TLoanAdjustmentList }

  TLoanAdjustmentList = class(TFilteredObjectList)
  private
  protected
    function  GetItems(i: integer): TLoanAdjustment; reintroduce;
    procedure SetItems(i: integer; const Value: TLoanAdjustment); reintroduce;
  public
    property  Items[i:integer]: TLoanAdjustment read GetItems write SetItems;
    procedure Add(AObject:TLoanAdjustment); reintroduce;
  published
  end;

  { TPayment }

  TPayment = class(TManualObject)
  private
    FAmount: Currency;
    FDocDate: TDateTime;
    FDocNumber: string;
    FPerson: TPersonBasic;
    FRemarks: string;
    FService: TServiceBasic;
    function GetPersonName: string;
    function GetServiceName: string;
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
    function IsValid(const pErrors: TtiObjectErrors): Boolean; override;
  published
    property Person: TPersonBasic read FPerson write SetPerson;
    property Service: TServiceBasic read FService write SetService;
    property DocDate: TDateTime read FDocDate write SetDocDate;
    property DocNumber: string read FDocNumber write SetDocNumber;
    property Amount: Currency read FAmount write SetAmount;
    property Remarks: string read FRemarks write SetRemarks;
    property PersonName: string read GetPersonName;
    property ServiceName: string read GetServiceName;
  end;

  { TPaymentList }

  TPaymentList = class(TFilteredObjectList)
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
    FActive: boolean;
    FName: string;
    FNumber: string;
    procedure SetActive(AValue: boolean);
    procedure SetName(AValue: string);
    procedure SetNumber(AValue: string);
    function GetCaption: string; override;
  published
    property Caption: string read GetCaption;
    property Name: string read FName write SetName;
    property Number: string read FNumber write SetNumber;
    property Active: boolean read FActive write SetActive;
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
    function GetDateJoinedAsString: string;
    function GetID: string;
    procedure SetDateJoined(AValue: TDate);
  protected
    function  GetOwner: TPersonList; reintroduce;
    procedure SetOwner(const Value: TPersonList); reintroduce;
  public
    property  Owner: TPersonList read GetOwner write SetOwner;
  published
    property ID: string read GetID;
    property DateJoined: TDate read FDateJoined write SetDateJoined;
    property DateJoinedAsString: string read GetDateJoinedAsString;
  end;

  { TPersonList }

  TPersonList = class(TFilteredObjectList)
  private
    //FPersonsFilter: TPersonsFilter;
  protected
    function  GetItems(i: integer): TPerson; reintroduce;
    procedure SetItems(i: integer; const Value: TPerson); reintroduce;
  public
    //property PersonsFilter: TPersonsFilter read FPersonsFilter write FPersonsFilter;
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
    function GetCaption: string; override;
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
    FActive: Boolean;
    FCSVUploadName: string;
    FInterestRate: Currency;
    FMaxAmount: Currency;
    FMaxTerm: integer;
    FMinAmount: Currency;
    FMinTerm: integer;
    FName: string;
    FRebateRate: Currency;
    FServiceType: string;
    function GetCaption: string;
    function GetServiceTypeGUI: string;
    procedure SetActive(AValue: Boolean);
    procedure SetCSVUploadName(AValue: string);
    procedure SetInterestRate(AValue: Currency);
    procedure SetMaxAmount(AValue: Currency);
    procedure SetMaxTerm(AValue: integer);
    procedure SetMinAmount(AValue: Currency);
    procedure SetMinTerm(AValue: integer);
    procedure SetName(AValue: string);
    procedure SetRebateRate(AValue: Currency);
    procedure SetServiceType(AValue: string);
    procedure SetServiceTypeGUI(AValue: string);
  protected
    function  GetOwner: TServiceList; reintroduce;
    procedure SetOwner(const Value: TServiceList); reintroduce;
  public
    property  Owner: TServiceList read GetOwner write SetOwner;
    property ServiceType: string read FServiceType write SetServiceType;
  published
    property Caption: string read GetCaption;
    property Name: string read FName write SetName;
    property MaxAmount: Currency read FMaxAmount write SetMaxAmount;
    property MinAmount: Currency read FMinAmount write SetMinAmount;
    property InterestRate: Currency read FInterestRate write SetInterestRate;
    property RebateRate: Currency read FRebateRate write SetRebateRate;
    property MinTerm: integer read FMinTerm write SetMinTerm;
    property MaxTerm: integer read FMaxTerm write SetMaxTerm;
    property ServiceTypeGUI: string read GetServiceTypeGUI write SetServiceTypeGUI;
    property CSVUploadName: string read FCSVUploadName write SetCSVUploadName;
    property Active: Boolean read FActive write SetActive;
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
    FBalance: currency;
    FCharges: Currency;
    FParticulars: string;
    FPayments: Currency;
    FPerson: TPersonBasic;
    FPersonUI: string;
    FReference: String;
    FService: TService;
    FServiceType: string;
    FServiceUI: string;
    FTransDate: TDate;
    procedure SetBalance(AValue: currency);
    procedure SetCharges(AValue: Currency);
    procedure SetParticulars(AValue: string);
    procedure SetPayments(AValue: Currency);
    procedure SetPerson(AValue: TPerson);
    procedure SetPerson(AValue: TPersonBasic);
    procedure SetPersonUI(AValue: string);
    procedure SetReference(AValue: String);
    procedure SetService(AValue: TService);
    procedure SetServiceType(AValue: string);
    procedure SetServiceUI(AValue: string);
    procedure SetTransDate(AValue: TDate);
  protected
    function  GetOwner: TPersonLedger; reintroduce;
    procedure SetOwner(const Value: TPersonLedger); reintroduce;
  public
    property  Owner: TPersonLedger read GetOwner write SetOwner;
    constructor create; override;
    destructor destroy; override;
  published
    property Person: TPersonBasic read FPerson write SetPerson;
    property Service: TService read FService write SetService;
    property PersonUI: string read FPersonUI write SetPersonUI;
    property ServiceUI: string read FServiceUI write SetServiceUI;
    property ServiceType: string read FServiceType write SetServiceType;
    property TransDate: TDate read FTransDate write SetTransDate;
    property Reference: String read FReference write SetReference;
    property Particulars: string read FParticulars write SetParticulars;
    property Charges: Currency read FCharges write SetCharges;
    property Payments: Currency read FPayments write SetPayments;
    property Balance: currency read FBalance write SetBalance;
  end;

  TPersonLedger = class(TFilteredObjectList)
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
    FAdjustmentList: TLoanAdjustmentList;
    FAdjustments: Currency;
    FAmortization: Currency;
    FBalance: Currency;
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
    function GetMember: String;
    function GetPersonID: string;
    function GetServiceID: string;
    function GetServiceName: String;
    procedure SetAdjustments(AValue: Currency);
    procedure SetAmortization(AValue: Currency);
    procedure SetBalance(AValue: Currency);
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
    procedure Init;
    procedure UpdateBasicData;
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
    property Adjustments: Currency read FAdjustments write SetAdjustments;
    Property NetProceeds: Currency read FNetProceeds write SetNetProceeds;
    Property InterestRate: Currency read FInterestRate write SetInterestRate;
    property RebateRate: currency read FRebateRate write SetRebateRate;
    Property Terms: Integer read FTerms write SetTerms;
    Property Amortization: Currency read FAmortization write SetAmortization;
    Property PaymentStart: Tdate read FPaymentStart write SetPaymentStart;
    Property PaymentEnd: Tdate read FPaymentEnd write SetPaymentEnd;
    property Balance: Currency read FBalance write SetBalance;
    property AdjustmentList: TLoanAdjustmentList read FAdjustmentList;
    property Member: String read GetMember;
    property ServiceName: String read GetServiceName;
  end;

  { TLoanList }

  TLoanList = class(TFilteredObjectList)
  protected
    function  GetItems(i: integer): TLoan; reintroduce;
    procedure SetItems(i: integer; const Value: TLoan); reintroduce;
  public
    property Items[i:integer]: TLoan read GetItems write SetItems;
    procedure Add(AObject:TLoan); reintroduce;
  published
  end;

const
  cSMAX = 2; // 3 service types
  cServiceTypesDB : array [0..cSMAX] of string = ('LOAN','CONT','OTH');
  cServiceTypesGUI : array [0..cSMAX] of string = ('Loan','Contribution','Others');

implementation
uses
  variants, dateutils
  , Model_View
  ;

const
  cNameMissing = 'Member name cannot be empty.';
  cLoanTypeMissing = 'Loan Type cannot be empty.';
  cValueNotAllowed = '%s value is not valid.';
  cAmountZeroNotValid = 'Amount cannot be zero (0).';
  cNumberMissing = 'Number cannot be empty.';
  cDateMissing = 'Date cannot be empty.';

{ TLoanAdjustmentList }

function TLoanAdjustmentList.GetItems(i: integer): TLoanAdjustment;
begin
  result := TLoanAdjustment(inherited GetItems(i));
end;

procedure TLoanAdjustmentList.SetItems(i: integer; const Value: TLoanAdjustment
  );
begin
  inherited SetItems(i, Value);
end;

procedure TLoanAdjustmentList.Add(AObject: TLoanAdjustment);
begin
  inherited Add(AObject);
end;

{ TLoanAdjustment }

procedure TLoanAdjustment.SetService(AValue: TService);
begin
  if FService=AValue then Exit;
  FService:=AValue;
  Mark;
end;

procedure TLoanAdjustment.SetAmount(AValue: Currency);
begin
  if FAmount=AValue then Exit;
  FAmount:=AValue;
  Mark;
end;

function TLoanAdjustment.GetAmountAsString: string;
begin
  Result := FormatFloat('#,0.00', Amount);
end;

procedure TLoanAdjustment.SetLoanID(AValue: string);
begin
  if FLoanID=AValue then Exit;
  FLoanID:=AValue;
  Mark;
end;

function TLoanAdjustment.GetOwner: TLoanAdjustmentList;
begin
  result := TLoanAdjustmentList(inherited GetOwner);
end;

procedure TLoanAdjustment.SetOwner(const Value: TLoanAdjustmentList);
begin
  inherited SetOwner(Value);
end;

constructor TLoanAdjustment.Create;
begin
  inherited Create;
  FService := TService.Create;
end;

destructor TLoanAdjustment.Destroy;
begin
  FService := nil;
  FService.Free;
  inherited Destroy;
end;

procedure TLoanAdjustment.AssignClassProps(ASource: TtiObject);
begin
  FService := TLoanAdjustment(ASource).Service;
end;

{ TFilteredObjectList }

constructor TFilteredObjectList.Create;
begin
  inherited Create;
  FListFilter := TObjectListFilter.Create;
  //FListFilter.Criteria:= '';
  //FListFilter.Active:= False;
  //  already performed by parent class
end;

destructor TFilteredObjectList.Destroy;
begin
  FListFilter.Free;
  inherited Destroy;
end;

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

function TPayment.GetPersonName: string;
begin
  result := Person.Name;
end;

function TPayment.GetServiceName: string;
begin
  result := service.name;
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
  FPerson.Assign(TPayment(ASource).Person);
  //FService.Assign(TPayment(ASource).Service);
  //FPerson := TPayment(ASource).Person;
  FService := TPayment(ASource).Service;
end;

function TPayment.IsValid(const pErrors: TtiObjectErrors): Boolean;
begin
  result := inherited IsValid(pErrors);
  if not result then exit; // <==

  if DocNumber = '' then
    pErrors.AddError('_DocNumber',cNumberMissing);

  if DocDate = 0 then
    pErrors.AddError('_DocDate', cDateMissing);

  if Person.Name = '' then
    pErrors.AddError('_Name', cNameMissing);

  if Person.Name <> '' then
  begin
    if Service = nil then
      pErrors.AddError('_Service.Name', cLoanTypeMissing)
  end;

  if Amount < 1 then
    pErrors.AddError('_Amount', cAmountZeroNotValid);

  Result := pErrors.Count = 0;
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

procedure TPersonBasic.SetNumber(AValue: string);
begin
  if FNumber=AValue then Exit;
  FNumber:=AValue;
end;

function TPersonBasic.GetCaption: string;
begin
  result := Name;
end;

procedure TPersonBasic.SetActive(AValue: boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;
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

procedure TLoan.SetBalance(AValue: Currency);
begin
  if FBalance=AValue then Exit;
  FBalance:=AValue;
end;

function TLoan.GetPersonID: string;
begin
  result := Person.OID.AsString;
end;

function TLoan.GetMember: String;
begin
  Result := Person.Name;
end;

function TLoan.GetServiceID: string;
begin
  result := Service.OID.AsString;
end;

function TLoan.GetServiceName: String;
begin
  Result := Service.Name;
end;

procedure TLoan.SetAdjustments(AValue: Currency);
begin
  if FAdjustments=AValue then Exit;
  FAdjustments:=AValue;
  RecomputeNetProceeds;
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
  FAdjustmentList := TLoanAdjustmentList.Create;
  FAdjustmentList.Owner := Self;
end;

destructor TLoan.Destroy;
begin
  FPerson := nil;
  FPerson.Free;
  FService := nil;
  FService.Free;
  FreeAndNil(FAdjustmentList);
  inherited Destroy;
end;

function TLoan.IsValid(const pErrors: TtiObjectErrors): Boolean;
begin
  result := inherited IsValid(pErrors);
  if not result then exit; // <==

  if DocNumber = '' then
    pErrors.AddError('_number', cNumberMissing);

  if DocDate = 0 then
    pErrors.AddError('_date', cDateMissing);

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
  FPerson.Assign(TLoan(ASource).Person);
  //FService.Assign(TLoan(ASource).Service );
  //FPerson := TLoan(ASource).Person;
  FService := TLoan(ASource).Service;
  FAdjustmentList.Assign(TLoan(ASource).AdjustmentList);
end;

procedure TLoan.RecomputeTotal;
begin
  if not RecomputeTotals then Exit;  // <===
  Interest:= Principal * InterestRate * 0.01 * (Terms/12);
  Total:= Principal + Interest;
  PaymentStart:= StartOfTheMonth( IncMonth(DocDate, 1) );
  if Terms > 0 then
    PaymentEnd:= StartOfTheMonth( IncMonth(PaymentStart, Terms-1) );
  RecomputeNetProceeds;
end;

procedure TLoan.RecomputeNetProceeds;
begin
  NetProceeds:= Principal - PreviousBalance + Rebates + Adjustments;
  if Terms > 0 then
    Amortization:= Total / Terms
  else
    Amortization:= 0;
  NotifyObservers;
end;

procedure TLoan.Init;
begin
  // Call this when selecting new Member in the application form

  Service         := nil;   // clear loan type
  Principal       := 0;     // clear principal
  Terms           := 0;     // clear terms
  InterestRate    := 0;
  RebateRate      := 0;
  PreviousBalance := 0;
  Rebates         := 0;
  Adjustments     := 0;

  RecomputeTotal;
end;

procedure TLoan.UpdateBasicData;
begin
  // call this after selecting a Loan Type in the UI,
  //  preferrably after a ComboBox OnCloseUp

  if (Principal > Service.MaxAmount) or (Principal = 0) then
    Principal:= service.maxamount;

  InterestRate:= Service.InterestRate;

  if (Terms > service.MaxTerm) or (terms = 0) then
    terms := service.MaxTerm;

  RebateRate:= service.RebateRate;

  RecomputeTotals:= True;
  RecomputeTotal;
end;

{ TManualObject }

procedure TManualObject.Mark;
begin
  Dirty:= True;
end;

procedure TManualObject.SaveObject;
begin
  Dirty:= True;
  Save;
  Dirty:= False;
end;

procedure TManualObject.DeleteObject(FromList: TtiObjectList; var s: string;
  const isPropagageToDB: boolean = True);
begin
  s := '';
  Deleted:=True;
  try
    if isPropagageToDB then
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

procedure TPersonLedgerItem.SetBalance(AValue: currency);
begin
  if FBalance=AValue then Exit;
  FBalance:=AValue;
end;

procedure TPersonLedgerItem.SetParticulars(AValue: string);
begin
  if FParticulars=AValue then Exit;
  FParticulars:=AValue;
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

procedure TPersonLedgerItem.SetPerson(AValue: TPersonBasic);
begin
  if FPerson=AValue then Exit;
  FPerson:=AValue;
end;

procedure TPersonLedgerItem.SetPersonUI(AValue: string);
begin
  if FPersonUI=AValue then Exit;
  FPersonUI:=AValue;
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

procedure TPersonLedgerItem.SetServiceType(AValue: string);
begin
  if FServiceType=AValue then Exit;
  FServiceType:=AValue;
end;

procedure TPersonLedgerItem.SetServiceUI(AValue: string);
begin
  if FServiceUI=AValue then Exit;
  FServiceUI:=AValue;
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

constructor TPersonLedgerItem.create;
begin
  inherited create;
  FPerson := TPersonBasic.Create;
  FService := TService.Create;
end;

destructor TPersonLedgerItem.destroy;
begin
  FService := nil;
  FService.Free;
  FPerson := nil;
  FPerson.free;
  inherited destroy;
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

function TService.GetServiceTypeGUI: string;
var
  i: Integer;
begin
  //Result := '<?>' ;
  //if FServiceType <> '' then
  begin
    for i := 0 to cSMAX do
      if cServiceTypesDB[i] = FServiceType then
      begin
        Result := cServiceTypesGUI[i];
        exit;   //<==
      end;
  end;
end;


procedure TService.SetActive(AValue: Boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;
end;

procedure TService.SetCSVUploadName(AValue: string);
begin
  if FCSVUploadName=AValue then Exit;
  FCSVUploadName:=AValue;
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

procedure TService.SetServiceType(AValue: string);
begin
  if FServiceType=AValue then Exit;
  FServiceType:=AValue;
end;

procedure TService.SetServiceTypeGUI(AValue: string);
var
  i: Integer;
begin
  ServiceType:= '';
  for i := 0 to cSMAX do
    if cServiceTypesGUI[i] = AValue then
    begin
      ServiceType := cServiceTypesDB[i];
      exit;  // <==
    end;
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
  //FPersonsFilter := TPersonsFilter.Create;
  //FPersonsFilter.Active:= False;
end;

destructor TPersonList.Destroy;
begin
  //FPersonsFilter.Free;
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

