unit ledgermanager;

{$mode objfpc}{$H+}

interface

uses
  ledger_bom
  , tiObject
  ;

type

  { TLedgerManager }

  TLedgerManager = class(TtiObject)
  private
    FLoans: TLoanList;
    FPaymentList: TPaymentList;
    FPersonList: TPersonList;
    FPersonsLookup: TPersonsLookUp;
    FServiceBasicList: TServiceBasicList;
    FServices: TServiceList;
    procedure SetLoans(AValue: TLoanList);
    procedure SetPaymentList(AValue: TPaymentList);
    procedure SetPersonList(AValue: TPersonList);
    procedure SetPersonsLookup(AValue: TPersonsLookUp);
    procedure SetServiceBasicList(AValue: TServiceBasicList);
    procedure SetServices(AValue: TServiceList);
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadPersons;
    procedure LoadServices;
    procedure LoadPersonsLookup( force: boolean = False );
    procedure LoadLoans;
    procedure LoadPayments;
  published
    property PersonList: TPersonList read FPersonList write SetPersonList;
    property Services: TServiceList read FServices write SetServices;
    property Loans: TLoanList read FLoans write SetLoans;
    property PersonsLookup: TPersonsLookUp read FPersonsLookup write SetPersonsLookup;
    property PaymentList: TPaymentList read FPaymentList write SetPaymentList;
    property ServiceBasicList: TServiceBasicList read FServiceBasicList write SetServiceBasicList;
  end;

  //global Singleton
  function gLedgerManager: TLedgerManager;

implementation

uses
  sysutils
  , tiOPFManager
  ;

var
  uLedgerManager: TLedgerManager;

function gLedgerManager: TLedgerManager;
begin
  if not Assigned(uLedgerManager) then
    uLedgerManager := TLedgerManager.Create;
  Result := uLedgerManager;
end;

{ TLedgerManager }

procedure TLedgerManager.SetPersonList(AValue: TPersonList);
begin
  if FPersonList=AValue then Exit;
  FPersonList:=AValue;
end;

procedure TLedgerManager.SetPersonsLookup(AValue: TPersonsLookUp);
begin
  if FPersonsLookup=AValue then Exit;
  FPersonsLookup:=AValue;
end;

procedure TLedgerManager.SetServiceBasicList(AValue: TServiceBasicList);
begin
  if FServiceBasicList=AValue then Exit;
  FServiceBasicList:=AValue;
end;

procedure TLedgerManager.SetLoans(AValue: TLoanList);
begin
  if FLoans=AValue then Exit;
  FLoans:=AValue;
end;

procedure TLedgerManager.SetPaymentList(AValue: TPaymentList);
begin
  if FPaymentList=AValue then Exit;
  FPaymentList:=AValue;
end;

procedure TLedgerManager.SetServices(AValue: TServiceList);
begin
  if FServices=AValue then Exit;
  FServices:=AValue;
end;

constructor TLedgerManager.Create;
begin
  inherited Create;
  FPersonList := TPersonList.Create;
  FPersonList.Owner := Self;

  FServices := TServiceList.Create;
  FServices.Owner := Self;

  FServiceBasicList := TServiceBasicList.Create;
  FServiceBasicList.Owner := Self;

  FLoans := TLoanList.Create;
  FLoans.Owner := Self;

  FPersonsLookup := TPersonsLookUp.Create;
  FPersonsLookup.Owner := Self;

  FPaymentList :=  TPaymentList.Create;
  FPaymentList.Owner := Self;
end;

destructor TLedgerManager.Destroy;
begin
  FPersonList.Free;
  FServices.Free;
  FLoans.Free;
  FPersonsLookup.Free;
  inherited Destroy;
end;

procedure TLedgerManager.LoadPersons;
begin
  FPersonList.Clear;
  GTIOPFManager.Read(FPersonList);
end;

procedure TLedgerManager.LoadServices;
begin
  FServices.Clear;
  FServiceBasicList.Clear; // populate also the name/oid list
  GTIOPFManager.Read(FServices);
end;

procedure TLedgerManager.LoadPersonsLookup(force: boolean);
begin
  if force or (FPersonsLookup.Count = 0) then
  begin
    FPersonsLookup.Clear;
    GTIOPFManager.Read(FPersonsLookup);
  end;
end;

procedure TLedgerManager.LoadLoans;
begin
  FLoans.Clear;
  GTIOPFManager.Read(FLoans);
end;

procedure TLedgerManager.LoadPayments;
begin
  FPaymentList.Clear;
  GTIOPFManager.Read(FPaymentList);
end;


initialization
  uLedgerManager := nil;

finalization
  uLedgerManager.Free;

end.

