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
    FPersonList: TPersonList;
    FPersonsLookup: TPersonsLookUp;
    FServices: TServiceList;
    procedure SetLoans(AValue: TLoanList);
    procedure SetPersonList(AValue: TPersonList);
    procedure SetPersonsLookup(AValue: TPersonsLookUp);
    procedure SetServices(AValue: TServiceList);
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadPersons;
    procedure LoadServices;
    procedure LoadPersonsLookup( force: boolean = False );
    procedure LoadLoans;
  published
    property PersonList: TPersonList read FPersonList write SetPersonList;
    property Services: TServiceList read FServices write SetServices;
    property Loans: TLoanList read FLoans write SetLoans;
    property PersonsLookup: TPersonsLookUp read FPersonsLookup write SetPersonsLookup;
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

procedure TLedgerManager.SetLoans(AValue: TLoanList);
begin
  if FLoans=AValue then Exit;
  FLoans:=AValue;
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

  FLoans := TLoanList.Create;
  FLoans.Owner := Self;

  FPersonsLookup := TPersonsLookUp.Create;
  FPersonsLookup.Owner := Self;
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


initialization
  uLedgerManager := nil;

finalization
  uLedgerManager.Free;

end.

