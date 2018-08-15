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
    FServices: TServiceList;
    procedure SetLoans(AValue: TLoanList);
    procedure SetPersonList(AValue: TPersonList);
    procedure SetServices(AValue: TServiceList);
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadPersons;
    procedure LoadServices;
  published
    property PersonList: TPersonList read FPersonList write SetPersonList;
    property Services: TServiceList read FServices write SetServices;
    property Loans: TLoanList read FLoans write SetLoans;
  end;

  //global Singleton
  function gLedgerManager: TLedgerManager;

implementation

uses
  sysutils
  , tiOPFManager;

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
end;

destructor TLedgerManager.Destroy;
begin
  FPersonList.Free;
  FServices.Free;
  FLoans.Free;
  inherited Destroy;
end;

procedure TLedgerManager.LoadPersons;
//var
//  P: TPerson;
begin
  //P := TPerson.Create;
  //P.Name:= 'Jeff Plata';
  //P.DateJoined:= Now;
  //FPersonList.Add(P);
  //
  //P := TPerson.Create;
  //P.Name:= 'John Carter';
  //P.DateJoined:= Now-1234;
  //FPersonList.Add(P);

  FPersonList.Clear;
  GTIOPFManager.Read(FPersonList);
end;

procedure TLedgerManager.LoadServices;
begin
  FServices.Clear;
  GTIOPFManager.Read(FServices);
end;


initialization
  uLedgerManager := nil;

finalization
  uLedgerManager.Free;

end.

