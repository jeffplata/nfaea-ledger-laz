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
    FPersonList: TPersonList;
    procedure SetPersonList(AValue: TPersonList);
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadPersons;
  published
    property PersonList: TPersonList read FPersonList write SetPersonList;
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

constructor TLedgerManager.Create;
begin
  inherited Create;
  FPersonList := TPersonList.Create;
  FPersonList.Owner := Self;
end;

destructor TLedgerManager.Destroy;
begin
  FPersonList.Free;
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

  GTIOPFManager.Read(FPersonList);
end;


initialization
  uLedgerManager := nil;

finalization
  uLedgerManager.Free;

end.

