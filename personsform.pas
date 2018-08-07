unit PersonsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids
  ,ledger_bom
  ,ledgermanager
  , tiModelMediator
  , tiListMediators
  , tiMediators
  ;

type

  { TfrmPersons }

  TfrmPersons = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FData: TPersonList;
    FMediator: TtiModelMediator;
    procedure SetData(AValue: TPersonList);
    procedure SetupMediators;
  public
    property Data: TPersonList read FData write SetData;
  end;

  procedure ShowPersons( Persons: TPersonList );

var
  frmPersons: TfrmPersons;

implementation

procedure ShowPersons(Persons: TPersonList);
begin
  with TfrmPersons.Create(Application) do
  try
    //Data := Persons;
    ShowModal;
  finally
    Free;
  end;
end;

{$R *.lfm}

{ TfrmPersons }

procedure TfrmPersons.FormCreate(Sender: TObject);
begin
  RegisterFallBackMediators;
  RegisterFallBackListmediators;
  gLedgerManager.LoadPersons;
  Data := gLedgerManager.PersonList;
  SetupMediators;
end;

procedure TfrmPersons.SetData(AValue: TPersonList);
begin
  if FData=AValue then Exit;
  FData:=AValue;
end;

procedure TfrmPersons.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(Self);
    FMediator.Name:= 'PersonListMediator';
    //FMediator.AddComposite('Name(100,"Name")',sgdPersonList);
  end;
  FMediator.Subject:= gLedgerManager.PersonList;
  FMediator.Active:= True;
end;

end.

