unit PersonsLookupForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ExtCtrls, Buttons, EditBtn, StdCtrls
  ,ledger_bom
  ,tiModelMediator
  ;

type

  { TfrmLkuPersons }

  TfrmLkuPersons = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    LabeledEdit1: TLabeledEdit;
    SpeedButton1: TSpeedButton;
    StringGrid1: TStringGrid;
  private
    FData: TPersonsLookUp;
    FMediator: TtiModelMediator;
    procedure SetData(AValue: TPersonsLookUp);
    procedure SetupMediator;
  public
    property Data: TPersonsLookUp read FData write SetData;
  end;

  function SelectPerson: TPersonBasic;

//var
//  frmLkuPersons: TfrmLkuPersons;

implementation

uses
  ledgermanager
  ;

{$R *.lfm}

function SelectPerson: TPersonBasic;
begin
  Result := nil;
  with TfrmLkuPersons.Create(Application) do
  try
    gLedgerManager.LoadPersonsLookup;
    Data := gLedgerManager.PersonsLookup;
    if ShowModal = mrOk then
      Result := TPersonBasic(FMediator.SelectedObject[StringGrid1]);
  finally
    Free;
  end;
end;

{ TfrmLkuPersons }


procedure TfrmLkuPersons.SetData(AValue: TPersonsLookUp);
begin
  if FData=AValue then Exit;
  FData:=AValue;
  SetupMediator;
end;

procedure TfrmLkuPersons.SetupMediator;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(Self);
    FMediator.Name:= 'PersonsLkUpMediator';
    FMediator.AddComposite('Name',StringGrid1);
  end;
  FMediator.Subject := FData;
  FMediator.Active:= True;
end;



end.

