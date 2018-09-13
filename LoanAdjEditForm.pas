unit LoanAdjEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, EditBtn, ledger_bom, tiModelMediator, JLabeledCurrencyEdit;

type

  { TfrmLoanAdjEdit }

  TfrmLoanAdjEdit = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cmbService: TComboBox;
    edtAmount: TJLabeledCurrencyEdit;
    Label1: TLabel;
    Label2: TLabel;
  private
    FData: TLoanAdjustment;
    FMediator: TtiModelMediator;
    procedure SetData(AValue: TLoanAdjustment);
    procedure SetupMediators;
    procedure SetupUI;
  public
    property Data: TLoanAdjustment read FData write SetData;
  end;

  function EditLoanAdjustment( AData: TLoanAdjustment ): Boolean;

implementation

uses ledgermanager, Model_View;

function EditLoanAdjustment(AData: TLoanAdjustment): Boolean;
begin
  with TfrmLoanAdjEdit.Create(nil) do
  try
    Data := Adata;
    SetupUI;
    result := (ShowModal = mrOK);
  finally
    Free;
  end;
end;

{$R *.lfm}

{ TfrmLoanAdjEdit }


procedure TfrmLoanAdjEdit.SetData(AValue: TLoanAdjustment);
begin
  if FData=AValue then Exit;
  FData:=AValue;
  SetupMediators;
end;

procedure TfrmLoanAdjEdit.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(Self);
    FMediator.AddProperty('Service', cmbService).ValueList := gLedgerManager.Services;
    FMediator.AddProperty('Amount', edtAmount);
  end;
  FMediator.Subject := FData;
  FMediator.Active:= True;
end;

procedure TfrmLoanAdjEdit.SetupUI;
begin
  if Visible then cmbService.SetFocus;
  edtAmount.EditLabel.Visible:= false;
end;

end.

