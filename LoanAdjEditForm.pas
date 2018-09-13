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
    procedure FormShow(Sender: TObject);
  private
    FData: TLoanAdjustment;
    FMediator: TtiModelMediator;
    procedure SetData(AValue: TLoanAdjustment);
    procedure SetupMediators;
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
    result := (ShowModal = mrOK);
  finally
    Free;
  end;
end;

{$R *.lfm}

{ TfrmLoanAdjEdit }

procedure TfrmLoanAdjEdit.FormShow(Sender: TObject);
begin
  if Visible then
  begin
    cmbService.SetFocus;
    edtAmount.EditLabel.Visible:= false;
  end;
end;

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
    FMediator.Name:= 'frmLoanAdjEditLoanAdj';
    FMediator.AddProperty('Service', cmbService).ValueList := gLedgerManager.Services;
    FMediator.AddProperty('Amount', edtAmount);
  end;
  FMediator.Subject := FData;
  FMediator.Active:= True;
end;


end.

