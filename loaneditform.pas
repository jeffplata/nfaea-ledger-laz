unit LoanEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, JLabeledCurrencyEdit
  ,ledger_bom
  ,tiModelMediator;

type

  { TfrmLoanEdit }

  TfrmLoanEdit = class(TForm)
    btnSearchMember: TButton;
    btnSearchMember1: TButton;
    Button1: TButton;
    Button2: TButton;
    cmbService: TComboBox;
    GroupBox1: TGroupBox;
    edtPrincipal: TJLabeledCurrencyEdit;
    JLabeledCurrencyEdit10: TJLabeledCurrencyEdit;
    JLabeledCurrencyEdit11: TJLabeledCurrencyEdit;
    JLabeledCurrencyEdit12: TJLabeledCurrencyEdit;
    JLabeledCurrencyEdit13: TJLabeledCurrencyEdit;
    JLabeledCurrencyEdit14: TJLabeledCurrencyEdit;
    JLabeledCurrencyEdit2: TJLabeledCurrencyEdit;
    JLabeledCurrencyEdit3: TJLabeledCurrencyEdit;
    edtPreviousBalance: TJLabeledCurrencyEdit;
    edtRebates: TJLabeledCurrencyEdit;
    JLabeledCurrencyEdit6: TJLabeledCurrencyEdit;
    edtNetProceeds: TJLabeledCurrencyEdit;
    JLabeledCurrencyEdit8: TJLabeledCurrencyEdit;
    JLabeledCurrencyEdit9: TJLabeledCurrencyEdit;
    Label1: TLabel;
    edtPerson: TLabeledEdit;
    edtTerms: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
  private
    FData: TLoan;
    FMediator: TtiModelMediator;
    procedure SetData(AValue: TLoan);
    procedure SetupMediators;
  public
    property Data: TLoan read FData write SetData;
  end;

  function EditLoan( AData: TLoan ): boolean;

implementation

uses
  ledgermanager
  ;

function EditLoan(AData: TLoan): boolean;
begin
  with TfrmLoanEdit.Create(nil) do
  try
    Data := Adata;
    result := (ShowModal = mrOK);
  finally
    Free;
  end;
end;

{$R *.lfm}

{ TfrmLoanEdit }

procedure TfrmLoanEdit.SetData(AValue: TLoan);
begin
  if FData=AValue then Exit;
  FData:=AValue;
  SetupMediators;
end;

procedure TfrmLoanEdit.SetupMediators;
begin

  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(Self);
    FMediator.AddProperty('Principal', edtPrincipal);
    FMediator.AddProperty('Service',cmbService).ValueList := gLedgerManager.Services;

  end;
  FMediator.Subject := FData;
  FMediator.Active:= True;
end;

{ TfrmLoanEdit }

end.

