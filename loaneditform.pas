unit LoanEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, EditBtn, ActnList, JLabeledCurrencyEdit,
  ledger_bom, tiModelMediator;

type

  { TfrmLoanEdit }

  TfrmLoanEdit = class(TForm)
    actSelectPerson: TAction;
    ActionList1: TActionList;
    btnSearchMember: TButton;
    btnSearchMember1: TButton;
    Button1: TButton;
    Button2: TButton;
    cmbService: TComboBox;
    dtpPaymentStarts: TDateEdit;
    dtpPaymentEnds: TDateEdit;
    dtpDocDate: TDateEdit;
    edtAmortization: TJLabeledCurrencyEdit;
    edtTotal: TJLabeledCurrencyEdit;
    GroupBox1: TGroupBox;
    edtPrincipal: TJLabeledCurrencyEdit;
    JLabeledCurrencyEdit10: TJLabeledCurrencyEdit;
    JLabeledCurrencyEdit11: TJLabeledCurrencyEdit;
    JLabeledCurrencyEdit12: TJLabeledCurrencyEdit;
    JLabeledCurrencyEdit13: TJLabeledCurrencyEdit;
    JLabeledCurrencyEdit14: TJLabeledCurrencyEdit;
    edtInterest: TJLabeledCurrencyEdit;
    edtInterestRate: TJLabeledCurrencyEdit;
    edtPreviousBalance: TJLabeledCurrencyEdit;
    edtRebates: TJLabeledCurrencyEdit;
    edtRebateRate: TJLabeledCurrencyEdit;
    edtNetProceeds: TJLabeledCurrencyEdit;
    JLabeledCurrencyEdit8: TJLabeledCurrencyEdit;
    JLabeledCurrencyEdit9: TJLabeledCurrencyEdit;
    Label1: TLabel;
    edtPerson: TLabeledEdit;
    edtTerms: TLabeledEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edtDocNumber: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    procedure actSelectPersonExecute(Sender: TObject);
    procedure cmbServiceCloseUp(Sender: TObject);
    procedure cmbServiceKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
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
  ,PersonsLookupForm
  ,LookupForm
  ,LCLType
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

procedure TfrmLoanEdit.actSelectPersonExecute(Sender: TObject);
var
  temp: TPersonBasic;
begin
  //temp := SelectPerson;
  //try
  //  if temp <> nil then
  //  begin
  //    FData.Person.Assign(temp); // := temp;
  //    FData.NotifyObservers;
  //  end;
  //finally
  //  temp := nil;
  //  temp.Free;
  //end;
  temp := TPersonBasic.Create;
  try
    gLedgerManager.LoadPersonsLookup;  sdfsdfsdf
    SelectObject( TClassOfObject(temp),gLedgerManager.PersonsLookup,'NAME containing ?','Name' );
    if temp <> nil then
    begin
      FData.Person.Assign(temp);
      FData.NotifyObservers;
    end;
  finally
    temp := nil;
    temp.Free;
  end;
end;


procedure TfrmLoanEdit.cmbServiceCloseUp(Sender: TObject);
begin
  if cmbService.ItemIndex < 0 then exit; //<==
  //AdjustPrincipal;fdssd
  Data.InterestRate:= Data.Service.InterestRate;
  Data.Interest:= Data.Principal*Data.InterestRate*0.01*(Data.Terms/12);
  Data.Total:= Data.Principal + Data.Interest;
  Data.NotifyObservers;
end;


procedure TfrmLoanEdit.cmbServiceKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  cmbService.OnCloseUp(Sender);
end;

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
    FMediator.Name := 'LoanEditMediator';
    FMediator.AddProperty('DocNumber', edtDocNumber);
    FMediator.AddProperty('DocDate', dtpDocDate);
    FMediator.AddProperty('Person.Name', edtPerson);
    FMediator.AddProperty('Service',cmbService).ValueList := gLedgerManager.Services;
    FMediator.AddProperty('Principal', edtPrincipal);
    FMediator.AddProperty('Terms', edtTerms);
    FMediator.AddProperty('Interest', edtInterest);
    FMediator.AddProperty('InterestRate', edtInterestRate);
    FMediator.AddProperty('Total', edtTotal);
    FMediator.AddProperty('PreviousBalance', edtPreviousBalance);
    FMediator.AddProperty('Rebates', edtRebates);
    FMediator.AddProperty('RebateRate', edtRebateRate);
    FMediator.AddProperty('NetProceeds', edtNetProceeds);
    FMediator.AddProperty('Amortization', edtAmortization);
    FMediator.AddProperty('PaymentStart', dtpPaymentStarts);
    FMediator.AddProperty('PaymentEnd', dtpPaymentEnds);
  end;
  FMediator.Subject := FData;
  FMediator.Active:= True;
end;

{ TfrmLoanEdit }

end.

onchange loan type:
-------------
if amount > max or 0
  amount = max

if terms > max or 0
  terms = max

RecomputeTotal()


on change amount:
---------
RecomputeTotal()




RecomputeTotal()
interest = p * ir * 0.01 *t/12
total = p + i
net = total - pbal + reb
amort = pi/terms


RecomputeNetProceeds()
net = total - pbal + reb

