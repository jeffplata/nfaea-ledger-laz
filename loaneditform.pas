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
    edtAdjustments: TJLabeledCurrencyEdit;
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FData: TLoan;
    FMediator: TtiModelMediator;
    procedure SetData(AValue: TLoan);
    procedure SetupMediators;
    procedure InitData;
    procedure SetupUI;
    procedure UpdateLoanData;
    procedure UpdateComboBox;
  public
    property Data: TLoan read FData write SetData;
  end;

  function EditLoan( AData: TLoan ): boolean;

implementation

uses
  ledgermanager
  ,LookupForm
  ,LCLType
  ;

function EditLoan(AData: TLoan): boolean;
begin
  with TfrmLoanEdit.Create(nil) do
  try
    Data := Adata;
    Data.RecomputeTotals:= True;
    //UpdateComboBox;
    SetupUI;
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
  temp := TPersonBasic.Create;
  try
    gLedgerManager.LoadPersonsLookup;
    SelectObject( TClassOfObject(temp),gLedgerManager.PersonsLookup,'NAME containing ?','Name' );
    if temp <> nil then
    begin
      FData.Person.Assign(temp);
      InitData;
      FData.NotifyObservers;
    end;
  finally
    temp := nil;
    temp.Free;
  end;
end;


procedure TfrmLoanEdit.cmbServiceCloseUp(Sender: TObject);
begin
  UpdateLoanData;
end;


procedure TfrmLoanEdit.cmbServiceKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  UpdateLoanData;
end;

procedure TfrmLoanEdit.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  s: string;
begin
  if ModalResult = mrOk then
  begin
    if not Data.IsValid(s) then
    begin
      ShowMessage(S);
      CanClose:= False;
    end
    else
      data.RecomputeTotals:= False;
  end;
end;

procedure TfrmLoanEdit.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F4 then
  begin
    if ActiveControl = edtPerson then
    begin
      actSelectPerson.Execute;
      Key:= 0;
    end;
  end;
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
    FMediator.AddProperty('Adjustments', edtAdjustments);
    FMediator.AddProperty('NetProceeds', edtNetProceeds);
    FMediator.AddProperty('Amortization', edtAmortization);
    FMediator.AddProperty('PaymentStart', dtpPaymentStarts);
    FMediator.AddProperty('PaymentEnd', dtpPaymentEnds);
  end;
  FMediator.Subject := FData;
  FMediator.Active:= True;
end;

procedure TfrmLoanEdit.InitData;
begin
  data.Init;
  //Data.Service         := nil;   // clear loan type
  //Data.Principal       := 0;     // clear principal
  //Data.Terms           := 0;     // clear terms
  //data.InterestRate    := 0;
  //data.RebateRate      := 0;
  //data.PreviousBalance := 0;
  //data.Rebates         := 0;
  //data.Adjustments     := 0;
//
//  data.RecomputeTotal;
end;

procedure TfrmLoanEdit.SetupUI;
begin
  edtPerson.Color:= clInfoBk;
  edtTotal.Color:= clInfoBk;
  edtNetProceeds.Color:= clInfoBk;
  edtInterestRate.Color:= clInfoBk;
  edtRebateRate.Color:= clInfoBk;
end;

procedure TfrmLoanEdit.UpdateLoanData;
begin
  if cmbService.ItemIndex < 0 then exit; //<==

  data.UpdateBasicData;

  //if (data.Principal > data.Service.MaxAmount) or (data.Principal = 0) then
  //  data.Principal:= data.service.maxamount;
  //
  //data.InterestRate:= data.Service.InterestRate;
  //
  //if (data.Terms > data.service.MaxTerm) or (data.terms = 0) then
  //  data.terms := data.service.MaxTerm;
  //
  //data.RebateRate:= data.service.RebateRate;
  //
  //data.RecomputeTotals:= True;
  //data.RecomputeTotal;

end;

procedure TfrmLoanEdit.UpdateComboBox;
var
  i: Integer;
begin
  if cmbService.Text <> Data.Service.Name then
  begin
    i := cmbService.Items.IndexOf(Data.Service.Name);
    cmbService.ItemIndex:= i;
  end;
end;

end.



