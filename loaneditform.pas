unit LoanEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, EditBtn, ActnList, Grids, JLabeledCurrencyEdit,
  ledger_bom, tiModelMediator;

type

  { TfrmLoanEdit }

  TfrmLoanEdit = class(TForm)
    actAddAdjustment: TAction;
    actDeleteAdjustment: TAction;
    actEditAdjustment: TAction;
    actSelectPerson: TAction;
    ActionList1: TActionList;
    btnSearchMember: TButton;
    btnSearchMember1: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
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
    Label5: TLabel;
    LabeledEdit3: TLabeledEdit;
    sgdAdjustments: TStringGrid;
    procedure actAddAdjustmentExecute(Sender: TObject);
    procedure actDeleteAdjustmentExecute(Sender: TObject);
    procedure actEditAdjustmentExecute(Sender: TObject);
    procedure actSelectPersonExecute(Sender: TObject);
    procedure cmbServiceCloseUp(Sender: TObject);
    procedure cmbServiceKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgdAdjustmentsDblClick(Sender: TObject);
  private
    FData: TLoan;
    FMediator: TtiModelMediator;
    FMedAdjustments: TtiModelMediator;
    procedure SetData(AValue: TLoan);
    procedure SetupMediators;
    procedure InitData;
    procedure SetupUI;
    procedure UpdateLoanData;
    procedure UpdateComboBox;
    function AdjustmentsTotal: Double;
  public
    property Data: TLoan read FData write SetData;
  end;

  function EditLoan( AData: TLoan ): boolean;

implementation

uses
  ledgermanager
  ,LookupForm, LoanAdjEditForm, DeleteFromListU
  ,LCLType
  ;

function EditLoan(AData: TLoan): boolean;
begin
  with TfrmLoanEdit.Create(nil) do
  try
    Data := Adata;
    Data.RecomputeTotals:= True;
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

procedure TfrmLoanEdit.actAddAdjustmentExecute(Sender: TObject);
var
  O : TLoanAdjustment;
begin
  O := TLoanAdjustment.Create;
  if EditLoanAdjustment(O) then
  begin
    O.LoanID:= FData.OID.AsString;
    Data.AdjustmentList.Add(O);
    FMedAdjustments.SelectedObject[sgdAdjustments] := O; // go to last inserted
    FData.Adjustments:= AdjustmentsTotal;
  end
  else
    O.Free;
end;

procedure TfrmLoanEdit.actDeleteAdjustmentExecute(Sender: TObject);
begin
  if FMedAdjustments.SelectedObject[sgdAdjustments] <> nil then
  begin
    DeleteFromList(sgdAdjustments, FData.AdjustmentList, TLoanAdjustment, False );
    FData.Adjustments:= AdjustmentsTotal;
  end;
end;

procedure TfrmLoanEdit.actEditAdjustmentExecute(Sender: TObject);
var
  O : TLoanAdjustment;
  B : TLoanAdjustment; //Buffer for undo
begin
  O := TLoanAdjustment(FMedAdjustments.SelectedObject[sgdAdjustments]);
  if not assigned(O) then exit; //<==

  B := TLoanAdjustment.Create;
  B.Assign(O);
  B.Dirty:= False;
  if EditLoanAdjustment(B) and B.Dirty then
  begin
    O.Assign(B);
    //O.SaveObject; // Save only when TLoan is saved (master-detail)
    O.NotifyObservers;
    FData.Adjustments:= AdjustmentsTotal;
  end;
  FreeAndNil(B);
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

procedure TfrmLoanEdit.sgdAdjustmentsDblClick(Sender: TObject);
begin
  actEditAdjustment.Execute;
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

  if not Assigned(FMedAdjustments) then
  begin
    FMedAdjustments := TtiModelMediator.Create(Self);
    FMedAdjustments.AddComposite('Service.Name(150,"Service");Amount;Dummy(100," ")', sgdAdjustments);
    //FMedAdjustments.AddComposite('Service.Name;Amount', sgdAdjustments);
  end;
  FMedAdjustments.Subject := FData.AdjustmentList;
  FMedAdjustments.Active:= True;
end;

procedure TfrmLoanEdit.InitData;
begin
  data.Init;
end;

procedure TfrmLoanEdit.SetupUI;
begin
  edtPerson.Color:= clInfoBk;
  edtTotal.Color:= clInfoBk;
  edtNetProceeds.Color:= clInfoBk;
  edtInterestRate.Color:= clInfoBk;
  edtRebateRate.Color:= clInfoBk;
  edtAdjustments.Color:= clInfoBk;
end;

procedure TfrmLoanEdit.UpdateLoanData;
begin
  if cmbService.ItemIndex < 0 then exit; //<==

  data.UpdateBasicData;

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

function TfrmLoanEdit.AdjustmentsTotal: Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FData.AdjustmentList.Count-1 do
    if not FData.AdjustmentList.Items[i].Deleted then
      Result := Result + FData.AdjustmentList.Items[i].Amount;
end;

end.



