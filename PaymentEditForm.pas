unit PaymentEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditBtn, MaskEdit, ExtCtrls, ComCtrls, ActnList, ledger_bom,
  tiModelMediator, JLabeledCurrencyEdit;

type

  { TfrmPaymentEdit }

  TfrmPaymentEdit = class(TForm)
    actSelectMember: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    btnSelectMember: TButton;
    cmbService: TComboBox;
    edtDate: TDateEdit;
    edtNumber: TEdit;
    edtAmount: TJLabeledCurrencyEdit;
    edtMember: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PageControl1: TPageControl;
    tabGeneral: TTabSheet;
    procedure actSelectMemberExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FData: TPayment;
    FMediator: TtiModelMediator;
    procedure SetData(AValue: TPayment);
    procedure SetupMediators;
    procedure SetupUI;
    procedure UpdateComboBoxes;
  public
    property Data: TPayment read FData write SetData;
  end;

  function EditPayment( AData: TPayment ): Boolean;

implementation

uses
  Model_View
  , ledgermanager
  , LookupForm
  ;

//todo: icon for user edit buton
function EditPayment(AData: TPayment): Boolean;
begin
  with TfrmPaymentEdit.Create(nil) do
  try
    Data := Adata;
    SetupUI;
    result := (ShowModal = mrOK);
  finally
    Free;
  end;
end;

{$R *.lfm}


{ TfrmPaymentEdit }

procedure TfrmPaymentEdit.actSelectMemberExecute(Sender: TObject);
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
      FData.NotifyObservers;
    end;
  finally
    temp := nil;
    temp.Free;
  end;
end;

procedure TfrmPaymentEdit.FormCreate(Sender: TObject);
begin

end;

procedure TfrmPaymentEdit.SetData(AValue: TPayment);
begin
  if FData=AValue then Exit;
  FData:=AValue;
  SetupMediators;
end;

procedure TfrmPaymentEdit.SetupMediators;
begin

  if not Assigned(FMediator) then
  begin

    FMediator := TtiModelMediator.Create(Self);
    FMediator.Name:= 'PaymentEditMediator';
    FMediator.AddProperty('DocNumber', edtNumber);
    FMediator.AddProperty('DocDate', edtDate);
    FMediator.AddProperty('Person.Name', edtMember);
    FMediator.AddProperty('Service', cmbService).ValueList := gLedgerManager.ServiceBasicList;
    FMediator.AddProperty('Amount', edtAmount);

  end;
  FMediator.Subject := FData;
  FMediator.Active:= True;
end;

procedure TfrmPaymentEdit.SetupUI;
begin
  edtMember.Color:= clInfoBk;
  UpdateComboBoxes;
end;

procedure TfrmPaymentEdit.UpdateComboBoxes;
var
  i: Integer;
begin
  //todo: the combox is till messed up every time cancel
  if cmbService.Text <> Data.Service.Name then
  begin
    i := cmbService.Items.IndexOf(Data.Service.Name);
    cmbService.ItemIndex:= i;
  end;
end;
end.

