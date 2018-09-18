unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, StdActns, ComCtrls, Grids, ExtCtrls, Buttons, StdCtrls
  ,ledger_bom
  ,tiModelMediator
  ,tiListMediators
  ,tiMediators
  ,tiOIDInteger
  , tiObject
  , SQLWhereBuilderNV, DisplayHelpers
  ;

type

  TManualObjectClass = class of TManualObject;

  { TfrmMain }

  TfrmMain = class(TForm)
    actHelpAbout: TAction;
    actEditMember: TAction;
    actAddMember: TAction;
    actDeleteMember: TAction;
    actAddService: TAction;
    actEditService: TAction;
    actDeleteService: TAction;
    actAddLoan: TAction;
    actEditLoan: TAction;
    actDeleteLoan: TAction;
    actAddPayment: TAction;
    actEditPayment: TAction;
    actDeletePayment: TAction;
    actCSVLoadPayment: TAction;
    actCSVLoadMember: TAction;
    actFilterPayments: TAction;
    ActClearPayee: TAction;
    actClearORNo: TAction;
    actClearService: TAction;
    actMembers: TAction;
    ActionList1: TActionList;
    actFileEXit: TFileExit;
    btnAddPayment: TButton;
    btnApplyPaymentFilter: TButton;
    btnDeletePayment: TButton;
    btnEditPayment: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    btnAddLoan: TButton;
    btnEditLoan: TButton;
    btnDeleteLoan: TButton;
    cmbPaymentsFilterService: TComboBox;
    edtFilter: TLabeledEdit;
    edtFilterLoans: TLabeledEdit;
    edtFilterPayments: TLabeledEdit;
    edtFilterPaymentsORNumber: TLabeledEdit;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PageControl1: TPageControl;
    sgdLoans: TStringGrid;
    sgdPayments: TStringGrid;
    spbClearPaymentsFilter: TSpeedButton;
    spbClearMembers: TSpeedButton;
    spbClearLoanFilter: TSpeedButton;
    spbClearPaymentsORNoFilter: TSpeedButton;
    spbClearPaymentsFilterService: TSpeedButton;
    StatusBar1: TStatusBar;
    sgdPersons: TStringGrid;
    sgdServices: TStringGrid;
    tabPersons: TTabSheet;
    tabServices: TTabSheet;
    tabLoans: TTabSheet;
    tabPayments: TTabSheet;
    procedure actAddLoanExecute(Sender: TObject);
    procedure actAddMemberExecute(Sender: TObject);
    procedure actAddPaymentExecute(Sender: TObject);
    procedure actAddServiceExecute(Sender: TObject);
    procedure actClearORNoExecute(Sender: TObject);
    procedure ActClearPayeeExecute(Sender: TObject);
    procedure actClearServiceExecute(Sender: TObject);
    procedure actCSVLoadPaymentExecute(Sender: TObject);
    procedure actDeleteLoanExecute(Sender: TObject);
    procedure actDeleteMemberExecute(Sender: TObject);
    procedure actDeletePaymentExecute(Sender: TObject);
    procedure actDeleteServiceExecute(Sender: TObject);
    procedure actEditLoanExecute(Sender: TObject);
    procedure actEditLoanUpdate(Sender: TObject);
    procedure actEditMemberExecute(Sender: TObject);
    procedure actEditPaymentExecute(Sender: TObject);
    procedure actEditServiceExecute(Sender: TObject);
    procedure actFilterPaymentsExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actCSVLoadMemberExecute(Sender: TObject);
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure actMembersExecute(Sender: TObject);
    procedure edtFilterKeyPress(Sender: TObject; var Key: char);
    procedure edtFilterLoansKeyPress(Sender: TObject; var Key: char);
    procedure edtFilterPaymentsKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure sgdLoansDblClick(Sender: TObject);
    procedure sgdPaymentsDblClick(Sender: TObject);
    procedure sgdPersonsDblClick(Sender: TObject);
    procedure sgdServicesDblClick(Sender: TObject);
    procedure spbClearLoanFilterClick(Sender: TObject);
    procedure spbClearPaymentsFilterClick(Sender: TObject);
    procedure spbClearMembersClick(Sender: TObject);
    procedure spbClearPaymentsFilterServiceClick(Sender: TObject);
    procedure spbClearPaymentsORNoFilter1Click(Sender: TObject);
    procedure spbClearPaymentsORNoFilterClick(Sender: TObject);
  private
    FLoanDisplayList: TLoanDisplayList;
    FPaymentDisplayList: TPaymentDisplayList;
    FPersonsMediator: TtiModelMediator;
    FMedServices: TtiModelMediator;
    FMedLoans: TtiModelMediator;
    FMedPayments: TtiModelMediator;
    FPersons: TPersonList;
    FServiceDisplayList: TServiceDisplayList;
    FServices: TServiceList;
    SQLWhereBuilderLoans : TSQLWhereBuilder;
    SQLWhereBuilderPayments: TSQLWhereBuilder;
    procedure FilterPayments;
    procedure SaveAdjustmentList(var O: TLoan);
    procedure SetPersons(AValue: TPersonList);
    procedure SetServices(AValue: TServiceList);
    procedure SetupMediators;
    procedure SetupDatabase;
    procedure FilterPersons( AText: string );
  public
    property Persons: TPersonList read FPersons write SetPersons;
    property Services: TServiceList read FServices write SetServices;
    property PaymentDisplayList: TPaymentDisplayList read FPaymentDisplayList write FPaymentDisplayList;
    property LoanDisplayList: TLoanDisplayList read FLoanDisplayList write FLoanDisplayList;
    property ServiceDisplayList: TServiceDisplayList read FServiceDisplayList write FServiceDisplayList;
  end;


var
  frmMain: TfrmMain;

implementation

uses
  PersonEditForm
  ,ServiceEditForm
  ,LoanEditForm
  ,ledgermanager
  ,tiOPFManager, tiBaseMediator
  ,MemberCSVLoad
  , PaymentEditForm
  , ResourceDM
  , PaymentCSVLoad
  , ObjectUtils
  ;

const
  cSQLFilterTextLoans = 'p.NAME containing ?';
  cSQLFilterPaymentsMember = 'p.NAME containing ?';
  cSQLFilterPaymentsORNumber = 'r.DOCNUMBER starting ?';
  cSQLFilterPaymentsService = 's.NAME = ?';

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.actHelpAboutExecute(Sender: TObject);
begin
  ShowMessage('NFAEA Loans Management System');
end;

procedure TfrmMain.actCSVLoadMemberExecute(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  try
    Filter:= 'CSV|*.CSV';
    if Execute then
      ShowMemberCSVLoad(FileName);
  finally
    Free;
  end;
end;

//todo: filter Loans by Date: create a Date filter form with quick access for every possible combination
procedure TfrmMain.ActionList1Update(AAction: TBasicAction; var Handled: Boolean
  );
begin
  if AAction = ActClearPayee then
    ActClearPayee.Enabled:= edtFilterPayments.Text <> ''
  else if AAction = actClearORNo then
    actClearORNo.Enabled:= edtFilterPaymentsORNumber.Text <> ''
  else if AAction = actClearService then
    actClearService.Enabled:= cmbPaymentsFilterService.Text <> '';
end;

procedure TfrmMain.actEditMemberExecute(Sender: TObject);
var
  P : TPerson;
  B : TPerson; //Buffer for undo
begin
  P := TPerson(FPersonsMediator.SelectedObject[sgdPersons]);
  B := TPerson.Create;
  B.Assign(P);
  B.Dirty := False;
  if EditPerson(B) and ObjectChanged(B.AsDebugString, P.AsDebugString) then
  begin
    P.Assign(B);
    P.SaveObject;
    P.NotifyObservers;
  end;
  B.Free;
end;

procedure TfrmMain.actEditPaymentExecute(Sender: TObject);
var
  D : TPaymentDisplay;
  M : TtiMediatorView;
  O : TPayment;
  B : TPayment; //Buffer for undo
begin
  M := FMedPayments.FindByComponent(sgdPayments).Mediator;
  D := TPaymentDisplay(TtiStringGridMediatorView(M).SelectedObject);
  O := D.Payment;

  //O := TPayment(FMedPayments.SelectedObject[sgdPayments]);
  if not assigned(O) then exit; //<==

  B := TPayment.Create;
  B.Assign(O);
  B.Dirty := False;
  if EditPayment(B) and ObjectChanged(B.AsDebugString, O.AsDebugString) then
  begin
    O.Assign(B);
    O.SaveObject;
    O.NotifyObservers;
    O.Owner.NotifyObservers;
  end;
  FreeAndNil(B);
end;

procedure TfrmMain.actEditServiceExecute(Sender: TObject);
//D : TLoanDisplay;
//M : TtiMediatorView;
//O : TLoan;
//B : TLoan; //Buffer for undo
//i: Integer;
//begin
//M := FMedLoans.FindByComponent(sgdLoans).Mediator;
//D := TLoanDisplay(TtiStringGridMediatorView(M).SelectedObject);
//O := D.Loan;
var
  O : TService;
  B : TService; //Buffer for undo
  D : TServiceDisplay;
  M : TtiMediatorView;
begin
  M := FMedServices.FindByComponent(sgdServices).Mediator;
  D := TServiceDisplay(TtiStringGridMediatorView(M).SelectedObject);
  O := D.Service;

  //O := TService(FMedServices.SelectedObject[sgdServices]);
  B := TService.Create;

  B.Assign(O);
  B.Dirty:= False;
  if EditService(B) and ObjectChanged(B.AsDebugString,O.AsDebugString) then
  begin
    O.Assign(B);
    O.SaveObject;
    O.NotifyObservers;
    O.Owner.NotifyObservers;
  end;
  B.Free;
end;

procedure TfrmMain.actFilterPaymentsExecute(Sender: TObject);
begin
  FilterPayments;
end;

procedure TfrmMain.actAddMemberExecute(Sender: TObject);
var
  P : TPerson;
begin
  //P := TPerson.CreateNew;
  P := TPerson.Create;
  if EditPerson(P) then
  begin
    GTIOPFManager.DefaultOIDGenerator.AssignNextOID(P.OID); // we generate oid only when before saving
    Persons.Add(P);
    P.SaveObject;
    FPersonsMediator.SelectedObject[sgdPersons] := P;  // go to last inserted
  end
  else
    P.Free;
end;

procedure TfrmMain.actAddPaymentExecute(Sender: TObject);
var
  O : TPayment;
begin
  O := TPayment.Create;
  if EditPayment(O) then
  begin
    GTIOPFManager.DefaultOIDGenerator.AssignNextOID(O.OID); // we generate oid only when before saving
    gLedgerManager.PaymentList.Add(O);
    O.SaveObject;
    FMedPayments.SelectedObject[sgdPayments] := O;  // go to last inserted
  end
  else
    O.Free;
end;

procedure TfrmMain.actAddLoanExecute(Sender: TObject);
var
  O : TLoan;
begin
  O := TLoan.Create;
  if EditLoan(O) then
  begin
    GTIOPFManager.DefaultOIDGenerator.AssignNextOID(O.OID); // we generate oid only when before saving
    gLedgerManager.Loans.Add(O);
    O.SaveObject;
    FMedLoans.SelectedObject[sgdLoans] := O;  // go to last inserted
    //save the AdjustmentsList, if any
    SaveAdjustmentList(O);
  end
  else
    O.Free;
end;

procedure TfrmMain.actAddServiceExecute(Sender: TObject);
var
  O : TService;
begin
  O := TService.Create;
  if EditService(O) then
  begin
    GTIOPFManager.DefaultOIDGenerator.AssignNextOID(O.OID); // we generate oid only when before saving
    Services.Add(O);
    O.SaveObject;
    FMedServices.SelectedObject[sgdServices] := O;  // go to last inserted
  end
  else
    O.Free;
end;

procedure TfrmMain.actClearORNoExecute(Sender: TObject);
begin
  edtFilterPaymentsORNumber.SetFocus;
  edtFilterPaymentsORNumber.Text:= '';
end;

procedure TfrmMain.ActClearPayeeExecute(Sender: TObject);
begin
  edtFilterPayments.SetFocus;
  edtFilterPayments.Text:= '';
end;

procedure TfrmMain.actClearServiceExecute(Sender: TObject);
begin
  cmbPaymentsFilterService.SetFocus;
  cmbPaymentsFilterService.ItemIndex := -1;
end;

procedure TfrmMain.actCSVLoadPaymentExecute(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  try
    Filter:= 'CSV|*.CSV';
    if Execute then
      ShowPaymentCSVLoad(FileName);
  finally
    Free;
  end;
end;

procedure TfrmMain.actDeleteLoanExecute(Sender: TObject);
begin
  DeleteFromList( sgdLoans, gLedgerManager.Loans, TLoan );
end;


procedure TfrmMain.actDeleteMemberExecute(Sender: TObject);
begin
  DeleteFromList(sgdPersons, Persons, TPerson);
end;

procedure TfrmMain.actDeletePaymentExecute(Sender: TObject);
begin
  DeleteFromList( sgdPayments, gLedgerManager.PaymentList, TPayment );
end;

procedure TfrmMain.actDeleteServiceExecute(Sender: TObject);
begin
  DeleteFromList( sgdServices, Services, TService );
end;

procedure TfrmMain.actEditLoanExecute(Sender: TObject);
  var
    D : TLoanDisplay;
    M : TtiMediatorView;
    O : TLoan;
    B : TLoan; //Buffer for undo
    i: Integer;
  begin
    M := FMedLoans.FindByComponent(sgdLoans).Mediator;
    D := TLoanDisplay(TtiStringGridMediatorView(M).SelectedObject);
    O := D.Loan;

    //O := TLoan(FMedLoans.SelectedObject[sgdLoans]);
    if not assigned(O) then exit; //<==

    B := TLoan.Create;
    B.Assign(O);
    B.Dirty := False;
    //this is required for the embedded list of adjustments
    for i := 0 to B.AdjustmentList.Count -1 do
      B.AdjustmentList.Items[i].Dirty:= False;
    if EditLoan(B) and ObjectChanged(B.AsDebugString,O.AsDebugString) then
    begin
      O.Assign(B);
      O.SaveObject;
      O.NotifyObservers;
      O.Owner.NotifyObservers;
      SaveAdjustmentList(O);
    end;
    FreeAndNil(B);
end;

procedure TfrmMain.actEditLoanUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := sgdLoans.Selection.Top > 0;
end;

procedure TfrmMain.actMembersExecute(Sender: TObject);
begin
  //ShowPersons(nil);
end;

procedure TfrmMain.edtFilterKeyPress(Sender: TObject; var Key: char);
begin
  if key = chr(13) then
  begin
    FPersons.BeginUpdate;

    FilterPersons( Trim(TEdit(Sender).Text) );

    FPersons.EndUpdate;
  end;
end;

procedure TfrmMain.edtFilterLoansKeyPress(Sender: TObject; var Key: char);
begin
  if key = chr(13) then
  begin
    gLedgerManager.Loans.BeginUpdate;

    SQLWhereBuilderLoans.UpdateWhereClauses;
    gLedgerManager.Loans.ListFilter.Criteria:= SQLWhereBuilderLoans.WhereList.Text;
    gLedgerManager.Loans.ListFilter.Active:= (SQLWhereBuilderLoans.WhereList.Text<>'');
    gLedgerManager.LoadLoans;

    gLedgerManager.Loans.EndUpdate;
  end;
end;

procedure TfrmMain.edtFilterPaymentsKeyPress(Sender: TObject; var Key: char);
begin
  if key = chr(13) then
  begin
    FilterPayments;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  RegisterFallBackMediators;
  RegisterFallBackListmediators;

  SetupDatabase;

  gLedgerManager.LoadPersons;
  FPersons := gLedgerManager.PersonList;

  FServiceDisplayList := TServiceDisplayList.CreateCustom(gLedgerManager.Services);
  gLedgerManager.LoadServices;
  FServices := gLedgerManager.Services;

  FLoanDisplayList := TLoanDisplayList.CreateCustom(gLedgerManager.Loans);
  gLedgerManager.LoadLoans;


  FPaymentDisplayList := TPaymentDisplayList.CreateCustom(gLedgerManager.PaymentList);
  gLedgerManager.LoadPayments;

  SetupMediators;

  PageControl1.ActivePage := tabPersons;
  sgdServices.Columns[4].Width:= 100;
  sgdPersons.Columns[2].Width:= 100;

  for i := 0 to gLedgerManager.Services.Count -1 do
    cmbPaymentsFilterService.items.Add(gLedgerManager.Services.Items[i].Name);

  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,spbClearMembers.Glyph);
  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,spbClearLoanFilter.Glyph);
  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,spbClearPaymentsFilter.Glyph);
  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,spbClearPaymentsORNoFilter.Glyph);
  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,spbClearPaymentsFilterService.Glyph);

  actEditLoan.OnUpdate:= @actEditLoanUpdate;
  actDeleteLoan.OnUpdate:= @actEditLoanUpdate;

  SQLWhereBuilderLoans := TSQLWhereBuilder.Create(Self);
  SQLWhereBuilderLoans.AddWhereClauseAnd(cSQLFilterTextLoans,[edtFilterLoans,'Text']);

  //Payments filter
  SQLWhereBuilderPayments := TSQLWhereBuilder.Create(Self);
  //SQLWhereBuilderPayments.AddWhereClauseAnd( cSQLFilterTextPayments,
  //  [edtFilterPayments,'Text',edtFilterPaymentsORNumber,'Text',cmbPaymentsFilterService,'Text']);
  SQLWhereBuilderPayments.AddWhereClauseAnd( cSQLFilterPaymentsMember,
    [edtFilterPayments, 'Text']);
  SQLWhereBuilderPayments.AddWhereClauseAnd( cSQLFilterPaymentsORNumber,
    [edtFilterPaymentsORNumber, 'Text']);
  SQLWhereBuilderPayments.AddWhereClauseAnd( cSQLFilterPaymentsService,
    [cmbPaymentsFilterService, 'Text']);
end;


procedure TfrmMain.sgdLoansDblClick(Sender: TObject);
begin
  actEditLoan.Execute;
end;

procedure TfrmMain.sgdPaymentsDblClick(Sender: TObject);
begin
  actEditPayment.Execute;
end;

procedure TfrmMain.sgdPersonsDblClick(Sender: TObject);
begin
  actEditMember.Execute;
end;

procedure TfrmMain.sgdServicesDblClick(Sender: TObject);
begin
  actEditService.Execute;
end;

procedure TfrmMain.spbClearLoanFilterClick(Sender: TObject);
begin
  edtFilterLoans.SetFocus;
  edtFilterLoans.Text:= '';
end;

procedure TfrmMain.spbClearPaymentsFilterClick(Sender: TObject);
begin

end;

procedure TfrmMain.spbClearMembersClick(Sender: TObject);
begin
  edtFilter.SetFocus;
  edtFilter.Text:= '';
end;

procedure TfrmMain.spbClearPaymentsFilterServiceClick(Sender: TObject);
begin
end;

procedure TfrmMain.spbClearPaymentsORNoFilter1Click(Sender: TObject);
begin

end;

procedure TfrmMain.spbClearPaymentsORNoFilterClick(Sender: TObject);
begin

end;


procedure TfrmMain.SetPersons(AValue: TPersonList);
begin
  if FPersons=AValue then Exit;
  FPersons:=AValue;
end;

procedure TfrmMain.SaveAdjustmentList(var O: TLoan);
var
  i: Integer;
begin
  for i := 0 to O.AdjustmentList.Count-1 do
  begin
    if O.AdjustmentList.Items[i].ObjectState = posCreate then
      GTIOPFManager.DefaultOIDGenerator.AssignNextOID(O.AdjustmentList.Items[i
        ].OID);
    O.AdjustmentList.Items[i].LoanID:= O.OID.AsString;
    O.AdjustmentList.Items[i].SaveObject;
  end;
end;

procedure TfrmMain.FilterPayments;
begin
  gLedgerManager.PaymentList.BeginUpdate;

  SQLWhereBuilderPayments.UpdateWhereClauses;
  gLedgerManager.PaymentList.ListFilter.Criteria:=
    SQLWhereBuilderPayments.WhereList.Text;
  gLedgerManager.PaymentList.ListFilter.Active:= (
    SQLWhereBuilderPayments.WhereList.Text<>'');
  gLedgerManager.LoadPayments;

  gLedgerManager.PaymentList.EndUpdate;
end;

procedure TfrmMain.SetServices(AValue: TServiceList);
begin
  if FServices=AValue then Exit;
  FServices:=AValue;
end;

procedure TfrmMain.SetupMediators;
begin
  //persons mediator
  if not assigned(FPersonsMediator) then
  begin
    FPersonsMediator := TtiModelMediator.Create(Self);
    FPersonsMediator.Name:= 'PersonsMediator';
    FPersonsMediator.AddComposite('Name(200,"Name");Number;DateJoinedAsString(100,"Date Joined");ID(100," ")',sgdPersons);
  end;
  FPersonsMediator.Subject:= FPersons;
  FPersonsMediator.Active:= True;

  //services mediator
  if not assigned(FMedServices) then
  begin
    FMedServices := TtiModelMediator.Create(Self);
    FMedServices.Name:= 'ServicesMediator';
    FMedServices.AddComposite('Name(150,"Name");ServiceTypeGUI(100,"Type");MaxAmount(100,"Max Amount",>);InterestRate(100,"Interest",>);MaxTerm(100,"Terms",>);Dummy(100," ")',sgdServices);
  end;
  //FMedServices.Subject:= FServices;
  FMedServices.Subject := ServiceDisplayList;
  FMedServices.Active:= True;

  //loans mediator
  if not assigned(FMedLoans) then
  begin
    FMedLoans := TtiModelMediator.Create(Self);
    FMedLoans.Name:= 'LoansMediator';
    FMedLoans.AddComposite('Person(120,"Member");Service(120,"Loan Type");Principal(100,"Amount",>);Interest(100,"Interest",>);Total(100,"Total",>);Adjustments(100,"Adjustments",>);Amortization(100,"Amortization",>);DocDate;DocNumber;ID(100," ")',sgdLoans);
  end;
  //FMedLoans.Subject:= gLedgerManager.Loans;
  FMedLoans.Subject := LoanDisplayList;
  FMedLoans.Active:= True;

  //payments mediator
  if not assigned(FMedPayments) then
  begin
    FMedPayments := TtiModelMediator.Create(Self);
    FMedPayments.AddComposite('Person(150,"Member");DocDate;DocNumber;Service;Amount(100,"Amount",>);dummy(100," ")', sgdPayments);
  end;
  //FMedPayments.Subject:= gLedgerManager.PaymentList;
  FMedPayments.Subject := PaymentDisplayList;
  FMedPayments.Active:= True;

end;

procedure TfrmMain.SetupDatabase;
var
  gen: TtiOIDGeneratorInteger;
begin
  GTIOPFManager.DefaultPersistenceLayerName:= 'Sqldb_IB';
  GTIOPFManager.ConnectDatabase('C:\projects\nfaea-ledger\NFAEA-LEDGER.FDB','sysdba','masterkey');
  gen := TtiOIDGeneratorInteger.CreateEx(5);
  GTIOPFManager.DefaultOIDGenerator := gen;
end;

procedure TfrmMain.FilterPersons(AText: string);
begin
  Persons.PersonsFilter.Active:= (AText <> '');
  Persons.PersonsFilter.Criteria:= 'NAME containing '+QuotedStr(AText);
  gLedgerManager.LoadPersons;
end;


end.

