unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IDEWindowIntf, Forms, Controls, Graphics,
  Dialogs, Menus, ActnList, StdActns, ComCtrls, Grids, ExtCtrls, Buttons,
  StdCtrls, EditBtn, DBGrids, ledger_bom, tiModelMediator, tiListMediators,
  tiMediators, tiOIDInteger, tiObject, SQLWhereBuilderNV, DisplayHelpers,
  BufDataset, db, dbf, LR_Class, LR_DSet, LR_DBSet, LR_E_CSV, LR_PGrid,
  LR_ChBox, lr_CrossTab, LR_Desgn
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
    actFilterLoans: TAction;
    actClearLoanMember: TAction;
    actClearLoanDate1: TAction;
    actClearLoanDate2: TAction;
    actClearMember: TAction;
    actCSVLoadLoan: TAction;
    actClearLoanType: TAction;
    actClearPaymentDate1: TAction;
    actClearPaymentDate2: TAction;
    actClearLedgerDate1: TAction;
    actClearLedgerDate2: TAction;
    actDesignPayments: TAction;
    actPrintPayments: TAction;
    actPrintLoans: TAction;
    actPrintLedger: TAction;
    actShowLedger: TAction;
    actSelectMember: TAction;
    actMembers: TAction;
    ActionList1: TActionList;
    actFileEXit: TFileExit;
    btnAddPayment: TButton;
    btnAddPayment1: TButton;
    btnApplyPaymentFilter: TButton;
    btnApplyLoanFilter: TButton;
    btnShowLedger: TButton;
    btnDeletePayment: TButton;
    btnEditPayment: TButton;
    BufDataset1: TBufDataset;
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
    cmbLoanLoanTypes: TComboBox;
    cmbLedgerService: TComboBox;
    DataSource1: TDataSource;
    dteLoans1: TDateEdit;
    dteLoans2: TDateEdit;
    dtePaymentDate1: TDateEdit;
    dtePaymentDate2: TDateEdit;
    dteLedgerDate1: TDateEdit;
    dteLedgerDate2: TDateEdit;
    edtLedgerName: TEdit;
    edtFilterMember: TLabeledEdit;
    edtLoanMember: TLabeledEdit;
    edtFilterPayments: TLabeledEdit;
    edtFilterPaymentsORNumber: TLabeledEdit;
    frCSVExport1: TfrCSVExport;
    frDBDataSet1: TfrDBDataSet;
    frReport1: TfrReport;
    frUserDatasetLoans: TfrUserDataset;
    frUserDatasetLedger: TfrUserDataset;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
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
    sgdLedger: TStringGrid;
    sgdPayments: TStringGrid;
    spbPrintLedger: TSpeedButton;
    spbClearLoanDate1: TSpeedButton;
    spbClearLoanDate2: TSpeedButton;
    spbPrintPayments: TSpeedButton;
    spbPrintLoans: TSpeedButton;
    spbClearPMTDate1: TSpeedButton;
    spbClearPMTDate2: TSpeedButton;
    spbClearLoanType: TSpeedButton;
    spbClearPaymentsFilter: TSpeedButton;
    spbClearMembers: TSpeedButton;
    spbClearLoanFilter: TSpeedButton;
    spbClearPaymentsORNoFilter: TSpeedButton;
    spbClearPaymentsFilterService: TSpeedButton;
    spbClearLedgerDate1: TSpeedButton;
    spbClearLedgerDate2: TSpeedButton;
    spbPrintPayments1: TSpeedButton;
    StatusBar1: TStatusBar;
    sgdPersons: TStringGrid;
    sgdServices: TStringGrid;
    tabPersons: TTabSheet;
    tabServices: TTabSheet;
    tabLoans: TTabSheet;
    tabPayments: TTabSheet;
    tabLedger: TTabSheet;
    procedure actAddLoanExecute(Sender: TObject);
    procedure actAddMemberExecute(Sender: TObject);
    procedure actAddPaymentExecute(Sender: TObject);
    procedure actAddServiceExecute(Sender: TObject);
    procedure actClearLedgerDate1Execute(Sender: TObject);
    procedure actClearLedgerDate2Execute(Sender: TObject);
    procedure actClearLoanDate1Execute(Sender: TObject);
    procedure actClearLoanMemberExecute(Sender: TObject);
    procedure actClearLoanTypeExecute(Sender: TObject);
    procedure actClearMemberExecute(Sender: TObject);
    procedure actClearORNoExecute(Sender: TObject);
    procedure ActClearPayeeExecute(Sender: TObject);
    procedure actClearPaymentDate1Execute(Sender: TObject);
    procedure actClearPaymentDate2Execute(Sender: TObject);
    procedure actClearServiceExecute(Sender: TObject);
    procedure actCSVLoadLoanExecute(Sender: TObject);
    procedure actCSVLoadPaymentExecute(Sender: TObject);
    procedure actDeleteLoanExecute(Sender: TObject);
    procedure actDeleteMemberExecute(Sender: TObject);
    procedure actDeletePaymentExecute(Sender: TObject);
    procedure actDeleteServiceExecute(Sender: TObject);
    procedure actDesignPaymentsExecute(Sender: TObject);
    procedure actEditLoanExecute(Sender: TObject);
    procedure actEditLoanUpdate(Sender: TObject);
    procedure actEditMemberExecute(Sender: TObject);
    procedure actEditPaymentExecute(Sender: TObject);
    procedure actEditServiceExecute(Sender: TObject);
    procedure actFilterLoansExecute(Sender: TObject);
    procedure actFilterPaymentsExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actCSVLoadMemberExecute(Sender: TObject);
    procedure actClearLoanDate2Execute(Sender: TObject);
    procedure actPrintLoansExecute(Sender: TObject);
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure actPrintLedgerExecute(Sender: TObject);
    procedure actPrintPaymentsExecute(Sender: TObject);
    procedure actSelectMemberExecute(Sender: TObject);
    procedure actShowLedgerExecute(Sender: TObject);
    procedure dteLedgerDate1ButtonClick(Sender: TObject);
    procedure dteLoans1ButtonClick(Sender: TObject);
    procedure dtePaymentDate1ButtonClick(Sender: TObject);
    procedure edtFilterMemberKeyPress(Sender: TObject; var Key: char);
    procedure edtLedgerNameKeyPress(Sender: TObject; var Key: char);
    procedure edtLoanMemberKeyPress(Sender: TObject; var Key: char);
    procedure edtFilterPaymentsKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure frReport1ExportFilterSetup(Sender: TfrExportFilter);
    procedure frReport1GetValue(const ParName: String; var ParValue: Variant);
    procedure frReport1GetValueForLoans(const ParName: String; var ParValue: Variant);
    procedure frUserDatasetLedgerCheckEOF(Sender: TObject; var Eof: Boolean);
    procedure frUserDatasetLedgerFirst(Sender: TObject);
    procedure frUserDatasetLedgerNext(Sender: TObject);
    procedure frUserDatasetLoansCheckEOF(Sender: TObject; var Eof: Boolean);
    procedure frUserDatasetLoansFirst(Sender: TObject);
    procedure frUserDatasetLoansNext(Sender: TObject);
    procedure sgdLoansDblClick(Sender: TObject);
    procedure sgdPaymentsDblClick(Sender: TObject);
    procedure sgdPersonsDblClick(Sender: TObject);
    procedure sgdServicesDblClick(Sender: TObject);
    procedure spbClearLoanFilterClick(Sender: TObject);
  private
    PreviewFlagged: boolean;
    LoanPointer: integer;
    LedgerPointer: integer;
    LedgerDebitTotal: Double;
    LedgerCreditTotal: Double;
    KeyBuffer: string;
    FLedgerDisplay: TLedgerDisplay;
    FLedgerPerson: TPersonBasic;
    FLoanDisplayList: TLoanDisplayList;
    FPaymentDisplayList: TPaymentDisplayList;
    FPersonsMediator: TtiModelMediator;
    FMedServices: TtiModelMediator;
    FMedLoans: TtiModelMediator;
    FMedPayments: TtiModelMediator;
    FMedLedger: TtiModelMediator;
    FPersons: TPersonList;
    FServiceDisplayList: TServiceDisplayList;
    FServices: TServiceList;
    SQLWhereBuilderLoans : TSQLWhereBuilder;
    SQLWhereBuilderPayments: TSQLWhereBuilder;
    SQLWhereBuilderMembers: TSQLWhereBuilder;
    SQLWhereBuilderLedger: TSQLWhereBuilder;
    procedure FilterPayments;
    procedure FilterLoans;
    procedure SaveAdjustmentList(var O: TLoan);
    procedure SetPersons(AValue: TPersonList);
    procedure SetServices(AValue: TServiceList);
    procedure SetupMediators;
    procedure SetupDatabase;
    procedure FilterPersons( AText: string );
  public
    property Persons: TPersonList read FPersons write SetPersons;
    property Services: TServiceList read FServices write SetServices;
    property LedgerPerson: TPersonBasic read FLedgerPerson write FLedgerPerson;
    property PaymentDisplayList: TPaymentDisplayList read FPaymentDisplayList write FPaymentDisplayList;
    property LoanDisplayList: TLoanDisplayList read FLoanDisplayList write FLoanDisplayList;
    property ServiceDisplayList: TServiceDisplayList read FServiceDisplayList write FServiceDisplayList;
    property LedgerDisplay: TLedgerDisplay read FLedgerDisplay write FLedgerDisplay;
  end;


var
  frmMain: TfrmMain;

implementation

uses
  PersonEditForm, ServiceEditForm, LoanEditForm, ledgermanager, tiOPFManager,
  tiBaseMediator, MemberCSVLoad, PaymentEditForm, ResourceDM, PaymentCSVLoad,
  ObjectUtils, PeriodSelectForm, LoanCSVLoad, LookupForm, ListToBufDatasetU,
  MyUtils;

const
  cSQLFilterMembers = 'NAME containing ? or EMPNO starting ?';

  cSQLFilterPaymentsMember = 'p.NAME containing ?';
  cSQLFilterPaymentsORNumber = 'r.DOCNUMBER starting ?';
  cSQLFilterPaymentsService = 's.NAME = ?';

  cSQLFilterLoansMember = 'p.NAME containing ?';
  cSQLFilterLoansLoanType = 's.NAME = ?';
  cSQLFilterLoansDateFrom = 'r.DOCDATE >= ?';
  cSQLFilterLoansDateTo = 'r.DOCDATE <= ?';

  cSQLFilterLedgerMember = 'p.NAME = ?';
  cSQLFilterLedgerService = 's.NAME = ?';

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

procedure TfrmMain.actClearLoanDate2Execute(Sender: TObject);
begin
  dteLoans2.Clear;
  dteLoans2.SetFocus;
end;

procedure TfrmMain.actPrintLoansExecute(Sender: TObject);
const
  fields = 'DocDate;DocNumber:20;Member:60;ServiceName:20;Principal;Interest;Total;PreviousBalance;Rebates;Adjustments;NetProceeds';
var
  lDataset: TBufDataset;
  lfrReport: TfrReport;
  lfrDBDataSet: TfrDBDataSet;
begin

  lDataset := nil;
  lfrDBDataSet := nil;
  lfrReport := nil;

  lDataset := TBufDataset.Create(Self);
  lfrDBDataSet := TfrDBDataSet.Create(Self);
  lfrReport := TfrReport.Create(Self);
  try
    lDataset.IndexFieldNames:= 'ServiceName;DocDate;DocNumber';
    ListToBufDataset(gLedgerManager.Loans,lDataset,fields);
    lfrDBDataSet.DataSet := lDataset;

    frVariables['Period'] := BeautifyDatePeriod(dteLoans1.Date, dteLoans2.Date);
    lfrReport.Clear;
    lfrReport.Dataset := lfrDBDataSet;
    lfrReport.OnExportFilterSetup:= @frReport1ExportFilterSetup;
    lfrReport.LoadFromFile('reports\Loans.lrf');
    lfrReport.ShowReport;
  finally
    lDataset.Free;
    lfrDBDataSet.Free;
    lfrReport.Free;
  end;

end;

procedure TfrmMain.ActionList1Update(AAction: TBasicAction; var Handled: Boolean
  );
begin
  if AAction = ActClearPayee then
    ActClearPayee.Enabled:= edtFilterPayments.Text <> ''
  else if AAction = actClearORNo then
    actClearORNo.Enabled:= edtFilterPaymentsORNumber.Text <> ''
  else if AAction = actClearService then
    actClearService.Enabled:= cmbPaymentsFilterService.Text <> ''
  else if AAction = actClearPaymentDate1 then
    actClearPaymentDate1.Enabled:= dtePaymentDate1.Text <> ''
  else if AAction = actClearPaymentDate2 then
    actClearPaymentDate2.Enabled:= dtePaymentDate2.Text <> ''
  else if AAction = actPrintPayments then
    actPrintPayments.Enabled:= gLedgerManager.PaymentList.Count > 0

  else if AAction = actClearLoanMember then
    actClearLoanMember.Enabled:= edtLoanMember.Text <> ''
  else if AAction = actClearLoanDate1 then
    actClearLoanDate1.Enabled:= dteLoans1.Text <> ''
  else if AAction = actClearLoanDate2 then
    actClearLoanDate2.Enabled:= dteLoans2.Text <> ''
  else if AAction = actClearLoanType then
    actClearLoanType.Enabled:= cmbLoanLoanTypes.Text <> ''
  else if AAction = actPrintLoans then
    actPrintLoans.Enabled:= gLedgerManager.Loans.Count > 0

  else if AAction = actClearLedgerDate1 then
    actClearLedgerDate1.Enabled:= dteLedgerDate1.Text <> ''
  else if AAction = actClearLedgerDate2 then
    actClearLedgerDate2.Enabled:= dteLedgerDate2.Text <> ''

  else if AAction = actClearMember then
    actClearMember.Enabled:= edtFilterMember.Text <> ''

  else if AAction = actShowLedger then
    actShowLedger.Enabled:= (edtLedgerName.Text <> '') and (cmbLedgerService.Text<>'')
  else if AAction = actPrintLedger then
    actPrintLedger.Enabled:= gLedgerManager.Ledger.Count > 0

  ;
end;

procedure TfrmMain.actPrintLedgerExecute(Sender: TObject);
begin
  with frReport1 do
  begin
    Clear;
    Dataset := frUserDatasetLedger;
    OnGetValue:= @frReport1GetValue;
    LoadFromFile('reports\Ledger.lrf');
    ShowReport;
  end;
end;

procedure TfrmMain.actPrintPaymentsExecute(Sender: TObject);
const
  _columns = 'PersonName:40';
  _valueColumns = 'Amount';
  waRound1Flag : Boolean = true;

var
  bufdataset : TBufDataset;
  crossFields_ : TStringList;
  lCrossFields , lPersonName, fldnm: string;
  i: Integer;
  frReport: TfrReport;
  names_ : TStringList;
  oPayment: TPayment;
  amount, totalAmt: Double;
  m, mpn: TfrMemoView;
begin
  frReport := TfrReport.Create(self);
  with frReport do
  try
    crossFields_ := TStringList.Create;
    crossFields_.Sorted:= True;
    crossFields_.Duplicates:= dupIgnore;
    crossFields_.Delimiter:= ';';

    //extract crosstab fields
    for i := 0 to gLedgerManager.PaymentList.Count -1 do
      crossFields_.Add(gLedgerManager.PaymentList.Items[i].Service.Name);
    crossFields_.Add('Total');
    lCrossFields := crossFields_.DelimitedText;

    bufdataset := TBufDataset.Create( Self );
    ListToBufDatasetCrossTab(gLedgerManager.PaymentList, bufdataset, _columns, lCrossFields, _valueColumns);
    bufdataset.IndexFieldNames:= 'PersonName';

    names_ := TStringList.Create;
    names_.Sorted:= True;
    names_.Duplicates:= dupIgnore;
    // names to bufdataset
    for i := 0 to Pred(gLedgerManager.PaymentList.Count) do
      names_.Add( gLedgerManager.PaymentList.Items[i].PersonName );
    for i := 0 to Pred(names_.Count) do
    begin
      bufdataset.Insert;
      bufdataset.FieldByName('PersonName').AsString:= names_.Strings[i];
      bufdataset.Post;
    end;
    //amounts to bufdataset
    for i := 0 to Pred(gLedgerManager.PaymentList.Count) do
    begin
      oPayment := gLedgerManager.PaymentList.Items[i];
      lPersonName := oPayment.PersonName;
      bufdataset.Locate('PersonName',lPersonName,[]);
      fldnm:= oPayment.ServiceName;
      try
        amount := bufdataset.FieldByName(fldnm).AsFloat;
      except
        amount := 0
      end;
      try
        totalAmt:= bufdataset.FieldByName('Total').AsFloat;
      except
        totalAmt:= 0;
      end;
      amount := amount + oPayment.Amount;
      totalAmt:= totalAmt + oPayment.Amount;
      bufdataset.Edit;
      bufdataset.FieldByName(fldnm).AsFloat := amount;
      bufdataset.FieldByName('Total').AsFloat:= totalAmt;
      bufdataset.Post;
    end;
    try
      frReport.Clear;
      frReport.Dataset := frDBDataSet1;
      frDBDataSet1.DataSet := bufdataset;
      frReport.OnExportFilterSetup:= @frReport1ExportFilterSetup;
      frReport.LoadFromFile('reports\Payments.lrf');
      //todo: error when repeating Payments report design
      //build crosstab titles
      if waRound1Flag then // bad workaround
    begin
      mpn := frReport.Pages.Pages[0].FindObject('MemoPersonTitle') as TfrMemoView;
      for i := 0 to Pred(crossFields_.Count) do
      begin
        m := TfrMemoView.Create(frReport.Pages[0]);
        m.CreateUniqueName;
        m.SetBounds(Trunc(mpn.Left+mpn.Width)+1,Trunc(mpn.Top),60,Trunc(mpn.Height));
        m.Memo.Text:= crossFields_.Strings[i];
        m.Alignment:= taRightJustify;
        m.Font.Style:= m.Font.Style + [fsBold];
        mpn := m;
      end;

      //build crosstab columns
      mpn := frReport.Pages.Pages[0].FindObject('MemoPersonName') as TfrMemoView;
      for i := 0 to Pred(crossFields_.Count) do
      begin
        m := TfrMemoView.Create(frReport.Pages[0]);
        m.CreateUniqueName;
        m.SetBounds(Trunc(mpn.Left+mpn.Width)+1,Trunc(mpn.Top),60,Trunc(mpn.Height));
        m.Memo.Text:= '['+ crossFields_.Strings[i] + ']';
        m.Format:= 16974382;
        m.Alignment:= taRightJustify;
        m.HideZeroValues:= true;
        mpn := m;
      end;

      //build crosstab footers
      mpn := frReport.Pages.Pages[0].FindObject('MemoPersonFooter') as TfrMemoView;
      for i := 0 to Pred(crossFields_.Count) do
      begin
        m := TfrMemoView.Create(frReport.Pages[0]);
        m.CreateUniqueName;
        m.SetBounds(Trunc(mpn.Left+mpn.Width)+1,Trunc(mpn.Top),60,Trunc(mpn.Height));
        m.Memo.Text:= '[SUM("'+ crossFields_.Strings[i] + '",MasterData1)]';
        m.Format:= 16974382;
        m.Alignment:= taRightJustify;
        m.Font.Style:= m.Font.Style + [fsBold];
        mpn := m;
      end;
   end;

      if PreviewFlagged then
        frReport.DesignReport
      else
        frReport.ShowReport;
    finally
      bufdataset.Free;
    end;
    waRound1Flag := false;
  finally
    names_.free;
    crossFields_.Free;
    frReport.Free;
  end;
end;

procedure TfrmMain.actSelectMemberExecute(Sender: TObject);
  var
    temp: TPersonBasic;
  begin
    temp := TPersonBasic.Create;
    try
      gLedgerManager.LoadPersonsLookup;
      SelectObject( TClassOfObject(temp),gLedgerManager.PersonsLookup,'NAME containing ?','Name', KeyBuffer );
      if temp <> nil then
      begin
        if not Assigned(FLedgerPerson) then
          FLedgerPerson := TPersonBasic.Create;
        FLedgerPerson.Assign(temp);
        edtLedgerName.Text := FLedgerPerson.Name;
      end;
    finally
      temp := nil;
      temp.Free;
    end;
  end;

procedure TfrmMain.actShowLedgerExecute(Sender: TObject);
begin
  gLedgerManager.Ledger.BeginUpdate;

  SQLWhereBuilderLedger.UpdateWhereClauses;

  gLedgerManager.Ledger.ListFilter.Criteria:=
    SQLWhereBuilderLedger.WhereList.Text;
  gLedgerManager.Ledger.ListFilter.Active:= (
    SQLWhereBuilderLedger.WhereList.Text<>'');
  gLedgerManager.LoadLedger;

  gLedgerManager.Ledger.EndUpdate;
end;



procedure TfrmMain.dteLedgerDate1ButtonClick(Sender: TObject);
var
  d1, d2, d3: TDateTime;
  start : TDateTime;
  curEditor: TDateEdit;
begin
  d1 := 0;
  d2 := 0;
  d3 := 0;
  curEditor :=  (sender as TDateEdit);
  start := curEditor.Date;
  SelectPeriod(d1, d2, d3, start);
  if d1 <> 0 then curEditor.Date:= d1
  else if d2 <> 0 then
    begin
      dteLedgerDate1.Date:= d2;
      dteLedgerDate2.Date:= d3;
    end;

  abort;
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

procedure TfrmMain.actFilterLoansExecute(Sender: TObject);
begin
  FilterLoans;
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
    //FPersonsMediator.SelectedObject[sgdPersons] := P;  // go to last inserted
    sgdPersons.Row:= sgdPersons.RowCount-1;
    sgdPersons.TopRow:= sgdPersons.RowCount-1;
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

    //FMedPayments.SelectedObject[sgdPayments] := O;  // go to last inserted

    sgdPayments.Row:= sgdPayments.RowCount-1;
    sgdPayments.TopRow:= sgdPayments.RowCount-1;
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
    //FMedLoans.SelectedObject[sgdLoans] := O;  // go to last inserted

    sgdLoans.Row:= sgdLoans.RowCount -1;
    sgdLoans.TopRow:= sgdLoans.RowCount -1;

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
    //FMedServices.SelectedObject[sgdServices] := O;  // go to last inserted
    sgdServices.Row := sgdServices.RowCount -1;
    sgdServices.TopRow:= sgdServices.RowCount -1;
  end
  else
    O.Free;
end;

procedure TfrmMain.actClearLedgerDate1Execute(Sender: TObject);
begin
  dteLedgerDate1.Clear; dteLedgerDate1.SetFocus;
end;

procedure TfrmMain.actClearLedgerDate2Execute(Sender: TObject);
begin
  dteLedgerDate2.Clear; dteLedgerDate2.SetFocus;
end;

procedure TfrmMain.actClearLoanDate1Execute(Sender: TObject);
begin
  dteLoans1.Clear;
  dteLoans1.SetFocus;
end;

procedure TfrmMain.actClearLoanMemberExecute(Sender: TObject);
begin
  edtLoanMember.Clear;
  edtLoanMember.SetFocus;
end;

procedure TfrmMain.actClearLoanTypeExecute(Sender: TObject);
begin
  cmbLoanLoanTypes.ItemIndex:= -1;
  cmbLoanLoanTypes.SetFocus;
end;

procedure TfrmMain.actClearMemberExecute(Sender: TObject);
begin
  edtFilterMember.SetFocus;
  edtFilterMember.Text:= '';
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

procedure TfrmMain.actClearPaymentDate1Execute(Sender: TObject);
begin
  dtePaymentDate1.Clear; dtePaymentDate1.SetFocus;;
end;

procedure TfrmMain.actClearPaymentDate2Execute(Sender: TObject);
begin
  dtePaymentDate2.Clear; dtePaymentDate2.SetFocus;
end;

procedure TfrmMain.actClearServiceExecute(Sender: TObject);
begin
  cmbPaymentsFilterService.SetFocus;
  cmbPaymentsFilterService.ItemIndex := -1;
end;

procedure TfrmMain.actCSVLoadLoanExecute(Sender: TObject);
begin
with TOpenDialog.Create(Self) do
  try
    Filter:= 'CSV|*.CSV';
    if Execute then
      ShowLoanCSVLoad(FileName);
  finally
    Free;
  end;
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

procedure TfrmMain.actDesignPaymentsExecute(Sender: TObject);
begin
  PreviewFlagged:= true;
  actPrintPayments.Execute;
  PreviewFlagged:= false;
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


procedure TfrmMain.dteLoans1ButtonClick(Sender: TObject);
var
  d1, d2, d3: TDateTime;
  start : TDateTime;
  curEditor: TDateEdit;
begin
  d1 := 0;
  d2 := 0;
  d3 := 0;
  curEditor :=  (sender as TDateEdit);
  start := curEditor.Date;
  SelectPeriod(d1, d2, d3, start);
  if d1 <> 0 then curEditor.Date:= d1
  else if d2 <> 0 then
    begin
      dteLoans1.Date:= d2;
      dteLoans2.Date:= d3;
    end;

  abort;
end;

procedure TfrmMain.dtePaymentDate1ButtonClick(Sender: TObject);
var
  d1, d2, d3: TDateTime;
  start : TDateTime;
  curEditor: TDateEdit;
begin
  d1 := 0;
  d2 := 0;
  d3 := 0;
  curEditor :=  (sender as TDateEdit);
  start := curEditor.Date;
  SelectPeriod(d1, d2, d3, start);
  if d1 <> 0 then curEditor.Date:= d1
  else if d2 <> 0 then
    begin
      dtePaymentDate1.Date:= d2;
      dtePaymentDate2.Date:= d3;
    end;

  abort;
end;


procedure TfrmMain.edtFilterMemberKeyPress(Sender: TObject; var Key: char);
begin
  if key = chr(13) then
  begin
    FPersons.BeginUpdate;

    FilterPersons( Trim(TEdit(Sender).Text) );

    FPersons.EndUpdate;
  end;
end;

procedure TfrmMain.edtLedgerNameKeyPress(Sender: TObject; var Key: char);
begin
  if key in ['A'..'Z','a'..'z'] then
  begin
    KeyBuffer:= Key;
    Key := #0;
    actSelectMember.Execute;
  end;
  KeyBuffer:= '';
end;

procedure TfrmMain.edtLoanMemberKeyPress(Sender: TObject; var Key: char);
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

  FLedgerDisplay := TLedgerDisplay.CreateCustom(gLedgerManager.Ledger);

  SetupMediators;

  PageControl1.ActivePage := tabPersons;
  sgdServices.Columns[4].Width:= 100;
  sgdPersons.Columns[2].Width:= 100;

  for i := 0 to gLedgerManager.Services.Count -1 do
  begin
    if gLedgerManager.Services.Items[i].ServiceType = 'LOAN' then
      cmbLoanLoanTypes.Items.Add(gLedgerManager.Services.items[i].Name);
    cmbPaymentsFilterService.items.Add(gLedgerManager.Services.Items[i].Name);
    cmbLedgerService.Items.add(gLedgerManager.Services.Items[i].Name);
  end;

  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,spbClearMembers.Glyph);

  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,spbClearLoanFilter.Glyph);
  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,spbClearLoanType.Glyph);
  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,spbClearLoanDate1.Glyph);
  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,spbClearLoanDate2.Glyph);
  dmResources.imlButtonGlyphs.GetBitmap(iindBtnPrint,spbPrintLoans.Glyph);

  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,spbClearPaymentsFilter.Glyph);
  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,spbClearPaymentsORNoFilter.Glyph);
  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,spbClearPaymentsFilterService.Glyph);
  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,spbClearPMTDate1.Glyph);
  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,spbClearPMTDate2.Glyph);
  dmResources.imlButtonGlyphs.GetBitmap(iindBtnPrint,spbPrintPayments.Glyph);

  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,spbClearLedgerDate1.Glyph);
  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,spbClearLedgerDate2.Glyph);
  dmResources.imlButtonGlyphs.GetBitmap(iindBtnPrint,spbPrintLedger.Glyph);

  actEditLoan.OnUpdate:= @actEditLoanUpdate;
  actDeleteLoan.OnUpdate:= @actEditLoanUpdate;

  //Members Filter
  SQLWhereBuilderMembers := TSQLWhereBuilder.Create(Self);
  SQLWhereBuilderMembers.AddWhereClauseAnd(cSQLFilterMembers,
    [edtFilterMember,'Text',edtFilterMember,'Text']);

  //Loans Filter
  SQLWhereBuilderLoans := TSQLWhereBuilder.Create(Self);
  SQLWhereBuilderLoans.AddWhereClauseAnd(cSQLFilterLoansMember,[edtLoanMember,'Text']);
  SQLWhereBuilderLoans.AddWhereClauseAnd(cSQLFilterLoansLoanType,[cmbLoanLoanTypes,'Text']);
  SQLWhereBuilderLoans.AddWhereClauseAnd(cSQLFilterLoansDateFrom,[dteLoans1,'Text']);
  SQLWhereBuilderLoans.AddWhereClauseAnd(cSQLFilterLoansDateTo,[dteLoans2,'Text']);


  //Payments filter
  SQLWhereBuilderPayments := TSQLWhereBuilder.Create(Self);
  SQLWhereBuilderPayments.AddWhereClauseAnd( cSQLFilterPaymentsMember,
    [edtFilterPayments, 'Text']);
  SQLWhereBuilderPayments.AddWhereClauseAnd( cSQLFilterPaymentsORNumber,
    [edtFilterPaymentsORNumber, 'Text']);
  SQLWhereBuilderPayments.AddWhereClauseAnd( cSQLFilterPaymentsService,
    [cmbPaymentsFilterService, 'Text']);

  //Ledger filter
  SQLWhereBuilderLedger := TSQLWhereBuilder.create(Self);
  SQLWhereBuilderLedger.AddWhereClauseAnd( cSQLFilterLedgerMember,
    [edtLedgerName, 'Text']);
  SQLWhereBuilderLedger.AddWhereClauseAnd( cSQLFilterLedgerService,
    [cmbLedgerService, 'Text']);

end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FLedgerPerson);
end;


procedure TfrmMain.frReport1ExportFilterSetup(Sender: TfrExportFilter);
begin
  sender.BandTypes:= [btGroupHeader,btMasterHeader,btMasterData,btMasterFooter,btGroupFooter];
end;

procedure TfrmMain.frReport1GetValue(const ParName: String;
  var ParValue: Variant);
var
  o: TLedgerItemDisplay;
begin
  if (LedgerPointer <= (LedgerDisplay.Count-1)) then
    o := TLedgerItemDisplay( LedgerDisplay.Items[LedgerPointer] );

  if (ParName = 'Member') or (ParName = 'Service') then
  begin
     if ParName = 'Member' then
      ParValue:= LedgerPerson.Name
    else if ParName = 'Service' then
      ParValue := cmbLedgerService.Text
  end
  else begin
    if ParName = 'TransDate' then
      ParValue:= o.TransDate
    else if ParName = 'Reference' then
      ParValue:= o.Reference
    else if ParName = 'Debit' then
      ParValue:= o.Charges
    else if ParName = 'Credit' then
      ParValue:= o.Payments
    else if ParName = 'Balance' then
      ParValue:= o.Balance
    else if ParName = 'DebitTotal' then
      ParValue:= FormatFloat('#,0.00;(#,0.00); ', LedgerDebitTotal)
    else if ParName = 'CreditTotal' then
      ParValue:= FormatFloat('#,0.00;(#,0.00); ', LedgerCreditTotal)
  end

    ;
end;

procedure TfrmMain.frReport1GetValueForLoans(const ParName: String;
  var ParValue: Variant);
var
  o: TLoanDisplay;
begin
  if (LoanPointer <= (LoanDisplayList.Count-1)) then
    o := TLoanDisplay( LoanDisplayList.Items[LoanPointer] );

  begin
    if ParName = 'Period' then
      ParValue:= datetostr(date)
    else if ParName = 'DocDate' then
      ParValue:= o.DocDate
    else if ParName = 'DocNumber' then
      ParValue := o.DocNumber
    else if ParName = 'Member' then
      ParValue:= o.Person
    else if ParName = 'Service' then
      ParValue:= o.Service
    else if ParName = 'Principal' then
      ParValue:= o.Loan.Principal
    else if ParName = 'Interest' then
      ParValue:= o.Loan.Interest
    else if ParName = 'Total' then
      ParValue:= o.Loan.Total
    else if ParName = 'PreviousBalance' then
      ParValue:= o.Loan.PreviousBalance
    else if ParName = 'Rebates' then
      ParValue:= o.Loan.Rebates
    else if ParName = 'Adjustments' then
      ParValue:= o.Loan.Adjustments
    else if ParName = 'NetProceeds' then
      ParValue:= o.Loan.NetProceeds
    else if ParName = 'foPrincipal' then
      ParValue:= 'foPrincipal'
    else if ParName = 'foInterest' then
      ParValue:= 'foInterest'
    else if ParName = 'foTotal' then
      ParValue:= 'foTotal'
    else if ParName = 'foPreviousBalance' then
      ParValue:= 'foPreviousBalance'
    else if ParName = 'foRebates' then
      ParValue:= 'foRebates'
    else if ParName = 'foAdjustments' then
      ParValue:= 'foAdjustments'
    else if ParName = 'foNetProceeds' then
      ParValue:= 'foNetProceeds'
  end

    ;
end;

procedure TfrmMain.frUserDatasetLedgerCheckEOF(Sender: TObject; var Eof: Boolean
  );
begin
  Eof := (LedgerPointer > (LedgerDisplay.Count-1));
end;

procedure TfrmMain.frUserDatasetLedgerFirst(Sender: TObject);
begin
  LedgerPointer:= 0;
  LedgerDebitTotal:= TLedgerItemDisplay( LedgerDisplay.items[0] ).PersonLedgerItem.Charges;
  LedgerCreditTotal:= TLedgerItemDisplay( LedgerDisplay.items[0] ).PersonLedgerItem.Payments;
end;

procedure TfrmMain.frUserDatasetLedgerNext(Sender: TObject);
begin
  Inc(LedgerPointer);
  if (LedgerPointer <= (LedgerDisplay.Count-1)) then
  begin
    LedgerDebitTotal:= LedgerDebitTotal + TLedgerItemDisplay( LedgerDisplay.items[LedgerPointer] ).PersonLedgerItem.Charges;
    LedgerCreditTotal:= LedgerCreditTotal + TLedgerItemDisplay( LedgerDisplay.items[LedgerPointer] ).PersonLedgerItem.Payments;
  end;
end;

procedure TfrmMain.frUserDatasetLoansCheckEOF(Sender: TObject; var Eof: Boolean
  );
begin
  Eof := (LoanPointer > (LoanDisplayList.Count-1));
end;

procedure TfrmMain.frUserDatasetLoansFirst(Sender: TObject);
begin
  LoanPointer:= 0;
  //LedgerDebitTotal:= TLedgerItemDisplay( LedgerDisplay.items[0] ).PersonLedgerItem.Charges;
  //LedgerCreditTotal:= TLedgerItemDisplay( LedgerDisplay.items[0] ).PersonLedgerItem.Payments;
end;

procedure TfrmMain.frUserDatasetLoansNext(Sender: TObject);
begin
  Inc(LoanPointer);
  if (LoanPointer <= (LoanDisplayList.Count-1)) then
  begin
    //LedgerDebitTotal:= LedgerDebitTotal + TLedgerItemDisplay( LedgerDisplay.items[LedgerPointer] ).PersonLedgerItem.Charges;
    //LedgerCreditTotal:= LedgerCreditTotal + TLedgerItemDisplay( LedgerDisplay.items[LedgerPointer] ).PersonLedgerItem.Payments;
  end;
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
  edtLoanMember.SetFocus;
  edtLoanMember.Text:= '';
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

procedure TfrmMain.FilterLoans;
begin
  gLedgerManager.Loans.BeginUpdate;

  SQLWhereBuilderLoans.UpdateWhereClauses;

  gLedgerManager.Loans.ListFilter.Criteria:=
    SQLWhereBuilderLoans.WhereList.Text;
  gLedgerManager.Loans.ListFilter.Active:= (
    SQLWhereBuilderLoans.WhereList.Text<>'');
  gLedgerManager.LoadLoans;

  gLedgerManager.Loans.EndUpdate;
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
  FMedServices.Subject := ServiceDisplayList;
  FMedServices.Active:= True;

  //loans mediator
  if not assigned(FMedLoans) then
  begin
    FMedLoans := TtiModelMediator.Create(Self);
    FMedLoans.Name:= 'LoansMediator';
    FMedLoans.AddComposite('DocDate;DocNumber;Person(120,"Member");Service(120,"Loan Type");Principal(100,"Amount",>);Interest(100,"Interest",>);Total(100,"Total",>);Adjustments(100,"Adjustments",>);NetProceeds(100,"Net Proceeds",>);Amortization(100,"Amortization",>);PaymentStart(100,"Pmt Start");ID(100," ")',sgdLoans);
  end;
  FMedLoans.Subject := LoanDisplayList;
  FMedLoans.Active:= True;

  //payments mediator
  if not assigned(FMedPayments) then
  begin
    FMedPayments := TtiModelMediator.Create(Self);
    FMedPayments.AddComposite('Person(150,"Member");DocDate;DocNumber;Service;Amount(100,"Amount",>);dummy(100," ")', sgdPayments);
  end;
  FMedPayments.Subject := PaymentDisplayList;
  FMedPayments.Active:= True;

  //ledger mediator
  if not Assigned(FMedLedger) then
  begin
    FMedLedger := TtiModelMediator.create(self);
    FMedLedger.AddComposite('transdate(100,"Date");reference(100,"Reference");charges(100,"Debit",>);payments(100,"Credit",>);balance(100,"Balance",>);d(100," ")',sgdLedger);
  end;
  FMedLedger.Subject := LedgerDisplay;
  FMedLedger.Active:= True;

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

  SQLWhereBuilderMembers.UpdateWhereClauses;
  gLedgerManager.PersonList.ListFilter.Criteria:=
    SQLWhereBuilderMembers.WhereList.Text;
  gLedgerManager.PersonList.ListFilter.Active:= (
    SQLWhereBuilderMembers.WhereList.Text<>'');
  gLedgerManager.LoadPersons;

  //Persons.PersonsFilter.Active:= (AText <> '');
  //Persons.PersonsFilter.Criteria:= 'NAME containing '+QuotedStr(AText);
  //gLedgerManager.LoadPersons;
end;


end.

