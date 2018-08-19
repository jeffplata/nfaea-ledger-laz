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
  ;

type

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
    actMemberCSVLoad: TAction;
    actMembers: TAction;
    ActionList1: TActionList;
    actFileEXit: TFileExit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    btnAddLoan: TButton;
    btnEditLoan: TButton;
    btnDeleteLoan: TButton;
    edtFilter: TLabeledEdit;
    edtFilterLoans: TLabeledEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
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
    SpeedButton1: TSpeedButton;
    spbClearLoanFilter: TSpeedButton;
    StatusBar1: TStatusBar;
    sgdPersons: TStringGrid;
    sgdServices: TStringGrid;
    tabPersons: TTabSheet;
    tabServices: TTabSheet;
    tabLoans: TTabSheet;
    procedure actAddLoanExecute(Sender: TObject);
    procedure actAddMemberExecute(Sender: TObject);
    procedure actAddServiceExecute(Sender: TObject);
    procedure actDeleteMemberExecute(Sender: TObject);
    procedure actDeleteServiceExecute(Sender: TObject);
    procedure actEditLoanExecute(Sender: TObject);
    procedure actEditMemberExecute(Sender: TObject);
    procedure actEditServiceExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actMemberCSVLoadExecute(Sender: TObject);
    procedure actMembersExecute(Sender: TObject);
    procedure edtFilterKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure sgdPersonsDblClick(Sender: TObject);
    procedure sgdServicesDblClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FPersonsMediator: TtiModelMediator;
    FMedServices: TtiModelMediator;
    FMedLoans: TtiModelMediator;
    FPersons: TPersonList;
    FServices: TServiceList;
    procedure SetPersons(AValue: TPersonList);
    procedure SetServices(AValue: TServiceList);
    procedure SetupMediators;
    procedure SetupDatabase;
    procedure FilterPersons( AText: string );
  public
    property Persons: TPersonList read FPersons write SetPersons;
    property Services: TServiceList read FServices write SetServices;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  PersonEditForm
  ,ServiceEditForm
  ,LoanEditForm
  ,ledgermanager
  ,tiOPFManager
  ,MemberCSVLoad
  ;

const
  cMsgDeleteOneRecord = 'Do you want to delete the selected record?';
  cMsgDeleteRecords = '%d selected records will be deleted.'#13#10'Do you want to continue?';

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.actHelpAboutExecute(Sender: TObject);
begin
  ShowMessage('NFAEA Loans Management System');
end;

procedure TfrmMain.actMemberCSVLoadExecute(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  try
    if Execute then
      ShowMemberCSVLoad(FileName);
  finally
    Free;
  end;
end;

procedure TfrmMain.actEditMemberExecute(Sender: TObject);
var
  P : TPerson;
  B : TPerson; //Buffer for undo
begin
  P := TPerson(FPersonsMediator.SelectedObject[sgdPersons]);
  B := TPerson.Create;
  B.Assign(P);
  if EditPerson(B) then
  begin
    P.Assign(B);
    P.SaveObject;
    P.NotifyObservers;
  end;
  B.Free;
end;

procedure TfrmMain.actEditServiceExecute(Sender: TObject);
var
  O : TService;
  B : TService; //Buffer for undo
begin
  O := TService(FMedServices.SelectedObject[sgdServices]);
  B := TService.Create;
  B.Assign(O);
  if EditService(B) then
  begin
    O.Assign(B);
    O.SaveObject;
    O.NotifyObservers;
  end;
  B.Free;
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
  end
  else
    O.Free;
end;

procedure TfrmMain.actAddServiceExecute(Sender: TObject);
var
  O : TService;
begin
  //P := TPerson.CreateNew;
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


procedure TfrmMain.actDeleteMemberExecute(Sender: TObject);
var
  P: TPerson;
  i: Integer;
  s: String;
  iRows: integer;
  oldtop : integer;
begin
  oldtop := sgdPersons.Selection.Top;
  iRows := sgdPersons.Selection.bottom - sgdPersons.Selection.Top +1;
  if iRows = 1 then
    s := cMsgDeleteOneRecord
  else
    s := Format(cMsgDeleteRecords,[iRows]);

  if MessageDlg('Delete?',s ,mtConfirmation,[mbYes, mbNo],0) = mrYes then
  begin
    Persons.BeginUpdate;
    try
      for i := sgdPersons.Selection.Bottom downto sgdPersons.Selection.Top do
        begin
          P := TPerson(TPerson(Persons[i-1]));
          P.DeleteObject(Persons);
        end;
    finally
      Persons.EndUpdate;
    end;
    // position to the correct record, the first after the last deleted
    sgdPersons.Row:= oldtop;
    //if iRows > 1 then
    //  sgdPersons.Row:= sgdPersons.Row - (iRows -1);
  end;
end;

procedure TfrmMain.actDeleteServiceExecute(Sender: TObject);
var
  O: TService;
  i: Integer;
  s: String;
  iRows: integer;
  oldtop: integer;
begin
  oldtop := sgdServices.Selection.Top;
  iRows := sgdServices.Selection.bottom - sgdServices.Selection.Top +1;
  if iRows = 1 then
    s := cMsgDeleteOneRecord
  else
    s := Format(cMsgDeleteRecords,[iRows]);

  if MessageDlg('Delete?',s ,mtConfirmation,[mbYes, mbNo],0) = mrYes then
  begin
    Services.BeginUpdate;
    try
      for i := sgdServices.Selection.Bottom downto sgdServices.Selection.Top do
        begin
          O := TService(TService(Services[i-1]));
          O.DeleteObject(Services);
        end;
    finally
      Services.EndUpdate;
    end;
    // position to the correct record, the first after the last deleted
    sgdServices.Row := oldtop;
    //if iRows > 1 then
    //  sgdServices.Row:= sgdServices.Row - (iRows -1);
  end;
end;

procedure TfrmMain.actEditLoanExecute(Sender: TObject);
  var
    O : TLoan;
    B : TLoan; //Buffer for undo
  begin
    O := TLoan(FMedLoans.SelectedObject[sgdLoans]);
    B := TLoan.Create;
    B.Assign(O);
    if EditLoan(B) then
    begin
      O.Assign(B);
      O.SaveObject;
      O.NotifyObservers;
    end;
    B.Free;
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

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  RegisterFallBackMediators;
  RegisterFallBackListmediators;

  SetupDatabase;

  gLedgerManager.LoadPersons;
  FPersons := gLedgerManager.PersonList;

  gLedgerManager.LoadServices;
  FServices := gLedgerManager.Services;

  SetupMediators;

  PageControl1.ActivePage := tabPersons;
  sgdServices.Columns[4].Width:= 100;
  sgdPersons.Columns[2].Width:= 100;
end;

procedure TfrmMain.sgdPersonsDblClick(Sender: TObject);
begin
  actEditMember.Execute;
end;

procedure TfrmMain.sgdServicesDblClick(Sender: TObject);
begin
  actEditService.Execute;
end;

procedure TfrmMain.SpeedButton1Click(Sender: TObject);
begin
  edtFilter.SetFocus;
  edtFilter.Text:= '';
end;


procedure TfrmMain.SetPersons(AValue: TPersonList);
begin
  if FPersons=AValue then Exit;
  FPersons:=AValue;
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
    FPersonsMediator.AddComposite('Name(200,"Name");DateJoinedAsString(100,"Date Joined");ID(100," ")',sgdPersons);
  end;
  FPersonsMediator.Subject:= FPersons;
  FPersonsMediator.Active:= True;

  //services mediator
  if not assigned(FMedServices) then
  begin
    FMedServices := TtiModelMediator.Create(Self);
    FMedServices.Name:= 'ServicesMediator';
    FMedServices.AddComposite('Name(200,"Name");MaxAmount(100,"Max Amount");InterestRate(100,"Interest");MaxTerm(100,"Terms");ID(100," ")',sgdServices);
  end;
  FMedServices.Subject:= FServices;
  FMedServices.Active:= True;

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

