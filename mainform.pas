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
    actMemberCSVLoad: TAction;
    actMembers: TAction;
    ActionList1: TActionList;
    actFileEXit: TFileExit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    edtFilter: TLabeledEdit;
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
    SpeedButton1: TSpeedButton;
    StatusBar1: TStatusBar;
    sgdPersons: TStringGrid;
    sgdServices: TStringGrid;
    tabPersons: TTabSheet;
    tabServices: TTabSheet;
    procedure actAddMemberExecute(Sender: TObject);
    procedure actDeleteMemberExecute(Sender: TObject);
    procedure actEditMemberExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actMemberCSVLoadExecute(Sender: TObject);
    procedure actMembersExecute(Sender: TObject);
    procedure edtFilterKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    procedure sgdPersonsDblClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FPersonsMediator: TtiModelMediator;
    FPersons: TPersonList;
    procedure SetPersons(AValue: TPersonList);
    procedure SetupMediators;
    procedure SetupDatabase;
    procedure FilterPersons( AText: string );
  public
    property Persons: TPersonList read FPersons write SetPersons;
  end;

var
  frmMain: TfrmMain;

implementation
uses
  PersonEditForm
  ,ledgermanager
  ,tiOPFManager
  ,MemberCSVLoad
  ;

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

procedure TfrmMain.actDeleteMemberExecute(Sender: TObject);
var
  P: TPerson;
begin
  if MessageDlg('Delete?','Delete this record?',mtConfirmation,[mbYes, mbNo],0) = mrYes then
  begin
    P := TPerson(FPersonsMediator.SelectedObject[sgdPersons]);
    P.DeleteObject(Persons);
  end;
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
    //FPersons.Clear;
    //gLedgerManager.LoadPersons;
    //FPersons := gLedgerManager.PersonList;

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

  SetupMediators;

  PageControl1.ActivePage := tabPersons;
end;

procedure TfrmMain.edtFilterChange(Sender: TObject);
begin

end;

procedure TfrmMain.sgdPersonsDblClick(Sender: TObject);
begin
  actEditMember.Execute;
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

procedure TfrmMain.SetupMediators;
begin
  if not assigned(FPersonsMediator) then
  begin
    FPersonsMediator := TtiModelMediator.Create(Self);
    FPersonsMediator.Name:= 'PersonsMediator';
    FPersonsMediator.AddComposite('Name(200,"Name");DateJoinedAsString(100,"Date Joined");ID(100," ")',sgdPersons);
  end;
  FPersonsMediator.Subject:= FPersons;
  FPersonsMediator.Active:= True;
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

