unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, StdActns, ComCtrls, Grids, ExtCtrls, Buttons, StdCtrls
  ,ledger_bom
  ,tiModelMediator
  ,tiListMediators
  ,tiMediators;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actHelpAbout: TAction;
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
    PageControl1: TPageControl;
    SpeedButton1: TSpeedButton;
    StatusBar1: TStatusBar;
    sgdPersons: TStringGrid;
    sgdServices: TStringGrid;
    tabPersons: TTabSheet;
    tabServices: TTabSheet;
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actMembersExecute(Sender: TObject);
    procedure edtFilterKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
  private
    FPersonsMediator: TtiModelMediator;
    FPersons: TPersonList;
    procedure SetPersons(AValue: TPersonList);
    procedure SetupMediators;
  public
    property Persons: TPersonList read FPersons write SetPersons;
  end;

var
  frmMain: TfrmMain;

implementation
uses
  PersonsForm
  ,ledgermanager
  ;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.actHelpAboutExecute(Sender: TObject);
begin
  ShowMessage('NFAEA Loans Management System');
end;

procedure TfrmMain.actMembersExecute(Sender: TObject);
begin
  ShowPersons(nil);
end;

procedure TfrmMain.edtFilterKeyPress(Sender: TObject; var Key: char);
begin
  if key = chr(13) then
  begin
    FPersons.BeginUpdate;
    FPersons.Clear;
    gLedgerManager.LoadPersons;
    FPersons := gLedgerManager.PersonList;
    FPersons.EndUpdate;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  RegisterFallBackMediators;
  RegisterFallBackListmediators;
  gLedgerManager.LoadPersons;
  FPersons := gLedgerManager.PersonList;
  SetupMediators;

  PageControl1.ActivePage := tabPersons;
end;

procedure TfrmMain.edtFilterChange(Sender: TObject);
begin

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
    FPersonsMediator.AddComposite('Name(200,"Name");DateJoinedAsString(100,"Date Joined")',sgdPersons);
  end;
  FPersonsMediator.Subject:= FPersons;
  FPersonsMediator.Active:= True;
end;


end.

