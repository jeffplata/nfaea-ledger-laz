unit PersonsLookupForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, Grids, ExtCtrls, Buttons, EditBtn, StdCtrls,
  ComCtrls, ledger_bom, tiModelMediator
  ,SQLWhereBuilderNV
  ;

type

  { TfrmLkuPersons }

  TfrmLkuPersons = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    LabeledEdit1: TLabeledEdit;
    SpeedButton1: TSpeedButton;
    StringGrid1: TStringGrid;
    Timer1: TTimer;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LabeledEdit1Change(Sender: TObject);
    procedure LabeledEdit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    SQLWhereBuilder1: TSQLWhereBuilder;
    FData: TPersonsLookUp;
    FMediator: TtiModelMediator;
    procedure SetData(AValue: TPersonsLookUp);
    procedure SetupMediator;
    procedure Init;
  public
    property Data: TPersonsLookUp read FData write SetData;
  end;

  function SelectPerson: TPersonBasic;


implementation

uses
  ledgermanager
  ,ResourceDM
  ,LCLType
  ;

{$R *.lfm}

function SelectPerson: TPersonBasic;
const
  lFilter: string = '';
begin
  Result := nil;
  with TfrmLkuPersons.Create(Application) do
  try
    gLedgerManager.LoadPersonsLookup;
    Data := gLedgerManager.PersonsLookup;
    LabeledEdit1.Text := lFilter;
    Init;
    if ShowModal = mrOk then
      Result := TPersonBasic(FMediator.SelectedObject[StringGrid1]);
  finally
    lFilter := LabeledEdit1.Text;
    Free;
  end;
end;

{ TfrmLkuPersons }

procedure TfrmLkuPersons.FormCreate(Sender: TObject);
begin
  SQLWhereBuilder1 := TSQLWhereBuilder.Create(Self);
  //SQLWhereBuilder1.AddCondition('NAME',swoContains,LabeledEdit1,'Text');
  SQLWhereBuilder1.AddWhereClauseAnd('NAME containing ?',[LabeledEdit1,'Text']);
end;

procedure TfrmLkuPersons.FormDestroy(Sender: TObject);
begin
  SQLWhereBuilder1.Free;
end;

procedure TfrmLkuPersons.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOk then
    CanClose := TPersonBasic(FMediator.SelectedObject[StringGrid1]) <> nil ;
end;

procedure TfrmLkuPersons.LabeledEdit1Change(Sender: TObject);
begin
  SpeedButton1.Enabled:= LabeledEdit1.Text <> '';
  Timer1.Enabled:= True;
end;

procedure TfrmLkuPersons.LabeledEdit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key in [vk_up, vk_down] then
    begin
      if Key = vk_down then
        StringGrid1.Row:= StringGrid1.Row +1
      else if key = vk_up then
        StringGrid1.Row := StringGrid1.Row -1;
      Key := 0;
    end;
end;

procedure TfrmLkuPersons.SpeedButton1Click(Sender: TObject);
begin
  LabeledEdit1.Clear;
  LabeledEdit1.SetFocus;
end;

procedure TfrmLkuPersons.StringGrid1DblClick(Sender: TObject);
begin
  btnOk.Click;
end;

procedure TfrmLkuPersons.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:= False;

  SQLWhereBuilder1.UpdateWhereClauses;

  //Data.ListFilter.Active:= LabeledEdit1.Text <> '';
  //Data.ListFilter.Criteria:= 'NAME containing '+QuotedStr(LabeledEdit1.Text);

  Data.ListFilter.Active := SQLWhereBuilder1.WhereList.Count > 0;
  Data.ListFilter.Criteria := SQLWhereBuilder1.WhereList.Text;

  gLedgerManager.LoadPersonsLookup(True);
  StringGrid1.RowHeights[0] := 0;
end;

procedure TfrmLkuPersons.SetData(AValue: TPersonsLookUp);
begin
  if FData=AValue then Exit;
  FData:=AValue;
  SetupMediator;
  StringGrid1.RowHeights[0] := 0;
end;

procedure TfrmLkuPersons.SetupMediator;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(Self);
    FMediator.Name:= 'PersonsLkUpMediator';
    FMediator.AddComposite('Name',StringGrid1);
  end;
  FMediator.Subject := FData;
  FMediator.Active:= True;
end;

procedure TfrmLkuPersons.Init;
begin
  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,SpeedButton1.Glyph);
  SpeedButton1.Enabled:= LabeledEdit1.Text <> '';
end;



end.

