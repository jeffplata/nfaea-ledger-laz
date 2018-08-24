unit LookupForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, Grids, ExtCtrls, Buttons, EditBtn, StdCtrls,
  ComCtrls
  , tiObject
  , tiModelMediator
  ,SQLWhereBuilderNV
  ,ObjectListFilter
  ;

type

  TClassOfLookUpForm = class of TfrmLookUp;
  TClassOfObjectList = class of TtiObjectList;
  TClassOfObject = class of TtiObject;

  { THackObjectList }

  THackObjectList = class(TtiObjectList)
  private
    FListFilter: TObjectListFilter;
  public
    property ListFilter: TObjectListFilter read FListFilter write FListFilter;
  end;

  { TfrmLookUp }

  TfrmLookUp = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    LabeledEdit1: TLabeledEdit;
    SpeedButton1: TSpeedButton;
    StringGrid1: TStringGrid;
    Timer1: TTimer;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
    procedure LabeledEdit1Change(Sender: TObject);
    procedure LabeledEdit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    SQLWhereBuilder1: TSQLWhereBuilder ;
    FData: TtiObjectList;
    FMediator: TtiModelMediator;
    SQLFilterText: string;
    UIColumns: string;
    procedure SetData(AValue: TtiObjectList);
    procedure SetupMediator;
    procedure Init;
  public
    property Data: TtiObjectList read FData write SetData;
  end;

  procedure SelectObject( O : TClassOfObject; AObjectList: TtiObjectList;
    ASQLFilterText: string; AUIColumns: string );


implementation

uses
  ledgermanager
  ,ResourceDM
  ,LCLType
  ;

{$R *.lfm}

procedure SelectObject(O: TClassOfObject; AObjectList: TtiObjectList;
  ASQLFilterText: string; AUIColumns: string);
const
  lFilter: string = '';
begin
  with TfrmLookUp.Create(Application) do
  try
    //gLedgerManager.LoadPersonsLookup;
    // load data before calling this procedure
    //Data := gLedgerManager.PersonsLookup;
    SQLFilterText:= ASQLFilterText;
    UIColumns:= AUIColumns;
    LabeledEdit1.Text := lFilter;

    Data := AObjectList;
    Init;
    if ShowModal = mrOk then
      O := TClassOfObject(FMediator.SelectedObject[StringGrid1]);
      //O := FMediator.SelectedObject[StringGrid1];
      //O.Assign( FMediator.SelectedObject[StringGrid1] );
  finally
    lFilter := LabeledEdit1.Text;
    Free;
  end;
end;

{ TfrmLookUp }

procedure TfrmLookUp.FormDestroy(Sender: TObject);
begin
  SQLWhereBuilder1.Free;
end;

procedure TfrmLookUp.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOk then
    CanClose := TClassOfObjectList(FMediator.SelectedObject[StringGrid1]) <> nil ;
end;

procedure TfrmLookUp.LabeledEdit1Change(Sender: TObject);
begin
  SpeedButton1.Enabled:= LabeledEdit1.Text <> '';
  Timer1.Enabled:= True;
end;

procedure TfrmLookUp.LabeledEdit1KeyDown(Sender: TObject; var Key: Word;
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

procedure TfrmLookUp.SpeedButton1Click(Sender: TObject);
begin
  LabeledEdit1.Clear;
  LabeledEdit1.SetFocus;
end;

procedure TfrmLookUp.StringGrid1DblClick(Sender: TObject);
begin
  btnOk.Click;
end;

procedure TfrmLookUp.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:= False;

  SQLWhereBuilder1.UpdateWhereClauses;

  THackObjectList(Data).ListFilter.Active := SQLWhereBuilder1.WhereList.Count > 0;
  THackObjectList(Data).ListFilter.Criteria := SQLWhereBuilder1.WhereList.Text;

  gLedgerManager.LoadPersonsLookup(True);
  StringGrid1.RowHeights[0] := 0;
end;

procedure TfrmLookUp.SetData(AValue: TtiObjectList);
begin
  if FData=AValue then Exit;
  FData:=AValue;
  SetupMediator;
  StringGrid1.RowHeights[0] := 0;
end;

procedure TfrmLookUp.SetupMediator;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(Self);
    //FMediator.Name:= 'PersonsLkUpMediator';
    FMediator.AddComposite(UIColumns, StringGrid1);
  end;
  FMediator.Subject := FData;
  FMediator.Active:= True;
end;

procedure TfrmLookUp.Init;
begin
  dmResources.imlButtonGlyphs.GetBitmap(iindBtnFilterCancel,SpeedButton1.Glyph);
  SpeedButton1.Enabled:= LabeledEdit1.Text <> '';
  SQLWhereBuilder1 := TSQLWhereBuilder.Create(Self);
  SQLWhereBuilder1.AddWhereClauseAnd(SQLFilterText,[LabeledEdit1,'Text']);

end;



end.

