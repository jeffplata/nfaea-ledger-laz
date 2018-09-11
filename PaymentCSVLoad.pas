unit PaymentCSVLoad;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SdfData, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, StdCtrls, ExtCtrls, EditBtn, ActnList
  , ledger_bom;

type

  { TfrmPaymentCSVLoad }

  TfrmPaymentCSVLoad = class(TForm)
    actSave: TAction;
    ActionList1: TActionList;
    btnSave: TButton;
    btnCancel: TButton;
    DataSource1: TDataSource;
    edtDate: TDateEdit;
    DBGrid1: TDBGrid;
    edtORNumber: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    SdfDataSet1: TSdfDataSet;
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    skip_ : TStringList;
    invalidEmpnos_: TStringList;
    L: TPaymentList;
    procedure BuildList( const ANumber: string; const ADate: TDateTime );
    procedure SaveToDB;
    function isValidORNumber( AORNumber : string ): Boolean;
  public

  end;

const
  cInvalidEmpno = 'Operation Aborted.'#13#10#13#10'The following Employee Numbers are invalid:'+
    #13#10'(Copied to Clipboard)'#13#10#13#10;
  cInvalidColumns = 'Verification required.'#13#10#13#10+
        'The following columns which are not defined'#13#10+
        'in the Services table will not be saved:'#13#10#13#10;
  cSaveAborted = 'Operation aborted.'#13#10'OR Number %s has already been used.';

procedure ShowPaymentCSVLoad( AFileName: string );


implementation

uses
  tiOPFManager
  ,ledgermanager, Clipbrd
  ;

procedure ShowPaymentCSVLoad(AFileName: string);
var
  s_ : TStringList;
  sInvalidColumns: string;
  i: Integer;
begin

  s_ := TStringList.Create;
  for i := 0 to gLedgerManager.Services.Count -1 do
    s_.Add(UpperCase(gLedgerManager.Services.Items[i].CSVUploadName ) );

  with TfrmPaymentCSVLoad.Create(Application) do
  try
    skip_.AddStrings(['EMPNO','NAME','TOTAL']);

    SdfDataSet1.FileName:= AFileName;
    SdfDataSet1.FirstLineAsSchema:= True;
    SdfDataSet1.Active := True;

    //addjust column sizes
    for i := 0 to DBGrid1.Columns.Count-1 do
      DBGrid1.Columns[i].Width := 100;

    //verify that CSV columns exist in Services.CSVUploadNames
    For i := 0 to SdfDataSet1.Schema.Count-1 do
      if ( skip_.IndexOf(SdfDataSet1.Schema[i]) = -1 ) and ( s_.IndexOf(SdfDataSet1.Schema[i]) = -1 ) then
        sInvalidColumns:= sInvalidColumns + SdfDataSet1.Schema[i] + #13#10;

    if sInvalidColumns<> '' then
      ShowMessage( cInvalidColumns + sInvalidColumns);
    if sInvalidColumns <> '' then
      btnSave.Tag:= 9;  // 9 = will not enable

    if ShowModal=mrOk then
      begin
        //validate ORNumber is not yet used
        if isValidORNumber( edtORNumber.Text ) then
        begin
          BuildList(edtORNumber.Text, edtDate.Date);
          if invalidEmpnos_.Count = 0 then
            SaveToDB;
        end // isValidOR
        else
          ShowMessage(Format(cSaveAborted,[edtORNumber.Text]));
      end;   // showmodal
  finally
    Free;
  end;

  s_.Free;
end;

{$R *.lfm}

{ TfrmPaymentCSVLoad }

procedure TfrmPaymentCSVLoad.FormCreate(Sender: TObject);
begin
  skip_          := TStringList.Create;
  invalidEmpnos_ := TStringList.Create;
  L              := TPaymentList.Create;
  edtDate.Date:= Date;
end;

procedure TfrmPaymentCSVLoad.actSaveExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmPaymentCSVLoad.actSaveUpdate(Sender: TObject);
begin
  actSave.Enabled:= (btnSave.Tag <> 9) and (edtORNumber.Text <> '')
    and (edtDate.Text <> '');
end;


procedure TfrmPaymentCSVLoad.FormDestroy(Sender: TObject);
begin
  FreeAndNil(skip_);
  FreeAndNil(invalidEmpnos_);
  FreeAndNil(L);
end;

procedure TfrmPaymentCSVLoad.BuildList(const ANumber: string;
  const ADate: TDateTime);
var
  P: TPayment;
  i: Integer;
  fldn: string;
  P_OID, S_OID: string;
  EmpNo: string;
  s: string;
  Amount: Currency;
begin
  // iterate
  DBGrid1.BeginUpdate;
  try
    SdfDataSet1.First;
    while not SdfDataSet1.EOF do
    begin
      EmpNo:= '';
      //todo: prevalidate: cannot continue without an EMPNO column

      for i := 0 to SdfDataSet1.FieldCount -1 do
      begin
        fldn := UpperCase( SdfDataSet1.Schema[i] );
        if skip_.IndexOf( fldn ) > -1 then Continue; //<==
        s := StringReplace( SdfDataSet1.FieldByName( fldn ).AsString, ',', '', [rfReplaceAll]);
        if s = '' then s := '0';
        Amount:= StrToCurr(s);
        if Amount = 0 then Continue; //<==

        S_OID := gLedgerManager.Services.FindByProps(['CSVUploadName'],[fldn]).OID.AsString;

        if Empno = '' then
        begin
          EmpNo := SdfDataSet1.FieldByName('EMPNO').AsString;
          gLedgerManager.PersonsLookup.ListFilter.Criteria:= 'EMPNO = '+QuotedStr(EmpNo);
          gLedgerManager.PersonsLookup.ListFilter.Active:= True;
          gLedgerManager.PersonsLookup.Clear;
          GTIOPFManager.Read(gLedgerManager.PersonsLookup);
          gLedgerManager.PersonsLookup.ListFilter.Active:= False;
          if gLedgerManager.PersonsLookup.Count = 0 then
          begin
            invalidEmpnos_.Add(EmpNo);
            Break;    //<==
          end;
          P_OID:= gLedgerManager.PersonsLookup.Items[0].OID.AsString;
        end;

        P := TPayment.Create;  //dont forget the OID on save
        P.DocDate              := ADate;
        P.DocNumber            := ANumber;
        P.Amount               := Amount;
        P.Service.OID.AsString := S_OID;
        P.Person.OID.AsString  := P_OID;
        L.Add( P );
      end;  //for

      SdfDataSet1.Next;
    end;  //while

  finally
    DBGrid1.EndUpdate;
  end;

  if invalidEmpnos_.Count > 0 then
  begin
    Clipboard.AsText:= invalidEmpnos_.Text ;
    ShowMessage( cInvalidEmpno + invalidEmpnos_.CommaText);
  end;
end;

procedure TfrmPaymentCSVLoad.SaveToDB;
var
  P: TPayment;
  i: Integer;
begin
  for i := 0 to L.Count-1 do
  begin
    P := TPayment(L.Items[i]);
    GTIOPFManager.DefaultOIDGenerator.AssignNextOID(P.OID);
    P.SaveObject;
  end;
end;

function TfrmPaymentCSVLoad.isValidORNumber(AORNumber: string): Boolean;
var
  PmtList: TPaymentList;
begin

  PmtList := TPaymentList.Create;
  PmtList.ListFilter.Criteria:= 'DOCNUMBER = '+QuotedStr(AORNumber);
  PmtList.ListFilter.Active:= True;
  GTIOPFManager.Read(PmtList);
  PmtList.ListFilter.Active:= False;
  //if PmtList.Count = 0 means OR Number is not yet used
  Result := (PmtList.Count = 0);

end;


end.

