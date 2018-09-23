unit LoanCSVLoad;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SdfData, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, StdCtrls, ExtCtrls, EditBtn, ActnList
  , ledger_bom;

type

  { TfrmLoanCSVLoad }

  TfrmLoanCSVLoad = class(TForm)
    actSave: TAction;
    ActionList1: TActionList;
    btnSave: TButton;
    btnCancel: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    SdfDataSet1: TSdfDataSet;
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    required_ : TStringList;
    missing_ : TStringList;
    skip_ : TStringList;
    invalidEmpnos_: TStringList;
    L: TLoanList;
    procedure BuildList( const ANumber: string; const ADate: TDateTime );
    procedure SaveToDB;
    function isValidORNumber( AORNumber : string ): Boolean;
  public

  end;

const
  cInvalidEmpno = 'Operation Aborted.'#13#10#13#10'The following Employee Numbers are invalid:'+
    #13#10'(Copied to Clipboard)'#13#10#13#10;

  cMissingColumns = 'Verification required.'#13#10#13#10+
    'The following required columns cannot be found:'#13#10#13#10;

  cCannotContinue = 'Operation cannot continue.';

  cSaveAborted = 'Operation aborted.'#13#10'OR Number %s has already been used.';

procedure ShowLoanCSVLoad( AFileName: string );


implementation

uses
  tiOPFManager
  ,ledgermanager, Clipbrd
  ;

procedure ShowLoanCSVLoad(AFileName: string);
var
  s_ : TStringList;
  sInvalidColumns: string;
  i: Integer;
begin

  s_ := TStringList.Create;
  for i := 0 to gLedgerManager.Services.Count -1 do
    s_.Add(UpperCase(gLedgerManager.Services.Items[i].CSVUploadName ) );

  with TfrmLoanCSVLoad.Create(Application) do
  try
    skip_.AddStrings(['EMPNO','NAME']);
    required_.AddStrings(['EMPNO','NAME','PRINCIPAL','INTEREST','TOTAL',
       'DATE','TERMS','FROM','TO','AMORTIZATION','BALANCE']);

    SdfDataSet1.FileName:= AFileName;
    SdfDataSet1.FirstLineAsSchema:= True;
    SdfDataSet1.Active := True;

    //addjust column sizes
    for i := 0 to DBGrid1.Columns.Count-1 do
      DBGrid1.Columns[i].Width := 100;

    //Verify that CSV columns are as expected
    for i := 0 to required_.Count-1 do
      if (SdfDataSet1.Schema.IndexOf(required_[i]) = -1) then
        missing_.Add(required_[i]);

    if missing_.Count > 0 then
    begin
      ShowMessage( cMissingColumns + missing_.Text +#13#10+ cCannotContinue );
      btnSave.Tag:= 9;  // 9 = will not enable
    end;

    showmodal;
    //if ShowModal=mrOk then
    //  begin
    //      BuildList(edtORNumber.Text, edtDate.Date);
    //      if invalidEmpnos_.Count = 0 then
    //        SaveToDB;
    //    end // isValidOR
    //    else
    //      ShowMessage(Format(cSaveAborted,[edtORNumber.Text]));
    //  end;   // showmodal
  finally
    Free;
  end;

  s_.Free;
end;


{$R *.lfm}

{ TfrmLoanCSVLoad }

procedure TfrmLoanCSVLoad.FormCreate(Sender: TObject);
begin
  required_      := TStringList.Create;
  missing_       := TStringList.Create;
  skip_          := TStringList.Create;
  invalidEmpnos_ := TStringList.Create;
  L              := TLoanList.Create;
end;

procedure TfrmLoanCSVLoad.actSaveExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmLoanCSVLoad.actSaveUpdate(Sender: TObject);
begin
  actSave.Enabled:= (btnSave.Tag <> 9);
end;


procedure TfrmLoanCSVLoad.FormDestroy(Sender: TObject);
begin
  FreeAndNil(required_);
  FreeAndNil(skip_);
  FreeAndNil(invalidEmpnos_);
  FreeAndNil(L);
end;

procedure TfrmLoanCSVLoad.BuildList(const ANumber: string;
  const ADate: TDateTime);
var
  O: TLoan;
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
      //todo: continue here
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

        O := TLoan.Create;  //dont forget the OID on save
        O.DocDate              := ADate;
        O.DocNumber            := ANumber;
        O.Amount               := Amount;
        O.Service.OID.AsString := S_OID;
        O.Person.OID.AsString  := P_OID;
        L.Add( O );
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

procedure TfrmLoanCSVLoad.SaveToDB;
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

function TfrmLoanCSVLoad.isValidORNumber(AORNumber: string): Boolean;
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

