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
    cmbService: TComboBox;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Label1: TLabel;
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
    procedure BuildList( AServiceOID: string );
    procedure SaveToDB;
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
  i: Integer;
  ServiceID: string;
begin

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

    if ShowModal=mrOk then
    begin
      ServiceID:= gLedgerManager.Services.FindByProps(['NAME'],[cmbService.Text]).OID.AsString;
      BuildList( ServiceID );
      if invalidEmpnos_.Count = 0 then
        SaveToDB;
    end;
  finally
    Free;
  end;

end;


{$R *.lfm}

{ TfrmLoanCSVLoad }

procedure TfrmLoanCSVLoad.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  required_      := TStringList.Create;
  missing_       := TStringList.Create;
  skip_          := TStringList.Create;
  invalidEmpnos_ := TStringList.Create;
  L              := TLoanList.Create;

  for i := 0 to gLedgerManager.Services.Count -1 do
    if gLedgerManager.Services.Items[i].ServiceType = 'LOAN' then
      cmbService.Items.Add(gLedgerManager.Services.Items[i].Name);
  cmbService.ItemIndex:= 0;
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

procedure TfrmLoanCSVLoad.BuildList(AServiceOID: string);
var
  O: TLoan;
  P_OID : string;
  EmpNo: string;
  PersonsLookup: TPersonsLookUp;
begin
  // iterate
  PersonsLookup := TPersonsLookUp.Create;

  DBGrid1.BeginUpdate;
  try
    SdfDataSet1.First;
    while not SdfDataSet1.EOF do
    begin
      EmpNo:= '';

      if Empno = '' then
      begin
        EmpNo := SdfDataSet1.FieldByName('EMPNO').AsString;
        PersonsLookup.ListFilter.Criteria:= 'EMPNO = '+QuotedStr(EmpNo);
        PersonsLookup.ListFilter.Active:= True;
        PersonsLookup.Clear;
        GTIOPFManager.Read(PersonsLookup);
        PersonsLookup.ListFilter.Active:= False;
        if PersonsLookup.Count = 0 then
        begin
          invalidEmpnos_.Add(EmpNo);
          SdfDataSet1.Next;
          Continue;    //<== next record (while)
        end;

        P_OID:= PersonsLookup.Items[0].OID.AsString;
        O := TLoan.Create;  //dont forget the OID on save
        O.DocDate      := SdfDataSet1.FieldByName('DATE').AsDateTime;
        O.DocNumber    := 'UPLOAD';
        O.Principal    := SdfDataSet1.FieldByName('PRINCIPAL').AsCurrency;
        O.Interest     := SdfDataSet1.FieldByName('INTEREST').AsCurrency;
        O.Total        := SdfDataSet1.FieldByName('TOTAL').AsCurrency;
        O.Terms        := sdfDataSet1.FieldByName('TERMS').AsInteger;
        O.PaymentStart := SdfDataSet1.FieldByName('FROM').AsDateTime;
        O.PaymentEnd   := SdfDataSet1.FieldByName('TO').AsDateTime;
        O.Amortization := SdfDataSet1.FieldByName('AMORTIZATION').AsCurrency;
        O.Balance      := SdfDataSet1.FieldByName('BALANCE').AsCurrency;

        O.Service.OID.AsString := AServiceOID;
        O.Person.OID.AsString  := P_OID;
        L.Add( O );

      end; // if empno
      SdfDataSet1.Next;
    end //while
  finally
    DBGrid1.EndUpdate;
    PersonsLookup.Free;
  end;

  if invalidEmpnos_.Count > 0 then
  begin
    Clipboard.AsText:= invalidEmpnos_.Text ;
    ShowMessage( cInvalidEmpno + invalidEmpnos_.CommaText);
  end;
end;

procedure TfrmLoanCSVLoad.SaveToDB;
var
  P: TLoan;
  i: Integer;
begin
  for i := 0 to L.Count-1 do
  begin
    P := TLoan(L.Items[i]);
    GTIOPFManager.DefaultOIDGenerator.AssignNextOID(P.OID);
    P.SaveObject;
  end;
end;


end.

