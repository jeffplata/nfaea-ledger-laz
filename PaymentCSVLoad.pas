unit PaymentCSVLoad;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SdfData, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, StdCtrls, ExtCtrls;

type

  { TfrmPaymentCSVLoad }

  TfrmPaymentCSVLoad = class(TForm)
    btnSave: TButton;
    btnCancel: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    SdfDataSet1: TSdfDataSet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    skip_ : TStringList;
    procedure SaveToDB;
  public

  end;

procedure ShowPaymentCSVLoad( AFileName: string );

//var
//  frmPaymentCSVLoad: TfrmPaymentCSVLoad;

implementation

uses
  ledger_bom
  ,tiOPFManager
  ,ledgermanager
  ;

procedure ShowPaymentCSVLoad(AFileName: string);
var
  s_ : TStringList;
  sMissing: string;
  i: Integer;
  //skip_ : TStringList;
begin
  //skip_ := TStringList.Create;
  skip_.AddStrings(['EMPNO','NAME','TOTAL']);

  s_ := TStringList.Create;
  for i := 0 to gLedgerManager.Services.Count -1 do
    s_.Add(UpperCase(gLedgerManager.Services.Items[i].CSVUploadName ) );

  //todo: Check Payment CSV columns: warn if not valid column from Services; use CSVUploadName

  with TfrmPaymentCSVLoad.Create(Application) do
  try
    SdfDataSet1.FileName:= AFileName;
    SdfDataSet1.FirstLineAsSchema:= True;
    SdfDataSet1.Active := True;

    //addjust column sizes
    for i := 0 to DBGrid1.Columns.Count-1 do
      DBGrid1.Columns[i].Width := 100;

    //verify that CSV columns exist in Services.CSVUploadNames
    For i := 0 to SdfDataSet1.Schema.Count-1 do
      if ( skip_.IndexOf(SdfDataSet1.Schema[i]) = -1 ) and ( s_.IndexOf(SdfDataSet1.Schema[i]) = -1 ) then
        sMissing:= sMissing + SdfDataSet1.Schema[i] + #13#10;

    if sMissing<> '' then
      ShowMessage('Verification required.'#13#10+
        'The following columns which are not defined '#13#10+
        'in the Services table will not be saved:'#13#10#13#10 +
        sMissing);
    btnSave.Enabled:= (sMissing = '');

    if ShowModal=mrOk then
      //SaveToDB;
      // not yet ready
  finally
    Free;
  end;

  s_.Free;
  //FreeAndNil(skip_);
end;

{$R *.lfm}

{ TfrmPaymentCSVLoad }

procedure TfrmPaymentCSVLoad.FormCreate(Sender: TObject);
begin
  skip_ := TStringList.Create;
end;

procedure TfrmPaymentCSVLoad.FormDestroy(Sender: TObject);
begin
  FreeAndNil(skip_);
end;

procedure TfrmPaymentCSVLoad.SaveToDB;
var
  P: TPayment;
  Per: TPayment;
  L: TPaymentList;
  i: Integer;
begin
  // iterate
  DBGrid1.BeginUpdate;
  L := TPaymentList.Create;
  try
    SdfDataSet1.First;
    while not SdfDataSet1.EOF do
    begin
      for i := 0 to SdfDataSet1.FieldCount -1 do
      begin
        if skip_.IndexOf( SdfDataSet1.Schema[i] ) > -1 then Continue; //<==
        //todo: continue here
      end;
      //P := TPayment.CreateNew;
      //P.Name:= SdfDataSet1.FieldByName('NAME').AsString;
      //if SdfDataSet1.FieldByName('DATEJOINED').AsString = '' then
      //  P.DateJoined:= 0
      //else
      //  P.DateJoined:= SdfDataSet1.FieldByName('DATEJOINED').AsDateTime;
      //L.Add(P);

      SdfDataSet1.Next;
    end;

    for i := 0 to L.Count-1 do
    begin
      Per := TPayment(L.Items[i]);
      GTIOPFManager.DefaultOIDGenerator.AssignNextOID(Per.OID);
      Per.SaveObject;
    end;
  finally
    L.Free;
    DBGrid1.EndUpdate;
  end;
end;


end.

