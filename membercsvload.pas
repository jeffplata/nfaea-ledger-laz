unit MemberCSVLoad;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SdfData, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, StdCtrls;

type

  { TfrmMemberCSVLoad }

  TfrmMemberCSVLoad = class(TForm)
    btnSave: TButton;
    btnCancel: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    SdfDataSet1: TSdfDataSet;
  private
    procedure SaveToDB;
  public

  end;

procedure ShowMemberCSVLoad( AFileName: string );

//var
//  frmMemberCSVLoad: TfrmMemberCSVLoad;

implementation

uses
  ledger_bom
  ,tiOPFManager
  ;

procedure ShowMemberCSVLoad(AFileName: string);
var
  s_ : TStringList;
  sMissing: string;
  i: Integer;
begin
  s_ := TStringList.Create;
  s_.AddStrings(['EMPNO','NAME','DATEJOINED']);

  with TfrmMemberCSVLoad.Create(Application) do
  try
    SdfDataSet1.FileName:= AFileName;
    SdfDataSet1.FirstLineAsSchema:= True;
    SdfDataSet1.Active := True;

    //addjust column sizes
    for i := 0 to DBGrid1.Columns.Count-1 do
      DBGrid1.Columns[i].Width := 100;

    //verify columns are ok
    //NAME, DATEJOINED
    For i := 0 to s_.Count-1 do
      if SdfDataSet1.Schema.IndexOf(s_[i]) = -1 then
        sMissing:= sMissing + s_[i] + #13#10;

    if sMissing<> '' then
      ShowMessage('The list cannot be saved because the following columns are missing:'+
        #13#10+sMissing);
    btnSave.Enabled:= (sMissing = '');

    if ShowModal=mrOk then
      SaveToDB;
  finally
    Free;
  end;

  s_.Free;
end;

{$R *.lfm}

{ TfrmMemberCSVLoad }

procedure TfrmMemberCSVLoad.SaveToDB;
var
  P: TPerson;
  Per: TPerson;
  L: TPersonList;
  i: Integer;
begin
  // iterate
  DBGrid1.BeginUpdate;
  L := TPersonList.Create;
  try
    SdfDataSet1.First;
    while not SdfDataSet1.EOF do
    begin
      //P := TPerson.CreateNew;
      P := TPerson.Create;  // OID will be manually assigned upon SaveObject
      P.Name:=       SdfDataSet1.FieldByName('NAME').AsString;
      P.Number:=     SdfDataSet1.FieldByName('EMPNO').AsString;
      P.Active:=     True;
      if SdfDataSet1.FieldByName('DATEJOINED').AsString = '' then
        P.DateJoined:= 0
      else
        P.DateJoined:= SdfDataSet1.FieldByName('DATEJOINED').AsDateTime;
      L.Add(P);

      SdfDataSet1.Next;
    end;

    for i := 0 to L.Count-1 do
    begin
      Per := TPerson(L.Items[i]);
      GTIOPFManager.DefaultOIDGenerator.AssignNextOID(Per.OID);
      Per.SaveObject;
    end;
  finally
    L.Free;
    DBGrid1.EndUpdate;
  end;
end;

end.

