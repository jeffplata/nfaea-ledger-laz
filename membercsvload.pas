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
  ;

procedure ShowMemberCSVLoad(AFileName: string);
var
  s_ : TStringList;
  sMissing: string;
  i: Integer;
begin
  s_ := TStringList.Create;
  s_.AddStrings(['NAME','DATEJOINED']);

  with TfrmMemberCSVLoad.Create(Application) do
  try
    SdfDataSet1.FileName:= AFileName;
    SdfDataSet1.FirstLineAsSchema:= True;
    SdfDataSet1.Active := True;

    //verify columns are ok
    //NAME, DATEJOINED
    For i := 0 to s_.Count-1 do
      if SdfDataSet1.Schema.IndexOf(s_[i]) = 0 then
        sMissing:= sMissing + s_[i] + #13#10;

    if sMissing<> '' then
      ShowMessage('The list cannot be saved because the following columns are missing:'+
        #13#10+sMissing);
    btnSave.Enabled:= (sMissing = '');

    if ShowModal=mrOk then
    // save to db
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
  L: TPersonList;
begin
  // iterate
  DBGrid1.BeginUpdate;
  L := TPersonList.Create;
  try
    SdfDataSet1.First;
    while not SdfDataSet1.EOF do
    begin
      P := TPerson.CreateNew;
      P.Name:=       SdfDataSet1.FieldByName('NAME').AsString;
      P.DateJoined:= SdfDataSet1.FieldByName('DATEJOINED').AsDateTime;
      L.Add(P);

      SdfDataSet1.Next;
    end;

  finally
    L.Free;
    DBGrid1.EndUpdate;
  end;
end;

end.

