unit ListToBufDatasetU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiObject, BufDataset;


procedure ListToBufDataset( AList: TtiObjectList; ABufDataset: TBufDataset; AFields: array of string  );

implementation

uses variants, typinfo, db;

procedure ListToBufDataset(AList: TtiObjectList; ABufDataset: TBufDataset; AFields: array of string );
var
  i, j: Integer;
  MyPropInfo: PPropInfo;
  PropTypeName: string;
  s: string;
  fields: TFieldDefs;
  ft: TFieldType;
begin
  fields := TFieldDefs.Create(ABufDataset);
  for i := 0 to Length(AFields)-1 do
  begin
    MyPropInfo := GetPropInfo(AList.Items[0], AFields[i]);
    if MyPropInfo<>nil then
    begin
      PropTypeName := MyPropInfo^.PropType^.Name;


      if PropTypeName = 'AnsiString' then
        ft:= ftString
      else if PropTypeName = 'TDate' then
        ft:= ftDate
      else if PropTypeName = 'Currency' then
        ft:= ftCurrency
      else
        ft:= ftVariant
        ;

      fields.Add(AFields[i],ft);
    end;
  end; //for i

  ABufDataset.FieldDefs.Assign(fields);
  ABufDataset.CreateDataset;
  ABufDataset.Active:= true;

  for i := 0 to AList.Count-1 do
  begin
    ABufDataset.Insert;
    for j := 0 to Length(AFields)-1 do
    begin
      if ABufDataset.Fields[j].DataType = ftCurrency then
        ABufDataset.Fields[j].AsFloat:= AList.Items[i].PropValue[AFields[j]]
      else if ABufDataset.Fields[j].DataType = ftDate then
        ABufDataset.Fields[j].AsDateTime:= AList.Items[i].PropValue[AFields[j]]
      else
        ABufDataset.Fields[j].AsString:= AList.Items[i].PropValue[AFields[j]]
        ;
      writeln(AList.Items[i].PropValue[AFields[j]] );
    end;
    ABufDataset.Post;
  end;

  {
  for i := 0 to AList.Count -1 do
  begin
    ABufDataset.Insert;
    if VarType(AList.Items[i]) = varcurrency then
      //ABufDataset.
  end;
  }
end;

end.

