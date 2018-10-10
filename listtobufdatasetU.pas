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
  i: Integer;
  MyPropInfo: PPropInfo;
  PropTypeName: string;
  s: string;
  fields: TFieldDefs;
begin
  for i := 0 to Length(AFields)-1 do
  begin
    fields.Add(AFields[i]);
    MyPropInfo := GetPropInfo(AList.Items[0], AFields[i]);
    if MyPropInfo<>nil then begin
      PropTypeName := MyPropInfo^.PropType^.Name;
      s := s + PropTypeName +#13#10;

      if PropTypeName = 'AnsiString' then
        fields[i].DataType:= ftString
      else if PropTypeName = 'TDate' then
          fields[i].DataType:= ftDate;

    end;
  end; //for i
  writeln(s);
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

