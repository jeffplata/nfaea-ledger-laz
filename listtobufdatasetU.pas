unit ListToBufDatasetU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiObject, BufDataset;

type
  TStringArray = array of string;
  TIntegerArray = array of Integer;


procedure ListToBufDataset( AList: TtiObjectList; ABufDataset: TBufDataset; AFields: string );

implementation

uses variants, strutils, typinfo, db;

procedure ListToBufDataset(AList: TtiObjectList; ABufDataset: TBufDataset; AFields: string );

  procedure Parse( AString: string; var FieldNames: TStringArray; var FieldSizes: TIntegerArray );
  var
    i: Integer;
    s: string;
    ts: string;
  begin
    with TStringList.Create do
    try
      Delimiter:= ';';
      DelimitedText:= AString;
      SetLength(FieldNames, Count);
      SetLength(FieldSizes, Count);
      for i := 0 to Count-1 do
      begin
        s := Strings[i];
        FieldNames[i] := ExtractDelimited(1,s,[':']);
        ts := ExtractDelimited(2,s,[':']);
        if ts = '' then ts := '0';
        FieldSizes[i] := StrToInt(ts);
      end;
    finally
      free;
    end;
  end;

var
  i, j: Integer;
  MyPropInfo: PPropInfo;
  PropTypeName: string;
  fields: TFieldDefs;
  fieldnames: TStringArray;
  fieldsizes: TIntegerArray;
  ft: TFieldType;              //49182
begin
  Parse( AFields, fieldnames, fieldsizes );
  writeln(length(fieldnames));

  fields := TFieldDefs.Create(ABufDataset);
  for i := 0 to Length(fieldnames)-1 do
  begin

    MyPropInfo := GetPropInfo(AList.Items[0], fieldnames[i]);
    if MyPropInfo<>nil then
    begin
      PropTypeName := MyPropInfo^.PropType^.Name;

      if PropTypeName = 'AnsiString' then
        ft:= ftString
      else if PropTypeName = 'TDate' then
        ft:= ftDate
      else if PropTypeName = 'Currency' then
        ft:= ftFloat
      else
        ft:= ftVariant
        ;
      //if ft=ftString then
      if fieldsizes[i] > 0 then
        fields.add(fieldnames[i],ft,fieldsizes[i])
      else
        fields.Add(fieldnames[i],ft);
    end;
  end; //for i

  ABufDataset.FieldDefs.Assign(fields);
  ABufDataset.CreateDataset;
  ABufDataset.Active:= true;

  for i := 0 to AList.Count-1 do
  begin
    ABufDataset.Insert;
    for j := 0 to Length(fieldnames)-1 do
    begin
      if ABufDataset.Fields[j].DataType = ftFloat then
        ABufDataset.Fields[j].AsFloat:= AList.Items[i].PropValue[fieldnames[j]]
      else if ABufDataset.Fields[j].DataType = ftDate then
        ABufDataset.Fields[j].AsDateTime:= AList.Items[i].PropValue[fieldnames[j]]
      else
        ABufDataset.Fields[j].AsString:= AList.Items[i].PropValue[fieldnames[j]]
        ;
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

