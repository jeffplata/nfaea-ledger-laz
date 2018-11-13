unit ListToBufDatasetU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiObject, BufDataset, db;

type
  TStringArray = array of string;
  TIntegerArray = array of Integer;


procedure _LocalParse( AString: string; var AFieldNames: TStringArray; var AFieldSizes: TIntegerArray );
procedure _CreateFields(AList: TtiObjectList; fields: TFieldDefs; fieldnames: TStringArray; fieldsizes: TIntegerArray);

procedure ListToBufDataset( AList: TtiObjectList; ABufDataset: TBufDataset; AFields: string );
procedure ListToBufDatasetCrossTab( AList: TtiObjectList; ABufDataset: TBufDataset; Columns, CrossTabColumn, ValueColumn: string );

implementation

uses variants, strutils, typinfo;

//------------
procedure _CreateFields(AList: TtiObjectList; fields: TFieldDefs; fieldnames: TStringArray; fieldsizes: TIntegerArray);
var
  MyPropInfo: PPropInfo;
  PropTypeName: string;
  ft: TFieldType;
  i: integer;
begin
  for i := 0 to Length(fieldnames)-1 do
  begin

    MyPropInfo := GetPropInfo(AList.Items[0], fieldnames[i]);
    if MyPropInfo<>nil then
    begin
      PropTypeName := MyPropInfo^.PropType^.Name;

      if PropTypeName = 'AnsiString' then
        ft:= ftString
      else if PropTypeName = 'TDateTime' then
        ft:= ftDate
      else if PropTypeName = 'TDate' then
        ft:= ftDate
      else if PropTypeName = 'Currency' then
        ft:= ftFloat
      else
        ft:= ftVariant
        ;

      if fieldsizes[i] > 0 then
        fields.add(fieldnames[i],ft,fieldsizes[i])
      else
        fields.Add(fieldnames[i],ft);
    end;
  end; //for i
end;
//============


//------------
procedure _LocalParse(AString: string; var AFieldNames: TStringArray;
  var AFieldSizes: TIntegerArray);
var
  i, c: Integer;
  s: string;
  ts: string;
begin
  with TStringList.Create do
  try
    Delimiter:= ';';
    DelimitedText:= AString;
    c := Count;
    SetLength(AFieldNames, c);
    SetLength(AFieldSizes, c);
    for i := 0 to Pred(c) do
    begin
      s := Strings[i];
      AFieldNames[i] := ExtractDelimited(1,s,[':']);
      ts := ExtractDelimited(2,s,[':']);
      if ts = '' then ts := '0';
      AFieldSizes[i] := StrToInt(ts);
    end;
  finally
    free;
  end;
end;
//============


procedure ListToBufDataset(AList: TtiObjectList; ABufDataset: TBufDataset; AFields: string );
var
  i, j: Integer;
  fields: TFieldDefs;
  fieldnames: TStringArray;
  fieldsizes: TIntegerArray;
begin
  _LocalParse( AFields, fieldnames, fieldsizes );

  fields := TFieldDefs.Create(ABufDataset);
  _CreateFields(AList, fields, fieldnames, fieldsizes);

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
      else if (ABufDataset.Fields[j].DataType in[ftDateTime, ftDate]) then
        ABufDataset.Fields[j].AsDateTime:= AList.Items[i].PropValue[fieldnames[j]]
      else
        ABufDataset.Fields[j].AsString:= AList.Items[i].PropValue[fieldnames[j]]
        ;
    end;
    ABufDataset.Post;
  end;
end;

procedure ListToBufDatasetCrossTab(AList: TtiObjectList;
  ABufDataset: TBufDataset; Columns, CrossTabColumn, ValueColumn: string);

var
  fields: TFieldDefs;
  fieldnames: TStringArray;
  fieldsizes: TIntegerArray;
  crossColumns: TStringArray;
  crossSizes: TIntegerArray;
  valueColumns: TStringArray;
  valueSizes: TIntegerArray;
  i: Integer;

  begin
    _LocalParse( Columns, fieldnames, fieldsizes );
    _LocalParse( CrossTabColumn, crossColumns, crossSizes );
    _LocalParse( ValueColumn, valueColumns, valueSizes );

    fields := TFieldDefs.Create(ABufDataset);
    _CreateFields( AList, fields, fieldnames, fieldsizes );
    for i := 0 to Length(crossColumns)-1 do
    begin
      //fields.add(StringReplace(crossColumns[i],' ','', [rfReplaceAll]),ftCurrency,crossSizes[i]);
      fields.add(crossColumns[i],ftCurrency,crossSizes[i]);
    end;
    _CreateFields( AList, fields, valueColumns, valueSizes );

    ABufDataset.FieldDefs.Assign(fields);
    ABufDataset.CreateDataset;
    ABufDataset.Active:= true;
  end;

end.

