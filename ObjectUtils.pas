unit ObjectUtils;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils
  ,Grids
  ,tiObject
  ,ledger_bom
  ;

type
  TObjectClass = class of TManualObject;

procedure DeleteFromList(AStringGrid: TStringGrid;
  AList: TtiObjectList; AClass: TManualObjectClass; const isPropagateToDB : boolean = True);

function ObjectChanged(const A, B: String): Boolean;

implementation

uses Dialogs, Controls;

const
  cMsgDeleteOneRecord = 'Do you want to delete the selected record?';
  cMsgDeleteRecords = '%d selected records will be deleted.'#13#10'Do you want to continue?';
  cMsgCannotDelete = 'This object cannot be deleted'#13#10' as it is referenced by other objects';


procedure DeleteFromList(AStringGrid: TStringGrid; AList: TtiObjectList;
  AClass: TManualObjectClass; const isPropagateToDB: boolean = True);
var
  O: TManualObject;
  i: Integer;
  s: String;
  iRows: integer;
  oldtop : integer;
  DeleteFailed: Boolean;
  lCaption: string;
begin
  oldtop := AStringGrid.Selection.Top;
  if oldtop = 0 then exit; // <==  no line selected

  DeleteFailed:= False;
  iRows := AStringGrid.Selection.bottom - AStringGrid.Selection.Top +1;
  if iRows = 1 then
    s := cMsgDeleteOneRecord
  else
    s := Format(cMsgDeleteRecords,[iRows]);

  if MessageDlg('Delete?',s ,mtConfirmation,[mbYes, mbNo],0) = mrYes then
  begin
    AList.BeginUpdate;
    try
      for i := AStringGrid.Selection.Bottom downto AStringGrid.Selection.Top do
        begin
          O := TManualObject(AList[i-1]);
          O.DeleteObject(AList, s, isPropagateToDB);
          if (Pos('FOREIGN KEY',s)>0) then
            begin
              lCaption:= (O as AClass).Caption;
              MessageDlg('Information',cMsgCannotDelete+#13#10#13#10+lCaption ,mtInformation,[mbOK],0);
              DeleteFailed := True;
            end;
        end;
    finally
      AList.EndUpdate;
    end;
    if not DeleteFailed then
    begin
      // position to the correct record, the first after the last deleted
      AStringGrid.Row:= oldtop;
      // set TopRow so that the current selected record is not hidden from view
      AStringGrid.TopRow:= AStringGrid.Row;
    end;
  end;
end;


function ObjectChanged(const A, B: String): Boolean;
begin
  Result := CompareStr(A, B)<>0;
end;

end.

