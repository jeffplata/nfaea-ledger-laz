unit MyUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function BeautifyDatePeriod( ADateFrom, ADateTo: TDateTime ): String;

implementation

uses dateutils;

function BeautifyDatePeriod(ADateFrom, ADateTo: TDateTime): String;
var
  y, m, d: string;
begin
  Result := '';
  if (ADateFrom = 0) and (ADateTo = 0) then Exit;   //<==
  if (ADateFrom = 0) or (ADateTo = 0) then
  begin
    if (ADateFrom = 0) then Result := 'Up to '+FormatDateTime('mmmm dd, yyyy', ADateTo, []);
    if (ADateTo = 0) then Result := 'From '+FormatDateTime('mmmm dd, yyyy', ADateFrom, []);;
    Exit;  //<==
  end;

  d := '';
  If YearOf(ADateFrom) = YearOf(ADateTo) then
  begin
    y := IntToStr(YearOf(ADateFrom));
    if MonthOf(ADateFrom) = MonthOf(ADateTo) then
    begin
      m := DefaultFormatSettings.LongMonthNames[MonthOf(ADateFrom)];
      if DayOf(ADateFrom) = DayOf(ADateTo) then
        d := IntToStr(DayOf(ADateFrom))
      else
        d := IntToStr(DayOf(ADateFrom)) + ' - ' + IntToStr(DayOf(ADateTo));
    end
    else
      m := FormatDateTime('mmmm d - ',ADateFrom,[]) + FormatDateTime('mmmm d',ADateTo,[]);
    Result := m + ' ' + d + ', ' + y;
  end
  else
    Result := FormatDateTime('mmmm d, yyyy', ADateFrom, []) + ' - ' + FormatDateTime('mmmm d, yyyy', ADateTo, [])
end;

end.

