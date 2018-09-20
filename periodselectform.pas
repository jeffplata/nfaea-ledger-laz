unit PeriodSelectForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DateTimePicker, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, Calendar, Menus, Buttons, ActnList,
  JLabeledDateEdit, jdblabeleddateedit, jdblabeledintegeredit;

type

  { TfrmPeriodSelect }

  TfrmPeriodSelect = class(TForm)
    actYearPlus: TAction;
    actYearMinus: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Calendar1: TCalendar;
    cmbMonth: TComboBox;
    cmbYear: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    RadioGroup1: TRadioGroup;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure actYearMinusExecute(Sender: TObject);
    procedure actYearPlusExecute(Sender: TObject);
    procedure Calendar1Change(Sender: TObject);
    procedure Calendar1Click(Sender: TObject);
    procedure cmbMonthChange(Sender: TObject);
    procedure cmbMonthCloseUp(Sender: TObject);
    procedure cmbMonthDropDown(Sender: TObject);
    procedure cmbYearEditingDone(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1SelectionChanged(Sender: TObject);
  private
    Fd1: TDateTime;
    Fd2: TDateTime;
    Fd3: TDateTime;
    function IncrementYear(const incBy: integer): integer;
  private
    procedure ResetPeriodSelection;
    procedure ResetYearMOnthSelection;
    function PeriodSelected: boolean;
    procedure SetPeriod( var dFrom, dTo: TDateTime );
    procedure Init(InitialDate: TDateTime);
    property d1 : TDateTime read Fd1 write Fd1;
    property d2 : TDateTime read Fd2 write Fd2;
    property d3 : TDateTime read Fd3 write Fd3;
  end;


procedure SelectPeriod(var Date1, Date2, Date3: TDateTime; const SelectedDate: TDateTime);

implementation

uses dateutils;

procedure SelectPeriod(var Date1, Date2, Date3: TDateTime;
  const SelectedDate: TDateTime);
begin
  with TfrmPeriodSelect.Create(Application) do
  try
    Init( SelectedDate );
    if showmodal = mrOK then
    begin
      //check if a custom period has been selected
      if PeriodSelected then
      begin
        SetPeriod( Fd2, Fd3 );
        Date1 := 0;
        Date2 := Fd2;
        Date3 := Fd3;
      end
      //selected from year/month combo
      else if cmbMonth.Text <> '' then
      begin
        Date1 := 0;
        Date2 := Fd2;
        Date3 := Fd3;
      end
      //else, date was selected from the calendar
      else
        Date1 := Fd1;
    end;
  finally
    free;
  end;
end;

{$R *.lfm}

{ TfrmPeriodSelect }

procedure TfrmPeriodSelect.FormCreate(Sender: TObject);
begin
  //Calendar1.DateTime:= Date;
  //Fd1 := Date;
  //Fd2 := 0;
  //Fd3 := 0;
  //ResetPeriodSelection;
end;

procedure TfrmPeriodSelect.RadioGroup1SelectionChanged(Sender: TObject);
begin
  ResetYearMOnthSelection;
end;

function TfrmPeriodSelect.IncrementYear(const incBy: integer): integer;
var
  y: word;
begin
  if (cmbYear.Text <> '') then
    try
      y := StrToInt(cmbYear.Text);
      if (y < 1) or (y > 9999) then
        abort;
      y := y + incBy;
    except
      MessageDlg('Error', 'Invalid year value.', mtError, [mbOk], 0);
      cmbYear.SetFocus;
      exit; // <==
    end
  else
    y := YearOf(Date);
  Result:=y;
end;

procedure TfrmPeriodSelect.ResetPeriodSelection;
begin
  RadioGroup1.ItemIndex:= -1;
  Fd2 := 0;
  Fd3 := 0;
end;

procedure TfrmPeriodSelect.ResetYearMOnthSelection;
begin
  cmbMonth.Text:= '';
  cmbYear.Text:= '';
end;

function TfrmPeriodSelect.PeriodSelected: boolean;
begin
  result := RadioGroup1.ItemIndex > -1 ;
end;

procedure TfrmPeriodSelect.SetPeriod(var dFrom, dTo: TDateTime);

  function StartMonthOfTheQuarter(ADate: TDateTime): integer;
  begin
    Case MonthOf(ADate) of
      1..3   : Result := 1;
      4..6   : Result := 4;
      7..9   : Result := 7;
      10..12 : Result := 10;
    end;
  end;

  function StartOfTheQuarter( ADate: TDateTime ): TDateTime;
  begin
    result := StartOfTheMonth(RecodeMonth(ADate,StartMonthOfTheQuarter(ADate)));
  end;

  function EndOfTheQuarter( ADate: TDateTime ): TDateTime;
  begin
    Result := EndOfTheMonth(RecodeMonth(ADate,StartMonthOfTheQuarter(ADate)+2));
  end;

var
  bd: TDateTime;   // base date (so that testing other dates is possible)
begin
  bd := date;
  //bd := strtodate('2/28/2018');
  case RadioGroup1.ItemIndex of
    0: {Today}      begin dFrom:= bd; dTo:= bd; end;
    1: {Yesterday}  begin dFrom:= bd-1; dTo:= bd-1; end;
    2: {This week}  begin dFrom:= bd+1-DayOfWeek(bd); dTo:= bd+7-DayOfWeek(bd); end;
    3: {Prev Week}  begin dFrom:= bd-7+1-DayOfWeek(bd); dTo:= bd-7+7-DayOfWeek(bd); end;
    4: {This Month} begin dFrom:= StartOfTheMonth(bd); dTo:= EndOfTheMonth(bd); end;
    5: {Prev Month} begin dFrom:= StartOfTheMonth(IncMonth(bd,-1)); dTo:= EndOfTheMonth(IncMonth(bd,-1)); end;
    6: {This Quart} begin dFrom:= StartOfTheQuarter(bd); dTo:= EndOfTheQuarter(bd); end;
    7: {Prev Quart} begin dFrom:= StartOfTheQuarter(IncMonth(bd,-3)); dTo:= EndOfTheQuarter(IncMonth(bd,-3)); end;
    8: {This Year}  begin dFrom:= StartOfTheYear(bd); dTo:= EndOfTheYear(bd); end;
    9: {Prev Year}  begin dFrom:= StartOfTheYear(StartOfTheYear(bd)-1); dTo:= StartOfTheYear(bd)-1; end;
  end;
end;

procedure TfrmPeriodSelect.Init(InitialDate: TDateTime);
var
  i: Integer;
begin
  if InitialDate = 0 then
    Calendar1.DateTime:= Date
  else
    Calendar1.DateTime:= InitialDate;
  Fd1 := Calendar1.DateTime;
  Fd2 := 0;
  Fd3 := 0;
  ResetPeriodSelection;

  for i := 1 to 12 do
    cmbMonth.Items.add(DefaultFormatSettings.LongMonthNames[i]);

  for i := YearOf(Date)-5 to YearOf(Date)+5 do
    cmbYear.Items.Add(IntToStr(i));

end;


procedure TfrmPeriodSelect.Calendar1Change(Sender: TObject);
begin
  ResetPeriodSelection;
  ResetYearMOnthSelection;
  Fd1:= Calendar1.DateTime;
end;

procedure TfrmPeriodSelect.actYearPlusExecute(Sender: TObject);
//var
//  i : integer;
begin
  //if sender = actYearPlus then i := 1 else i := -1;
  cmbYear.Text := IntToStr( IncrementYear(1) );
end;

procedure TfrmPeriodSelect.actYearMinusExecute(Sender: TObject);
begin
  cmbYear.Text := IntToStr( IncrementYear(-1) );
end;

procedure TfrmPeriodSelect.Calendar1Click(Sender: TObject);
begin
  ResetPeriodSelection;
  ResetYearMOnthSelection;
  Fd1:= Calendar1.DateTime;
end;

procedure TfrmPeriodSelect.cmbMonthChange(Sender: TObject);
begin
  if (cmbMonth.Text <> '') and (cmbYear.Text = '') then
    cmbYear.Text:= IntToStr(YearOf(Date));
  ResetPeriodSelection;
end;

procedure TfrmPeriodSelect.cmbMonthCloseUp(Sender: TObject);
begin
  if (cmbMonth.Text <> '') and (cmbYear.Text = '') then
    cmbYear.Text:= IntToStr(YearOf(Date));
  ResetPeriodSelection;
end;

procedure TfrmPeriodSelect.cmbMonthDropDown(Sender: TObject);
begin
  if cmbMonth.Text = '' then
    cmbMonth.Text:= DefaultFormatSettings.LongMonthNames[MonthOf(Date)];
end;

procedure TfrmPeriodSelect.cmbYearEditingDone(Sender: TObject);
begin

end;

procedure TfrmPeriodSelect.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
var
  y: word;
begin
  if ModalResult <> mrOk then exit; //<==
  CanClose:= True;

  if ((cmbMonth.Text <> '') and (cmbYear.Text = '')) or
     ((cmbYear.Text <> '') and (cmbMonth.Text = '')) then
  begin
    if cmbMonth.Text = '' then cmbMonth.SetFocus
    else cmbYear.SetFocus;
    MessageDlg('Error','Both year and month values must be supplied.', mtError, [mbOk], 0);
    CanClose:= False;
  end;

  if (cmbYear.Text <> '') then
  try
    y := StrToInt(cmbYear.Text);
    if (y < 1) or (y > 9999) then
      abort;
  except
    MessageDlg('Error','Invalid year value.', mtError, [mbOk], 0);
    cmbYear.SetFocus;
    CanClose:= False;
  end;

  if CanClose and (cmbMonth.Text <> '') then
  begin
    Fd1:= 0;
    Fd2:= EncodeDate(StrToInt(cmbYear.Text),cmbMonth.ItemIndex+1,1);
    Fd3:= EndOfTheMonth(Fd2);
  end;
end;


end.

