unit DisplayHelpers;

{$mode objfpc}{$H+}

interface

uses
  tiDisplayHelpers, tiObject, ledger_bom;

type

  { TLoanDisplay }

  TLoanDisplay = class(TBaseDisplayObject)
  private
    FLoan: TLoan;
    function GetDisplay(AIndex : Integer) : String;
    procedure SetLoan(AValue: TLoan);
  public
    constructor CreateCustom(const ALoan : TLoan);
    destructor Destroy; override;
    property Loan: TLoan read FLoan write SetLoan;
  published
    property Person:          string index 0 read GetDisplay;
    property Service:         string index 1 read GetDisplay;
    property DocNumber:       string index 2 read GetDisplay;
    property DocDate:         string index 3 read GetDisplay;
    property Notes:           string index 4 read GetDisplay;
    property Principal:       string index 5 read GetDisplay;
    property Interest:        string index 6 read GetDisplay;
    property Total:           string index 7 read GetDisplay;
    Property PreviousBalance: string index 8 read GetDisplay;
    Property Rebates:         string index 9 read GetDisplay;
    property Adjustments:     string index 10 read GetDisplay;
    Property NetProceeds:     string index 11 read GetDisplay;
    Property InterestRate:    string index 12 read GetDisplay;
    property RebateRate:      string index 13 read GetDisplay;
    Property Terms:           string index 14 read GetDisplay;
    Property Amortization:    string index 15 read GetDisplay;
    Property PaymentStart:    string index 16 read GetDisplay;
    Property PaymentEnd:      string index 17 read GetDisplay;
  End;

  { TLoanDisplayList }

  TLoanDisplayList = class(TBaseDisplayList)
  protected
    function CreateDisplayInstance(AItem: TtiObject): TBaseDisplayObject; override;
    function FindDisplayObject(AObject: TtiObject): TBaseDisplayObject; override;
  end;


  { TPaymentDisplay }

  TPaymentDisplay = class(TBaseDisplayObject)
  private
    FPayment: TPayment;
    function GetDisplay(AIndex : Integer) : String;
    procedure SetPayment(AValue: TPayment);
  public
    constructor CreateCustom(const APayment : TPayment);
    destructor Destroy; override;
    property Payment: TPayment read FPayment write SetPayment;
  published
    property Person: string index 0 read GetDisplay;
    property Service: string index 1 read GetDisplay;
    property DocDate: string index 2 read GetDisplay;
    property DocNumber: string index 3 read GetDisplay;
    property Amount: string index 4 read GetDisplay;
    property Remarks: string index 5 read GetDisplay;
  End;

  { TPaymentDisplayList }

  TPaymentDisplayList = class(TBaseDisplayList)
  protected
    function CreateDisplayInstance(AItem: TtiObject): TBaseDisplayObject; override;
    function FindDisplayObject(AObject: TtiObject): TBaseDisplayObject; override;
  end;

implementation

uses sysutils;

{ TLoanDisplayList }

function TLoanDisplayList.CreateDisplayInstance(AItem: TtiObject
  ): TBaseDisplayObject;
begin
  Result := TLoanDisplay.CreateCustom(TLoan(AItem));
end;

function TLoanDisplayList.FindDisplayObject(AObject: TtiObject
  ): TBaseDisplayObject;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
    if (TLoanDisplay(Items[i]).Loan = AObject) then
    begin
      Result := TBaseDisplayObject(Items[i]);
      break;
    end;
  end;
end;

{ TLoanDisplay }

function TLoanDisplay.GetDisplay(AIndex: Integer): String;
begin
  if Assigned(FLoan) then
  begin
    case AIndex of
     0:  Result :=  Loan.Person.Name                       ;
     1:  Result :=  Loan.Service.Name                      ;
     2:  Result :=  Loan.DocNumber                         ;
     3:  Result :=  DateToStr(Loan.DocDate)                ;
     4:  Result :=  Loan.Notes                             ;
     5:  Result :=  FormatFloat('#,0.00', Loan.Principal);
     6:  Result :=  FormatFloat('#,0.00', Loan.Interest);
     7:  Result :=  FormatFloat('#,0.00', Loan.Total);
     8:  Result :=  FormatFloat('#,0.00', Loan.PreviousBalance);
     9:  Result :=  FormatFloat('#,0.00', Loan.Rebates);
     10: Result :=  FormatFloat('#,0.00', Loan.Adjustments);
     11: Result :=  FormatFloat('#,0.00', Loan.NetProceeds);
     12: Result :=  FormatFloat('#,0.00', Loan.InterestRate);
     13: Result :=  FormatFloat('#,0.00', Loan.RebateRate);
     14: Result :=  IntToStr(Loan.Terms);
     15: Result :=  FormatFloat('#,0.00', Loan.Amortization);
     16: Result :=  DateToStr(Loan.PaymentStart)            ;
     17: Result :=  DateToStr(Loan.PaymentEnd)              ;
    end; { Case }
  end;
end;

procedure TLoanDisplay.SetLoan(AValue: TLoan);
begin
  if FLoan=AValue then Exit;
  if Assigned(FLoan) then
    FLoan.DetachObserver(Self);
  FLoan:=AValue;
  if Assigned(FLoan) then
    FLoan.AttachObserver(Self);
end;

constructor TLoanDisplay.CreateCustom(const ALoan: TLoan);
begin
  inherited Create;
  FLoan := ALoan;
end;

destructor TLoanDisplay.Destroy;
begin
  FLoan := nil;
  inherited Destroy;
end;

{ TPaymentDisplayList }

function TPaymentDisplayList.CreateDisplayInstance(AItem: TtiObject
  ): TBaseDisplayObject;
begin
  Result := TPaymentDisplay.CreateCustom(TPayment(AItem));
end;

function TPaymentDisplayList.FindDisplayObject(AObject: TtiObject
  ): TBaseDisplayObject;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
    if (TPaymentDisplay(Items[i]).Payment = AObject) then
    begin
      Result := TBaseDisplayObject(Items[i]);
      break;
    end;
  end;
end;

{ TPaymentDisplay }

function TPaymentDisplay.GetDisplay(AIndex: Integer): String;
begin
  if Assigned(FPayment) then
  begin
    case AIndex of
      0 : Result := Payment.Person.Name;
      1 : Result := Payment.Service.Name;
      2 : Result := DateToStr(Payment.DocDate);
      3 : Result := Payment.DocNumber;
      4 : Result := FormatFloat('#,0.00', Payment.Amount);
      5 : Result := Payment.Remarks;
    end; { Case }
  end;
end;

procedure TPaymentDisplay.SetPayment(AValue: TPayment);
begin
  if FPayment=AValue then Exit;
  if Assigned(FPayment) then
    FPayment.DetachObserver(Self);
  FPayment:=AValue;
  if Assigned(FPayment) then
    FPayment.AttachObserver(Self);
end;

constructor TPaymentDisplay.CreateCustom(const APayment: TPayment);
begin
  inherited Create;
  FPayment := APayment;
end;

destructor TPaymentDisplay.Destroy;
begin
  FPayment := nil;
  inherited Destroy;
end;

end.

