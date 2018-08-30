unit Model_View;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,tiMediators
  ,JLabeledCurrencyEdit
  , EditBtn
  ;

type

  { TService_Money_Mediator }

  TService_Money_Mediator = class(TtiEditMediatorView)
  public
    function    View: TJLabeledCurrencyEdit; reintroduce;
    class function ComponentClass: TClass; override;
  protected
    procedure SetupGUIandObject; override;
  end;

  { TEditButton_Mediator }

  TEditButton_Mediator = class(TtiCustomEditMediatorView)
  public
    function    View: TEditButton; reintroduce;
    class function ComponentClass: TClass; override;
  protected
    procedure SetupGUIandObject; override;
  end;

implementation

uses
  ledger_bom
  ,tiBaseMediator
  ;

procedure RegisterMediators;
begin
  // Fallbacks (generic)
  RegisterFallBackMediators;

  // Specific
  gMediatorManager.RegisterMediator(TService_Money_Mediator, TService, 'MaxAmount');
  gMediatorManager.RegisterMediator(TService_Money_Mediator, TService, 'MinAmount');
  gMediatorManager.RegisterMediator(TService_Money_Mediator, TLoan, 'Principal');
  gMediatorManager.RegisterMediator(TService_Money_Mediator, TLoan, 'Interest');
  gMediatorManager.RegisterMediator(TService_Money_Mediator, TLoan, 'InterestRate');
  gMediatorManager.RegisterMediator(TService_Money_Mediator, TLoan, 'Total');
  gMediatorManager.RegisterMediator(TService_Money_Mediator, TLoan, 'PreviousBalance');
  gMediatorManager.RegisterMediator(TService_Money_Mediator, TLoan, 'Rebates');
  gMediatorManager.RegisterMediator(TService_Money_Mediator, TLoan, 'RebateRate');
  gMediatorManager.RegisterMediator(TService_Money_Mediator, TLoan, 'NetProceeds');
  gMediatorManager.RegisterMediator(TService_Money_Mediator, TLoan, 'Amortization');

  gMediatorManager.RegisterMediator(TService_Money_Mediator, TPayment, 'Amount');
  //gMediatorManager.RegisterMediator(TEditButton_Mediator, TPayment, 'Person.Name');
  //todo: TEditButton_Mediator is useless; delete
end;

{ TEditButton_Mediator }

function TEditButton_Mediator.View: TEditButton;
begin
  result := TEditButton(inherited View);
end;

class function TEditButton_Mediator.ComponentClass: TClass;
begin
  Result:= TEditButton;
end;

procedure TEditButton_Mediator.SetupGUIandObject;
begin
  GUIFieldName:= 'Text';
end;

{ TService_Money_Mediator }

function TService_Money_Mediator.View: TJLabeledCurrencyEdit;
begin
  result := TJLabeledCurrencyEdit(inherited View);
end;

class function TService_Money_Mediator.ComponentClass: TClass;
begin
  Result:= TJLabeledCurrencyEdit;
end;

procedure TService_Money_Mediator.SetupGUIandObject;
begin
  //inherited SetupGUIandObject;
  GUIFieldName:= 'Value';
end;



initialization
  { Register all your Mediator Views here }
  RegisterMediators;

end.

