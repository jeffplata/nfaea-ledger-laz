unit Model_View;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,tiMediators
  ,JLabeledCurrencyEdit
  ;

type

  { TService_Money_Mediator }

  TService_Money_Mediator = class(TtiEditMediatorView)
  public
    function    View: TJLabeledCurrencyEdit; reintroduce;
    class function ComponentClass: TClass; override;
  protected
    procedure SetupGUIandObject; override;
    //procedure DoGUIToObject; override;
    //procedure DoObjectToGUI; override;
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

//procedure TService_Money_Mediator.DoGUIToObject;
//begin
//  inherited DoGUIToObject;
// //if FieldName = 'MaxAmount' then
// //  TService(Subject).MaxAmount:= TJLabeledCurrencyEdit(View).Value
// //else if FieldName = 'MinAmount' then
// //  TService(Subject).MinAmount:= TJLabeledCurrencyEdit(View).Value ;
//end;
//
//procedure TService_Money_Mediator.DoObjectToGUI;
//begin
//  inherited DoObjectToGUI;
// //if FieldName = 'MaxAmount' then
// //  TJLabeledCurrencyEdit(View).Value := TService(Subject).MaxAmount
// //else if FieldName = 'MinAmount' then
// //  TJLabeledCurrencyEdit(View).Value := TService(Subject).MinAmount ;
//end;


initialization
  { Register all your Mediator Views here }
  RegisterMediators;

end.

