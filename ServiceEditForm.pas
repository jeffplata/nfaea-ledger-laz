unit ServiceEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditBtn, MaskEdit, ExtCtrls, ledger_bom, tiModelMediator,
  JLabeledCurrencyEdit;

type

  //{ TCurrencyEdit }
  //
  //TCurrencyEdit = class(TJLabeledCurrencyEdit)
  //private
  //  function GetText: string;
  //published
  //  property Text: string read GetText;
  //end;

  { TfrmServiceEdit }

  TfrmServiceEdit = class(TForm)
    Button1: TButton;
    Button2: TButton;
    edtInterest: TEdit;
    edtRebate: TEdit;
    edtMaxTerms: TEdit;
    edtDateJoined: TDateEdit;
    edtName: TEdit;
    edtMinTerms: TEdit;
    edtMaxAmount: TJLabeledCurrencyEdit;
    edtMinAmount: TJLabeledCurrencyEdit;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
  private
    FData: TService;
    FMediator: TtiModelMediator;
    procedure SetData(AValue: TService);
    procedure SetupMediators;
  public
    property Data: TService read FData write SetData;
  end;

  function EditService( AData: TService ): Boolean;

implementation

uses
  Model_View
  ;

function EditService(AData: TService): Boolean;
begin
  with TfrmServiceEdit.Create(nil) do
  try
    Data := Adata;
    result := (ShowModal = mrOK);
  finally
    Free;
  end;
end;

{$R *.lfm}

//{ TCurrencyEdit }
//
//function TCurrencyEdit.GetText: string;
//begin
//  Result := FormatFloat('%15.2f',Value);
//end;


{ TfrmServiceEdit }


procedure TfrmServiceEdit.SetData(AValue: TService);
begin
  if FData=AValue then Exit;
  FData:=AValue;
  SetupMediators;
end;

procedure TfrmServiceEdit.SetupMediators;
begin

  if not Assigned(FMediator) then
  begin

    FMediator := TtiModelMediator.Create(Self);
    FMediator.AddProperty('Name', edtName);
    FMediator.AddProperty('MaxAmount', edtMaxAmount);
    FMediator.AddProperty('MinAmount', edtMinAmount);
    FMediator.AddProperty('InterestRate', edtInterest);
    FMediator.AddProperty('RebateRate', edtRebate);
    FMediator.AddProperty('MinTerm', edtMinTerms);
    FMediator.AddProperty('MaxTerm', edtMaxTerms);

  end;
  FMediator.Subject := FData;
  FMediator.Active:= True;


end;

end.

