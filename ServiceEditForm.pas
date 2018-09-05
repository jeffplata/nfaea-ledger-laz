unit ServiceEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditBtn, MaskEdit, ExtCtrls, ComCtrls, ledger_bom, tiModelMediator,
  JLabeledCurrencyEdit;

type

  { TfrmServiceEdit }

  TfrmServiceEdit = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ckbActive: TCheckBox;
    edtDateJoined: TDateEdit;
    edtInterest: TEdit;
    edtMaxAmount: TJLabeledCurrencyEdit;
    edtMaxTerms: TEdit;
    edtMinAmount: TJLabeledCurrencyEdit;
    edtMinTerms: TEdit;
    edtName: TEdit;
    edtCSVUploadName: TEdit;
    edtRebate: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    PageControl1: TPageControl;
    tabGeneral: TTabSheet;
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
    FMediator.AddProperty('Active', ckbActive);

  end;
  FMediator.Subject := FData;
  FMediator.Active:= True;


end;

end.

