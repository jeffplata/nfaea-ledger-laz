unit BaseEditform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, EditBtn, ledger_bom, tiModelMediator;

type

  { TfrmObjectEdit }

  TfrmObjectEdit = class(TForm)
    Button1: TButton;
    Button2: TButton;
    edtName: TEdit;
    Label1: TLabel;
  private
    FData: TPerson;
    FMediator: TtiModelMediator;
    procedure SetData(AValue: TPerson);
    procedure SetupMediators;
  public
    property Data: TPerson read FData write SetData;
  end;

  function EditObjectXXX( AData: TPerson ): Boolean;

implementation

function EditObjectXXX(AData: TPerson): Boolean;
begin
  with TfrmObjectEdit.Create(nil) do
  try
    Data := Adata;
    result := (ShowModal = mrOK);
  finally
    Free;
  end;
end;

{$R *.lfm}

{ TfrmObjectEdit }

procedure TfrmObjectEdit.SetData(AValue: TPerson);
begin
  if FData=AValue then Exit;
  FData:=AValue;
  SetupMediators;
end;

procedure TfrmObjectEdit.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(Self);
    FMediator.AddProperty('Name', edtName);
  end;
  FMediator.Subject := FData;
  FMediator.Active:= True;
end;

end.

