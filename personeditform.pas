unit PersonEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, EditBtn, ledger_bom, tiModelMediator;

type

  { TfrmPersonEdit }

  TfrmPersonEdit = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ckbActive: TCheckBox;
    edtDateJoined: TDateEdit;
    edtName: TEdit;
    edtNumber: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private
    FData: TPerson;
    FMediator: TtiModelMediator;
    procedure SetData(AValue: TPerson);
    procedure SetupMediators;
  public
    property Data: TPerson read FData write SetData;
  end;

  function EditPerson( AData: TPerson ): Boolean;

implementation

function EditPerson(AData: TPerson): Boolean;
begin
  with TfrmPersonEdit.Create(nil) do
  try
    Data := Adata;
    result := (ShowModal = mrOK);
  finally
    Free;
  end;
end;

{$R *.lfm}

{ TfrmPersonEdit }

procedure TfrmPersonEdit.SetData(AValue: TPerson);
begin
  if FData=AValue then Exit;
  FData:=AValue;
  SetupMediators;
end;

procedure TfrmPersonEdit.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(Self);
    FMediator.AddProperty('Name', edtName);
    FMediator.AddProperty('Number', edtNumber);
    FMediator.AddProperty('DateJoined', edtDateJoined);
    FMediator.AddProperty('Active', ckbActive);
  end;
  FMediator.Subject := FData;
  FMediator.Active:= True;
end;

end.

