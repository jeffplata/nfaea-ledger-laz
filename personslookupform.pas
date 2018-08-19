unit PersonsLookupForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type

  { TfrmLkuPersons }

  TfrmLkuPersons = class(TForm)
  private

  public
    procedure SelectPerson;  sfasfsafsfsf
  end;

//var
//  frmLkuPersons: TfrmLkuPersons;

implementation

{$R *.lfm}

{ TfrmLkuPersons }

procedure TfrmLkuPersons.SelectPerson;
begin
  with TfrmLkuPersons.Create(Application) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

end.

