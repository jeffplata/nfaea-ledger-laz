program nfaealedger;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainform, ledger_bom, ledgermanager, visitors, PersonEditForm,
  datetimectrls, sdflaz, lazcontrols, MemberCSVLoad, Model_View,
  PersonsLookupForm, ResourceDM, SQLWhereBuilderNV, ObjectListFilter;

{$R *.res}

begin
  Application.Title:='NFAEA Ledger';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TdmResources, dmResources);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

