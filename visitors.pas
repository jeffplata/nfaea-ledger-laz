unit visitors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  ,tiOPFManager
  ,tiObject
  ,tiVisitorDB
  ,ledger_bom
  ;

type

  { TReadPersonsVisitor }

  TReadPersonVisitor = class(TtiVisitorSelect);

  TReadPersonsVisitor = Class(TtiVisitorSelect)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
    Procedure MapRowToObject; override;
  end;

  { TSavePersonVisitor }

  TSavePersonVisitor = Class(TtiVisitorUpdate)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
  end;

  procedure RegisterVisitors;


implementation


const
  SQLReadPersons = 'select OID, NAME, DATEJOINED from PERSON';
  SQLCreatePerson = 'insert into PERSON(OID,NAME,DATEJOINED) values (:OID,:NAME,:DATEJOINED)';
  SQLUpdatePerson = 'update PERSON set NAME=:NAME, DATEJOINED=:DATEJOINED where OID=:OID';
  SQLDeletePerson = 'delete from PERSON where OID=:OID';


{ TSavePersonVisitor }

procedure TSavePersonVisitor.Init;
begin
  Case Visited.ObjectState of
    posCreate:
      Query.SQLText:=SQLCreatePerson;
    posUpdate:
      Query.SQLText:=SQLUpdatePerson;
    posDelete:
      Query.SQLText:=SQLDeletePerson;
  end;
end;

function TSavePersonVisitor.AcceptVisitor: Boolean;
begin
  Result:=Visited is TPerson;
  Result:=Result and (Visited.ObjectState in [posCreate,posdelete,posUpdate]);
end;

procedure TSavePersonVisitor.SetupParams;
var
  O : TPerson;

begin
  O:=TPerson(Visited);
  O.OID.AssignToTIQuery('OID',Query);
  if (Visited.ObjectState<>posDelete) then
  begin
    Query.ParamAsString['NAME'] := O.Name;
    Query.ParamAsDateTime['DATEJOINED'] := O.DateJoined;
  end;
end;

{ TReadPersonsVisitor }

procedure TReadPersonsVisitor.Init;
begin
  Query.SQLText:= SQLReadPersons;
end;

function TReadPersonsVisitor.AcceptVisitor: Boolean;
begin
  Result:= Visited is TPersonList;
end;

procedure TReadPersonsVisitor.SetupParams;
begin

end;

procedure TReadPersonsVisitor.MapRowToObject;
  var
    O : TPerson;

  begin
    O:= TPerson.Create;
    O.OID.AssignFromTIQuery('OID',Query);

    O.Name:= Query.FieldAsString['NAME'];
    O.DateJoined:= Query.FieldAsDateTime['DATEJOINED'];
    O.ObjectState:=posClean;
    TPersonList(Visited).Add(O);
end;

procedure RegisterVisitors;
begin
  with GTIOPFManager do
  begin
    RegReadVisitor(TReadPersonsVisitor);
    RegSaveVisitor(TSavePersonVisitor);
  end;
end;


initialization
  RegisterVisitors;

end.

