unit visitors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  ,tiVisitorDB
  ,ledger_bom
  ;

type

  { TReadPersonsVisitor }

  TReadPersonsVisitor = Class(TtiVisitorSelect)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
    Procedure MapRowToObject; override;
  end;

  TSavePersonsVisitor = Class(TtiVisitorUpdate)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
  end;


implementation

const
  SQLReadPersons = 'select OID, NAME, DATEJOINED from PERSON';
  SQLCreatePerson = 'insert into PERSON(OID,NAME,DATEJOINED) values (:OID,:NAME,:DATEJOINED)';
  SQLUpdatePerson = 'update PERSON set NAME=:NAME, DATEJOINED=:DATEJOINED where OID=:OID';
  SQLDeletePerson = 'delete from PERSON where OID=:OID';

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
    O.DateJoined:= Query.FieldAsString['DATEJOINED'];
    O.DateJoinedAsString := Query.FieldAsString['DATEJOINED'];
    O.ObjectState:=posClean;
    TPersonList(Visited).Add(O);
end;

end.

