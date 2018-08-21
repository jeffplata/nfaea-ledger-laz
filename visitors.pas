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

  { TReadPersonsLkUpVisitor }

  TReadPersonsLkUpVisitor = Class(TtiVisitorSelect)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
    Procedure MapRowToObject; override;
  end;

  { TReadServicesVisitor }

  TReadServicesVisitor = Class(TtiVisitorSelect)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
    Procedure MapRowToObject; override;
  end;

  { TSaveServiceVisitor }

  TSaveServiceVisitor = Class(TtiVisitorUpdate)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
  end;


  { TReadPersonsVisitor }

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

  { TReadLoansVisitor }

  TReadLoansVisitor = Class(TtiVisitorSelect)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
    Procedure MapRowToObject; override;
  end;



  procedure RegisterVisitors;


implementation


const
  SQLReadPersons = 'select OID, NAME, DATEJOINED from PERSON';
  SQLCreatePerson = 'insert into PERSON(OID,NAME,DATEJOINED) values (:OID,:NAME,:DATEJOINED)';
  SQLUpdatePerson = 'update PERSON set NAME=:NAME, DATEJOINED=:DATEJOINED where OID=:OID';
  SQLDeletePerson = 'delete from PERSON where OID=:OID';
  SQLLookUpPersons = 'select OID, NAME from PERSON';

  SQLReadServices = 'select * from SERVICE';
  SQLCreateService = 'insert into SERVICE (OID, NAME, MAXAMOUNT, MINAMOUNT, INTERESTRATE, REBATE_RATE, MINTERM, MAXTERM) '+
                     'values (:OID, :NAME, :MAXAMOUNT, :MINAMOUNT, :INTERESTRATE, :REBATE_RATE, :MINTERM, :MAXTERM)';
  SQLUpdateService = 'update SERVICE set NAME=:NAME, MAXAMOUNT=:MAXAMOUNT, MINAMOUNT=:MINAMOUNT, '+
                     'INTERESTRATE=:INTERESTRATE, REBATE_RATE=:REBATE_RATE, MINTERM=:MINTERM, MAXTERM=:MAXTERM where OID=:OID';
  SQLDeleteService = 'delete from SERVICE where OID=:OID';

  SQLReadLoans = 'select * from LOAN';
  SQLCreateLoan = '';
  SQLUpdateLoan = '';
  SQLDeleteLoan = '';

{ TReadLoansVisitor }

procedure TReadLoansVisitor.Init;
begin
  Query.SQLText:= SQLReadLoans;
  // where clause start
  //if TPersonList(Visited).PersonsFilter.Active then
  //begin
  //  Query.SQL.Add(' WHERE');
  //  Query.SQL.Add(' '+TPersonList(Visited).PersonsFilter.Criteria);
  //  TPersonList(Visited).PersonsFilter.Active  := False;
  //end;
  //where clause end
end;

function TReadLoansVisitor.AcceptVisitor: Boolean;
begin
  Result:=inherited AcceptVisitor;
end;

procedure TReadLoansVisitor.SetupParams;
begin
  inherited SetupParams;
end;

procedure TReadLoansVisitor.MapRowToObject;
begin
  inherited MapRowToObject;
end;

{ TReadPersonsLkUpVisitor }

procedure TReadPersonsLkUpVisitor.Init;
begin
  Query.SQLText:= SQLLookUpPersons;
  // where clause start
  if TPersonsLookUp(Visited).ListFilter.Active then
  begin
    Query.SQL.Add(' WHERE');
    Query.SQL.Add(' '+TPersonsLookUp(Visited).ListFilter.Criteria);
    //TPersonsLookUp(Visited).ListFilter.Active  := False;
  end;
  //where clause end
end;

function TReadPersonsLkUpVisitor.AcceptVisitor: Boolean;
begin
  Result:=Visited is TPersonsLookUp;
end;

procedure TReadPersonsLkUpVisitor.SetupParams;
begin

end;

procedure TReadPersonsLkUpVisitor.MapRowToObject;
  var
    O : TPersonBasic;

  begin
    O:= TPersonBasic.Create;
    O.OID.AssignFromTIQuery('OID',Query);

    O.Name:= Query.FieldAsString['NAME'];
    O.ObjectState:=posClean;
    TPersonsLookUp(Visited).Add(O);
end;

{ TSaveServiceVisitor }

procedure TSaveServiceVisitor.Init;
begin
  Case Visited.ObjectState of
    posCreate:
      Query.SQLText:=SQLCreateService;
    posUpdate:
      Query.SQLText:=SQLUpdateService;
    posDelete:
      Query.SQLText:=SQLDeleteService;
  end;
end;

function TSaveServiceVisitor.AcceptVisitor: Boolean;
begin
  Result:=Visited is TService;
  Result:=Result and (Visited.ObjectState in [posCreate,posdelete,posUpdate]);
end;

procedure TSaveServiceVisitor.SetupParams;
var
  O : TService;

begin
  O:=TService(Visited);
  O.OID.AssignToTIQuery('OID',Query);
  if (Visited.ObjectState<>posDelete) then
  begin
    Query.ParamAsString['NAME']        := O.Name;
    Query.ParamAsFloat['MAXAMOUNT']    := O.MaxAmount;
    Query.ParamAsFloat['MINAMOUNT']    := O.MinAmount;
    Query.ParamAsFloat['INTERESTRATE'] := O.InterestRate;
    Query.ParamAsFloat['REBATE_RATE'] := O.RebateRate;
    Query.ParamAsInteger['MINTERM'] := O.MinTerm;
    Query.ParamAsInteger['MAXTERM'] := O.MaxTerm;
  end;
end;

{ TReadServicesVisitor }

procedure TReadServicesVisitor.Init;
begin
  Query.SQLText:= SQLReadServices;
end;

function TReadServicesVisitor.AcceptVisitor: Boolean;
begin
  Result:= Visited is TServiceList;
end;

procedure TReadServicesVisitor.SetupParams;
begin

end;

procedure TReadServicesVisitor.MapRowToObject;
var
  O : TService;

begin
  O:= TService.Create;
  O.OID.AssignFromTIQuery('OID',Query);

  O.Name:= Query.FieldAsString['NAME'];
  O.MaxAmount:= Query.FieldAsFloat['MAXAMOUNT'];
  O.MinAmount:= Query.FieldAsFloat['MINAMOUNT'];
  O.InterestRate:= Query.FieldAsFloat['INTERESTRATE'];
  o.RebateRate:= Query.FieldAsFloat['REBATE_RATE'];
  O.MinTerm:= Query.FieldAsInteger['MINTERM'];
  O.MaxTerm:= Query.FieldAsInteger['MAXTERM'];
  O.ObjectState:=posClean;
  TServiceList(Visited).Add(O);
end;

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
  // where clause start
  if TPersonList(Visited).PersonsFilter.Active then
  begin
    Query.SQL.Add(' WHERE');
    Query.SQL.Add(' '+TPersonList(Visited).PersonsFilter.Criteria);
    TPersonList(Visited).PersonsFilter.Active  := False;
  end;
  //where clause end
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

    RegReadVisitor(TReadServicesVisitor);
    RegSaveVisitor(TSaveServiceVisitor);

    RegReadVisitor(TReadPersonsLkUpVisitor);

    RegReadVisitor(TReadLoansVisitor);  sdsfsdfsdf write TSaveLoanVisitor
  end;
end;


initialization
  RegisterVisitors;

end.

