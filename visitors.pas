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

  { TSaveLoanVisitor }

  TSaveLoanVisitor = Class(TtiVisitorUpdate)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
  end;


  procedure RegisterVisitors;


implementation

uses
  ledgermanager
  ;


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
  SQLCreateLoan =
      'INSERT INTO LOAN (OID, PERSON_OID, SERVICE_OID, DOCNUMBER, DOCDATE, NOTES, PRINCIPAL,'+
      '    INTEREST, TOTAL, PREVIOUSBALANCE, REBATES, NETPROCEEDS, INTERESTRATE,'+
      '    REBATERATE, TERMS, AMORTIZATION, PAYMENTSTART, PAYMENTEND)'+
      'VALUES ('+
      '    :OID, '+
      '    :PERSON_OID, '+
      '    :SERVICE_OID, '+
      '    :DOCNUMBER, '+
      '    :DOCDATE, '+
      '    :NOTES, '+
      '    :PRINCIPAL, '+
      '    :INTEREST, '+
      '    :TOTAL, '+
      '    :PREVIOUSBALANCE, '+
      '    :REBATES, '+
      '    :NETPROCEEDS, '+
      '    :INTERESTRATE, '+
      '    :REBATERATE, '+
      '    :TERMS, '+
      '    :AMORTIZATION, '+
      '    :PAYMENTSTART, '+
      '    :PAYMENTEND'+
      ');'
      ;

  SQLUpdateLoan =
      'UPDATE LOAN a '+
      'SET '+
      '    a.PERSON_OID = :PERSON_OID, '+
      '    a.SERVICE_OID = :SERVICE_OID, '+
      '    a.DOCNUMBER = :DOCNUMBER, '+
      '    a.DOCDATE = :DOCDATE, '+
      '    a.NOTES = :NOTES, '+
      '    a.PRINCIPAL = :PRINCIPAL, '+
      '    a.INTEREST = :INTEREST, '+
      '    a.TOTAL = :TOTAL, '+
      '    a.PREVIOUSBALANCE = :PREVIOUSBALANCE, '+
      '    a.REBATES = :REBATES, '+
      '    a.NETPROCEEDS = :NETPROCEEDS, '+
      '    a.INTERESTRATE = :INTERESTRATE, '+
      '    a.REBATERATE = :REBATERATE, '+
      '    a.TERMS = :TERMS, '+
      '    a.AMORTIZATION = :AMORTIZATION, '+
      '    a.PAYMENTSTART = :PAYMENTSTART, '+
      '    a.PAYMENTEND = :PAYMENTEND '+
      'WHERE '+
      '    a.OID = :OID'
      ;

  SQLDeleteLoan = 'delete from LOAN where OID=:OID';

{ TSaveLoanVisitor }

procedure TSaveLoanVisitor.Init;
begin
  Case Visited.ObjectState of
    posCreate:
      Query.SQLText:=SQLCreateLoan;
    posUpdate:
      Query.SQLText:=SQLUpdateLoan;
    posDelete:
      Query.SQLText:=SQLDeleteLoan;
  end;
end;

function TSaveLoanVisitor.AcceptVisitor: Boolean;
begin
  Result:=Visited is TLoan;
  Result:=Result and (Visited.ObjectState in [posCreate,posdelete,posUpdate]);
end;

procedure TSaveLoanVisitor.SetupParams;
  var
    O : TLoan;
  begin
    O:=TLoan(Visited);
    O.OID.AssignToTIQuery('OID',Query);
    if (Visited.ObjectState<>posDelete) then
    begin
      Query.ParamAsString['PERSON_OID']     := O.Person.OID.AsString;
      Query.ParamAsString['SERVICE_OID']    := O.Service.OID.AsString;
      Query.ParamAsString['DOCNUMBER']      := O.DocNumber;
      Query.ParamAsDateTime['DOCDATE']      := O.DocDate;
      Query.ParamAsString['NOTES']          := O.Notes;
      Query.ParamAsFloat['PRINCIPAL']       := O.Principal;
      Query.ParamAsFloat['INTEREST']        := O.Interest;
      Query.ParamAsFloat['TOTAL']           := O.Total;
      Query.ParamAsFloat['PREVIOUSBALANCE'] := O.PreviousBalance;
      Query.ParamAsFloat['REBATES']         := O.Rebates;
      Query.ParamAsFloat['NETPROCEEDS']     := O.NetProceeds;
      Query.ParamAsFloat['INTERESTRATE']    := O.InterestRate;
      Query.ParamAsFloat['REBATERATE']      := O.RebateRate;
      Query.ParamAsInteger['TERMS']         := O.Terms;
      Query.ParamAsFloat['AMORTIZATION']    := O.Amortization;
      Query.ParamAsDateTime['PAYMENTSTART'] := O.PaymentStart;
      Query.ParamAsDateTime['PAYMENTEND']   := O.PaymentEnd;
    end;
  end;


{ TReadLoansVisitor }

procedure TReadLoansVisitor.Init;
begin
  Query.SQLText:= SQLReadLoans;
end;

function TReadLoansVisitor.AcceptVisitor: Boolean;
begin
  //Result:= inherited AcceptVisitor;
  Result := Visited is TLoanList;
end;

procedure TReadLoansVisitor.SetupParams;
begin

end;

procedure TReadLoansVisitor.MapRowToObject;
  var
    O : TLoan;

    S:TService;
    P:TPersonBasic;

  begin
    O:= TLoan.Create;
    O.OID.AssignFromTIQuery('OID',Query);

    //O.Person.OID.AsString  := Query.FieldAsString['PERSON_OID'];
    //O.Service.OID.AsString := Query.FieldAsString['SERVICE_OID'];

    O.DocNumber            := Query.FieldAsString['DOCNUMBER'];
    O.DocDate              := Query.FieldAsDateTime['DOCDATE'];
    O.Notes                := Query.FieldAsString['NOTES'];
    O.Principal            := Query.FieldAsFloat['PRINCIPAL'];
    O.Interest             := Query.FieldAsFloat['INTEREST'];
    O.Total                := Query.FieldAsFloat['TOTAL'];
    O.PreviousBalance      := Query.FieldAsFloat['PREVIOUSBALANCE'];
    O.Rebates              := Query.FieldAsFloat['REBATES'];
    O.NetProceeds          := Query.FieldAsFloat['NETPROCEEDS'];
    O.InterestRate         := Query.FieldAsFloat['INTERESTRATE'];
    O.RebateRate           := Query.FieldAsFloat['REBATERATE'];
    O.Terms                := Query.FieldAsInteger['TERMS'];
    O.Amortization         := Query.FieldAsFloat['AMORTIZATION'];
    O.PaymentStart         := Query.FieldAsDateTime['PAYMENTSTART'];
    O.PaymentEnd           := Query.FieldAsDateTime['PAYMENTEND'];

    P := TPersonBasic(gLedgerManager.PersonList.Find(Query.FieldAsString['PERSON_OID']) );
    S := TService(gLedgerManager.Services.Find(Query.FieldAsString['SERVICE_OID']) );

    if P <> nil then
      O.Person.Assign(P);
    if S <> nil then
      O.Service.Assign(S);


    O.Person.OID.AssignFromTIQuery('PERSON_OID', Query);
    O.Service.OID.AssignFromTIQuery('SERVICE_OID',Query);

    //o.service.OID.assign(s.OID);

    //O.Service.Assign( TService(gLedgerManager.Services.Find(Query.FieldAsString['SERVICE_OID']) ) );
    //O.Person.Assign( TPerson(gLedgerManager.PersonList.Find(Query.FieldAsString['PERSON_OID']) ) );

    O.ObjectState:=posClean;
    TLoanList(Visited).Add(O);
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
    Query.ParamAsFloat['REBATE_RATE']  := O.RebateRate;
    Query.ParamAsInteger['MINTERM']    := O.MinTerm;
    Query.ParamAsInteger['MAXTERM']    := O.MaxTerm;
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

    RegReadVisitor(TReadLoansVisitor);
    RegSaveVisitor(TSaveLoanVisitor);
  end;
end;


initialization
  RegisterVisitors;

end.

