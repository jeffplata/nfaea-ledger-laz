unit ObjectListFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TObjectListFilter = class
  private
    FActive: Boolean;
    FCriteria: string;
    //FUIValue: Variant;
    //FWhereClause: string;
  public
    property Active: Boolean read FActive write FActive;
    //property UIValue: Variant read FUIValue write FUIValue;
    property Criteria: string read FCriteria write FCriteria;
    //property WhereClause: string read FWhereClause write FWhereClause;
    procedure Init;
    constructor Create;
  end;

implementation

{ TObjectListFilter }

procedure TObjectListFilter.Init;
begin
  FCriteria:= '';
  Active:= False;
end;

constructor TObjectListFilter.Create;
begin
  Init;
end;



end.

