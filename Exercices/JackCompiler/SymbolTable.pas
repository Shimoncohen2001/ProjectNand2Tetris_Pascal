{$mode objfpc}

unit SymbolTable;

interface

uses
  SysUtils, Generics.Collections;

type
  // Enumeration pour les différents types de symboles
  TSymbolKind = (skStatic, skField, skArg, skVar, skNone);

  // Enregistrement pour stocker les informations sur chaque symbole
  TSymbolInfo = record
    varType: string;
    kind: TSymbolKind;
    index: Integer;
  end;

  // Classe pour gérer la table des symboles
  TSymbolTable = class
  private
    classScope: specialize TDictionary<string, TSymbolInfo>; // Portée de classe
    subroutineScope: specialize TDictionary<string, TSymbolInfo>; // Portée de sous-programme
    indexCounters: array[TSymbolKind] of Integer; // Compteurs d'index pour chaque type de symbole
  public
    constructor Create();
    destructor Destroy; override;
    procedure startSubroutine();
    procedure define(name, varType: string; kind: TSymbolKind);
    function varCount(kind: TSymbolKind): Integer;
    function kindOf(name: string): TSymbolKind;
    function typeOf(name: string): string;
    function indexOf(name: string): Integer;
  end;

implementation

constructor TSymbolTable.Create;
begin
  classScope := specialize TDictionary<string, TSymbolInfo>.Create;
  subroutineScope := specialize TDictionary<string, TSymbolInfo>.Create;
  FillChar(indexCounters, SizeOf(indexCounters), 0); // Initialise les compteurs à zéro
end;

destructor TSymbolTable.Destroy;
begin
  classScope.Free;
  subroutineScope.Free;
  inherited Destroy;
end;

procedure TSymbolTable.startSubroutine;
begin
  subroutineScope.Clear; // Réinitialise la portée de sous-programme
  indexCounters[skArg] := 0;
  indexCounters[skVar] := 0;
end;

procedure TSymbolTable.define(name, varType: string; kind: TSymbolKind);
var
  symbolInfo: TSymbolInfo;
begin
  writeln('je suis dans la function define avec name = ');
  writeln(name);
  symbolInfo.varType := varType;
  symbolInfo.kind := kind;
  symbolInfo.index := indexCounters[kind];
  Inc(indexCounters[kind]);
  writeln(kind);
  case kind of
    skStatic, skField:
    begin
      writeln('je suis dans le premier case');
      classScope.Add(name, symbolInfo);
      writeln('jai ajoute a classScope');
    end;
    skArg, skVar:
      subroutineScope.Add(name, symbolInfo);
  end;
end;

function TSymbolTable.varCount(kind: TSymbolKind): Integer;
begin
  Result := indexCounters[kind];
end;

function TSymbolTable.kindOf(name: string): TSymbolKind;
begin
  if subroutineScope.ContainsKey(name) then
    Result := subroutineScope[name].kind
  else if classScope.ContainsKey(name) then
    Result := classScope[name].kind
  else
    Result := skNone;
end;

function TSymbolTable.typeOf(name: string): string;
begin
  if subroutineScope.ContainsKey(name) then
    Result := subroutineScope[name].varType
  else if classScope.ContainsKey(name) then
    Result := classScope[name].varType
  else
    raise Exception.Create('Symbol not found: ' + name);
end;

function TSymbolTable.indexOf(name: string): Integer;
begin
  writeln(subroutineScope.ContainsKey(name));
  if subroutineScope.ContainsKey(name) then
    Result := subroutineScope[name].index
  else if classScope.ContainsKey(name) then
    Result := classScope[name].index
  else 
    Result := 0;
  //else
  //  raise Exception.Create('Symbol not found: ' + name);
end;

end.
