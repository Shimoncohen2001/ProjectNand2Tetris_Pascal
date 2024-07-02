//unit SymbolTable;
//
//interface
//
//uses
//  SysUtils, Generics.Collections;
//
//type
//  TSymbolKind = (skStatic, skField, skArg, skVar);
//
//  TSymbolInfo = record
//    VarType: string;
//    Kind: TSymbolKind;
//    Index: Integer;
//  end;
//
//  TSymbolTable = class
//  private
//    classScope: TDictionary<string, TSymbolInfo>;
//    subroutineScope: TDictionary<string, TSymbolInfo>;
//    indexCounters: array[TSymbolKind] of Integer;
//  public
//    constructor Create();
//    procedure startSubroutine();
//    procedure define(name, varType: string; kind: TSymbolKind);
//    function varCount(kind: TSymbolKind): Integer;
//    function kindOf(name: string): TSymbolKind;
//    function typeOf(name: string): string;
//    function indexOf(name: string): Integer;
//  end;
//
//implementation
//
//constructor TSymbolTable.Create();
//begin
//  classScope := TDictionary<string, TSymbolInfo>.Create;
//  subroutineScope := TDictionary<string, TSymbolInfo>.Create;
//  indexCounters[skStatic] := 0;
//  indexCounters[skField] := 0;
//  indexCounters[skArg] := 0;
//  indexCounters[skVar] := 0;
//end;
//
//procedure TSymbolTable.startSubroutine();
//begin
//  subroutineScope.Clear;
//  indexCounters[skArg] := 0;
//  indexCounters[skVar] := 0;
//end;
//
//procedure TSymbolTable.define(name, varType: string; kind: TSymbolKind);
//var
//  info: TSymbolInfo;
//begin
//  info.VarType := varType;
//  info.Kind := kind;
//  info.Index := indexCounters[kind];
//  indexCounters[kind] := indexCounters[kind] + 1;
//
//  if (kind = skStatic) or (kind = skField) then
//    classScope.Add(name, info)
//  else
//    subroutineScope.Add(name, info);
//end;
//
//function TSymbolTable.varCount(kind: TSymbolKind): Integer;
//begin
//  Result := indexCounters[kind];
//end;
//
//function TSymbolTable.kindOf(name: string): TSymbolKind;
//begin
//  if subroutineScope.ContainsKey(name) then
//    Result := subroutineScope[name].Kind
//  else if classScope.ContainsKey(name) then
//    Result := classScope[name].Kind
//  else
//    raise Exception.Create('Identifier not found: ' + name);
//end;
//
//function TSymbolTable.typeOf(name: string): string;
//begin
//  if subroutineScope.ContainsKey(name) then
//    Result := subroutineScope[name].VarType
//  else if classScope.ContainsKey(name) then
//    Result := classScope[name].VarType
//  else
//    raise Exception.Create('Identifier not found: ' + name);
//end;
//
//function TSymbolTable.indexOf(name: string): Integer;
//begin
//  if subroutineScope.ContainsKey(name) then
//    Result := subroutineScope[name].Index
//  else if classScope.ContainsKey(name) then
//    Result := classScope[name].Index
//  else
//    raise Exception.Create('Identifier not found: ' + name);
//end;
//
//end.
//