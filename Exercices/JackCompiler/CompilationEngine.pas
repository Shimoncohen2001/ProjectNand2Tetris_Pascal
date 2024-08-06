{$mode objfpc}

unit CompilationEngine;

interface

uses
  JackTokenizer, SysUtils, SymbolTable, VMWriter;

type
  TCompilationEngine = class
  private
    tokenizer: TJackTokenizer;
    vmWriter: TVMWriter;
    symbolTable: TSymbolTable;
    currentClassName: string;
    subroutineName:string;
    procedure expect(tokenType: TTokenType; const value: string);
    procedure compileExpression();
    procedure compileTerm();
    procedure compileExpressionList();
  public
    Fnumargs : integer;
    constructor Create(t: TJackTokenizer; const outputFileName: string);
    destructor Destroy; override;
    procedure compileClass();
    procedure compileClassVarDec();
    procedure compileSubroutine();
    procedure compileSubroutineBody();
    procedure compileParameterList();
    procedure compileVarDec();
    procedure compileStatements();
    procedure compileDo();
    procedure compileLet();
    procedure compileWhile();
    procedure compileReturn();
    procedure compileIf();
  end;

implementation

constructor TCompilationEngine.Create(t: TJackTokenizer; const outputFileName: string);
begin
  tokenizer := t;
  vmWriter := TVMWriter.Create(outputFileName);
  symbolTable := TSymbolTable.Create;
end;

destructor TCompilationEngine.Destroy;
begin
  vmWriter.close;
  vmWriter.Free;
  symbolTable.Free;
  inherited Destroy;
end;

procedure TCompilationEngine.expect(tokenType: TTokenType; const value: string);
begin
  if (tokenizer.TokenType <> tokenType) or (tokenizer.KeyWord <> value) then
    raise Exception.CreateFmt('Expected %s, but found %s', [value, tokenizer.KeyWord]);
  tokenizer.advance;
end;

procedure TCompilationEngine.compileClass();
begin
  tokenizer.advance; // 'class'
  tokenizer.advance; // className
  currentClassName := tokenizer.Identifier;
  tokenizer.advance; // '{'

  tokenizer.advance; // static |field ou |constructor |function |method
  writeln('dernier advance de la fonction compile class');
  writeln(tokenizer.currentToken);
  while tokenizer.TokenType = ttKeyword do
  begin
    if (tokenizer.KeyWord = 'static') or (tokenizer.KeyWord = 'field') then
      compileClassVarDec
    else
      break;
  end;

  while (tokenizer.TokenType = ttKeyword) and
    ((tokenizer.KeyWord = 'constructor') or (tokenizer.KeyWord = 'function') or (tokenizer.KeyWord = 'method')) do
  begin
    compileSubroutine;
  end;

  //tokenizer.advance; // '}'
end;

procedure TCompilationEngine.compileClassVarDec();
var
  kind: TSymbolKind;
  varName, varType: string;
begin

  // 'static' | 'field'

  if tokenizer.KeyWord = 'static' then
    kind := skStatic
  else
    kind := skField;

  tokenizer.advance; // 'static' | 'field'

  // type
  if tokenizer.TokenType = ttKeyword then
  begin
    varType := tokenizer.KeyWord;
  end
  else
  begin
    varType := tokenizer.Identifier;
  end;
  
  tokenizer.advance; // type

  // varName
  varName := tokenizer.Identifier;
  symbolTable.define(varName, varType, kind);
  tokenizer.advance; // varName

  while tokenizer.TokenType = ttSymbol do
  begin
    if tokenizer.symbol = ',' then
    begin
      tokenizer.advance; // ','

      varName := tokenizer.Identifier;
      symbolTable.define(varName, varType, kind);
      tokenizer.advance; // next varName
    end
    else
      break;
  end;

  // Expecting ';'
  //expect(ttSymbol, ';');

  tokenizer.advance; // ';'
end;

procedure TCompilationEngine.compileSubroutine();
var
  subroutineType: string;
  //numLocals: Integer;
begin
  // 'constructor' | 'function' | 'method'
  subroutineType := tokenizer.KeyWord;
  tokenizer.advance;  // 'void' | type
  tokenizer.advance; // subroutineName
  subroutineName := tokenizer.Identifier;
  tokenizer.advance; // symbol '(' 
  symbolTable.startSubroutine; // Reset subroutine scope

  // If the subroutine is a method, we need to account for "this" in the symbol table
  if subroutineType = 'method' then
    symbolTable.define('this', currentClassName, skArg);

  compileParameterList;
  
  //tokenizer.advance; // ')'
  //numLocals := symbolTable.varCount(skVar);
  //vmWriter.writeFunction(currentClassName + '.' + subroutineName, numLocals);

  compileSubroutineBody;

  // Write the function declaration to VM
  //numLocals := symbolTable.varCount(skVar);
  //vmWriter.writeFunction(currentClassName + '.' + subroutineName, numLocals);
end;

procedure TCompilationEngine.compileSubroutineBody();
var 
numLocals: Integer;
begin
  tokenizer.advance; // Symbol '{'
  tokenizer.advance; // Expecting either 'var' keyword or statement
  while (tokenizer.TokenType = ttKeyword) and (tokenizer.KeyWord = 'var') do
  begin
    compileVarDec;
    //tokenizer.advance; // Move to the next token after var declaration
  end;
  numLocals := symbolTable.varCount(skVar);
  vmWriter.writeFunction(currentClassName + '.' + subroutineName, numLocals);

  compileStatements;
  //expect(ttSymbol, '}');
  tokenizer.advance; // Consume the closing '}' symbol
end;


procedure TCompilationEngine.compileParameterList();
var
  varType, varName: string;
begin
  tokenizer.advance; // param or symbol ')'
  while tokenizer.TokenType <> ttSymbol do
  begin
    // type
    if tokenizer.TokenType = ttKeyword then
    begin
      varType := tokenizer.KeyWord;
    end
    else
    begin
      varType := tokenizer.Identifier;
    end;
    tokenizer.advance; // type

    // varName
    varName := tokenizer.Identifier;
    symbolTable.define(varName, varType, skArg);
    tokenizer.advance; // varName

    if tokenizer.TokenType = ttSymbol then
    begin
      if tokenizer.symbol = ',' then
      begin
        tokenizer.advance; // ','
      end
      else
        break;
    end;
  end;
end;


procedure TCompilationEngine.compileVarDec();
var
  varType, varName: string;
begin
  tokenizer.advance; // type: int, array ...
  if tokenizer.TokenType = ttKeyword then
  begin
    varType := tokenizer.KeyWord;
  end
  else
  begin
    varType := tokenizer.Identifier;
  end;

  tokenizer.advance; // varName
  varName := tokenizer.Identifier;
  symbolTable.define(varName, varType, skVar);
  tokenizer.advance; // symbol: ',' ';' ...
  while tokenizer.TokenType = ttSymbol do
  begin
    if tokenizer.symbol = ',' then
    begin
      tokenizer.advance; // consume another identifier
      varName := tokenizer.Identifier;
      symbolTable.define(varName, varType, skVar);
      tokenizer.advance;
    end
    else
      break;
  end;
  tokenizer.advance;
end;

procedure TCompilationEngine.compileStatements();
begin
  while tokenizer.TokenType = ttKeyword do
  begin
    if tokenizer.KeyWord = 'let' then
      compileLet
    else if tokenizer.KeyWord = 'if' then
      compileIf
    else if tokenizer.KeyWord = 'while' then
      compileWhile
    else if tokenizer.KeyWord = 'do' then
      compileDo
    else if tokenizer.KeyWord = 'return' then
      compileReturn
    else
      break;
  end;
end;

procedure TCompilationEngine.compileDo();
var
  //subroutineName: string;
  numArgs: Integer;
begin
  // Bouffé par la fonction qui a appelé do
  tokenizer.advance; // Consomme subroutineName ou className
  subroutineName := tokenizer.Identifier;
  tokenizer.advance; // Consomme le '.' ou '('
  if tokenizer.symbol = '.' then
  begin
    tokenizer.advance; // Consomme subroutineName
    subroutineName := subroutineName + '.' + tokenizer.Identifier;
    tokenizer.advance; // Consomme '('
  end;

  tokenizer.advance; // On bouffe le premier paramètre ou ')'
  compileExpressionList;
  numArgs := Fnumargs; // Compile les arguments et retourne le nombre d'arguments//erreur
  tokenizer.advance; // Consomme le symbole ';'
  // Écrit l'appel de sous-routine au VM
  vmWriter.writeCall(subroutineName, numArgs);
  vmWriter.writePop(sgTemp, 0);
  tokenizer.advance; // Consomme ';'
end;

procedure TCompilationEngine.compileLet();
var
  varName: string;
  kind: TSymbolKind;
  index: Integer;
begin
  tokenizer.advance; // varName
  varName := tokenizer.Identifier;
  kind := symbolTable.kindOf(varName);
  index := symbolTable.indexOf(varName);
  tokenizer.advance; // on bouffe soit un = soit un '['
  if tokenizer.symbol = '[' then
  begin
    tokenizer.advance; // on bouffe ce qui nous faut pour compileExpression
    compileExpression;
    tokenizer.advance; // j'ajoute ce advance pour que on affiche une seule fois le = après un ]. et dans ce cas il est affiché à la ligne 351 en dessous
  end;

  tokenizer.advance; // on bouffe ce qui nous faut pour compileExpression
  compileExpression;

  // Write the assignment to VM
  if kind <> skNone then
    vmWriter.writePop(sgLocal, index);// ct comme ca vmWriter.writePop(TSegment(kind), index);

  tokenizer.advance; // on bouffe le while, }
  
  // ajout d'un log dans la fonction compileLet
  writeln('affichage du current token en sortant pour la dernière fois de la fonction compileLet ', tokenizer.tokenType);
end;


procedure TCompilationEngine.compileWhile();
var
  whileLabel, endWhileLabel: string;
begin
  whileLabel := 'WHILE_EXP' + IntToStr(symbolTable.varCount(skVar)); // Unique label for each while statement
  endWhileLabel := 'WHILE_END' + IntToStr(symbolTable.varCount(skVar));

  tokenizer.advance; // '('

  tokenizer.advance;
  vmWriter.writeLabel(whileLabel); // Write while condition label to VM
  compileExpression;
  vmWriter.writeArithmetic(vmNot); // Negate the expression for the while condition
  vmWriter.writeIf(endWhileLabel); // Write conditional goto to endWhileLabel


  tokenizer.advance; // '{'

  tokenizer.advance;
  compileStatements;

  vmWriter.writeGoto(whileLabel); // Go back to the while condition
  vmWriter.writeLabel(endWhileLabel); // Write the end label for the while loop

  tokenizer.advance;
end;

procedure TCompilationEngine.compileReturn();
begin
  tokenizer.advance; // 'return'
  writeln('je suis rentrer dans CompileReturn!');
  if tokenizer.TokenType <> ttSymbol then
  begin
    compileExpression;
    vmWriter.writeReturn;
  end
  else
    vmWriter.writePush(sgConstant, 0); // Push constant 0 if no return value
    vmWriter.writeReturn;

  //expect(ttSymbol, ';');
  tokenizer.advance;// ';'
end;

procedure TCompilationEngine.compileIf();
var
  trueLabel, falseLabel, endLabel: string;
begin
  tokenizer.advance; // Consomme 'if'
  tokenizer.advance; // Consomme '('
  compileExpression;
  tokenizer.advance; // Consomme ')'
  tokenizer.advance; // Consomme '{'
  compileStatements;
  tokenizer.advance; // Consomme '}'
  trueLabel := 'IF_TRUE' + IntToStr(symbolTable.varCount(skVar));
  falseLabel := 'IF_FALSE' + IntToStr(symbolTable.varCount(skVar));
  endLabel := 'IF_END' + IntToStr(symbolTable.varCount(skVar));

  vmWriter.writeIf(trueLabel);
  vmWriter.writeGoto(falseLabel);
  vmWriter.writeLabel(trueLabel);

  if (tokenizer.TokenType = ttKeyword) and (tokenizer.KeyWord = 'else') then
  begin
    tokenizer.advance; // Consomme 'else'
    tokenizer.advance; // Consomme '{'
    compileStatements;
    tokenizer.advance; // Consomme '}'
  end;

  vmWriter.writeLabel(falseLabel);
  vmWriter.writeGoto(endLabel);
  vmWriter.writeLabel(endLabel);
end;


procedure TCompilationEngine.compileExpression();
var
  op: string;
begin
  writeln(tokenizer.tokenType,tokenizer.currentToken);
  compileTerm;

  while (tokenizer.TokenType = ttSymbol) and (tokenizer.symbol in ['+', '-', '*', '/', '&', '|', '<', '>', '=']) do
  begin
    op := tokenizer.symbol;
    tokenizer.advance; // consume operator
    compileTerm;
    case op of
      '+': vmWriter.writeArithmetic(vmAdd);
      '-': vmWriter.writeArithmetic(vmSub);
      '*': vmWriter.writeCall('Math.multiply', 2);
      '/': vmWriter.writeCall('Math.divide', 2);
      '&': vmWriter.writeArithmetic(vmAnd);
      '|': vmWriter.writeArithmetic(vmOr);
      '<': vmWriter.writeArithmetic(vmLt);
      '>': vmWriter.writeArithmetic(vmGt);
      '=': vmWriter.writeArithmetic(vmEq);
    end;
  end;
end;

procedure TCompilationEngine.compileTerm();
var
  varName, op: string;//subroutineName a rajouter
  kind: TSymbolKind;
  index: Integer;
  i: Integer;
begin
  case tokenizer.TokenType of
    ttIntConst:
    begin
      vmWriter.writePush(sgConstant, tokenizer.IntVal);
      tokenizer.advance; // Consomme integer
    end;
    ttStringConst:
    begin
      // Handling string constants
      vmWriter.writePush(sgConstant, Length(tokenizer.StringVal));
      vmWriter.writeCall('String.new', 1);
      for i := 1 to Length(tokenizer.StringVal) do
      begin
        vmWriter.writePush(sgConstant, Ord(tokenizer.StringVal[i]));
        vmWriter.writeCall('String.appendChar', 2);
      end;
      tokenizer.advance; // Consomme string
    end;
    ttKeyword:
    begin
      // Handle 'true', 'false', 'null', 'this'
      if tokenizer.KeyWord = 'true' then
      begin
        vmWriter.writePush(sgConstant, 1);// il y avait 0
        vmWriter.writeArithmetic(vmNeg);// il y avait vmNot
      end
      else if (tokenizer.KeyWord = 'false') or (tokenizer.KeyWord = 'null') then
        vmWriter.writePush(sgConstant, 0)
      else if tokenizer.KeyWord = 'this' then
        vmWriter.writePush(sgPointer, 0);
      tokenizer.advance; // Consomme keyword
    end;
    ttIdentifier:
    begin
      writeln('je suis dans le compile term');
      varName := tokenizer.currentToken;
      writeln(varName);
      writeln('je suis dans le compile term');
      kind := symbolTable.kindOf(varName);// sknone
      writeln('je suis dans le compile term');
      //symbolTable.define(varName, tokenizer.identifier, skField);
      index := symbolTable.indexOf(varName);
      writeln('je suis dans le compile term');
      tokenizer.advance; // Consomme identifier
      if tokenizer.TokenType = ttSymbol then
      begin
        if tokenizer.Symbol = '[' then
        begin
          tokenizer.advance; // Consomme '['
          compileExpression;
          vmWriter.writePush(TSegment(kind), index);
          vmWriter.writeArithmetic(vmAdd);
          vmWriter.writePop(sgPointer, 1);
          vmWriter.writePush(sgThat, 0);
          tokenizer.advance; // Consomme ']'
        end
        else if tokenizer.Symbol = '(' then
        begin
          tokenizer.advance; // Consomme '('
          compileExpressionList;
          vmWriter.writeCall(varName, 0); // Le nombre d'arguments devrait être passé correctement// je vien sde modiff
          tokenizer.advance; // Consomme ')'
        end
        else if tokenizer.Symbol = '.' then
        begin
          tokenizer.advance; // Consomme '.'
          subroutineName := tokenizer.Identifier;
          tokenizer.advance; // Consomme subroutineName
          tokenizer.advance; // Consomme '('
          compileExpressionList;
          vmWriter.writeCall(varName + '.' + subroutineName, Fnumargs); // Le nombre d'arguments devrait être passé correctement
          tokenizer.advance; // Consomme ')'
        end
        else
        begin
          // Variable access
          vmWriter.writePush(sgLocal, index);//vmWriter.writePush(TSegment(kind), index);
        end;
      end
      else
      begin
        // Variable access
        vmWriter.writePush(TSegment(kind), index);
      end;
    end;
    ttSymbol:
    begin
      if tokenizer.Symbol = '(' then
      begin
        tokenizer.advance; // Consomme '('
        compileExpression;
        tokenizer.advance; // Consomme ')'
      end
      else if tokenizer.Symbol in ['-', '~'] then
      begin
        op := tokenizer.Symbol;
        tokenizer.advance; // Consomme '-' ou '~'
        compileTerm;
        if op = '-' then
          vmWriter.writeArithmetic(vmNeg)
        else
          vmWriter.writeArithmetic(vmNot);
      end;
    end;
  end;
end;

procedure TCompilationEngine.compileExpressionList();
begin
  Fnumargs:=0;
  writeln(tokenizer.tokenType);
  if (((tokenizer.TokenType = ttSymbol) and (tokenizer.Symbol = '(')) or 
      (tokenizer.TokenType = ttIdentifier) or 
      (tokenizer.TokenType = ttIntConst) or 
      (tokenizer.TokenType = ttKeyword) or 
      (tokenizer.TokenType = ttStringConst)) then
  begin
    compileExpression;
    if ((tokenizer.TokenType=ttSymbol) and (tokenizer.Symbol=')')) then
    begin
      Fnumargs:=1;
    end
    else
    begin
      Fnumargs:=1;
        while tokenizer.Symbol = ',' do
        begin
          Inc(Fnumargs);
          tokenizer.advance; // Consume the comma
          compileExpression;
        end;
      end;
  end;
  
end;

end.