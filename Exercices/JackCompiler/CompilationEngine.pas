
{$mode objfpc}
unit CompilationEngine;

interface

uses
  JackTokenizer,SysUtils;

type
  TCompilationEngine = class
  private
    tokenizer: TJackTokenizer;
    outputFile: TextFile;
    //symbolTable: TSymbolTable;
    procedure writeLine(const line: string);
    //procedure //expect(tokenType: TTokenType; const value: string);
  public
    constructor Create(t: TJackTokenizer; outputFileName: string);
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
    procedure compileExpression();
    procedure compileTerm();
    procedure compileExpressionList();
  end;

implementation

constructor TCompilationEngine.Create(t: TJackTokenizer; outputFileName: string);
begin
  tokenizer := t;
  AssignFile(outputFile, outputFileName);
  Rewrite(outputFile);
  //symbolTable := TSymbolTable.Create;
end;

destructor TCompilationEngine.Destroy;
begin
  CloseFile(outputFile);
  //symbolTable.Free;
  inherited Destroy;
end;

procedure TCompilationEngine.writeLine(const line: string);
begin
  WriteLn(outputFile, line);
end;

//procedure TCompilationEngine.////expect(tokenType: TTokenType; const value: string);
//begin
//  if (tokenizer.TokenType <> tokenType) or (tokenizer.KeyWord <> value) then
//    raise Exception.CreateFmt('////expected %s, but found %s', [value, tokenizer.KeyWord]);
//  tokenizer.Advance;
//end;

procedure TCompilationEngine.compileClass();
begin
  writeLine('<class>');
  tokenizer.Advance; // 'class'
  ////expect(KEYWORD, 'class');
  writeLine('<keyword> class </keyword>');
  
  tokenizer.Advance; // className
  writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
  
  tokenizer.Advance; // '{'
  ////expect(SYMBOL, '{');
  writeLine('<symbol> { </symbol>');
  
  tokenizer.Advance;
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

  //expect(SYMBOL, '}');
  writeLine('<symbol> } </symbol>');
  writeLine('</class>');
end;

procedure TCompilationEngine.compileClassVarDec();
begin
  writeLine('<classVarDec>');
  // 'static' | 'field'
  writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>');
  tokenizer.Advance;
  
  // type
  if tokenizer.TokenType = ttKeyword then
    writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>')
  else
    writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
  tokenizer.Advance;
  
  // varName
  writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
  tokenizer.Advance;

  while tokenizer.TokenType = ttSymbol do
  begin
    if tokenizer.symbol = ',' then
    begin
      writeLine('<symbol> , </symbol>');
      tokenizer.Advance;
      writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
      tokenizer.Advance;
    end
    else
      break;
  end;

  //expect(SYMBOL, ';');
  writeLine('<symbol> ; </symbol>');
  writeLine('</classVarDec>');
  tokenizer.Advance;
end;

procedure TCompilationEngine.compileSubroutine();
begin
  writeLine('<subroutineDec>');
  // 'constructor' | 'function' | 'method'
  writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>');
  tokenizer.Advance;
  
  // 'void' | type
  if tokenizer.TokenType = ttKeyword then
    writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>')
  else
    writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
  tokenizer.Advance;

  // subroutineName
  writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
  tokenizer.Advance;

  //expect(SYMBOL, '(');
  writeLine('<symbol> ( </symbol>');
  
  compileParameterList;

  //expect(SYMBOL, ')');
  writeLine('<symbol> ) </symbol>');

  compileSubroutineBody;
  writeLine('</subroutineDec>');
end;

procedure TCompilationEngine.compileSubroutineBody();
begin
  writeLine('<subroutineBody>');
  //expect(SYMBOL, '{');
  writeLine('<symbol> { </symbol>');

  tokenizer.Advance;
  while tokenizer.TokenType = ttKeyword do
  begin
    if tokenizer.KeyWord = 'var' then
      compileVarDec
    else
      break;
  end;

  compileStatements;

  //expect(SYMBOL, '}');
  writeLine('<symbol> } </symbol>');
  writeLine('</subroutineBody>');
  tokenizer.Advance;
end;

procedure TCompilationEngine.compileParameterList();
begin
  writeLine('<parameterList>');
  tokenizer.Advance;
  while tokenizer.TokenType <> ttSymbol do
  begin
    // type
    if tokenizer.TokenType = ttKeyword then
      writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>')
    else
      writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
    tokenizer.Advance;

    // varName
    writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
    tokenizer.Advance;

    if tokenizer.TokenType = ttSymbol then
    begin
      if tokenizer.symbol = ',' then
      begin
        writeLine('<symbol> , </symbol>');
        tokenizer.Advance;
      end
      else
        break;
    end;
  end;
  writeLine('</parameterList>');
end;

procedure TCompilationEngine.compileVarDec();
begin
  writeLine('<varDec>');
  writeLine('<keyword> var </keyword>');
  tokenizer.Advance;

  // type
  if tokenizer.TokenType = ttKeyword then
    writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>')
  else
    writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
  tokenizer.Advance;

  // varName
  writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
  tokenizer.Advance;

  while tokenizer.TokenType = ttSymbol do
  begin
    if tokenizer.symbol = ',' then
    begin
      writeLine('<symbol> , </symbol>');
      tokenizer.Advance;
      writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
      tokenizer.Advance;
    end
    else
      break;
  end;

  //expect(SYMBOL, ';');
  writeLine('<symbol> ; </symbol>');
  writeLine('</varDec>');
  tokenizer.Advance;
end;

procedure TCompilationEngine.compileStatements();
begin
  writeLine('<statements>');
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
  writeLine('</statements>');
end;

procedure TCompilationEngine.compileDo();
begin
  writeLine('<doStatement>');
  writeLine('<keyword> do </keyword>');
  tokenizer.Advance;

  // subroutineCall
  writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
  tokenizer.Advance;

  //expect(SYMBOL, '(');
  writeLine('<symbol> ( </symbol>');

  compileExpressionList;

  //expect(SYMBOL, ')');
  writeLine('<symbol> ) </symbol>');

  //expect(SYMBOL, ';');
  writeLine('<symbol> ; </symbol>');
  writeLine('</doStatement>');
  tokenizer.Advance;
end;

procedure TCompilationEngine.compileLet();
begin
  writeLine('<letStatement>');
  writeLine('<keyword> let </keyword>');
  tokenizer.Advance;

  // varName
  writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
  tokenizer.Advance;

  if tokenizer.symbol = '[' then
  begin
    writeLine('<symbol> [ </symbol>');
    tokenizer.Advance;

    compileExpression;

    //expect(SYMBOL, ']');
    writeLine('<symbol> ] </symbol>');
  end;

  //expect(SYMBOL, '=');
  writeLine('<symbol> = </symbol>');

  compileExpression;

  //expect(SYMBOL, ';');
  writeLine('<symbol> ; </symbol>');
  writeLine('</letStatement>');
  tokenizer.Advance;
end;

procedure TCompilationEngine.compileWhile();
begin
  writeLine('<whileStatement>');
  writeLine('<keyword> while </keyword>');
  tokenizer.Advance;

  //expect(SYMBOL, '(');
  writeLine('<symbol> ( </symbol>');

  compileExpression;

  //expect(SYMBOL, ')');
  writeLine('<symbol> ) </symbol>');

  //expect(SYMBOL, '{');
  writeLine('<symbol> { </symbol>');

  compileStatements;

  //expect(SYMBOL, '}');
  writeLine('<symbol> } </symbol>');
  writeLine('</whileStatement>');
  tokenizer.Advance;
end;

procedure TCompilationEngine.compileReturn();
begin
  writeLine('<returnStatement>');
  writeLine('<keyword> return </keyword>');
  tokenizer.Advance;

  if tokenizer.TokenType <> ttSymbol then
    compileExpression;

  //expect(SYMBOL, ';');
  writeLine('<symbol> ; </symbol>');
  writeLine('</returnStatement>');
  tokenizer.Advance;
end;

procedure TCompilationEngine.compileIf();
begin
  writeLine('<ifStatement>');
  writeLine('<keyword> if </keyword>');
  tokenizer.Advance;

  //expect(SYMBOL, '(');
  writeLine('<symbol> ( </symbol>');

  compileExpression;

  //expect(SYMBOL, ')');
  writeLine('<symbol> ) </symbol>');

  //expect(SYMBOL, '{');
  writeLine('<symbol> { </symbol>');

  compileStatements;

  //expect(SYMBOL, '}');
  writeLine('<symbol> } </symbol>');

  if tokenizer.KeyWord = 'else' then
  begin
    tokenizer.Advance;
    writeLine('<keyword> else </keyword>');

    //expect(SYMBOL, '{');
    writeLine('<symbol> { </symbol>');

    compileStatements;

    //expect(SYMBOL, '}');
    writeLine('<symbol> } </symbol>');
  end;

  writeLine('</ifStatement>');
  tokenizer.Advance;
end;

procedure TCompilationEngine.compileExpression();
begin
  writeLine('<expression>');
  compileTerm;

  while (tokenizer.TokenType = ttSymbol) and (tokenizer.symbol in ['+', '-', '*', '/', '&', '|', '<', '>', '=']) do
  begin
    writeLine('<symbol> ' + tokenizer.symbol + ' </symbol>');
    tokenizer.Advance;
    compileTerm;
  end;

  writeLine('</expression>');
end;

procedure TCompilationEngine.compileTerm();
begin
  writeLine('<term>');

  case tokenizer.TokenType of
    ttIntConst:
    begin
      writeLine('<integerConstant> ' + IntToStr(tokenizer.IntVal) + ' </integerConstant>');
      tokenizer.Advance;
    end;
    ttStringConst:
    begin
      writeLine('<stringConstant> ' + tokenizer.StringVal + ' </stringConstant>');
      tokenizer.Advance;
    end;
    ttKeyword:
    begin
      writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>');
      tokenizer.Advance;
    end;
    ttIdentifier:
    begin
      writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
      tokenizer.Advance;

      if tokenizer.symbol = '[' then
      begin
        writeLine('<symbol> [ </symbol>');
        tokenizer.Advance;
        compileExpression;
        //expect(SYMBOL, ']');
        writeLine('<symbol> ] </symbol>');
      end
      else if tokenizer.symbol = '(' then
      begin
        writeLine('<symbol> ( </symbol>');
        tokenizer.Advance;
        compileExpressionList;
        //expect(SYMBOL, ')');
        writeLine('<symbol> ) </symbol>');
      end
      else if tokenizer.symbol = '.' then
      begin
        writeLine('<symbol> . </symbol>');
        tokenizer.Advance;
        writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
        tokenizer.Advance;
        //expect(SYMBOL, '(');
        writeLine('<symbol> ( </symbol>');
        compileExpressionList;
        //expect(SYMBOL, ')');
        writeLine('<symbol> ) </symbol>');
      end;
    end;
    ttSymbol:
    begin
      if tokenizer.symbol = '(' then
      begin
        writeLine('<symbol> ( </symbol>');
        tokenizer.Advance;
        compileExpression;
        //expect(SYMBOL, ')');
        writeLine('<symbol> ) </symbol>');
      end
      else if tokenizer.symbol in ['-', '~'] then
      begin
        writeLine('<symbol> ' + tokenizer.symbol + ' </symbol>');
        tokenizer.Advance;
        compileTerm;
      end;
    end;
  end;

  writeLine('</term>');
end;

procedure TCompilationEngine.compileExpressionList();
begin
  writeLine('<expressionList>');

  if tokenizer.symbol <> ')' then
  begin
    compileExpression;

    while tokenizer.symbol = ',' do
    begin
      writeLine('<symbol> , </symbol>');
      tokenizer.Advance;
      compileExpression;
    end;
  end;

  writeLine('</expressionList>');
end;

end.
