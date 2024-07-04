
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
//  tokenizer.advance;
//end;

procedure TCompilationEngine.compileClass();
begin
  writeLine('<class>');
  tokenizer.advance; // 'class'
  writeLine('<keyword> class </keyword>');
  
  tokenizer.advance; // className
  writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
  
  tokenizer.advance; // '{'
  writeLine('<symbol> { </symbol>');
  
  tokenizer.advance; // static |field    ou |constructor |function |method
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

  writeLine('<symbol> } </symbol>');
  writeLine('</class>');
end;

procedure TCompilationEngine.compileClassVarDec();
begin
  writeLine('<classVarDec>');
  // 'static' | 'field'
  writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>');
  tokenizer.advance;
  
  // type
  if tokenizer.TokenType = ttKeyword then
    writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>')
  else
    writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
  tokenizer.advance;
  
  // varName
  writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
  tokenizer.advance;

  while tokenizer.TokenType = ttSymbol do
  begin
    if tokenizer.symbol = ',' then
    begin
      writeLine('<symbol> , </symbol>');
      tokenizer.advance;
      writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
      tokenizer.advance;
    end
    else
      break;
  end;

  //expect(SYMBOL, ';');
  writeLine('<symbol> ; </symbol>');
  writeLine('</classVarDec>');
  tokenizer.advance;
end;

procedure TCompilationEngine.compileSubroutine();
begin
  writeLine('<subroutineDec>');
  // 'constructor' | 'function' | 'method'
  writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>');
  
  
  tokenizer.advance;  // 'void' | type
  if tokenizer.TokenType = ttKeyword then
    writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>')
  else
    writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');


  tokenizer.advance; // subroutineName
  writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');

  tokenizer.advance; // symbol '(' 
  writeLine('<symbol> ( </symbol>');

  compileParameterList;
  // j ai bouffer la parenthese fermer
  writeLine('<symbol> ) </symbol>');

  //je viens de rajouter ca;
  //tokenizer.advance;

  compileSubroutineBody;
  writeLine('</subroutineDec>');
end;

procedure TCompilationEngine.compileSubroutineBody();
begin
  writeLine('<subroutineBody>');
  tokenizer.advance; // Symbol '{'
  writeLine('<symbol> { </symbol>');
  
  // Log the current token type and value
  writeln('Current Token Type: ', Ord(tokenizer.TokenType));
  if tokenizer.TokenType = ttKeyword then
    writeln('Current Keyword: ', tokenizer.KeyWord)
  else if tokenizer.TokenType = ttSymbol then
    writeln('Current Symbol: ', tokenizer.Symbol)
  else if tokenizer.TokenType = ttIdentifier then
    writeln('Current Identifier: ', tokenizer.Identifier)
  else if tokenizer.TokenType = ttIntConst then
    writeln('Current Integer Constant: ', tokenizer.IntVal)
  else if tokenizer.TokenType = ttStringConst then
    writeln('Current String Constant: ', tokenizer.StringVal);

  tokenizer.advance;// ttKeyword
  while tokenizer.TokenType = ttKeyword do
  begin
    // Log the keyword to ensure it is 'var'
    writeln('Current Keyword in Loop: ', tokenizer.KeyWord);

    if tokenizer.KeyWord = 'var' then
      compileVarDec
    else
      break;
  end;

  compileStatements;

  //expect(SYMBOL, '}');
  writeLine('<symbol> } </symbol>');
  writeLine('</subroutineBody>');
  tokenizer.advance;
end;


procedure TCompilationEngine.compileParameterList();
begin
  writeLine('<parameterList>');
  tokenizer.advance;// param or symbol ')'
  while tokenizer.TokenType <> ttSymbol do
  begin
    // type
    if tokenizer.TokenType = ttKeyword then
      writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>')
    else
      writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
    tokenizer.advance;

    // varName
    writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
    tokenizer.advance;

    if tokenizer.TokenType = ttSymbol then
    begin
      if tokenizer.symbol = ',' then
      begin
        writeLine('<symbol> , </symbol>');
        tokenizer.advance;
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
  // deja bouffer dans la compilesubroutinebody
  writeLine('<keyword> var </keyword>');


  tokenizer.advance;//type: int, array ...
  if tokenizer.TokenType = ttKeyword then
    writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>')
  else
    writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');

  tokenizer.advance; // varName
  writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');

  tokenizer.advance;// symbol: ',' ';' ...
  while tokenizer.TokenType = ttSymbol do
  begin
    if tokenizer.symbol = ',' then
    begin
      writeLine('<symbol> , </symbol>');

      tokenizer.advance;// on bouffe un autre identifieur
      writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');

      tokenizer.advance;
    end
    else
      break;
  end;
  // ';' bouffer dans le while
  writeLine('<symbol> ; </symbol>');
  writeLine('</varDec>');


  // a verifier
  tokenizer.advance;
end;

procedure TCompilationEngine.compileStatements();
begin
  writeLine('<statements>');
  // le currentoken qui est verifier en dessous a ete bouffer dans Compilesubroutinebody
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

  // Log the current token type and value after compileStatements
  writeln('Exiting compileStatements - Current Token Type: ', Ord(tokenizer.TokenType));
  case tokenizer.TokenType of
    ttKeyword: writeln('Current Keyword: ', tokenizer.KeyWord);
    ttSymbol: writeln('Current Symbol: ', tokenizer.Symbol);
    ttIdentifier: writeln('Current Identifier: ', tokenizer.Identifier);
    ttIntConst: writeln('Current Integer Constant: ', tokenizer.IntVal);
    ttStringConst: writeln('Current String Constant: ', tokenizer.StringVal);
  else
    writeln('Unknown Token Type');
  end;
end;

procedure TCompilationEngine.compileDo();
begin
  writeLine('<doStatement>');
  writeLine('<keyword> do </keyword>');
  tokenizer.advance;

  // subroutineCall
  writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
  tokenizer.advance;

  //expect(SYMBOL, '(');
  writeLine('<symbol> ( </symbol>');

  compileExpressionList;

  //expect(SYMBOL, ')');
  writeLine('<symbol> ) </symbol>');

  //expect(SYMBOL, ';');
  writeLine('<symbol> ; </symbol>');
  writeLine('</doStatement>');
  tokenizer.advance;
end;

procedure TCompilationEngine.compileLet();
begin
  writeLine('<letStatement>');
  // on a bouffer deja le let dans statement;
  writeLine('<keyword> let </keyword>');

  tokenizer.advance;// varName
  writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');

  tokenizer.advance;// on bouffe soit un = sois un '['
  if tokenizer.symbol = '[' then
  begin
    writeLine('<symbol> [ </symbol>');

    tokenizer.advance;// on bouffe ce qui nous faut pour compileExpression
    compileExpression;

    //expect(SYMBOL, ']');
    writeLine('<symbol> ] </symbol>');
  end;

  //on a bouffer le = au dessus
  writeLine('<symbol> = </symbol>');
  
  tokenizer.advance;// on bouffe ce qui nous faut pour compileExpression
  compileExpression;

  //expect(SYMBOL, ';');
  writeLine('<symbol> ; </symbol>');
  writeLine('</letStatement>');
  tokenizer.advance;
  tokenizer.advance;
  // ajout d un log  dans la fonction compilelet;
  writeln('affichage du current token en sortant pour la derniere fois de la fonction CompileLet ',tokenizer.tokenType);
end;

procedure TCompilationEngine.compileWhile();
begin
  writeLine('<whileStatement>');
  writeLine('<keyword> while </keyword>');
  tokenizer.advance;

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
  tokenizer.advance;
end;

procedure TCompilationEngine.compileReturn();
begin
  writeLine('<returnStatement>');
  writeLine('<keyword> return </keyword>');
  tokenizer.advance;

  if tokenizer.TokenType <> ttSymbol then
    compileExpression;

  //expect(SYMBOL, ';');
  writeLine('<symbol> ; </symbol>');
  writeLine('</returnStatement>');
  tokenizer.advance;
end;

procedure TCompilationEngine.compileIf();
begin
  writeLine('<ifStatement>');
  writeLine('<keyword> if </keyword>');
  tokenizer.advance;

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
    tokenizer.advance;
    writeLine('<keyword> else </keyword>');

    //expect(SYMBOL, '{');
    writeLine('<symbol> { </symbol>');

    compileStatements;

    //expect(SYMBOL, '}');
    writeLine('<symbol> } </symbol>');
  end;

  writeLine('</ifStatement>');
  tokenizer.advance;
end;

procedure TCompilationEngine.compileExpression();
begin
  writeLine('<expression>');
  compileTerm;

  while (tokenizer.TokenType = ttSymbol) and (tokenizer.symbol in ['+', '-', '*', '/', '&', '|', '<', '>', '=']) do
  begin
    writeLine('<symbol> ' + tokenizer.symbol + ' </symbol>');
    tokenizer.advance;
    compileTerm;
  end;

  writeLine('</expression>');
end;

procedure TCompilationEngine.compileTerm();
begin
  writeLine('<term>');

  // Log the current token type before entering the case statement
  writeln('Current Token Type: ', Ord(tokenizer.TokenType));
  // il y avait un advance avant qu on se pose aevc yona serieusemet

  case tokenizer.TokenType of
    ttIntConst:
    begin
      writeln('Matched Token Type: ttIntConst');
      writeLine('<integerConstant> ' + IntToStr(tokenizer.IntVal) + ' </integerConstant>');
      
      tokenizer.advance;// on bouffe pr la suite
    end;
    ttStringConst:
    begin
      writeln('Matched Token Type: ttStringConst');
      writeLine('<stringConstant> ' + tokenizer.StringVal + ' </stringConstant>');

      tokenizer.advance;// on bouffe pr la suite
    end;
    ttKeyword:
    begin
      writeln('Matched Token Type: ttKeyword');
      writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>');

      tokenizer.advance;// on bouffe pr la suite
    end;
    ttIdentifier:
    begin
      writeln('Matched Token Type: ttIdentifier');
      writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');

      tokenizer.advance;// on bouffe pr la suite

      if tokenizer.symbol = '[' then
      begin
        writeLine('<symbol> [ </symbol>');

        tokenizer.advance;//
        compileExpression;
        //expect(SYMBOL, ']');
        writeLine('<symbol> ] </symbol>');
      end
      else if tokenizer.symbol = '(' then
      begin
        writeLine('<symbol> ( </symbol>');
        tokenizer.advance;
        compileExpressionList;
        //expect(SYMBOL, ')');
        writeLine('<symbol> ) </symbol>');
        // viens aussi de rajouter ca
        tokenizer.advance;
      end
      else if tokenizer.symbol = '.' then
      begin
        writeLine('<symbol> . </symbol>');
        tokenizer.advance;
        writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
        tokenizer.advance;
        //expect(SYMBOL, '(');
        writeLine('<symbol> ( </symbol>');
        compileExpressionList;
        //expect(SYMBOL, ')');
        writeLine('<symbol> ) </symbol>');
      end;
    end;
    ttSymbol:
    begin
      writeln('Matched Token Type: ttSymbol');
      if tokenizer.symbol = '(' then
      begin
        writeLine('<symbol> ( </symbol>');
        tokenizer.advance;
        compileExpression;
        //expect(SYMBOL, ')');
        writeLine('<symbol> ) </symbol>');
      end
      else if tokenizer.symbol in ['-', '~'] then
      begin
        writeLine('<symbol> ' + tokenizer.symbol + ' </symbol>');
        tokenizer.advance;
        compileTerm;
      end;
    end;
  else
    writeln('No matching Token Type found');
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
      tokenizer.advance;
      compileExpression;
    end;
  end;

  writeLine('</expressionList>');
end;

end.
